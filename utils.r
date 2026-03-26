library(tidyverse)

pay_project  <-  function(
    .data = basedata,
    .se_tab=sem_eco_tab,
    .year_interval = 2024:2028,
    .yield_matrix = NULL,
    .yield_baserate= 0.0261,
    .yield_serates = c(0,-0.004,-0.002,-0.003),
    .yield_absrates = c(-0.02,0,0,0,0),
    .yield_changerates = c(0,0,0,0,0),
    .area_direction = "inverse",
    .area_matrix = NULL,
    .area_baserate = 0,
    .area_serates = c(0,0,0,0),
    .area_absrates = c(0,0,0,0,0),
    .area_absrates1 = c(0,0,0,0,0),
    .area_absrates2 = c(0,0,0,0,0),
    .area_conschange1=0,
    .area_conschange2=0,
    .area_changerates = c(0,0,0,0,0),
    .year_absrates = c(-2,0,0,0,0),
    .yield_yearrates = c(0,0,0,0,0),
    .area_yearrates = c(0,0,0,0,0),
    .cagr_cuts = c(-Inf,-5,-2.5,0,2.5,5,Inf),
    .cagr_changerates = c(0,.01,0.02,0.03,0.04,0.05),
    .reg_provlist=reg_prov,
    .population = Pop_1625,
    .pastyr_foodcon = foodcon2023,
    .combine_summaries = FALSE,
    .suaproject = TRUE,
    .scenarioname = "scenario1",
    .calam_matrix = NULL,
    .mr =0.65,
    .percap = 120,
    .seeduse = 49.05,
    .processing = 0.04,
    .feeds_waste = 0.065,
    .buffer_days = 60,
    .calamity_loss_percent = 0,
    .calamity_loss_percent_s1 = 0,
    .calamity_loss_percent_s2 = 0,
    .print_important = TRUE,
    .projection_level = c("national","regional","provincial"),
    .change_level=c("national","regional","provincial"),
    .change_coverage = "philippines",
    .include_prov = TRUE,
    .include_reg = TRUE,
    .include_nat = TRUE,
    baseline_include = FALSE,
    aggregate_type = "calendar",
    ...
) 
{
  
  if (.change_level =="regional"){
    
    .data_coverage <- 
      .data |> 
      filter(region == .change_coverage)
    
    
  } else if (.change_level == "provincial") {
    .data_coverage  <- 
      .data |> 
      filter(location == .change_coverage)
  } else {
    .data_coverage <- 
      .data
  }
  
  
  
  
  if (is.null(.se_tab)){
    .se_tab <- tibble(
      sem = factor(c("sem1","sem1","sem2","sem2")),
      eco = factor(c("irrig","rain","irrig","rain")),
      area_se_rates = .area_serates,
      yield_se_rates = .yield_serates
    )
  }
  
  .area_matrix_cons <- tibble(
    sem = c("sem1","sem2"),
    area_conschange = c(.area_conschange1, .area_conschange2)
  )
  
  # area change in abs hectares
  if(is.null(.area_matrix)) 
  {
    #constant area inc/dec every year
    .distributed_area <- 
      .data_coverage %>%filter(var=="area3") %>%  
      group_by(location,type,sem) %>% 
      summarise(value=sum(value,na.rm=TRUE)) %>% 
      mutate(inv_value = if_else(is.infinite(1/value),0,1/value)) %>% 
      group_by(type,sem) %>% 
      add_count(wt=value,name="total") %>% 
      add_count(wt = inv_value,name = "inv_total") %>% 
      ungroup() %>% 
      mutate(weight_dir = value/total,
             weight_inv = inv_value/inv_total,
             area_cons = case_when(
               sem =="sem1"~.area_conschange1,
               sem == "sem2"~.area_conschange2
             ),
             area_add = case_when(.area_direction=="direct"~area_cons*weight_dir,
                                  .area_direction=="inverse"~area_cons*weight_inv)
      )%>%
      ungroup() %>% 
      select(location,type,sem,area_cons,area_add) %>% 
      mutate(eco="irrig") %>% 
      full_join(.data_coverage %>% 
                  distinct(location,type,sem) %>% 
                  mutate(eco="rain",
                         area_cons=0,
                         area_add=0))
  } 
  else
  {
    #specific change every year/sem
    .distributed_area <- 
      .data_coverage %>%filter(var=="area3") %>%  
      group_by(location,type,sem) %>% 
      summarise(value=sum(value,na.rm=TRUE)) %>% 
      mutate(inv_value = if_else(is.infinite(1/value),0,1/value)) %>% 
      group_by(type,sem) %>% 
      add_count(wt=value,name="total") %>% 
      add_count(wt = inv_value,name = "inv_total") %>% 
      ungroup() %>% 
      left_join(.area_matrix %>% 
                  as.data.frame() %>% 
                  rownames_to_column(var="sem") %>% 
                  mutate(sem = case_match(sem,
                                          "Semester 1"~"sem1",
                                          "Semester 2"~"sem2")
                  )) %>% 
      left_join(.area_matrix_cons) %>% 
      mutate(weight_dir = value/total,
             weight_inv = inv_value/inv_total,
             weight = case_when(.area_direction=="direct"~value/total,
                                .area_direction=="inverse"~inv_value/inv_total,
                                .default = value/total)
      )%>%
      rowwise() %>% 
      mutate_at(vars(.year_interval[1]:.year_interval[length(.year_interval)]),
                list(w.change = ~(replace_na(.,area_conschange)* weight))) %>% 
      ungroup() %>% 
      select(location,type,sem,contains("w.change")) %>% 
      pivot_longer(contains("w.change"),
                   names_to = c("year","var"),
                   names_sep = "_",
                   values_to ="area_add") %>% 
      select(location,type,sem,year,area_add) %>% 
      mutate(year = factor(year,levels = .year_interval),
             eco="irrig") %>% 
      full_join(.data_coverage %>% 
                  group_by(location,type,sem) %>% 
                  expand(year = .year_interval) %>% 
                  mutate(year = factor(year,levels =.year_interval),
                         eco="rain",
                         area_add=0))
    
  }
  
  #yield matrix exact growth rates
  
  tryCatch({
    
    if(is.null(.yield_matrix))
    {
      .yield_matrix_absrates <- tibble(year = factor(.year_interval),
                                       yield_matrix_absrates = 0)
    } else {
      
      .yield_matrix_absrates <-   {
        
        yrlen <-  length(.year_interval)
        
        if ("Annual" %in% rownames(.yield_matrix)) {
          .s1 <- .yield_matrix %>%
            as.data.frame() %>%
            pivot_longer(everything(),
                         names_to="year",
                         values_to="yield_matrix_absrates"
            ) %>%
            mutate(eco =NA)
          
        } else {
          .s1 <- .yield_matrix %>%
            as.data.frame() %>%
            rownames_to_column(var="var") %>%
            separate(var, into = c("sem","eco"), sep="-") %>%
            mutate(sem = case_match(sem,
                                    "Semester 1"~"sem1",
                                    "Semester 2"~"sem2"),
                   eco = case_match(eco,
                                    "Irrigated"~"irrig",
                                    "Rainfed"~"rain")
            ) %>%
            pivot_longer(all_of(as.character(.year_interval)),
                         names_to="year",
                         values_to="yield_matrix_absrates"
            )
        }
        
        if(is.na(unique(.s1$eco))[1]) {
          .out <-  .s1 %>%
            select(-eco) %>%
            mutate(yield_matrix_absrates = if_else(
              is.na(yield_matrix_absrates),0,
              as.numeric(yield_matrix_absrates)/100)
            )
        } else
        {
          .out <-  .s1 %>%
            mutate(yield_matrix_absrates = if_else(
              is.na(yield_matrix_absrates),0,
              as.numeric(yield_matrix_absrates)/100)
            )
        }
      }
    }
  },
  error = function(e) {
    stop("Error in yield matrix", e$message)
  }
  )
  
  
  .pop_table <- .population %>% 
    filter(year %in% seq(min(.year_interval) - 1, max(.year_interval)+1)) %>% 
    mutate(year = factor(year))
  
  
  
  cagr_change_rates <- tibble(
    x = .cagr_cuts[.cagr_cuts!=-Inf],
    cagr_grp = cut(x,breaks=.cagr_cuts),
    cagr_crates = .cagr_changerates
  ) %>% 
    select(-x)
  
  
  matrix_rates <- tibble(
    year = factor(.year_interval),
    y_rates = .yield_absrates,
    y_crates = .yield_changerates,
    y_yrates = .yield_yearrates,
    area_rates = .area_absrates,
    area_rates1 = .area_absrates1,
    area_rates2 = .area_absrates2,
    area_crates = .area_changerates,
    area_yrates = .area_yearrates
  )
  
  tryCatch({
    .project_tbl1 <- 
      .data_coverage %>% 
      group_by(location,type,sem,eco,var,value)%>% 
      expand(year = factor(.year_interval)) %>% 
      ungroup() %>% 
      pivot_wider(names_from=var,
                  values_from=value) %>% 
      mutate(base_yieldrate = .yield_baserate,
             base_arearate = .area_baserate,
             cagr_grp = cut(cagr*100,
                            breaks = .cagr_cuts)
      ) %>% 
      left_join(.se_tab) %>% 
      left_join(matrix_rates) %>% 
      left_join(cagr_change_rates) %>% 
      left_join(.distributed_area) %>% 
      left_join(.yield_matrix_absrates)
  },
  error = function(e) {
    stop("Error in project_tbl1 ", e$message)
  }
  )
  
  glimpse(.project_tbl1)
  print(head(.distributed_area %>% filter(type=="natl"),20))
  print(head(.project_tbl1,20))
  
  
  
  .project_tbl <-
    .project_tbl1 %>% 
    mutate(
      final_yieldrate = case_when(
        baseline_include == TRUE & year == min(.year_interval) ~ 0,
        yield_matrix_absrates!=0 ~ yield_matrix_absrates,
        y_rates!=0~y_rates,
        .default = base_yieldrate + yield_se_rates+y_crates+y_yrates+cagr_crates),
      final_arearate = case_when(
        baseline_include == TRUE & year == min(.year_interval) ~ 0,
        sem == "sem1" & area_rates1!=0 ~area_rates1,
        sem == "sem2" & area_rates2!=0 ~area_rates2,
        area_rates!=0~area_rates,
        .default = base_arearate + area_se_rates + area_crates + area_yrates
      )
    ) %>% 
    group_by(location,type,sem,eco) %>% 
    arrange(location,type,sem,eco,year) %>% 
    mutate(
      proj_yield = accumulate(final_yieldrate,
                              ~if_else(is.na(.x),
                                       yield[1] *(1+.y),
                                       .x*(1+.y)
                              ),
                              .init=NA
      ) %>% 
        tail(-1),
      proj_area =
        accumulate(seq_along(area_add),
                    ~(.x+area_add[.y])*(1 + final_arearate[.y]),
                   .init = area3[1]
        )[-1]
      # proj_area = 
      #   # if_else(sum(.$area_add)!=0,
      #   accumulate(area_add,
      #              ~case_when(is.na(.x)~area3[1]+replace_na(.y,0),
      #                         is.na(.y) | .y==0 ~ .x *(1+final_arearate),
      #                         .default =.x+.y),
      #              .init=NA) %>%
      #   tail(-1)
      # accumulate(final_arearate,
      #            ~if_else(is.na(.x),
      #                     area3[1] *(1+.y),
      #                     .x*(1+.y)),
      #            .init=NA) %>%
      #   tail(-1)
      # )
    ) %>% 
    mutate(
      proj_yield = if_else(is.na(proj_yield),0,proj_yield),
      proj_area = if_else(is.na(proj_area),0,proj_area),
      proj_prod = proj_yield*proj_area
    )
  
  print("final prjected table")
  print(.project_tbl %>% 
          select(location:year,proj_yield:proj_prod),
        n=20
  )
  
  
  summaries <-  list()
  
  
  summaries[["provincial"]] <- 
    
    tryCatch({
      .project_tbl %>% 
        select(location:year,proj_yield:proj_prod) %>% 
        filter(type == "prov") %>% 
        full_join(.project_tbl %>% 
                    select(location:year,proj_yield:proj_prod) %>% 
                    filter(type == "prov") %>% 
                    group_by(location,year,sem) %>% 
                    summarise_at(vars(proj_area,proj_prod),
                                 sum,na.rm=TRUE) %>% 
                    mutate(eco = "alleco",
                           type="prov",
                           proj_yield = proj_prod/proj_area)
        ) %>% 
        full_join(.project_tbl %>% 
                    select(location:year,proj_yield:proj_prod) %>% 
                    filter(type == "prov") %>% 
                    group_by(location,year,eco) %>% 
                    summarise_at(vars(proj_area,proj_prod),
                                 sum,na.rm=TRUE) %>% 
                    mutate(sem = "annual",
                           type="prov",
                           proj_yield = proj_prod/proj_area)
        ) %>% 
        full_join(.project_tbl %>% 
                    select(location:year,proj_yield:proj_prod) %>% 
                    filter(type == "prov") %>% 
                    group_by(location,year) %>% 
                    summarise_at(vars(proj_area,proj_prod),
                                 sum,na.rm=TRUE) %>% 
                    mutate(eco = "alleco",
                           sem="annual",
                           type="prov",
                           proj_yield = proj_prod/proj_area)
        ) %>% 
        left_join(.reg_provlist,
                  by = join_by(location ==province)
        ) %>% 
        relocate(region, .before=location) %>% 
        group_by(location,type,sem,eco) %>% 
        arrange(region,location,type,sem,eco,year) %>% 
        ungroup() %>% 
        left_join(.pop_table %>% 
                    filter(type=="prov")) %>% 
        left_join(.pastyr_foodcon %>% 
                    select(location,foodcon)) |> 
        mutate(type  ="prov")
    },
    error = function(e) {
      stop("Error join in Province", e$message)
    }
    )
  
  
  print(summaries[["provincial"]])
  
  
  if(.include_reg == TRUE){
    
    summaries[["regional"]]  <-
      tryCatch(
        summaries[["provincial"]]  %>%
          group_by(region,sem,eco,year) %>%
          summarise_at(vars(proj_area,proj_prod),
                       sum,na.rm=TRUE) %>%
          mutate(proj_yield = proj_prod/proj_area,
                 location = region) %>%
          arrange(region,sem,eco,year) %>%
          ungroup() %>%
          left_join(.pop_table %>% 
                      filter(type=="reg"))%>%
          left_join(.pastyr_foodcon %>%
                      select(location,foodcon)) |> 
          mutate(type="reg"),
        error = function(e) {
          stop("Error join in summary region:", e$message)
        }
      )
    
    print(summaries[["regional"]])
    
  }
  
  
  
  if(.include_nat == TRUE){
    summaries[["national"]]  <- 
      summaries[["provincial"]]  %>% 
      group_by(sem,eco,year) %>% 
      summarise_at(vars(proj_area,proj_prod),
                   sum,na.rm=TRUE) %>% 
      mutate(proj_yield = proj_prod/proj_area) %>% 
      arrange(sem,eco,year) %>% 
      ungroup() %>% 
      mutate(type ="natl",
             region = "Philippines",
             location = "Philippines") %>% 
      left_join(.pop_table %>% 
                  filter(type=="natl") %>% 
                  select(year,population)) %>% 
      left_join(.pastyr_foodcon %>% 
                  filter(type=="natl") %>% 
                  select(type,foodcon))
    
    print("national")
    
  }
  

  if (.combine_summaries==TRUE){
    
    summaries2 <-   
      do.call(rbind,summaries)
    
  } else {
    
    summaries2 = summaries
    
  }
  
  print(summaries2)
  
  
  if (.suaproject==TRUE){
    
    if (.combine_summaries==TRUE){
      
      .suainit <- summaries2 %>% 
        rename(Production=proj_prod,
               Area_Harvested = proj_area,
               Yield =proj_yield) %>% 
        mutate(scenario = .scenarioname)
      
      
      .sem1 <- .suainit %>% 
        filter(sem=="sem1")
      
      .sem2 <- .suainit %>% 
        filter(sem=="sem2")
      
      
      .sua_project <- 
        sua_project(
          .sem1,
          .sem2,
          year_interval = .year_interval,
          calam_matrix =.calam_matrix,
          mr =.mr,
          percap = .percap,
          pop_data = .population,
          seeduse = .seeduse,
          processing = .processing,
          feeds_waste = .feeds_waste,
          buffer_days = .buffer_days,
          calamity_loss_percent= .calamity_loss_percent,
          calamity_loss_percent_s1 =.calamity_loss_percent_s1,
          calamity_loss_percent_s2 = .calamity_loss_percent_s2,
          aggregate_type = aggregate_type)
      
    } else {
      
      .suainit <- lapply(summaries2, 
                         function(x){
                           x %>% 
                             mutate(scenario = .scenarioname)
                         })
      
      .sem1 <-  lapply(.suainit,
                       function(x){
                         x %>% 
                           filter(sem=="sem1")
                       })
      
      .sem2 <-lapply(.suainit,
                     function(x){
                       x %>% 
                         filter(sem=="sem2")
                     })
      
      .sua_project <- Map(sua_project (x,y,
                                       mr =.mr,
                                       percap = .percap,
                                       seeduse = .seeduse,
                                       processing = .processing,
                                       feeds_waste = .feeds_waste,
                                       buffer_days = .buffer_days,
                                       calamity_loss_percent= .calamity_loss_percent,
                                       calamity_loss_percent_s1 =.calamity_loss_percent_s1,
                                       calamity_loss_percent_s2 = .calamity_loss_percent_s2,
                                       aggregate_type = aggregate_type),
                          .sem1,
                          .sem2)
      
    }
    
  } else {
    .sua_project = NULL
  }
  
  print("growth rates")
  
  print(.sua_project|> filter(location =="Philippines") |> select(year,eco,prod_rate))
  
  .argument <- match.call()
  
  argument_list <- list(
    .data = .data,
    .se_tab=.se_tab,
    .year_interval = .year_interval,
    .yield_matrix = .yield_matrix,
    .yield_baserate= .yield_baserate,
    .yield_serates =.yield_serates,
    .yield_absrates = .yield_absrates,
    .yield_changerates = .yield_changerates,
    .area_direction =.area_direction,
    .area_matrix = .area_matrix,
    .area_baserate = .area_baserate,
    .area_serates = .area_serates,
    .area_absrates =.area_absrates,
    .area_absrates1 = .area_absrates1,
    .area_absrates2 = .area_absrates2,
    .area_conschange1=.area_conschange1,
    .area_conschange2=.area_conschange2,
    .area_changerates = .area_changerates,
    .year_absrates =.year_absrates,
    .yield_yearrates = .yield_yearrates,
    .area_yearrates = .area_yearrates,
    .cagr_cuts =.cagr_cuts,
    .cagr_changerates = .cagr_changerates,
    .reg_provlist=.reg_provlist,
    .population = .population,
    .pastyr_foodcon = .pastyr_foodcon,
    .combine_summaries = .combine_summaries,
    .suaproject = .suaproject,
    .scenarioname = .scenarioname,
    .calam_matrix = .calam_matrix,
    .mr =.mr,
    .percap = .percap,
    .seeduse = .seeduse,
    .processing = .processing,
    .feeds_waste = .feeds_waste,
    .buffer_days = .buffer_days,
    .calamity_loss_percent = .calamity_loss_percent,
    .calamity_loss_percent_s1 = .calamity_loss_percent_s1,
    .calamity_loss_percent_s2 = .calamity_loss_percent_s2,
    .print_important = .print_important,
    .projection_level = .projection_level,
    .change_level=.change_level,
    .change_coverage =.change_coverage,
    .include_prov = .include_prov,
    .include_reg = .include_reg,
    .include_nat = .include_nat,
    aggregate_type = aggregate_type
  )
  
  
  return(projected_pay = list(
    summaries = summaries2,
    sua = .sua_project,
    full_projection = .project_tbl,
    arguments = argument_list
  )
  )
  
}



sua_project<-  function(sem1_data,
                        sem2_data,
                        annu_data = NULL,
                        year_interval = 2024:2028,
                        mr =0.654,
                        percap = 120,
                        seeduse = 49.05,
                        processing = 0.04,
                        pop_data = Pop_1625,
                        feeds_waste = 0.065,
                        buffer_days = 60,
                        loss2024 = 495821,
                        calamity_loss_percent = 0,
                        calamity_loss_percent_s1 = 0,
                        calamity_loss_percent_s2 = 0,
                        calamity_loss_yearly_annual = rep(0,length(year_interval)),
                        calamity_loss_yearly_s1 = c(5.544692657775778,rep(0,length(year_interval)-1)),
                        calamity_loss_yearly_s2 =rep(0,length(year_interval)),
                        calam_matrix=NULL,
                        aggregate_type = c("calendar","cropping"),
                        .remove_firstyear = TRUE) {
  
  if(is.null(calam_matrix)){
    calam_tab <- tibble(
      year = factor(year_interval),
      annual = calamity_loss_yearly_annual,
      sem1 = calamity_loss_yearly_s1,
      sem2 = calamity_loss_yearly_s2 
    ) %>% 
      pivot_longer(annual:sem2,
                   names_to = "sem",
                   values_to = "cal_percent_mx")
  } else {
    
    calam_tab <- 
      if(all(c("Semester 1","Semester 2") %in% rownames(calam_matrix))) {
        calam_matrix %>%
          as.data.frame() %>% 
          rownames_to_column(var="sem") %>% 
          mutate(sem = case_match(sem,
                                  "Semester 1"~"sem1",
                                  "Semester 2"~"sem2")) %>% 
          pivot_longer(all_of(as.character(year_interval)),
                       names_to = "year",
                       values_to = "cal_percent_mx") %>% 
          mutate(year = factor(year, levels = year_interval))
      } else if ("ANNUAL" %in% rownames(calam_matrix)) {
        tibble(year=factor(year_interval),
               cal_percent_mx = calam_matrix %>% as.vector())
      } else {
        tibble(sem = c("sem1","sem2"),
               cal_percent_mx = calam_matrix %>% as.vector())
      }
  }
  
  .cropyear_b <-  seq(min(year_interval)-1, max(year_interval)+1)
  
  .cropyear <-  paste(head(.cropyear_b, -1), "-", tail(.cropyear_b, -1), sep = "")
  
  param_tab <-  tibble(year = if (aggregate_type=="calendar") {
    factor(year_interval) } else {
      factor(.cropyear)
    },
    .mr = mr,
    .percap = percap,
    .seeduse =seeduse,
    .processing = processing,
    .feeds_waste = feeds_waste,
    .buffer_days = buffer_days
  )
  
  
  if (is.null(annu_data)) {
    
    .pop_pastyear <-  pop_data |> 
      filter(year == min(year_interval)-1) |> 
      select(location,year, population_c = population)
    
    print("popn last year")
    print(.pop_pastyear)
    
    
    .sem1 <- 
      sem1_data %>% 
      left_join(calam_tab) %>% 
      mutate(
        # Production = case_when(year==2024 & location=="Philippines" & eco=="alleco"~8794188.794,
        #                        year==2024 & location=="Philippines" & eco=="irrig"~7130422.278,
        #                        year==2024 & location=="Philippines" & eco=="rain"~1663766.526,
        #                        .default = Production),
        # Area_Harvested =case_when(year==2024 & location=="Philippines" & eco=="alleco"~2065191.32,
        #                           year==2024 & location=="Philippines" & eco=="irrig"~1541114.41,
        #                           year==2024 & location=="Philippines" & eco=="rain"~524076.91,
        #                           .default = Area_Harvested),
        # Yield = case_when(year==2024 & location=="Philippines" & eco=="alleco"~4.130543765,
        #                   year==2024 & location=="Philippines" & eco=="irrig"~4.487992303,
        #                   year==2024 & location=="Philippines" & eco=="rain"~3.079421167,
        #                   .default = Yield),
        cal_percent = case_when(cal_percent_mx!=0~cal_percent_mx,
                                calamity_loss_percent_s1!=0~calamity_loss_percent_s1,
                                .default =calamity_loss_percent),
        calamity = Production * (cal_percent/100),
        # aProduction = case_when(year==2024 & location=="Philippines" & eco=="alleco"~8530363.13,
        #                        year==2024 & location=="Philippines" & eco=="irrig"~6916509.61,
        #                        year==2024 & location=="Philippines" & eco=="rain"~1613853.53,
        #                        .default = Production - calamity),
        aProduction = Production - calamity,
        crop_year = paste0(as.numeric(as.character(year))-1,"-",as.numeric(as.character(year))
        )) |> 
      left_join(.pop_pastyear) |> 
      group_by(type,location,eco) |> 
      arrange(type,location,eco,year) |> 
      mutate(population_b = if_else(year==first(year),population_c,lag(population,1)),
             population_s1 = population,
             population_s2 = population_b
      ) |> 
      select(-population_c)
    
    .sem2 <- 
      sem2_data %>%
      left_join(calam_tab) %>% 
      mutate(
        # Production = case_when(year==2024 & location=="Philippines" & eco=="alleco"~11099378.35,
        #                        year==2024 & location=="Philippines" & eco=="irrig"~8047438.144,
        #                        year==2024 & location=="Philippines" & eco=="rain"~3051939.175,
        #                        .default = Production),
        # Area_Harvested =case_when(year==2024 & location=="Philippines" & eco=="alleco"~2592502,
        #                           year==2024 & location=="Philippines" & eco=="irrig"~1750893,
        #                           year==2024 & location=="Philippines" & eco=="rain"~841609,
        #                           .default = Area_Harvested),
        # Yield = case_when(year==2024 & location=="Philippines" & eco=="alleco"~4.152898243,
        #                   year==2024 & location=="Philippines" & eco=="irrig"~4.458304991,
        #                   year==2024 & location=="Philippines" & eco=="rain"~3.517525359,
        #                   .default = Yield),
        cal_percent = case_when(cal_percent_mx!=0~cal_percent_mx,
                                calamity_loss_percent_s2!=0~calamity_loss_percent_s2,
                                .default =calamity_loss_percent),
        calamity = Production * (cal_percent/100),
        # aProduction = case_when(year==2024 & location=="Philippines" & eco=="alleco"~10766397,
        #                         year==2024 & location=="Philippines" & eco=="irrig"~7806015,
        #                         year==2024 & location=="Philippines" & eco=="rain"~2960381,
        #                         .default = Production - calamity),
        aProduction = Production - calamity,
        crop_year = paste0(as.numeric(as.character(year)),"-",as.numeric(as.character(year))+1),
        population_b = population
      ) |> 
      group_by(type,location,eco) |> 
      arrange(type,location,eco,year) |> 
      mutate(population_s1 = lead(population,1),
             population_s2 = population)
    
    
    if (aggregate_type=="calendar") {
      .all <- 
        .sem1 %>% 
        full_join(.sem2) %>% 
        ungroup() %>% 
        select(-sem) %>% 
        group_by(type,location,eco,year,population,foodcon) %>% 
        summarise_at(vars(Area_Harvested,Production,calamity,aProduction), sum, na.rm=TRUE) %>% 
        mutate(Yield = aProduction/Area_Harvested)
      
    } else {
      
      .all <- 
        .sem1 %>% 
        full_join(.sem2) %>% 
        ungroup() %>% 
        select(-sem) %>% 
        group_by(type,location,eco,crop_year,population_b,population_s1,population_s2,foodcon) %>% 
        summarise_at(vars(Area_Harvested,Production,calamity,aProduction), sum, na.rm=TRUE) %>% 
        mutate(Yield = aProduction/Area_Harvested) |> 
        rename(year = crop_year,
               population = population_b)
      
      if(.remove_firstyear == TRUE) {
        .all <- .all |> 
          group_by(type,location,eco) |> 
          arrange(type,location,eco,year) |> 
          filter(year != first(year))
        
      }
    }
    
  } else {
    
    .all <-  annu_data |> 
      mutate(sem = "annual") |> 
      left_join(calam_tab) |> 
      mutate(
        # Production = case_when(year==2024 & location=="Philippines" & eco=="alleco"~19893567.14,
        #                        year==2024 & location=="Philippines" & eco=="irrig"~15177860.42,
        #                        year==2024 & location=="Philippines" & eco=="rain"~4715705.701,
        #                        .default = Production),
        # Area_Harvested =case_when(year==2024 & location=="Philippines" & eco=="alleco"~4657693.32,
        #                           year==2024 & location=="Philippines" & eco=="irrig"~3292007.41,
        #                           year==2024 & location=="Philippines" & eco=="rain"~1365685.91,
        #                           .default = Area_Harvested),
        # Yield = case_when(year==2024 & location=="Philippines" & eco=="alleco"~4.142986411,
        #                   year==2024 & location=="Philippines" & eco=="irrig"~4.472202755,
        #                   year==2024 & location=="Philippines" & eco=="rain"~3.349404498,
        #                   .default = Yield),
        cal_percent = case_when(cal_percent_mx!=0~cal_percent_mx,
                                .default =calamity_loss_percent),
        calamity = Production * (cal_percent/100),
        # aProduction = case_when(year==2024 & location=="Philippines" & eco=="alleco"~19296760.13,
        #                         year==2024 & location=="Philippines" & eco=="irrig"~14722524.61,
        #                         year==2024 & location=="Philippines" & eco=="rain"~4574234.53,
        #                         .default = Production - calamity),
        aProduction = Production - calamity,
        Yield = aProduction/Area_Harvested)
    
  }
  
  
  
  
  .all %>% 
    left_join(param_tab) %>% 
    mutate(Production_rice = aProduction*.mr,
           Food = if(aggregate_type=="calendar"){
             (population*percap)/1000
             } else {
               ((population_s1 +population_s2)*(.percap/2))/1000
               },
           Seeds =(Area_Harvested*.seeduse)/1000, #seed use per ha
           Processing = Production_rice*.processing, #processing constant
           Feeds_waste= Production_rice*.feeds_waste, # feeds and waste constant
           Food = if_else(year==2024 & location =="Philippines",
                          sum(c(Production_rice,2027180,4797762.599))-2155924.7- sum(c(Seeds,Processing,Feeds_waste)),
                          Food),
           Food_req = Food/365, #constant days in a year
           buffer = Food_req*.buffer_days ,#60 days or 2 months buffer
           # totEndingStock = if_else(year==2024 & location =="Philippines",sum(c(Production_rice,2027180,4797762.599))- sum(c(Food,Seeds,Processing,Feeds_waste)),buffer)
           totEndingStock = if_else(year==2024 & location =="Philippines",2155924.7,buffer),
# 
#            sum(c(Production_rice,2027180,4630928.15)) - sum(c(Food,Seeds,Processing,Feeds_waste)),
#            buffer)
           )%>%
    #get beginning stock from 2023 ending inventrory
    left_join(.sem1 %>% 
                mutate(aggtype = aggregate_type,
                       year = if_else(aggtype=="calendar",year,crop_year),
                       population = if_else(aggtype=="calendar",population,population_b),
                       population_s1 = if_else(aggtype=="calendar",population,population_s1),
                       population_s2 = if_else(aggtype=="calendar",population,population_s2),
                ) |> 
                select(year,
                       eco,
                       type,
                       location,
                       sem1_area = Area_Harvested,
                       sem1_population = population,
                       sem1_pops1 = population_s1,
                       sem1_pops2 = population_s2,
                       sem1_Production = Production,
                       sem1_calamity = calamity,
                       sem1_aProduction = aProduction)) %>% 
    left_join(.sem2 %>%
                mutate(aggtype = aggregate_type,
                       year = if_else(aggtype=="calendar",year,crop_year),
                       population = if_else(aggtype=="calendar",population,population_b),
                       population_s1 = if_else(aggtype=="calendar",population,population_s1),
                       population_s2 = if_else(aggtype=="calendar",population,population_s2),
                ) |> 
                select(year,
                       eco,
                       type,
                       location,
                       sem2_area = Area_Harvested,
                       sem2_population = population,
                       sem2_pops1 = population_s1,
                       sem2_pops2 = population_s2,
                       sem2_Production = Production,
                       sem2_calamity = calamity,
                       sem2_aProduction = aProduction)
    ) %>% 
    relocate(sem1_area:sem2_aProduction,.after = year) %>% 
    group_by(type,location) %>% 
    mutate(
      begStock = case_when(
        year==2024 & location =="Philippines"~2027180,
        year==2024~(foodcon/365)*buffer_days,
        year==first(year)~(foodcon/365)*buffer_days,
        .default = lag(totEndingStock,1)
      ),
      prod_rate = (Production_rice - lag(Production_rice,1))/Production_rice, #temp
      ) %>% 
    rowwise() %>% 
    mutate(
      #beg inventory 2023 pop and 2023 annual per cap
      `Imports` = if_else(year==2024 & location =="Philippines",4797762.599,
                          sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),na.rm=TRUE) -Production_rice-begStock
                          ),
      # totEndingStock = if_else(year==2024 & location =="Philippines",
      #                          sum(c(Production_rice,begStock,Imports)) - sum(c(Food,Seeds,Processing,Feeds_waste)),
      #                          totEndingStock),
      `SUPPLY` = NA_real_ ,
        
      `UTILIZATION` = NA_real_,
      SuffRatio = (Production_rice/(sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                        na.rm=TRUE)-begStock))*100
    ) %>% 
    select(-foodcon) |> 
    relocate(prod_rate, .after=Production_rice)
}


sua_format <- function(
    .sua,
    .location,
    .eco="alleco") {
  
  .var_order <- c("SUPPLY", 
                  "Beginning Inventory",
                  "Semester 1 Area",
                  "Semester 1 Base Palay Production",
                  "Semester 1 Calamity Loss",
                  "Semester 1 Palay Production",
                  "Semester 2 Area",
                  "Semester 2 Base Palay Production",
                  "Semester 2 Calamity Loss",
                  "Semester 2 Palay Production",
                  "Area", 
                  "Yield",
                  "Base Palay Production",
                  "Total Calamity loss",
                  "Palay Production",
                  "Milled Rice Production",  
                  "Imports (Exports)",
                  "UTILIZATION",
                  "Population",
                  "Per Capita Rice Consumption",
                  "Food disposable",
                  "Seeds",
                  "Processing", 
                  "Feeds and wastes",
                  "Ending Inventory and Buffer stock",
                  "Buffer stock",
                  "Derived Daily Food Requirement",
                  "Self-Sufficiency Ratio")
  
  .result <- list()
  
  .result[[".alltab"]] <- 
    .sua %>% 
    filter(location == .location) %>% 
    filter(eco==.eco) %>%  
    ungroup() %>% 
    select(-c(type,location)) %>% 
    pivot_longer(sem1_area:SuffRatio,
                 names_to="variables",
                 values_to="value") %>% 
    mutate(value = case_when(variables %in% c("Yield","SuffRatio") ~round(value,3),
                             .default = value),
    ) %>% 
    pivot_wider(names_from = year,
                values_from = value) %>% 
    mutate(variables = case_match(variables,
                                  "sem1_area" ~ "Semester 1 Area",
                                  "sem1_Production" ~ "Semester 1 Base Palay Production",
                                  "sem1_calamity" ~ "Semester 1 Calamity Loss",
                                  "sem1_aProduction" ~ "Semester 1 Palay Production",
                                  "sem2_area" ~ "Semester 2 Area",
                                  "sem2_Production" ~ "Semester 2 Base Palay Production",
                                  "sem2_calamity" ~ "Semester 2 Calamity Loss",
                                  "sem2_aProduction" ~ "Semester 2 Palay Production",
                                  "Production"~"Base Palay Production",
                                  "Area_Harvested"~"Area",
                                  "aProduction" ~"Palay Production",
                                  "calamity"~"Total Calamity loss",
                                  "Production_rice"~"Milled Rice Production",
                                  "PerCap"~"Per Capita Rice Consumption",     
                                  "Food"~"Food disposable",
                                  "Imports"~"Imports (Exports)",
                                  "Feeds_waste"~"Feeds and wastes",
                                  "Food_req"~"Derived Daily Food Requirement",
                                  "totEndingStock"~"Ending Inventory and Buffer stock",
                                  "buffer"~"Buffer stock",
                                  "begStock" ~"Beginning Inventory",
                                  "SuffRatio"~"Self-Sufficiency Ratio",
                                  "SUPPLY"~"SUPPLY",
                                  "UTILIZATION"~"UTILIZATION",
                                  .default = str_to_title(variables)
    )) %>% 
    mutate(variables = factor(variables,
                              levels =.var_order,
                              ordered = TRUE)) %>% 
    arrange(variables) %>% 
    select(-eco) %>% 
    left_join(sua_hist %>% distinct(variables,unit) %>%
                filter(unit!="gm/day")) %>%
    mutate(unit = case_when(variables=="Buffer stock"~"MT",
                            str_detect(variables,"Area")~"HA",
                            str_detect(variables,"Production|Calamity")~"MT",
                            .default=unit)
    )%>%
    relocate(unit,.after="variables")
  
  
  .result[[".semdata1"]] <-  .result[[".alltab"]] %>% 
    filter(variables %in% c("Semester 1 Area",
                            "Semester 1 Base Palay Production",
                            "Semester 1 Calamity Loss",
                            "Semester 1 Palay Production")) %>% 
    mutate_if(is.numeric,
              ~round(./1000,digits=0)) %>% 
    mutate(unit = case_when(
      str_detect(variables,"Area")~"1000 HA",
      .default = "1000 MT"),
      variables = str_remove(variables,"Semester 1")
    )
  
  .result[[".semdata2"]] <-  .result[[".alltab"]] %>% 
    filter(variables %in% c("Semester 2 Area",
                            "Semester 2 Base Palay Production",
                            "Semester 2 Calamity Loss",
                            "Semester 2 Palay Production")) %>% 
    mutate_if(is.numeric,
              ~round(./1000,digits=0)) %>% 
    mutate(unit = case_when(
      str_detect(variables,"Area")~"1000 HA",
      .default = "1000 MT"),
      variables = str_remove(variables,"Semester 2")
    )
  
  .result[[".annual"]] <-  .result[[".alltab"]] %>% 
    filter(variables %in% c("Area",
                            "Base Palay Production",
                            "Total Calamity loss",
                            "Palay Production")) %>% 
    mutate_if(is.numeric,
              ~round(./1000,digits=0)) %>% 
    mutate(unit = case_when(
      str_detect(variables,"Area")~"1000 HA",
      .default = "1000 MT")
    )
  
  .result[[".supply"]] <-  .result[[".alltab"]] %>% 
    filter(variables %in% c("SUPPLY",
                            "Beginning Inventory",
                            "Area", 
                            "Yield",
                            "Base Palay Production",
                            "Palay Production",
                            "Total Calamity loss",
                            "Milled Rice Production")) %>% 
    mutate_at(vars(starts_with("2")),
              ~ case_when(variables=="SUPPLY"~" ",
                          variables=="Yield"~as.character(round(.,2)),
                          .default = format(round(./1000,digits=0), big.mark=","))
    ) %>% 
    mutate(unit = case_when(
      variables == "Area" ~"HA",
      variables == "Yield"~"MT per HA",
      .default = "1000 MT"
    )
    )
  
  
  .result[[".imports"]] <- .result[[".alltab"]] %>% 
    filter(variables == "Imports (Exports)") %>% 
    mutate_at(vars(starts_with("2")),
              ~case_when(.<=0~paste0("(",format(round(./1000,digits=0), big.mark=","),")"),
                         .default = format(round(./1000,digits=0), big.mark=",")
              )
    ) %>% 
    mutate(unit = "1000 MT"
    )
  
  .result[[".demand"]] <-  .result[[".alltab"]] %>% 
    filter(variables %in% c("UTILIZATION",
                            "Population",
                            "Per Capita Rice Consumption",
                            "Food disposable",
                            "Seeds",
                            "Processing", 
                            "Feeds and wastes",
                            "Ending Inventory and Buffer stock",
                            "Derived Daily Food Requirement")) %>% 
    mutate_at(vars(starts_with("2")),
              ~ case_when(variables %in% c("Per Capita Rice Consumption",
                                           "Derived Daily Food Requirement")~.,
                          variables == "Population"~ round(./1000000,digits=0),
                          .default = round(./1000,digits=0))
    ) %>% 
    mutate(unit = case_when(
      str_detect(variables,"Population")~"million",
      variables == "Per Capita Rice Consumption" ~"kg/yr",
      variables == "Derived Daily Food Requirement" ~"MT",
      .default = "1000 MT"
    )
    ) 
  
  .result[[".ssr"]] <- .result[[".alltab"]] %>% 
    filter(variables == "Self-Sufficiency Ratio") %>% 
    mutate_if(is.numeric,
              ~round(.,digits=0))
  
  return(.result)
  
  
}


clean_location <-  function(variable){
  case_match(
    str_to_lower(str_squish(variable)),
    c("region i","ilocos","region 1","ilocos region", "region i (ilocos region)","region 1 (ilocos region)") ~ "Region I (Ilocos Region)",
    c("region ii","region 2","cagayan valley", "region ii (cagayan valley)","region 2 (cagayan valley)") ~ "Region II (Cagayan Valley)",
    c("region iii","region 3","central luzon", "region iii (central luzon)","region 3 (central luzon)") ~ "Region III (Central Luzon)",
    c("region iv-a","southern tagalog","region 4a","calabarzon", "region iv-a (calabarzon)","region 4-a (calabarzon)") ~ "Region IV-A (CALABARZON)",
    c("region v","bicol","region 5","bicol region", "region v (bicol region)","region 5 (bicol region)" ) ~ "Region V (Bicol Region)",
    c("region vi","region 6","western visayas", "region vi (western visayas)", "region 6 (western visayas)") ~ "Region VI (Western Visayas)",
    c("region vii","region 7","central visayas", "region vii (central visayas)", "region 7 (central visayas)") ~ "Region VII (Central Visayas)",
    c("region viii","region 8","eastern visayas", "region viii (eastern visayas)","region 8 (eastern visayas)") ~ "Region VIII (Eastern Visayas)",
    c("region ix","region 9","zamboanga peninsula", "region ix (zamboanga peninsula)", "region 9 (zamboanga peninsula)") ~ "Region IX (Zamboanga Peninsula)",
    c("region x","region 10","northern mindanao", "region x (northern mindanao)","region 10 (northern mindanao)") ~ "Region X (Northern Mindanao)",
    c("region xi","region 11","davao region", "region xi (davao region)","region 11 (davao region)") ~ "Region XI (Davao Region)",
    c("region xii","region 12","soccsksargen", "region xii (soccsksargen)","region 12 (soccsksargen)") ~ "Region XII (SOCCSKSARGEN)",
    c("region xiii","region 13","caraga", "region xiii (caraga)","region 13 (caraga region)","REGION XIII (Caraga) ") ~ "Region XIII (Caraga)",
    c("region iv-b","mimaropa", "region iv-b (mimaropa)","mimaropa region","region 4-b (mimaropa region)" ) ~ "MIMAROPA Region",
    c("cordillera administrative region (car)", "car","cordillera administrative region") ~ "Cordillera Administrative Region (CAR)",
    c("autonomous region in muslim mindanao (armm)","armm","barmm","bangsamoro autonomous region in muslim mindanao (barmm)", "bangsamoro autonomous region in muslim mindanao") ~ "Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)",
    c("city of davao","davao city")~"davao city",
    c("cotabato (north cotabato)","north cotabato")~"north cotabato",
    c("compostela valley","davao de oro","davao de oro (compostela valley)")~"davao de oro (compostela valley)",
    c("western samar","samar","samar (western samar")~"samar (western samar)",
    c("maguindanao (excluding cotabato city)") ~ "maguindanao",
    c("basilan (excluding city of isabela)")~"basilan",
    c("ncr")~"national capital region (ncr)",
    c("agusan norte")~"agusan del norte",
    c("agusan sur")~"agusan del sur",
    c("davao norte")~"davao del norte",
    c("davao sur")~"davao del sur",
    c("lanao norte")~"lanao del norte",
    c("lanao sur")~"lanao del sur",
    c("mt. province")~"mountain province",
    c("mindoro occidental") ~"occidental mindoro",
    c("mindoro oriental") ~"oriental mindoro",
    c("surigao norte")~"surigao del norte",
    c("surigao sur")~"surigao del sur",
    c("zamboanga norte")~"zamboanga del norte",
    c("zamboanga sur")~"zamboanga del sur",
    .default = str_to_lower(variable)
  )
}



combine_regions <-  function(path, sua_default,reg_prov) {
  # List all .rds files in the folder and subfolders
  rds_files <- list.files(path = path, 
                          pattern = "\\.rds$", 
                          full.names = TRUE, 
                          recursive = TRUE)
  
  sua_regs <- lapply(rds_files, function(file) {
    readRDS(file)
  })
  
  names(sua_regs) <- basename(rds_files) %>% gsub("\\.rds$", "", .)
  
  extract_dataframes_named_data1 <- function(obj,collected_data = list()) {
    if (is.list(obj)) {
      for (name in names(obj)) {
        if (name == "sua" && is.data.frame(obj[[name]])) {
          # Collect the data frame if it is named "data"
          collected_data <- append(collected_data, list(obj[[name]]))
        } else if (is.list(obj[[name]])) {
          # Recurse into nested lists
          collected_data <- extract_dataframes_named_data1(obj[[name]], collected_data)
        }
      }
    }
    
    collected_data
    
    
  }
  
  
  extract_dataframes_named_data2 <- function(obj,collected_data = list()) {
    if (is.list(obj)) {
      for (name in names(obj)) {
        if (name == "full_projection" && is.data.frame(obj[[name]])) {
          # Collect the data frame if it is named "data"
          collected_data <- append(collected_data, list(obj[[name]]))
        } else if (is.list(obj[[name]])) {
          # Recurse into nested lists
          collected_data <- extract_dataframes_named_data2(obj[[name]], collected_data)
        }
      }
    }
    
    collected_data
    
    
  }

  
  mergedata <-  extract_dataframes_named_data1(sua_regs) |> 
    bind_rows()
  
  
  mergedata2 <-  extract_dataframes_named_data2(sua_regs) |> 
    bind_rows()
  
  sua_prov <- 
    sua_default[["sua"]] |> 
    filter(type == "prov") |>
    anti_join(mergedata |> 
                filter(type=="prov"),
              by = c("type","location","eco","year")) |> 
    full_join(mergedata |> 
                filter(type=="prov")) |> 
    left_join(reg_prov,
              by = c("location" = 'province'))
  
  
  sua_out <-  list()
  
  sua_out[["sua"]] <- 
    sua_prov |> 
    full_join(sua_prov |> 
                group_by(region,eco,year) |> 
                summarise_at(vars(sem1_area:aProduction,Production_rice:Feeds_waste,buffer:UTILIZATION),
                             sum,
                             na.rm=TRUE) |> 
                left_join(sua_prov |> 
                            group_by(region,eco,year) |> 
                            summarise_at(vars(.mr:.buffer_days,Food_req),
                                         mean,
                                         na.rm=TRUE)
                ) |> 
                ungroup() |> 
                mutate(type = "reg",
                       Yield = aProduction/Area_Harvested,
                       location = as.character(region)) |> 
                rowwise() |> 
                mutate(
                  `Imports` = sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                  na.rm=TRUE) -Production_rice-begStock,
                  `SUPPLY` = NA_real_ ,
                  `UTILIZATION` = NA_real_,
                  SuffRatio = (Production_rice/(sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                                    na.rm=TRUE)-begStock))*100
                ) |> 
                select(-region)
    ) |> 
    full_join(sua_prov |> 
                group_by(eco,year) |> 
                summarise_at(vars(sem1_area:aProduction,Production_rice:Feeds_waste,buffer:UTILIZATION),
                             sum,
                             na.rm=TRUE) |> 
                left_join(sua_prov |> 
                            group_by(eco,year) |> 
                            summarise_at(vars(.mr:.buffer_days,Food_req),
                                         mean,
                                         na.rm=TRUE)
                ) |> 
                ungroup() |> 
                mutate(type = "natl",
                       Yield = aProduction/Area_Harvested,
                       location ="Philippines") |> 
                rowwise() |> 
                mutate(
                  `Imports` = sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                  na.rm=TRUE) -Production_rice-begStock,
                  `SUPPLY` = NA_real_ ,
                  `UTILIZATION` = NA_real_,
                  SuffRatio = (Production_rice/(sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                                    na.rm=TRUE)-begStock))*100
                )
    ) |> 
    select(-region)
    
  
  sua_fp <- 
    sua_default[["full_projection"]] |> 
    filter(type == "prov") |>
    anti_join(mergedata2 |> 
                filter(type=="prov"),
              by = c("type","location","eco","year")) |> 
    full_join(mergedata2 |> 
                filter(type=="prov")) |> 
    left_join(reg_prov,
              by = c("location" = 'province'))
  
  
  sua_out[["full_projection"]] <- 
    sua_fp |> 
    full_join(sua_fp |> 
                mutate(prod = area3*yield) |> 
                group_by(region,sem,eco,year) |> 
                summarise_at(vars(area3,prod,proj_area,proj_prod),
                             sum,
                             na.rm=TRUE) |> 
                left_join(sua_fp |> 
                            group_by(region,sem,eco,year) |> 
                            summarise_at(vars(cagr,base_yieldrate,base_arearate,area_se_rates:final_arearate),
                                         mean,
                                         na.rm=TRUE)
                ) |> 
                ungroup() |> 
                mutate(type = "reg",
                       yield = prod/area3,
                       proj_yield = proj_prod/proj_area,
                       location = as.character(region)) |> 
                select(-region)
    ) |> 
    full_join(sua_fp |> 
                mutate(prod = area3*yield) |> 
                group_by(sem,eco,year) |> 
                summarise_at(vars(area3,prod,proj_area,proj_prod),
                             sum,
                             na.rm=TRUE) |> 
                left_join(sua_fp |> 
                            group_by(sem,eco,year) |> 
                            summarise_at(vars(cagr,base_yieldrate,base_arearate,area_se_rates:final_arearate),
                                         mean,
                                         na.rm=TRUE)
                ) |> 
                ungroup() |> 
                mutate(type = "natl",
                       yield = prod/area3,
                       proj_yield = proj_prod/proj_area,
                       location ="Philippines")
    )
  
  return(sua_out)

  
  
}

import_fromDrive <- function(linkid, filetype = c("csv","xlsx"), sheet = 1){
  
  .pkgs <-  c("tidyverse","googledrive", "openxlsx")
  
  missing_packages <- .pkgs[!sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)]
  
  
  if (length(missing_packages) > 0) {
    stop("The following packages are not loaded: ", paste(missing_packages, collapse = ", "))
  }
  message("All packages are loaded.")
  
  message("Fetching data from Google Drive. GoogleDrive library might request access to google account for first run")
  
  .data_id <-  as_id(linkid)
  .data_meta <-  drive_get(.data_id)
  .data_name <-  .data_meta$name
  
  message("Download start. Temporary saving to working directory")
  
  #download
  drive_download(.data_id,overwrite = TRUE)
  
  message("Importing to R using openxlsx")
  
  #import
  
  if (filetype =="xlsx"){
    .data_out <- read.xlsx(.data_name, sheet)
  } else {
    .data_out <- read_csv(.data_name)
  }
  
  message("File uploaded.")
  
  message("Removing temporary file from working directory")
  #remove from local
  file.remove(.data_name)
  
  return(.data_out)
  
}


sua_format_ensemb <-  function(
    .ensemb_sua,
    .location,
    .eco ="alleco"
) {
  .ensemb_var_order <- c("SUPPLY", 
                         "Beginning Inventory",
                         "Area", 
                         "Yield",
                         "Base Palay Production",
                         "Total Calamity loss",
                         "Palay Production",
                         "Milled Rice Production",  
                         "Imports (Exports)",
                         "UTILIZATION",
                         "Population",
                         "Per Capita Rice Consumption",
                         "Food disposable",
                         "Seeds",
                         "Processing", 
                         "Feeds and wastes",
                         "Ending Inventory and Buffer stock",
                         "Buffer stock",
                         "Derived Daily Food Requirement",
                         "Self-Sufficiency Ratio")
  
  
  .result <- list()
  
  .result[[".alltab"]] <- 
    .ensemb_sua |> 
    relocate(type,eco,sem, .before = year) |> 
    filter(location == .location) |> 
    filter(eco == .eco) |> 
    pivot_longer(Production:SuffRatio,
                 names_to = "variables",
                 values_to = 'value') |> 
    mutate(value = case_when(variables %in% c("Yield","SuffRatio") ~round(value,3),
                             .default = value)) |> 
    pivot_wider(names_from = year,
                values_from = value) |> 
    mutate(variables = case_match(variables,
                                  "Production"~"Base Palay Production",
                                  "Area_Harvested"~"Area",
                                  "aProduction" ~"Palay Production",
                                  "calamity"~"Total Calamity loss",
                                  "Production_rice"~"Milled Rice Production",
                                  "PerCap"~"Per Capita Rice Consumption",     
                                  "Food"~"Food disposable",
                                  "Imports"~"Imports (Exports)",
                                  "Feeds_waste"~"Feeds and wastes",
                                  "Food_req"~"Derived Daily Food Requirement",
                                  "totEndingStock"~"Ending Inventory and Buffer stock",
                                  "buffer"~"Buffer stock",
                                  "begStock" ~"Beginning Inventory",
                                  "SuffRatio"~"Self-Sufficiency Ratio",
                                  "SUPPLY"~"SUPPLY",
                                  "UTILIZATION"~"UTILIZATION",
                                  .default = str_to_title(variables)
    )) %>% 
    mutate(variables = factor(variables,
                              levels =.ensemb_var_order,
                              ordered = TRUE)) |> 
    arrange(variables) |> 
    select(-eco) %>% 
    left_join(sua_hist %>% distinct(variables,unit) %>%
                filter(unit!="gm/day")) %>%
    mutate(unit = case_when(variables=="Buffer stock"~"MT",
                            str_detect(variables,"Area")~"HA",
                            str_detect(variables,"Production|Calamity")~"MT",
                            .default=unit)
    )%>%
    relocate(unit,.after="variables") |> 
    ungroup() |> 
    select(-c(location,type,sem))
  
  
  
  
  
  .result[[".supply"]] <-  .result[[".alltab"]] %>%
    filter(variables %in% c("SUPPLY",
                            "Beginning Inventory",
                            "Area",
                            "Yield",
                            "Base Palay Production",
                            "Palay Production",
                            "Total Calamity loss",
                            "Milled Rice Production")) %>%
    mutate_at(vars(`2024`:`2028`),
              ~ case_when(variables=="SUPPLY"~" ",
                          variables=="Yield"~as.character(round(.,2)),
                          .default = format(round(./1000,digits=0), big.mark=","))
    ) %>%
    mutate(unit = case_when(
      variables == "Area" ~"HA",
      variables == "Yield"~"MT per HA",
      .default = "1000 MT"
    )
    )
  
  
  .result[[".imports"]] <- .result[[".alltab"]] %>%
    filter(variables == "Imports (Exports)") %>%
    mutate_at(vars(`2024`:`2028`),
              ~case_when(.<=0~paste0("(",format(round(./1000,digits=0), big.mark=","),")"),
                         .default = format(round(./1000,digits=0), big.mark=",")
              )
    ) %>%
    mutate(unit = "1000 MT"
    )
  
  .result[[".demand"]] <-  .result[[".alltab"]] %>%
    filter(variables %in% c("UTILIZATION",
                            "Population",
                            "Per Capita Rice Consumption",
                            "Food disposable",
                            "Seeds",
                            "Processing",
                            "Feeds and wastes",
                            "Ending Inventory and Buffer stock",
                            "Derived Daily Food Requirement")) %>%
    mutate_at(vars(`2024`:`2028`),
              ~ case_when(variables %in% c("Per Capita Rice Consumption",
                                           "Derived Daily Food Requirement")~.,
                          variables == "Population"~ round(./1000000,digits=0),
                          .default = round(./1000,digits=0))
    ) %>%
    mutate(unit = case_when(
      str_detect(variables,"Population")~"million",
      variables == "Per Capita Rice Consumption" ~"kg/yr",
      variables == "Derived Daily Food Requirement" ~"MT",
      .default = "1000 MT"
    )
    )
  
  .result[[".ssr"]] <- .result[[".alltab"]] %>%
    filter(variables == "Self-Sufficiency Ratio") %>%
    mutate_if(is.numeric,
              ~round(.,digits=0))
  
  
  return(.result)
}