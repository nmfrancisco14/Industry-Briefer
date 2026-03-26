import_fromDrive <- function(linkid, filetype = c("csv","xlsx")){
  
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
    .data_out <- read.xlsx(.data_name)
  } else {
    .data_out <- read_csv(.data_name)
  }
  
  message("File uploaded.")
  
  message("Removing temporary file from working directory")
  #remove from local
  file.remove(.data_name)
  
  return(.data_out)
  
}