download_shp <- function(url, dest){
  
  temp <- tempfile()
  
  download.file(url, temp, mode = "wb")
  
  unzip(temp, exdir = dest)
  
}
