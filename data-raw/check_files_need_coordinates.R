check_files_need_coordinates <- function() {
  
  path <- ".\\data-raw\\DUERO\\"
  f <- list.files(path = path, pattern = "csv")
  df <- data.frame(filename = character(), filecoord = character())
  for (i in f) {
    a <- read.csv2(paste0(path, i))
    if ("indroea" %in% colnames(a)) {
      df <- rbind(df, data.frame(filename = i, filecoord = "estaf"))
    } else if ("ref_ceh" %in% colnames(a)) {
      df <- rbind(df, data.frame(filename = i, filecoord = "canal"))
    } else {
      
    }
    
  }
  
  
  
}
