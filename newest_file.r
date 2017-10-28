library(dplyr)

newest.file <- function(path) {
  fls <- file.info(dir(path, full.names = T))
  fls$file <- rownames(fls)
  fls <- arrange(fls, desc(atime))
  return(fls$file[1])
}