# Function returning the newest file in a directory
#
# Arguments
#   path          directory (folder)
#   pattern       pattern passed to dir
#
# Returns
#   character specifying the path of the newest file or NA if none found
#

library(dplyr)

newest.file <- function(path, pattern = NULL) {
  fls <- file.info(dir(path, full.names = T, pattern = pattern))
  fls$file <- rownames(fls)
  fls <- arrange(fls, desc(atime))
  return(fls$file[1])
}
