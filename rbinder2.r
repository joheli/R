# rbinder2
#
# Version 0.1
#
# Author: Johannes Elias (joheli@gmx.net)
#
# Function for merging data frames contained in a list using a unique key to check for new entries.
# Useful for merging data frames with possible duplicates.
# 
# Arguments
#   df.list             a list containing named data frames
#   unique.field.name   character specifying the column name identifying unique entries
# 
# Returns
#   a list containing the merged data.frame and a report detailing the unique contributions of each data.frame.
#

rbinder2 <- function(df.list,
                    unique.field.name) {
  # stop if df.list is empty
  if (length(df.list) < 1) 
    stop(paste0(deparse(substitute(df.list)), " appears to be empty!"))
  # stop if df.list contents are not named
  if (length(names(df.list)) < 1)
    stop(paste0("Contents of ", deparse(substitute(df.list)), "are not named!"))
  # stop if names of df.list contains NA
  if (any(is.na(names(df.list))))
    stop(paste0(deparse(substitute(df.list)), " has unnamed entries!"))
  d <- data.frame()
  nrows <- numeric()
  uniquerows  <- numeric()
  for (df.name in names(df.list)) {
    df.current <- df.list[[df.name]]
    nrows  <- c(nrows, nrow(df.current))
    if (nrow(d) == 0) {
      d <- df.current
      uniquerows <- nrows
    } else {
      df.current <- df.current[!(df.current[, unique.field.name] %in% d[, unique.field.name]), ]
      uniquerows <- c(uniquerows, nrow(df.current))
      d <- rbind(d, df.current)
    }
  }
  report <- data.frame(`file name` = names(df.list), `number of rows` = nrows, `unique rows` = uniquerows)
  report  <- rbind(report,
                   data.frame(
                     `file name` = "merged data.frame",
                     `number of rows` = nrow(d),
                     `unique rows` = length(unique(d[, unique.field.name]))
                   ))
  return(list(`merged data.frame` = d, report = report))
}