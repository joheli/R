# Function merging data frames using a unique key to check for new entries.
# Useful for merging data frames with possible duplicates.
# Returns a list containing the merged data.frame 'd' and a report ('r') listing the
# unique contributions of each data.frame.
# 
# Arguments
#   data.frame.pattern      character containing a regular expression capturing all data frames of interest
#   data.frame.names        character vector specifying data frame names, alternative to data.frame.pattern
#   unique.field.name       character specifying the column (field) name of unique key not to be duplicated
#   pos                     integer specifying the environment into which the result is to be saved
# 

rbinder <- function(data.frame.pattern = NULL,
                    data.frame.names = NULL,
                    unique.field.name,
                    pos = 1) {
  # validate
  if (is.null(data.frame.pattern) & is.null(data.frame.names))
    stop("Please provide either data.frame.pattern or data.frame.names!")
  # override data.frame.names
  if (!is.null(data.frame.pattern)) 
    data.frame.names <- ls(pattern = data.frame.pattern, pos = pos)
  d <- data.frame()
  nrows <- numeric()
  uniquerows  <- numeric()
  for (dfn in data.frame.names) {
    fd <- get(dfn, pos = pos)
    nrows  <- c(nrows, nrow(fd))
    if (nrow(d) == 0) {
      d <- fd
      uniquerows <- nrows
    } else {
      fd <- fd[!(fd[, unique.field.name] %in% d[, unique.field.name]), ]
      uniquerows <- c(uniquerows, nrow(fd))
      d <- rbind(d, fd)
    }
  }
  report <- data.frame(df = data.frame.names, nrows = nrows, uniquerows = uniquerows)
  return(list(d = d, report = report))
}