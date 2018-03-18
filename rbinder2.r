# rbinder2
#
# Version 0.2
#
# Author: Johannes Elias (joheli@gmx.net)
#
# Function for merging data frames contained in a list using a unique key to check for new entries.
# Useful for merging data frames with possible duplicates.
# 
# Arguments
#   df.list             a list containing named data frames
#   unique.field.name   character specifying the column name(s) identifying unique entries
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
  
  # d is the dataframe returned at the end
  d <- data.frame()
  
  # nrows holds the number of rows contained within each assessed data.frame
  nrows <- numeric()
  
  # uniquerows lists the number of unique rows contributed by each assessed data.frame
  uniquerows  <- numeric()
  
  # function for temporary key generation
  tk <- function(df, u) 
    return(eval(parse(text = paste0("with(df, paste(", paste(u, collapse = ", "), "))"))))
  
  # loop through the list of data.frames 
  for (df.name in names(df.list)) {
    df.current <- df.list[[df.name]]      # current data.frame in loop
    nrows  <- c(nrows, nrow(df.current))  # how much rows does the current data.frame have?
    if (nrow(d) == 0) {
      # if d is empty (at start), set d to df.current
      d <- df.current
      uniquerows <- nrows
    } else {
      # if d is not empty (after first iteration) reduce df.current to rows that are unique
      df.current <- df.current[!(tk(df.current, unique.field.name) %in% tk(d, unique.field.name)), ]
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
