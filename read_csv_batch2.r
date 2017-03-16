# read.csv.batch2
#
# Version 0.1
#
# Author: Johannes Elias (joheli@gmx.net)
#
# Function for reading in similar csv files from a given directory.
#
# Arguments
#   file.pattern        character containing a regular expression capturing all file names of interest
#   readf               function name used for reading in files, e.g. read.table, read.csv (see ?read.table)
#   path                character specifying the path to look for files of interest
#   file.ending         character specifying the file ending of files of interest
#   ...                 arguments passed to function specified in readf
#
# Returns
#   a list containing the data.frames read in.
#

read.csv.batch2 <- function (file.pattern,
                            readf = read.csv2, 
                            path = ".",
                            file.ending = "csv",
                            ...) {
  # file.names: a vector of matching file names
  file.names <- dir(path, paste0(file.pattern, ".*", file.ending, "$"))
  # Stop if no matching files found
  if (length(file.names) < 1) stop(paste0("No files matching the pattern '", file.pattern, "' found!"))
  # create list to save data.frames
  df.list <- list()
  # loop, read in files to df.list
  for (i in 1:length(file.names)) {
    f.name <- file.names[i]
    f.path <- f.name
    if (path != ".") f.path <- paste0(path, f.name)
    d <- readf(f.path, ...)
    df.list[[f.name]] <- d
  }
  return(df.list)
}