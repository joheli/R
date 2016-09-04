# Function for reading in similar cv files from a given directory
#
# Arguments
#   file.pattern        character containing a regular expression capturing all file names of interest
#   file.names          character vector specifying file names, alternative to file.pattern
#   readf               function name used for reading in files, e.g. read.table, read.csv (see ?read.table)
#   path                character specifying the path to look for files of interest
#   file.ending         character specifying the file ending of files of interest
#   data.frame.names    character vector specifying the names of data frames into which csv files are converted
#   write.to.pos        integer specifying the environment into which data frames are put
#

read.csv.batch <- function (file.pattern = NULL,
                            file.names = NULL,
                            readf = read.csv2, 
                            path = ".",
                            file.ending = "csv", 
                            data.frame.names = NULL,
                            write.to.pos = 1,
                            ...) {
  # validate
  if (is.null(file.names) & is.null(file.pattern)) 
    stop("Provide either file.names or file.pattern!")
  # override file.names if file.pattern not NA
  if (!is.null(file.pattern)) 
    file.names <- dir(path, paste0(file.pattern, ".*", file.ending))
  if (is.null(data.frame.names) | length(data.frame.names) < length(file.names)) 
    data.frame.names <- unlist(strsplit(file.names, paste0(".", file.ending)))
  # loop, read in files
  for (i in 1:length(file.names)) {
    fn <- file.names[i]
    fp <- fn
    if (path != ".") fp <- paste0(path, fn)
    d <- readf(fp, ...)
    assign(data.frame.names[i], d, write.to.pos)
  }
}