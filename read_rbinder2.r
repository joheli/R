# read.rbinder2
#
# Version 0.1
#
# Author: Johannes Elias (joheli@gmx.net)
#
# Wrapper for functions read.csv.batch2 and rbinder2
# 
# Arguments
#   file.pattern        character containing a regular expression capturing all file names of interest
#   readf               function name used for reading in files, e.g. read.table, read.csv (see ?read.table)
#   path                character specifying the path to look for files of interest
#   file.ending         character specifying the file ending of files of interest
#   unique.field.name   character specifying the column name identifying unique entries
#   result              character specifying if only a data frame ("only") or an additional report ("report") should be returned
#   ...                 arguments passed to function specified in readf
# 
# Returns
#   depending on 'result' either a merged data.frame ("only") or a merged data.frame and a report detailing 
#   the unique contributions of each data.frame ("report").
#

source("read_csv_batch2.r")
source("rbinder2.r")

read.rbinder2 <- function(file.pattern,
                          readf = read.csv2,
                          path = ".",
                          file.ending = "csv",
                          unique.field.name,
                          result = c("only joined data-frame", 
                                     "report and data-frame"),
                          ...) {
  result = match.arg(result)
  step1 <- read.csv.batch2(file.pattern, 
                           readf, 
                           path, 
                           file.ending, 
                           ...)
  step2 <- rbinder2(step1, unique.field.name)
  rtn <- step2
  if (result == "only joined data-frame") rtn <- step2[[1]]
  return(rtn)
}
