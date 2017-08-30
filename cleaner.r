# Function .cleaner
#
# Returns: data.frame with unique occurrences of a field (or fields) designated
# by clean.cols.
#
# Parameters
#
#   tb            a data.frame
#   time.col      character designating a field name which is orderable, i.e. 
#                 numeric, Date, etc.
#   clean.cols    character with one or more elements designating field names
#                 which are to be reduced to unique occurrences in result
#   decreasing    logical designating whether field designated by time.col
#                 is to be ordered in decreasing fashion or not.
#   keepExtraCols logical designating whether a temporary column is to be
#                 kept - use for debugging only.
#

require(dplyr)

.cleaner  <- function(tb, time.col, clean.cols, decreasing = FALSE, 
                       keepExtraCols = FALSE) {
  tb$extraCol <- tb[, clean.cols[1]]
  if (length(clean.cols) > 1) 
    tb$extraCol <- apply(tb[, clean.cols], 1, paste, collapse = "")
  clean.cols2 <- paste(clean.cols, collapse = ", ")
  dcr <- time.col
  if (decreasing) dcr <- paste0("desc(", dcr, ")")
  etext <- paste0("tb <- arrange(tb, ", clean.cols2, ", ", dcr, ")")
  eval(parse(text = etext))
  tb$duplicated <- duplicated(tb$extraCol)
  tb <- subset(tb, !duplicated)
  if (!keepExtraCols) tb <- select(tb, -extraCol, -duplicated)
  return(tb)
}

