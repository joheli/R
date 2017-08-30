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

# EXAMPLE
#
# # create test data frame 'test' with fields 'A', 'B', 'C', 'D'
#
# test <- data.frame(A = rep(LETTERS[1:10], round(runif(10,1,4))))
# test$B <- round(runif(nrow(test), 1, 5))
# test$C <- rnorm(nrow(test))
# test$D <- as.roman(round(runif(nrow(test), 1, 3)))
# 
# # test.c.1 only only unique entries of field "A"
#
# test.c.1 <- .cleaner(test, "B", "A")
#
# # test.c.2 only contains unique entries of combinations of "A" and "D"
#
# test.c.2 <- .cleaner(test, "B", c("A", "D"))
#
