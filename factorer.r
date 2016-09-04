# factorer, v 0.2
# 
# This function cleans up categorical variables and provides further options for aggregation of strata.
# It uses a data.frame which contains search patterns and replace values in the first and second 
# columns, respectively. If further columns are provided (e.g. representing coarser strata), these are
# appended to the result.
#
# Arguments
#   f         character vector containing values to be cleaned
#   fm        data.frame guiding the search and replace process
#

factorer <- function(f, fm) {
  # basic argument validation
  if (!is.data.frame(fm)) stop("fm needs to be a data.frame!")
  if (ncol(fm) < 2) stop("fm needs to have at least two columns!")
  if (is.factor(f)) f <- as.character(f)
  
  # copy f
  f.clean <- f
  
  # for each pattern provide in the first column, replace entry with corresponding value in second column
  # i.e. search and replace
  for (r in 1:nrow(fm)) f.clean[grep(fm[r, 1], f.clean)] <- as.character(fm[r, 2])

  # if more than two columns are provided in the matrix, merge those values to the cleaned factor
  if (ncol(fm) > 2) {
    res1 <- data.frame(f.clean)
    colnames(res1) <- colnames(fm)[2]
    f.clean <- merge(res1, fm[, -1], all.x = TRUE, sort = FALSE)
  } 
  
  # return
  return(f.clean)
}