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

