# Version 5, 2016-09-01
require(dplyr)

# # .selectOneRow selektiert den ‰ltesten (oder neuesten) Datensatz
# # gem‰ﬂ Bedingung "sel" ("sel" ist ein boolescher Vektor).
# .selectOneRow <- function(tb, time.col, sel, decreasing=FALSE){ 
# 	tb.sel <- tb[sel,]
# 	return(tb.sel[order(tb.sel[,time.col], decreasing = decreasing),][1,])
# }
# 
# # .cleanerOneCol putzt Doubletten raus
# .cleanerOneCol <- function(tb, time.col, clean.col, decreasing=FALSE){
# 	# wandle time.col in Date um, wenn notwendig
# 	if (!inherits(tb[,time.col],"Date")) tb[,time.col] <- as.Date(tb[,time.col])
# 	res <- data.frame()
# 	for (u in unique(tb[,clean.col])) {
# 		res <- rbind(res,.selectOneRow(tb,time.col,tb[,clean.col]==u,decreasing))
# 	}
# 	return(res)
# }
# 
# # .cleanerOneCol putzt Doubletten raus
# .cleanerOneCol1 <- function(tb, time.col, clean.col, decreasing=FALSE){
#   # wandle time.col in Date um, wenn notwendig
#   if (!inherits(tb[,time.col],"Date")) tb[,time.col] <- as.Date(tb[,time.col])
#   res <- data.frame()
#   u <- unique(tb[, clean.col])
#   one <- function(x) .selectOneRow(tb, time.col, tb[,clean.col]==x, decreasing)
#   res <- Reduce(rbind, Map(one, u))
#   return(res)
# }
# 
# # .mergedCol fasst mehrere Spalten einer data.frame in eine Spalte zusammen
# # funktioniert analog zur Funktion 'paste'
# .mergedCol  <- function(tb, cols = NULL, sep = "_") {
# 	if (!is.null(cols)) tb <- tb[, cols]
# 	if (!is.data.frame(tb)) stop("The selection must result in a data.frame!")
# 	vc <- tb[,1]
# 	if (ncol(tb) > 1) for (i in 2:ncol(tb)) vc <- paste(vc,tb[,i],sep=sep)
# 	return(vc)
# }
# 
# # .cleaner putzt ein data.frame, sodass nur die chronologisch ersten (decreasing=FALSE) 
# # oder neuesten (decreasing=TRUE) Eintr‰ge ¸brigbleiben. Es sind beliebig viele 
# # Kriterienspalten (clean.cols) mˆglich. 
# .cleaner  <- function(tb, time.col, clean.cols, decreasing=FALSE, keepMergedCol=FALSE) {
# 	extraCol <- ncol(tb) + 1
# 	if (length(clean.cols) > 1) {
# 		tb[, extraCol] <- .mergedCol(tb, clean.cols)
# 	} else {
# 		tb[, extraCol] <- tb[, clean.cols]
# 	}
# 	res <- .cleanerOneCol1(tb, time.col, extraCol, decreasing)
# 	if (!keepMergedCol) res <- res[, -extraCol]
# 	return(res)
# }

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

