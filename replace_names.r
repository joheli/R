replace.names <- function(x,from,to) {
	if (length(from) != length(to)) stop("Vectors from and to have to be of same length!")
	new.names <- names(x)
	for (i in 1:length(from)) new.names <- gsub(from[i],to[i],new.names)
	names(x) <- new.names
	return(x)
}
