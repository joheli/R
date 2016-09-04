# Relabel factor from -> to
#
# Arguments:
#	ctg				factor
#	from			vector containing factor labels
#	to				vector of equal length containing new factor labels

relabel.cat2 <- function(ctg, from, to) {
	# basic validation
	if (length(from) != length(to)) stop("Lengths of 'from' and 'to' must be equal!")
	# drop unused labels
	ctg <- ctg[, drop = TRUE]
	# relabel
	for (i in 1:length(from)) levels(ctg)[grep(from[i], levels(ctg))] <- to[i]
	return(ctg)
}
