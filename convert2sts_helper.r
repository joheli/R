convert2sts.helper <- function(tb,time.col,cut.timeunit,complete.year=TRUE,observation.start=NULL){
	# diese Funktion wandelt angegebene Datumswerte in eine für ein sts-Objekt
	# nutzbare Form um
	tbx <- table(cut(tb[,time.col],cut.timeunit))
	dates <- as.numeric(as.Date(names(tbx)))
	observed <- as.vector(tbx)
	# complete.year sorgt dafür, dass das gesamte vorhergehende Jahr mit in die
	# Beobachtungsperiode reinkommt, selbst wenn keine Fälle gezählt wurden
	# observation.start hebt jedoch complete.year auf; sobald observation start angegeben wurde zählt
	# complete.year nicht mehr!
	if (is.null(observation.start)) {
		if (complete.year) {
			yr <- format(min(as.Date(tb[,time.col])),"%Y")
			dates <- as.numeric(seq(as.Date(cut(as.Date(paste(yr,"01","01",sep="-")),cut.timeunit)),max(as.Date(tb[,time.col])),cut.timeunit))
			observed <- c(rep(0,length(dates)-length(observed)),observed)
		}
	} else {
		if (class(observation.start)!="Date") stop("observation.start muss ein 'Date'-Objekt sein!")
		if (min(dates) < as.numeric(observation.start)) stop(paste("observation.start muss vor dem frühesten Ereignis in",deparse(substitute(tb)),"sein!"))
		dates <- as.numeric(seq(observation.start,max(as.Date(tb[,time.col])),cut.timeunit))
		observed <- c(rep(0,length(dates)-length(observed)),observed)
	}
	return(list(dates=dates,observed=observed))
}
	