# df2sts ist eine Hilfsfunktion für die Erstellgung von sts Objekten (Paket 'surveillance')
# ausgehend von einer data.frame ('tb')
#	Parameter:
#	tb					Tabelle (data.frame)	
#	time.col			Bezeichnung (character) oder Nummer (numeric) der Spalte, welche Datumsangaben (Date) enthält
#	cut.timeunit		Einheit der Datenaggregation
#	complete.timeunit	Einheit, welche komplettiert werden soll (z.B. das Jahr oder der Monat vor dem ersten Fall) 

df2sts <- function(tb, time.col, cut.timeunit = c("weeks","months","years","days"), complete.timeunit = c("year","month","none")) {
	cut.timeunit <- match.arg(cut.timeunit)
	format.string <- switch(cut.timeunit, weeks = "%W", months = "%m", years = "%Y", days = "%j")	# wird für Berechnung von start.unit gebraucht
	freq <- switch(cut.timeunit, weeks = 52, months = 12, years = 1, days = 365)		# kann sein, dass das nicht funktioniert bei years und days
	complete.timeunit <- match.arg(complete.timeunit)
	
	dts <- as.Date(tb[,time.col])
	min.date <- min(dts)
	max.date <- max(dts)
	
	start.yr <- as.numeric(format(min.date, "%Y"))
	start.month <- as.numeric(format(min.date, "%m")) 	# wird für Option complete.timeunit gebraucht
	start.unit <- as.numeric(format(as.Date(cut(min.date, breaks = cut.timeunit)), format = format.string))
	
	tbx <- table(cut(tb[,time.col],cut.timeunit))
	dates <- as.numeric(as.Date(names(tbx)))
	observed <- as.vector(tbx)
	
	if (complete.timeunit != "none") {
		complete.min.date <- switch(complete.timeunit, year = as.Date(paste(start.yr, "01", "01", sep="-")), month = as.Date(paste(start.yr, start.month, "01",sep="-")))
		start.unit <- as.numeric(start.month)			# für Monatskomplettierung; bei Jahr Anpassung weiter unten (immer 1)
		dates <- as.numeric(seq(complete.min.date, max.date, cut.timeunit))
		observed <- c(rep(0,length(dates)-length(observed)),observed)
	}
	
	if (cut.timeunit == "years" | complete.timeunit == "year") start.unit <- 1		# bei Jahr immer 1, egal was
	
	# Erstellung eines sts Objektes
	s <- new("sts", epoch = dates, start = c(start.yr, start.unit), freq = freq, observed = observed, epochAsDate = TRUE)
	return(s)
}
	