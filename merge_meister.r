# Sequentielle Vereinigung mehrerer 'data.frame' Objekte mit der Funktion 'merge'
#
# Parameter:
#	x		Das erste data.frame Objekt; an dieses wird alles angehängt.
#	ys		Eine Liste von anzuhängenden data.frame Objekten.
#	by.xs	Die einzelnen by.x Argumente (siehe ?merge) welche bei jedem
#			Anhängeschritt genutzt werden; können verschiedene sein, siehe Test)
#	by.ys	Die einzelnen by.y Argumente (siehe ?merge).
#	...		Wird an 'merge' übergeben.

merge.meister <- function(x, ys, by.xs, by.ys,...) {
	# Argumentevaluation
	if (!is.data.frame(x)) stop("x has to be a data.frame!")
	err.ys <- "ys has to be a list of data.frame objects!"
	l <- length(ys)
	if (is.list(ys)) {
		for (i in 1:l) if (!is.data.frame(ys[[i]])) stop(err.ys)
	} else {
		stop(err.ys)
	}

	# by.xs und by.ys werden auf die Länge der Liste ys gebracht (hier wird nur der erste Eintrag gezählt)
	if (length(by.xs) < l) by.xs <- rep(by.xs[1],l)
	if (length(by.ys) < l) by.ys <- rep(by.ys[1],l)
	d <- x

	# Sequentielles Anhängen jedes data.frame Objektes in ys
	for (i in 1:l) d <- merge(d,ys[[i]],by.x=by.xs[i],by.y=by.ys[i],all.x=T,...)
	
	# Rückgabe des Ergebnisses
	return(d)
}
