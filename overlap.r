# The function "overlap" plots time bars representing periods of inpatient stay of cases 
# of an outbreak; it also displays events of interest along these timespans.
#
# VERSION 6
#
# Author:			Johannes Elias, jelias@hygiene.uni-wuerzburg.de
#
# Parameters:
# d 				a dataframe containing 
#					* case ids (columd no. given in 'caseids')
#					* date of admission (column no. given in 'dadm')
#					* date of discharge (column no. given in 'ddch')
#					* and optional further columns containing dates of interest
#					  (all further columns are treated as dates)	
# dadm				the column number of d representing date of admission
# ddch				the column number of d representing date of discharge
# caseids			the column number of d representing case ids
# unit				optional column number representing unit (e.g. ward)
# orderby			order dataframe by this column, e.g. date of admission
# wi				width of time bars
# cl				color of time bars
# line.end			line end type (lend) of time bar
# pchs				plotting characters of dates of interest
# y.axis.lb			label of y axis ticks
# bx				draw a box (T) or not (F)
# start.date		start date, defaults to earliest date
# end.date			end date, defaults to newest date
# las				line axis style (see ?plot)
# leg				draw legend (T) or not (F)
# leg.x				parameter "x" in function "legend", see ?legend; defaults to
#					"bottomright"
# stay.text			text label for inpatient stay (only used if unit is NULL)
# leg.text			custom legend text entries for units and procedures
# leg.bty			parameter "bty" (= box type) in function "legend", see ?legend
# ylab				legend of y axis
# width				plot width
# heigth			plot heigth
# ...				parameters forwarded to function plot()
# 
# Example:
# caselist <- read.csv2("caselist.csv",na.strings="")
# overlap(d=caselist, dadm=2, ddch=3, caseids=1, unit=7, xlab="Calendar week", ylab="Cases", main = "Outbreak, ward a", width=9, height=5, leg.bty="b")
#

overlap <- function(d, dadm = 1, ddch = 2, caseids = 3, unit = NULL, orderby = dadm, wi = 26, cl = topo.colors(10)[as.numeric(rbind(seq(10,2,-2),seq(1,9,2)))], line.end = 1,
		pchs = c(15, 2, 3:14, 1), y.axis.lb = unique(d[order(d[, orderby]), caseids]), bx = T,
		start.date = min(as.Date(as.matrix(d[, -caseids])), na.rm = T),
		end.date = max(as.Date(as.matrix(d[, -caseids])), na.rm = T), las = 1, leg = T, leg.x = "bottomright",
		stay.text = "stay", leg.text = NULL,
		leg.bty = "n", ylab = "", width = 8, height = 3,...) {

# if d is not a data.frame stop
if (!is.data.frame(d)) stop("Please supply a data frame object 'd'!")

# order data.frame d by column 'orderby'
d <- d[order(d[, orderby]),]
# extract variable 'cids', which is a vector of case ids
cids <- as.character(d[,caseids])
if (is.null(unit)) {
	nondate.cols <- caseids
	cl <- cl[1]
	leg.text.units <- stay.text
} else {
	d[, unit] <- as.factor(d[, unit])
	nondate.cols <- c(caseids, unit)
	cl <- cl[1:length(levels(d[, unit]))]
	leg.text.units <- levels(d[, unit])
}
# now d should contain only values convertable to 'Date'

if (is.null(leg.text)) leg.text <- c(leg.text.units, colnames(d)[-c(dadm, ddch, nondate.cols)]) 

# convert all columns (except nondate.cols) to 'Date'
for (x in 1:ncol(d)) {
	tryCatch({ 
		if (!(x %in% nondate.cols)) d[, x] <- as.Date(d[, x]) 
		},
		warning = function(w) {},
		error = function(e) { stop(paste("Column no. ", x, " cannot be converted to Date. Hint: provide dates as 'YYYY-MM-DD' or as NA if not available.", sep = "")) }
		)
}

# create a sequence of weeks spanning the outbreak (from start to end)
date.series <- seq(start.date, end.date, by="week")
# convert dates into week of year (the function is supplied below)
wk.series <- weekofyear(date.series)

# set plotting parameters
# extend left margin and adjust title of y axis, if number of characters in cids exceeds 3
if (max(nchar(cids)) > 3) {
	mar <- c(5, ceiling(max(nchar(cids))/1.5), 4, 2)
	lin <- ceiling(max(nchar(cids))/1.7)
} else {
	mar <- c(5, 4, 4, 2)
	lin <- 2
}

# plotting canvas, plotting parameters
dev.new(width = width, height = height)
par(lend = line.end, mar = mar)

# define the plotting area, but do not plot anything yet
plot(c(d[1, dadm], d[1, ddch]), c(1, 1), type = "n", ylim = c(1, length(unique(cids))), xlim=c(start.date, end.date), lwd = wi, axes = F, ylab = "", ...)

# add a box, if specified
if(bx) box()
# define case counter cc
cc <- 1

# proceed with plotting case by case
for (i in unique(cids)) {
  # copy rows pertaining to present case into data.frame 'dc'
  dc <- d[cids==i,]
  # for each row of this case, plot into the same line defined by 'cc'
  for (j in 1:nrow(dc)) {
	# check, whether dc[j,dadm] or dc[j,ddch] is na
	# if either value is 'na', assign start.date and end.date, respectively
	if (is.na(dc[j, ddch])) dc[j, ddch] <- end.date
	if (is.na(dc[j, dadm])) dc[j, dadm] <- start.date
	# line color depends on unit
	if (is.null(unit)) {
		line.col <- cl
	} else {
		line.col <- cl[as.numeric(dc[j, unit])]
	}
	# draw the time bar
	lines(c(dc[j, dadm], dc[j, ddch]), c(cc, cc), lwd = wi, col = line.col)
	# continue with further events of interest (if available)
	# first copy dates into vector dts
	dts <- dc[j,-c(dadm, ddch, nondate.cols)]
	# check if size of 'dts' is greater than 0
	if (length(dts) > 0) {
		# for every position, display an event if cell is not 'NA' (not available)
		for (k in 1:length(dts)) {
			if (!is.na(dts[k])) points(dts[k], cc, pch=pchs[k])
		}
	}
  }
  # increase cc by 1 (i.e. go to next case = next line up in plot)
  cc <- cc + 1
}

# add axes
y.axis.ticks <- 1:length(unique(cids))
axis(1,date.series,wk.series,las=las)
axis(2,y.axis.ticks,y.axis.lb,las=las)
mtext(ylab,2,line=lin)

# add legend, if specified
if (leg) {
	d.leg <- d[,-c(nondate.cols, dadm, ddch)]
	legend(x=leg.x, legend=leg.text ,pch=c(rep(NA, length(leg.text.units)),pchs[1:(ncol(d.leg))]),
	lwd = c(rep(wi, length(leg.text.units)), rep(NA, ncol(d.leg))),
	lty = c(rep(1, length(leg.text.units)), rep(NA, ncol(d.leg))),
	col = c(cl,rep(1,ncol(d.leg))),
	y.intersp = 1.5,bty = leg.bty)
}
}

# the function 'weekofyear' is needed by 'overlap'
weekofyear <- function(dt) {
    as.numeric(format(as.Date(dt), "%W"))
}


