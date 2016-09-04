setClass(
	Class="EventLog",
	representation=representation(label="character",time="POSIXct"),
	validity=function(object){
		if (length(object@label)!=length(object@time)) stop("EventLog validation: length of 'label' differs from length of 'time'.")
		return(TRUE)
	},
	prototype=prototype(label="unspecified",time=Sys.time())
)

setMethod(
	f="as.data.frame",
	signature="EventLog",
	definition=function (x, row.names = NULL, optional = FALSE, ...){
		return(data.frame(label=x@label,time=x@time))
	}
)

setMethod(
	f="show",
	signature="EventLog",
	definition=function(object) head(as.data.frame(object))
)

setMethod(
	f="print",
	signature="EventLog",
	definition=function(x,...) as.data.frame(x)
)

setGeneric(
	name="addEvent<-",
	def=function(object, value){ standardGeneric("addEvent<-") }
)

setReplaceMethod(
	f="addEvent",
	signature="EventLog",
	definition=function(object, value){
		object@label <- c(object@label, value)
		object@time <- c(object@time, rep(Sys.time(), length(value)))
		validObject(object)
		return(object)
	}
)

setGeneric(
	name="lastEvent",
	def=function(object, l) { standardGeneric("lastEvent") }
)

setMethod(
	f="lastEvent",
	signature="EventLog",
	definition=function(object, l) {
		d <- data.frame(label=object@label, time=object@time)
		d <- subset(d, label==l)
		if (nrow(d) == 0) return(NULL)
		d <- d[order(d$time, decreasing=TRUE),]
		return(d[1,"time"])
	}
)

setGeneric(
	name="lastEventD",
	def=function(object, l) { standardGeneric("lastEventD") }
)

setMethod(
	f="lastEventD",
	signature="EventLog",
	definition=function(object, l) {
		le <- lastEvent(object, l)
		if (!is.null(le)) {
			return(as.Date(format(le, "%Y-%m-%d")))
		} else {
			return(NULL)
		}
	}
)
setGeneric(
	name="lastEventDays",
	def=function(object, l) { standardGeneric("lastEventDays") }
)

setMethod(
	f="lastEventDays",
	signature="EventLog",
	definition=function(object, l) {
		led <- lastEventD(object, l)
		if (!is.null(led)) {
			return(Sys.Date() - led)
		} else {
			return(NULL)
		}
	}
)

#source("EventLog.r")
test <- new(Class="EventLog", label="fut", time=Sys.time())
addEvent(test) <- "hure"
show(test)
