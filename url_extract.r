# Extract data from form data
#
# Arguments
#	x			character, urlencoded data 
#	method		character, specifies if 'x' comes from a url string or a request body submitted by "get" or "post", respectively
#	get.pattern	character, pattern used to get rid of first part of url
#	return.what	character, specifies object returned ('data.frame' or named 'character' vector) 
#	show.error	logical, show error or not?
#

url.extract <- function(x, method = c("post", "get"), get.pattern = "/cgi-bin/.*\\?", 
	return.what = c("data.frame", "vector"), show.error = FALSE) {
	
	# save original character for error message, see below
	x.orig <- x
	# match arguments 'method' and 'return.what'
	method <- match.arg(method)
	return.what <- match.arg(return.what)
	# surround everything with 'tryCatch' as so many things can go wrong from here
	tryCatch({
		# only use part after 'get.pattern' if method equals "get"
		if (method == "get") x <- unlist(strsplit(x, get.pattern))[2]
		# stepwise transformation of character string into dataframe ...
		step1 <- unlist(strsplit(x, "&"))
		step2 <- strsplit(step1, "=")
		step3 <- t(as.data.frame(step2))
		colnames(step3) <- c("key", "value")
		rownames(step3) <- NULL
		d <- as.data.frame(step3)
		d$key <- as.character(d$key)
		d$value <- as.character(d$value)
		# ... URLdecode values (this only works in a loop)
		for (i in 1:nrow(d)) d[i, "value"] <- URLdecode(d[i, "value"])
		v <- d$value
		names(v) <- d$key
		# ... finally return object as specified in 'return.what'
		if (return.what == "data.frame") {
			return(d)
		} else {
			return(v)
		}
	}, error = function(e) {
		# if an error occurs return 'NULL'; if specified ('show.error'), also return an error message.
		if (show.error) cat(paste("The supplied character '", x.orig, "' cannot be transformed. Sorry.\n\nError message:\n", e, sep = ""))
		return(NULL)
	})	
}