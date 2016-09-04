# Convert a data.frame into a html table, optionally add class and id attributes
#
# Returns:
#	character, html-formatted table with optional class and id attributes
#
# Arguments:
#	d		data.frame, the one to be converted
#	table.header	logical, use colnames?
#	use.th 		logical, use <th> tags for first row?
#	class 		named character vector, contains class attributes to be added to 
#			the tags <table>, <th>, <tr>, or <td>
#	ids		named character vector, contains id attributes, same as class.
#

df2html <- function(d, table.header = TRUE, use.th = table.header, class = NULL, id = NULL) {
	# .tags contains the html tags that class or id attributes can be added to	
	.tags <- c("table", "th", "tr", "td")	

	# basic validation
	if (!is.data.frame(d)) stop("'d' has to be a data.frame!")
	
	# convert to first html table, which is optionally altered later	
	out <- "<table><tr><td>"
	if (table.header) out <- paste0(out, paste(colnames(d), collapse = "</td><td>"), "</td></tr><tr><td>")
	out <- paste(out, paste(apply(d, 1, function(r) paste(r, collapse="</td><td>")), collapse = "</td></tr><tr><td>"), collapse = "")
	out <- paste0(out, "</td></tr></table>")

	# replace first row occurrences of <td> by <th>?	
	if (use.th) {
		for (i in 1:ncol(d)) {
			out <- sub("<td>", "<th>", out)
			out <- sub("</td>", "</th>", out)	
		}
	}
	
	# helper function to add attributes inside tags	
	attributer <- function(what) {	
		if (!is.null(what)) {
			for (tg in .tags) {
				if (tg %in% names(what)) {
					pattern <- paste0("<", tg)
					replacement <- paste0("<", tg, " ", deparse(substitute(what)), "=\"", what[tg], "\"")
					out <<- gsub(pattern, replacement, out)
				}			
			}	
		}
	}
	
	# add attributes, if present	
	attributer(class)
	attributer(id)
	
	# return the result
	return(out)	
}

