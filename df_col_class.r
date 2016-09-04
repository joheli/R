# df.col.class transforms a column into a specified class
# e.g. transform columns into 'Date' using the function 'as.Date'
#
# Parameters:
#		dfs			a character vector with data.frame names
#		cols		a character vector containing column names to be transformed
#		class.f		a character containing the transform function (e.g. "as.Date")
#

df.col.class <- function(dfs, cols, class.f, ...) {
	if (class(dfs) != "character" | class(cols) != "character" | class(class.f) != "character") stop("'dfs', 'cols', and 'class.f' have to be character vectors!")
	for (df in dfs) {
		for (col in cols) {
			eval(parse(text=paste(df, "$", col, " <<- ", class.f, "(", df, "$", col, ", ...)", sep="")))
		}
	}
}
