# function to produce all combinations of character vectors
# using paste
#
# ...	character vectors
# f	function used for pasting, e.g. paste
#

paste.all <- function(..., f = paste0) Reduce(f, expand.grid(...))
