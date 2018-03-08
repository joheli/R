# factorer (see below) uses either function replace.all or replace.all.or, which both replace
# given patterns in a character vector. Despite their similar function, replace.all.or behaves
# differently, in that it replaces one after the other pattern from top to bottom, allowing
# patterns down the list to override (hence the suffix '.or') patterns further up; e.g. pattern
# 'test' will be superseded pattern 'test abc', if latter pattern occurs further down the list.
# Also, despite its application of a loop, replace.all.or is quicker than replace.all. Nevertheless,
# both function can be used; if override is not required, replace.all is the right choice.

# function to replace multiple patterns in a character vector; in contrast to replace.all 
# similar patterns are (partly) overridden by those following them, i.e. a pattern at the top is replaced
# by an overlapping pattern at the bottom of the list. This is useful if a general category is
# to be preserved along with an important subcategory; e.g. to preserve both the more general
# category 'Klebsiella species' (designating the biologic genus) as well as the category 
# 'Klebsiella pneumoniae' (designating the species),
# pattern 'Klebsiella' (which matches all Klebsiella) is partly overridden by
# pattern 'Klebsiella pneumoniae' (which matches all Klebsiella pneumoniae); thus, the resulting
# column contains both 'Klebsiella' (labelling all Klebsiella which are not K. pneumoniae) and 
# 'Klebsiella pneumoniae'
#
# Arguments
#
#   target            character vector where replacements are to take place
#   patterns          character vector containing regex patterns
#   replacements      character vector containing replacements for each of the 
#                     patterns
#   replace.whole     boolean; this parameter has no function in replace.all.or and is
#                     merely kept to preserve compatibility to replace.all
#   default           default behaviour regarding entries without pattern matches
#                     in target; "same": leave unchanged, "other": replace with
#                     other.text
#   other.text        character containing text to use in case default == "other"
#
# Return value
#
#   character vector containging all replacements
#

replace.all.or <- function(target,
                        patterns,
                        replacements,
                        replace.whole = TRUE,
                        default = c("same", "other"),
                        other.text = "other") {
  
  # extract default behaviour for replacement of unmatched entries
  default <- match.arg(default)
  
  result <- character(length(target))
  
  for (i in 1:length(patterns)) result[grepl(patterns[i], target)] <- replacements[i]
  
  nochange <- result == ""
  if (default == "other") {
    result[nochange] <- other.text
  } else {
    result[nochange] <- target[nochange]
  }
  
  return(result)
}

# function to replace multiple patterns in a character vector
#
# Arguments
#
#   target            character vector where replacements are to take place
#   patterns          character vector containing regex patterns
#   replacements      character vector containing replacements for each of the 
#                     patterns
#   replace.whole     boolean, specifying whether the pattern should match the whole
#                     target string
#   default           default behaviour regarding entries without pattern matches
#                     in target; "same": leave unchanged, "other": replace with
#                     other.text
#   other.text        character containing text to use in case default == "other"
#
# Return value
#
#   character vector containging all replacements
#

replace.all <- function(target,
                        patterns,
                        replacements,
                        replace.whole = TRUE,
                        default = c("same", "other"),
                        other.text = "other") {
  
  # extract default behaviour for replacement of unmatched entries
  default <- match.arg(default)
  
  # wrap patterns with ".*" if replace.whole is true
  if (replace.whole) patterns <- paste0(".*", patterns, ".*")
  
  # return list with replacements, where each list element represents one 
  # pattern replacement
  r1 <- Map(function(p, r) {
    target <- gsub(p, r, target)
  }, patterns, replacements)
  
  # return a list of boolean vectors indicating replacement positions
  chng <- Map(function(r)
    r != target, r1)
  
  # get vector of unique changes
  uchng <- !duplicated(chng)
  
  # only allow unique change patterns (i.e. first changes are kept, rest is 
  # discarded)
  r1 <- r1[uchng]
  chng <- chng[uchng]
  
  # return a list containing replacements only; other values are set to ""
  only.chng <- Map(function(r, cx) {
    rs <- rep("", length(r))
    rs[cx] <- r[cx]
    return(rs)
  }, r1, chng)
  
  # merge the changes to obtain final vector containing all changes
  res <- Reduce(function(a, b)
    paste0(a, b), only.chng)
  
  # get positions where no changes occurred
  nochange <- res == ""
  
  # insert original values at unchanged positions
  if (default == "same") {
    res[nochange] <- target[nochange]
  } else {
    res[nochange] <- other.text
  }
  
  # return merged vector of same size as target, where patterns have been 
  # replaced by replacements
  return(res)
}

# function to replace multiple patterns in a character vector using a control table containing
# patterns in the first column and corresponding (rowwise) replacements in any number of further
# columnns.
#
# Arguments
#
#   f       character vector where replacements are to take place
#   fm      data.frame containing control table; first column contains regex patterns that are
#           sought in f; further column contains replacements; in the most simple case
#           fm contains of only two columns with the first column harbouring patterns and
#           the second column specifying replacements. If more than one replacement column is 
#           given, additional columns are returned applying those.
#   ...     arguments passed on to replace.all
#
# Return value
#
#   data.frame containing replaced values; in the most simple case, where fm consists of two
#   columns the returned data.frame contains one column; for any additional replacement column
#   in fm additional result columns are output.
#

factorer <- function(f, fm, replace.func = replace.all.or, ...) {
  # basic argument validation
  if (!is.data.frame(fm)) stop("fm needs to be a data.frame!")
  if (ncol(fm) < 2) stop("fm needs to have at least two columns!")
  if (is.factor(f)) f <- as.character(f)
  
  # patterns are contained in first columns
  # replacements in the remaining columns
  patterns <- fm[, 1]
  replacementsx <- fm[, -1]
  
  # initiate result list (list is chosen as can be concatenated)
  res <- list()
  
  # recursive function performing replacements according to columns given in 
  # replacementsx; each replacement column yields a result column containing
  # replacements according to patterns
  replacer <- function(rx, result = res, ...) {
    # ra: helper function using replace.all
    ra <- function(r) list(replace.func(f, patterns, r, ...))
    if (is.null(dim(rx))) {
      iresult <- ra(rx)
      return(c(result, iresult))
    } else {
      iresult <- c(result, ra(rx[, 1]))
      # recursive call of function to accomodate any number of columns
      Recall(rx[, -1], iresult)
    }
  }
  
  # the result is transformed into a data.frame
  res2 <- as.data.frame(replacer(replacementsx))
  # colnames of replacement matrix (fm) are reused
  colnames(res2) <- colnames(fm)[-1]
  # return final result res2
  return(res2)
}