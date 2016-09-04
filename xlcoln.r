# Translate value of a specified digit system to decimal number
#
# Author: Johannes Elias, joheli@gmx.net
#
# Arguments:
#   x               character, vector of characters to be translated
#   character.base  character, vector of characters used in the digit system
#   values          numeric, vector of values assigned to characters in character.base
#   change          function, optional function changes x; defaults to no change
#
# Returns:
#   decimal value of x
#
# Example:
#
# xval("aa", character.base = letters, change = tolower) # 27; useful for Excel spredsheet column numbers
# xval("a1", c(0:9,letters[1:6]), 0:15) # 161; hexadecimal value of "a1".
#

xval <- function(x, 
                 character.base,
                 values = 1:length(character.base),
                 change = function(x) x) {
  nc <- length(character.base)
  
  ln <- function(a) {
    res <- grep(change(a), character.base, fixed = TRUE)
    if (length(res) == 0) {
      res <- NA
    } else {
      return(values[res])
    }
  }
  
  lnm <- function(a) {
    chars <- strsplit(a, "")[[1]]
    sum(nc^((length(chars) - 1):0) * as.numeric(Map(ln, chars)))
  }
  
  lnm2 <- function(a) Map(lnm, a)
  
  return(as.numeric(lnm2(x)))
}

# Function xlcoln: translate Excel column heading to number
xlcoln <- function(x) return(xval(x, character.base = letters, change = tolower))
# Function xhex: translate hexadecimal number to decimal number
xhex <- function(x) return(xval(x, c(0:9,letters[1:6]), 0:15))
