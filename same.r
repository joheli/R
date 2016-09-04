# Funktion zum säulenweisem Vergleich einer Zahlenmatrix A
# mit einer zweiten Matrix B. A und B haben dieselbe Anzahl 
# von Zeilen.
# 
# Matrix A wird Säule für Säule durchlaufen.
# Matrix B enthält Optionen für die gerade untersuchte Säule aus A.
# Wenn mindestens eine (any) der Optionen gleicht, wird TRUE für
# diese Säule zurückgegeben.
#
# Beispiel
# > a <- matrix(c(3,2,5,3,2,4), ncol = 2)
# > a
# [,1] [,2]
# [1,]    3    3
# [2,]    2    2
# [3,]    5    4
# > b <- matrix(c(3,2,5,7,8,9), ncol = 2)
# > b
# [,1] [,2]
# [1,]    3    7
# [2,]    2    8
# [3,]    5    9
# > same(a, b)
# [1]  TRUE FALSE

same <- function(a, b, f = any) {
  a <- as.matrix(a)
  sm1 <- function(j) {
    b <- as.matrix(b)
    sm2 <- function(i) sum(a[, j] - b[, i]) == 0
    f(unlist(Map(sm2, 1:ncol(b))))
  }
  unlist(Map(sm1, 1:ncol(a)))
}