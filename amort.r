# Function 'amort' calculates amortization tables for loans.
#
# Returns
#	a data.frame detailing payment according to 'freq', e.g. monthly payment
#
# Parameters 
#	P			numeric, loan size (= principal)
#	i			numeric, annual interest rate in percent
#	a			numeric, duration of repayment in years
#	freq		numeric, select frequency of installments and calculation of interest (default is 'monthly')
#	start.date	Date, specifies date of first installment
# 	lct			locale
#	add.sums	logical, specifies if an additional row with sums of total pay, principal, and interest shall be added.
#

amort <- function(P, i, a, freq = c("month", "week"), start.date = as.Date(Sys.time()) + 30, lct = "C", add.sums = TRUE) {
	lct.old <- Sys.getlocale("LC_TIME")
	Sys.setlocale("LC_TIME", lct)
	
	freq <- match.arg(freq)
	n <- switch(freq, month = 12 * a, week = 52 * a)	
	i <- i/100
	i <- i/n
	pay <- (i * P * (1 + i) ^ n)/((1 + i) ^ n - 1)

	# format dates of payment
	sq <- seq(start.date, by = freq, length.out = n)
	sq <- switch(freq, month = format(sq, "%B, %Y"), week = paste("Week", format(sq, "%W, %Y")))
	
	# table
	tb <- data.frame()
	interest <- P * i
	principal <- pay - interest
	balance <- P - principal

	for (j in 1:n) {
		rw <- data.frame(j, pay, principal, interest, balance)
		tb <- rbind(tb, rw)
		interest <- rw[, 5] * i
		principal <- pay - interest
		balance <- balance - principal	
	}
	
	if (add.sums) tb <- rbind(tb, c(NA, sum(tb[, 2]), sum(tb[, 3]), sum(tb[, 4]), NA))
	tb <- as.data.frame(round(as.matrix(tb), 2))
	#tb[, 1] <- ifelse(add.sums, c(sq, "Sums"), sq)
	if (add.sums) {
		tb[, 1] <- c(sq, "Sums")
	} else {
		tb[, 1] <- sq
	}
	
	colnames(tb) <- c("Payment due", "Amount", "Principal", "Interest", "Balance")
	
	Sys.setlocale("LC_TIME", lct.old)
	
	return(tb)
}
