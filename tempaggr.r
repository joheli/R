library(dplyr)
library(ggplot2)

# aggregate
tempaggr <- function(dt, 
                     start = min(dt),
                     aggr = c("month", "day", "week", "year")) {
  
  aggr <- match.arg(aggr)
  
  fmt <- switch(aggr,
                month = "%b %Y",
                day = "%Y-%m-%d",
                week = "%Y %W",
                year = "%Y")
 
  tt <- table(cut(c(as.POSIXct(start), dt), breaks = aggr)[-1])
  
  rt <- data.frame(d = as.Date(names(tt)), n = as.numeric(tt)) %>%
    mutate(a = factor(format(d, fmt), levels = format(d, fmt)))
  
  return(rt)
}

# plot with ggplot2
tempaggr.plot <- function(ta, xl = "", yl = "", mn = "") {
  ggplot(ta, aes(x = a, y = n)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
    xlab(xl) +
    ylab(yl) +
    ggtitle(mn)
}