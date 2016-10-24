source("read_csv_batch.r")
source("rbinder.r")

read.rbinder <- function(file.pattern,
                         readf = read.csv2,
                         path = ".",
                         file.ending = "csv",
                         unique.field.name,
                         result = c("only joined data-frame", 
                                    "report and data-frame"),
                         pos = 2,
                         ...) {
  result = match.arg(result)
  read.csv.batch(file.pattern, NULL, readf, path, file.ending,
                 NULL, pos, ...)
  rr <- rbinder(file.pattern, NULL, unique.field.name, pos)
  rtn <- rr
  if (result == "only joined data-frame") rtn <- rr$d
  return(rtn)
}