# Zusammenfassung von seltenen Ausprägungen eines Faktors
# zu "Rest"
#
# Parameter:
# ctg			kategorische Variable (Faktor)
# min.occ		Mindestvorkommen, unter welchem die Ausprägung
#				zu Rest gezählt wird
# rest.label	Bezeichnung des Restes

relabel.cat <- function(ctg, min.occ=10, rest.label="Rest") {
	ct <- ctg[,drop=T]
	tb <- table(ct)
	levels(ct)[levels(ct) %in% names(tb[tb < min.occ])] <- rest.label
	return(relevel(ct, ref=rest.label))
}
