function (DATA) 
{
  listAU = strsplit(as.character(DATA$AU), ";")
  listAU = lapply(listAU, function(l) trim.leading(l))
  if (DATA$DB[1] == "scopus") {
    listAU = lapply(listAU, function(l) {
      l = trim(l)
      l = sub(" ", ",", l, fixed = TRUE)
      l = sub(",,", ",", l, fixed = TRUE)
      l = gsub(" ", "", l, fixed = TRUE)
    })
  }
  FirstAuthors = gsub(",", " ", unlist(lapply(listAU, function(l) l[[1]])))
  if (!is.null(DATA$J9)) {
    no_art = which(is.na(DATA$J9) & is.na(DATA$JI))
    DATA$J9[no_art] = DATA$SO[no_art]
    ind = which(is.na(DATA$J9))
    DATA$J9[ind] = trim(gsub("\\.", " ", DATA$JI[ind]))
    SR = paste(FirstAuthors, DATA$PY, DATA$J9, sep = ", ")
  }
  else {
    no_art = which(is.na(DATA$JI))
    DATA$JI[no_art] = DATA$SO[no_art]
    J9 = trim(gsub("\\.", " ", DATA$JI))
    SR = paste(FirstAuthors, DATA$PY, J9, sep = ", ")
  }
  st <- i <- 0
  while (st == 0) {
    ind <- which(duplicated(SR))
    if (length(ind) > 0) {
      i <- i + 1
      SR[ind] = paste0(SR[ind], "-", letters[i], sep = "")
    }
    else {
      st <- 1
    }
  }
  DATA$SR <- gsub("\\s+", " ", SR)
  return(DATA)
}
