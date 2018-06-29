# ----- Initialisation -----
# Loading required libraries
require(bibliometrix, quietly = TRUE)
require(stringdist, quietly = TRUE)
require(tm, quietly = TRUE)
require(countrycode, quietly = TRUE)

# Loading input data
load(file = "1_process/bumo_rare.Rdata")
rare <- bumo_rare 
rare <- rare[order(rare$year, rare$author), ]

# ----- Author -----
# Extracting unique authors
original <- unlist(strsplit(rare$author, split = " and "))

original <- original[!duplicated(tolower(original))]
authors <- as.data.frame(original, stringsAsFactors = FALSE)
authors$name <- authors$original
authors$order <- 1:nrow(authors)
authors$length <- nchar(authors$original)
authors$phonetic <- phonetic(authors$original)
# Deduplication using phonetic and string distance measurements
tokens <- as.data.frame(table(authors$phonetic), stringsAsFactors = FALSE)
tokens <- subset(x = tokens, subset = Freq > 1)
tokens <- tokens$Var1
for (i in 1:length(tokens)) {
  #i <- grep(pattern = "juellskielse, g", x = authors$name); i <- grep(pattern = authors$phonetic[i], x = tokens)
  index <- grep(pattern = tokens[i], x = authors$phonetic)
  subset.a <- authors[-index, ]
  subset.b <- authors[index, ]
  temp <- stringdistmatrix(a = subset.b$name, b = subset.b$name, method = "jw")
  temp <- data.frame(which(temp < 0.10, arr.ind = TRUE))
  temp <- subset(x = temp, subset = !(row == col))
  if (nrow(temp) == 0) { next }
  # Reduction to only one value without multiple parameter selection
  subset.c <- subset.b[temp$row, ]
  subset.b <- subset.b[-temp$row, ]
  index <- which(subset.c$length == max(subset.c$length))[1]
  subset.c$name <- subset.c$name[index]
  subset.b <- rbind(subset.b, subset.c)
  authors <- rbind(subset.a, subset.b)
}

# ----- Manual corrections
# Missing commas
index <- grep(pattern = "[,]", x = authors$name, invert = TRUE)
authors$name[index] <- gsub(pattern = "(^[[:alpha:]]+)(.*$)",
                            replacement = "\\1,\\2", x = authors$name[index])
index <- grep(pattern = "(^.+),(.+,.+$)", x = authors$name)
authors$name[index] <- gsub(pattern = "(^.+),(.+,.+$)",
                            replacement = "\\1\\2", x = authors$name[index])

# abbrviated second names
index <- grep(pattern = "^.*, [[:alpha:]]{2}$", x = authors$name)
if (length(index) != 0) {
  for (i in index) {
    temp <- gsub(pattern = "(^.*, [[:alpha:]]{1})([[:alpha:]]{1}$)",
                 replacement = "\\2", x = authors$name[i])
    temp <- iconv(x = temp, from = "UTF-8", "ASCII//TRANSLIT") # Transliteration of the names
    if (is.element(el = temp, set = c("a", "e", "i", "o", "u")) == FALSE) {
      authors$name[i]<- gsub(pattern = "(^.*, [[:alpha:]]{1})([[:alpha:]]{1}$)",
                             replacement = "\\1 \\2", x = authors$name[i])
    }
  }
}
# Removing general signs
index <- grep(pattern = "[\\.()]", x = authors$name)
authors$name[index]<- gsub(pattern = "[\\.()]", replacement = "", x = authors$name[index])

# ----- Authors name abbrviation
authors$unique <- !duplicated(authors$name)
authors$abbr <-authors$name
for (i in 1:nrow(authors)) {
  #temp <- trimws(unlist(strsplit("carbajal,", split = ",")), which = "both")
  temp <- trimws(unlist(strsplit(authors$abbr[i], split = ",")), which = "both")
  if (length(temp) > 1) {
    tokens <- trimws(unlist(strsplit(temp[2], split = " ")), which = "both")
    name <- paste(c(toupper(temp[1]), ", "), collapse = "")
    for (j in 1:length(tokens)) {
      name <- paste(c(name, toupper(substr(tokens[j], 1, 1))), collapse = "")
    }
    authors$abbr[i] <- name
  } else {
    authors$abbr[i] <- toupper(temp)
  }
}

# Distinguishing authors
#authors$abbr <- c(authors$abbr[177:352], authors$abbr[265:352], authors$abbr[265:352])
authors$a.dup <- authors$abbr
#index[authors$unique == FALSE] <- FALSE
index <- duplicated(authors$a.dup) & authors$unique
i<-2
while(sum(index) != 0) {
  authors$a.dup[index] <- paste(authors$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(authors$a.dup) & authors$unique
}
# Replacing non unique authors ID
temp <- which(authors$unique == FALSE)
for (i in temp) {
  index <- grep(pattern = authors$name[i], x = authors$name, fixed = TRUE)
  index <- index[which(authors$unique[index] == TRUE)[1]]
  authors$a.dup[i] <- authors$a.dup[index]
}

# Constructing final structure
authors <- authors[order(authors$order), ]
authors$local.id <- paste("a", 1:nrow(authors),sep = "_")
authors <- subset(x = authors, select = c("local.id", "name", "a.dup", "phonetic", "original"))
colnames(authors) <- c("local.id", "name", "short", "phonetic", "original")
rownames(authors) <- authors$local.id

# Introduction of authors name and additional 
rare$author.full <- NA
rare$author.shrt <- NA
rare$author.numb <- NA
rare$author.fail <- 0
# Routine to find and construct clean authors name
for (i in 1:nrow(rare)) {
  #i <- grep(pattern = "juellskielse", x = rare$author)
  temp <- trimws(unlist(strsplit(rare$author[i], split = "and")), which = "both")
  rare$author.numb[i] <- length(temp)
  for (j in temp) {
    index <- grep(pattern = j, x = authors$original, fixed = TRUE)
    if (length(index) != 0) {
      rare$author.full[i] <- paste(na.omit(c(rare$author.full[i], authors$name[index])), collapse = "; ")
      rare$author.shrt[i] <- paste(na.omit(c(rare$author.shrt[i], authors$short[index])), collapse = "; ")
    } else {
      rare$author.fail[i] <- rare$author.fail[i]+1
      rare$author.full[i] <- paste(na.omit(c(rare$author.full[i], j)), collapse = "; ")
      rare$author.shrt[i] <- paste(na.omit(c(rare$author.full[i], toupper(j))), collapse = "; ")
    }
    #j<-temp[2]
  }
  # Debugging rutine
  #rare$author[i];rare$author.fail[i]; rare$author.full[i]; rare$author.shrt[i];
  #i<-i+1
}
# Identification of the first author
temp <- strsplit(rare$author.shrt, split = "; ")
rare$author.first <- trimws(unlist(lapply(temp, function(l) l[1])), which = "both")

# Removing errors in the filling
index <- which(is.na(rare$author))
rare$author.shrt[index] <- NA
rare$author.full[index] <- NA
rare$author.numb[index] <- 0
rare$author.first[index] <- NA

# ----- Title -----
#Removing unwanted signs
index <- grep(pattern = ";   ", x = rare$title, fixed = TRUE)
rare$title[index] <- gsub(pattern = ";   ", replacement = " ", x = rare$title[index])
index <- grep(pattern = ".*\\.$", x = rare$title)
rare$title[index] <- gsub(pattern = "\\.$", replacement = "", x = rare$title[index])
index <- grep(pattern = "r\\&d", x = rare$title, fixed = TRUE)
rare$title[index] <- gsub(pattern = "r\\\\&d", replacement = "R&D", x = rare$title[index])
index <- grep(pattern = "[`{'}]+", x = rare$title)
rare$title[index] <- gsub(pattern = "[`{'}]", replacement = "", x = rare$title[index])
index <- grep(pattern = " - +", x = rare$title)
rare$title[index] <- gsub(pattern = "- +", replacement = "-", x = rare$title[index])
index <- !grepl(pattern = "-and", x = rare$title) & grepl(pattern = " +-[[:alpha:]]+", x = rare$title)
rare$title[index] <- gsub(pattern = "( -)", replacement = ": ", x = rare$title[index])
# Capitilising special characters
index <- grep(pattern = "[:] [[:alpha:]]", x = rare$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^:]+: ([[:alpha:]]).*", replacement = "\\1", x = rare$title[i]))
  rare$title[i] <- gsub(pattern = "(^[^;]+: )[[:alpha:]](.*)",
                         replacement = paste(c("\\1", temp, "\\2"), collapse = ""), x = rare$title[i])
}
index <- grep(pattern = "[?] [[:alpha:]]", x = rare$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^?]+\\? ([[:alpha:]]).*", replacement = "\\1", x = rare$title[i]))
  rare$title[i] <- gsub(pattern = "(^[^?]+?) *\\? [[:alpha:]](.*)",
                         replacement = paste(c("\\1? ", temp, "\\2"), collapse = ""), x = rare$title[i])
}
index <- grep(pattern = "[.] [[:alpha:]]", x = rare$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^.]+\\. ([[:alpha:]]).*", replacement = "\\1", x = rare$title[i]))
  rare$title[i] <- gsub(pattern = "(^[^.]+)\\. [[:alpha:]](.*)",
                         replacement = paste(c("\\1: ", temp, "\\2"), collapse = ""), x = rare$title[i])
}
index <- grep(pattern = "[!] [[:alpha:]]", x = rare$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^!]+! ([[:alpha:]]).*", replacement = "\\1", x = rare$title[i]))
  rare$title[i] <- gsub(pattern = "(^[^!]+! )[[:alpha:]](.*)",
                         replacement = paste(c("\\1", temp, "\\2"), collapse = ""), x = rare$title[i])
}
# Capitilising first letter
for (i in 1:nrow(rare)) {
  rare$title[i] <- paste(c(toupper(substr(rare$title[i], 1, 1)), substr(rare$title[i], 2, nchar(rare$title[i]))), collapse = "")
}

# ----- Journal -----
# Replacing journal name by journal iso
temp <- which(is.na(rare$journal))
rare$journal[temp] <- gsub(pattern = "[[:punct:]]", replacement = "", rare$journal.iso[temp])

# Creating unique journals
index <- grep(pattern = ";   ", x = rare$journal)
rare$journal[index] <- gsub(pattern = ";   ", replacement = " ", x = rare$journal[index])
rare$journal <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare$journal)
journals <- subset(x = rare, select = c("journal", "journal.iso"))
colnames(journals) <- c("original", "iso")
journals <- journals[!is.na(journals$original), ]
journals <- journals[!duplicated(journals$original), ]

# Cleaning names
journals$name <- journals$original
index <- grep(pattern = "\\&", x = journals$name)
journals$name[index] <- gsub(pattern = "\\\\&", replacement = "and", journals$name[index])
index <- grep(pattern = "[-]", x = journals$name)
journals$name[index] <- gsub(pattern = "[-]", replacement = " ", journals$name[index])
# manual changes
index <- grep(pattern = "bmj", x = journals$name)
journals$name[index] <- gsub(pattern = "bmj", replacement = "", journals$name[index])
index <- grep(pattern = "(^| )it ", x = journals$name)
journals$name[index] <- gsub(pattern = "it", replacement = "I T", journals$name[index])
index <- grep(pattern = "mis", x = journals$name)
journals$name[index] <- gsub(pattern = "mis", replacement = "M I S", journals$name[index])

# Triming and removing double spaces
journals$name <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = journals$name), which = "both")

# Inserting ISO manually<
index <- grep(pattern = "organization studies", x = journals$name)
journals$iso[index] <- tolower("Organ. Stud.")
index <- grep(pattern = "journal of the association for information science and technology", x = journals$name)
journals$iso[index] <- tolower("J. Assoc. Inf. Sci. Technol.")

index <- which(is.na(journals$iso))
if (length(index) > 0) {
  temp <- removeWords(journals$name[index], stopwords(kind = "en"))
  temp <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temp)
  temp <- strsplit(x = temp, split = " ")
  for (i in 1:length(temp)) {
    for (j in 1:length(temp[[i]])) {
      temp[[i]][j] <- paste(c(toupper(substr(temp[[i]][j], 1, 1)), 
                              substr(temp[[i]][j], 2, 5), "."), collapse = "")
    }
    temp[[i]] <- paste(unlist(temp[[i]]), collapse = " ")
    temp[[i]] <- paste(c("xx.", temp[[i]]), collapse = " ")
  }
  journals$iso[index] <- tolower(unlist(temp))
}

# abbrviation of journals names
journals$abbr <- journals$name
journals$abbr <- removeWords(x = journals$abbr, stopwords(kind = "en"))
journals$abbr <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = journals$abbr))
for (i in 1:nrow(journals)) {
  #i <- grep(pattern = "ambio", x = journals$name)
  temp <- unlist(strsplit(x = journals$abbr[i], split = " "))
  name <- character()
  if (length(temp) > 1) {
    for (j in 1:length(temp)) {
      name <- paste(c(name, toupper(substr(temp[j], 1, 1))), collapse = "")
    }
  } else if (nchar(temp) > 1) {
    name <- paste(c(toupper(substr(temp, 1, 1)), tolower(substr(temp, 2, 3))), collapse = "")
  } else {
    name <- toupper(substr(temp, 1, 1))
  }
  journals$abbr[i] <- name
}

# Distinguishing Journals
journals$a.dup <- journals$abbr
index <- duplicated(journals$a.dup)
i<-2
while(sum(index) != 0) {
  journals$a.dup[index] <- paste(journals$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(journals$a.dup)
}

# Constructing final structure
journals <- journals[order(journals$a.dup), ]
journals$local.id <- paste("j", 1:nrow(journals),sep = "_")
journals <- subset(x = journals, select = c("local.id", "name", "a.dup", "iso", "original"))
colnames(journals) <- c("local.id", "name", "abbr", "iso", "original")
rownames(journals) <- journals$local.id

# ----- addition of journal information
rare$journal.clean <- NA
rare$journal.abbr <- NA
rare$journal.fail <- 0
# Routine to find and construct clean journals name
tokens <- unique(na.omit(rare$journal))
for (i in tokens) {
  index <- which(x = journals$original == i)
  # Replacing the valid info
  temp <- which(x = rare$journal == i)
  if (length(index) != 0) {
    rare$journal.clean[temp] <- journals$name[index][1]
    rare$journal.iso[temp] <- journals$iso[index][1]
    rare$journal.abbr[temp] <- journals$abbr[index][1]
  } else {
    rare$journal.clean[temp] <- rare$journal[temp]
    rare$journal.fail[temp] <- rare$journal.fail[temp]+1
  }
}

# ----- Number -----
rare$number <- as.numeric(gsub("[^0-9]", "", rare$number))

# ----- Data entry -----
rare$entry.type <- as.factor(rare$entry.type)
levels(rare$entry.type) <- c("article", "book", "proceeding")
rare$entry.type <- as.character(rare$entry.type)

# ----- Keywords -----
# Formatting author keywords
rare$keywords <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = rare$keywords), which = "both" )
# Counting author keywords
rare$keywords.num <- NA
index <- which(is.na(rare$keywords) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(rare$keywords[i], split = ";"))
  rare$keywords.num[i] <- length(temp)
}
# Extracting unique author keywords
kw.author <- trimws(unlist(strsplit(rare$keywords, split = ";")), which = "both")
kw.author <- as.data.frame(table(kw.author), stringsAsFactors = FALSE)

# Formating WoS keywords
rare$keywords.plus <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = rare$keywords.plus), which = "both" )
# Counting wos keywords
rare$keywords.plus.num <- NA
index <- which(is.na(rare$keywords.plus) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(rare$keywords.plus[i], split = ";"))
  rare$keywords.plus.num[i] <- length(temp)
}
# Extracting unique wos keywords
kw.wos <- trimws(unlist(strsplit(rare$keywords.plus, split = ";")), which = "both")
kw.wos <- as.data.frame(table(kw.wos), stringsAsFactors = FALSE)

# ----- Research area -----
# Formating research areas
index <- grep(pattern = "\\\\&", x = rare$research.areas)
rare$research.areas[index] <- gsub(pattern = "\\\\&", replacement = "and", x = rare$research.areas[index])
rare$research.areas <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = rare$research.areas), which = "both" )
# Counting research areas
rare$research.areas.num <- NA
index <- which(is.na(rare$research.areas) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(rare$research.areas[i], split = ";"))
  rare$research.areas.num[i] <- length(temp)
}
# Extracting unique research areas
ra.wos <- trimws(unlist(strsplit(rare$research.areas, split = ";")), which = "both")
ra.wos <- as.data.frame(table(ra.wos), stringsAsFactors = FALSE)

# Formating categories
index <- grep(pattern = ";   ", x = rare$web.of.science.categories, fixed = TRUE)
rare$web.of.science.categories[index] <- gsub(pattern = ";   ", replacement = " ", x = rare$web.of.science.categories[index])
index <- grep(pattern = "\\\\&", x = rare$web.of.science.categories)
rare$web.of.science.categories[index] <- gsub(pattern = "\\\\&", replacement = "and", x = rare$web.of.science.categories[index])
rare$web.of.science.categories <- trimws(gsub(pattern = "[[:space:]]+",
                                              replacement = " ", x = rare$web.of.science.categories), which = "both" )
# Counting wos categories
rare$wos.categories.num <- NA
index <- which(is.na(rare$web.of.science.categories) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(rare$web.of.science.categories[i], split = ";"))
  rare$wos.categories.num[i] <- length(temp)
}
# Extracting unique wos categories
cat.wos <- trimws(unlist(strsplit(rare$web.of.science.categories, split = ";")), which = "both")
cat.wos <- as.data.frame(table(cat.wos), stringsAsFactors = FALSE)

# ----- Abstract -----
# Manual cleaning an removing of uneeded symbols
rare$abstract.raw <- rare$abstract
index <- grep(pattern = "`'", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "`'", replacement = " <", x = rare$abstract[index])
index <- grep(pattern = "``", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "``", replacement = " <", x = rare$abstract[index])
index <- grep(pattern = "(^| )`", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "(^| )`", replacement = " <", x = rare$abstract[index])
index <- grep(pattern = "\\{''\\}", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "[[:punct:]]*\\{''\\}", replacement = "> ", x = rare$abstract[index])
index <- grep(pattern = "[^s]\' ", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "[^s]\' ", replacement = "> ", x = rare$abstract[index])

index <- grep(pattern = "\\{\\[\\}", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "\\{\\[\\}", replacement = "[", x = rare$abstract[index])

index <- grep(pattern = "\\&", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "\\&", replacement = "and", x = rare$abstract[index])
index <- grep(pattern = "\\\\", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "\\\\", replacement = "", x = rare$abstract[index])
index <- grep(pattern = "\\{\\*\\}", x = rare$abstract)
rare$abstract[index] <- gsub(pattern = "\\{\\*\\}", replacement = "*", x = rare$abstract[index])

# Removing undesired spaces
index <- grep(pattern = "[[:space:]]+", x = rare$abstract)
rare$abstract[index] <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = rare$abstract[index]), which = "both")

# ----- Cited references -----
# Creation of a lis of cited references
raw <- trimws(unlist(strsplit(rare$cited.references, split = ";   ")), which = "both")
references <- as.data.frame(x = table(raw), stringsAsFactors = FALSE)

# ----- Times cited -----
rare$times.cited <- as.numeric(sub("\\D*(\\d+).*", "\\1", rare$times.cited))

# ----- Year -----
rare$year <- as.numeric(sub("\\D*(\\d+).*", "\\1", rare$year))

# ----- Unique ID and database -----
rare$database <- gsub(pattern = "([^:]+):([^:]+)", replacement = "\\1", x = rare$unique.id)
rare$unique.id <- gsub(pattern = "([^:]+):([^:]+)", replacement = "\\1\\2", x = rare$unique.id)

# ----- Reprint address -----
rare$reprint.address <- NA
for (i in 1:nrow(rare)) {
  temp <- unlist(strsplit(rare$affiliation[i], ";   "))
  index <- grep(pattern = "(reprint author)", x = temp, fixed = TRUE)
  if (length(temp) > 0) {
    rare$reprint.address[i] <- gsub(pattern = "(^.*)\\(reprint author\\)(.*$)", replacement = "\\2", x = temp[index[1]])
    rare$reprint.address[i] <- gsub(pattern = "^[^[:alnum:]]*[[:space:]]+(.+$)", replacement = "\\1", x = rare$reprint.address[i])
    rare$reprint.address[i] <- trimws(x = rare$reprint.address[i], which = "both")
  }
}

# ----- Booktitle -----
# Extracting unique books
index <- which(!is.na(rare$booktitle))
books <- as.data.frame(cbind(rare$booktitle[index], rare$book.author[index],
                             rare$type[index], rare$year[index]), stringsAsFactors = FALSE)
names(books) <- c("name.raw", "book.author", "type", "year")
books <- books[!duplicated(paste(books$name.raw, books$year)), ]
books <- books[order(books$year, books$name.raw), ]
# Removing unwanted signs
books$name <- books$name.raw
index <- grep(pattern = "\\\\&", x = books$name)
books$name[index] <- gsub(pattern = "\\\\&", replacement = "and", x = books$name[index])
index <- grep(pattern = "/ ", x = books$name)
books$name[index] <- gsub(pattern = "/ ", replacement = "/", x = books$name[index])
index <- grep(pattern = ", proceedings$", x = books$name)
books$name[index] <- gsub(pattern = "(.*), proceedings$", replacement = "\\1", x = books$name[index])

# Generating short version 
books$name.short <- books$name
books$name.short <- removeWords(x = books$name.short, stopwords(kind = "en"))
books$name.short <- gsub(pattern = "[[:punct:]]", replacement = "", x = books$name.short)
books$name.short <- trimws(x = gsub(pattern = "[[:space:]]+", replacement = " ", x = books$name.short), which = "both")

temp <- strsplit(x = books$name.short, split = " ")
temp <- lapply(temp, function(l) l[1:4])
temp <- lapply(temp, function(l) paste(na.omit(l), collapse = " "))
books$name.short <- unlist(temp)

# abbrviation of books names
books$abbr <- books$name.short
books$abbr <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = books$abbr))

for (i in 1:nrow(books)) {
  #i <- grep(pattern = "ambio", x = books$name)
  temp <- unlist(strsplit(x = books$abbr[i], split = " "))
  name <- character()
  if (length(temp) > 1) {
    for (j in 1:length(temp)) {
      name <- paste(c(name, toupper(substr(temp[j], 1, 1))), collapse = "")
    }
  } else if (nchar(temp) > 1) {
    name <- paste(c(toupper(substr(temp, 1, 1)), tolower(substr(temp, 2, 3))), collapse = "")
  } else {
    name <- toupper(substr(temp, 1, 1))
  }
  books$abbr[i] <- name
}

# Distinguishing Journals
books$a.dup <- books$abbr
index <- duplicated(books$a.dup)
i<-2
while(sum(index) != 0) {
  books$a.dup[index] <- paste(books$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(books$a.dup)
}

# Constructing final structure
books <- books[order(books$year, books$a.dup), ]
books$local.id <- paste("book", 1:nrow(books),sep = "_")
books <- subset(x = books, select = c("local.id", "name", "book.author", "type", "year",
                                      "name.short", "a.dup", "name.raw"))

colnames(books) <- c("local.id", "name", "book.author", "type", "year", "name.short", "abbr", "original")
rownames(books) <- books$local.id

# ----- addition of journal information
rare$booktitle.clean <- NA
rare$booktitle.short <- NA
rare$booktitle.abbr <- NA
rare$booktitle.id <- NA
rare$booktitle.fail <- 0
# Routine to find and construct clean journals name
index <- which(!is.na(rare$booktitle))
tokens <- unique(na.omit(paste(rare$booktitle[index], rare$year[index])))
books$original.year <- paste(books$original, books$year)
for (i in tokens) {
  index <- which(x = books$original.year == i)
  # Replacing the valid info
  temp <- which(x = paste(rare$booktitle, rare$year) == i)
  if (length(index) != 0) {
    rare$booktitle.clean[temp] <- books$name[index]
    rare$booktitle.short[temp] <- books$name.short[index]
    rare$booktitle.abbr[temp] <- books$abbr[index]
    rare$booktitle.id[temp] <- books$local.id[index]
    } else {
    rare$booktitle.clean[temp] <- rare$booktitle[temp]
    rare$booktitle.fail[temp] <- rare$booktitle.fail[temp]+1
  }
}

# ----- Source -----
rare$source <- rare$journal.clean
rare$source.short <- rare$journal.iso
rare$source.abbr <- rare$journal.abbr

index <- which(is.na(rare$source))
rare$source[index] <- rare$booktitle.clean[index]
rare$source.short[index] <- rare$booktitle.id[index]
rare$source.abbr[index] <- rare$booktitle.abbr[index]


# ----- Single register SR -----
rare$simple.register <- toupper(with(rare, paste(author.first, source.short, year, sep = "; ")))
