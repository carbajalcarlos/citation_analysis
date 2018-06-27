# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Loading input data
raw <- readFiles("0_data/CC01_business_models.bib", "0_data/CC02_business_models.bib",
                 "0_data/CC03_business_models.bib", "0_data/CC04_business_models.bib",
                 "0_data/RP01_business_models.bib", "0_data/RP02_business_models.bib",
                 "0_data/RP03_business_models.bib", "0_data/HCF_business_models.bib")

# categories <- read.csv(file = "0_input/jcr_categories.csv",
#                        header = TRUE, stringsAsFactors = FALSE)
# journals <- read.csv(file = "0_input/jcr_journals.csv",
#                      header = TRUE, stringsAsFactors = FALSE)
# mjl <- read.csv(file = "0_input/mjl_index.csv",
#                 header = TRUE, stringsAsFactors = FALSE)
# 
# # Parsing data form addtional lists
# categories <- categories[1:234, ]
# journals <- journals[1:12120, ]
# for (i in 1:ncol(mjl)) {
#   index <- grep(pattern = ">", x = mjl[, i])
#   if (length(index) > 0) {
#     mjl[index, i] <- gsub(pattern = ">", replacement = ",", x = mjl[index, i])
#   }
# }

# ----- Parsing the data -----
# Listing individual entries 
index <- c(grep(pattern = "^@[[:alpha:]]+\\{", x = raw)-1, length(raw)+1)
list_raw <- list()
for (i in 1:(length(index)-1)) {
  temp <- list(x = raw[(index[i]+1):(index[i+1]-1)])
  list_raw <- c(list_raw, temp)
}

# Retrieving all the fields used in the list
fields <- character()
for (i in 1:length(list_raw)) {
  index <- grep(pattern = " = {" , x = list_raw[[i]], fixed = TRUE)
  temp <- list_raw[[i]][index]
  temp <- trimws(gsub(pattern = "(^.*)= \\{+(.*)",replacement = "\\1", x = temp), which = "both")
  temp <- tolower(trimws(x = temp, which = "both"))
  fields <- c(fields, temp)
}
fields <- as.data.frame(table(fields), stringsAsFactors = FALSE)
fields <- fields[order(fields$fields, decreasing = FALSE), ]
fields <- c("entry-type", fields$fields)

# Creation of bibliography dataframe
rare <- data.frame(matrix(data = NA, ncol = length(fields), nrow = length(list_raw)), 
                    stringsAsFactors = FALSE)
colnames(rare) <- make.names(fields)
for (i in 1:length(list_raw)) {
  index <- grep(pattern = " = {" , x = list_raw[[i]], fixed = TRUE)
  fields <- list_raw[[i]][index]
  fields <- gsub(pattern = "(^.*) = \\{+(.*)",replacement = "\\1", x = fields)
  fields <- make.names(tolower(trimws(x = fields, which = "both")))
  # Filling individual attributes
  rare$entry.type[i] <- gsub(pattern = "^@(.*)\\{(.*)",replacement = "\\1", x = list_raw[[i]][1])
  index <- c(index, length(list_raw[[i]]))
  for (j in 1:length(fields)) {
    pat <- paste(c("^", fields[j], "$"), collapse = "")
    column <- grep(pattern = pat, x = colnames(rare))
    clean <- c("abstract", "author")
    if (is.element(el = fields[j], set = clean)) {
      temp <- paste(list_raw[[i]][index[j]:(index[j+1]-1)], collapse = "")
      temp <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temp)
    } else {
      temp <- paste(list_raw[[i]][index[j]:(index[j+1]-1)], collapse = ";")
    }
    if (fields[j] == "author") {
      temp <- gsub(pattern = ".*\\{(.*)\\}.*",replacement = "\\1", x = temp)
    } else  {
      temp <- gsub(pattern = ".*\\{{2}(.*)\\}{2}.*",replacement = "\\1", x = temp)
    }
    rare[i,column] <- tolower(temp)
  }
}

# # Invert order to comply with chronographical order ascending.
# rare <- rare[nrow(rare):1, ]

# Removing repetitive entries
rare <- rare[!duplicated(rare$unique.id), ]

# ----- Closing project -----
# Insert name and store of the interest dataframe
bumo_rare <- rare
save(file = "1_process/bumo_rare.Rdata", list = "bumo_rare")

# Removing and storing information 
rm(raw)
rm(list_raw)
rm(rare)
rm(clean)
rm(column)
rm(fields)
rm(i)
rm(j)
rm(index)
rm(pat)
rm(temp)

# # Saving the working space
save.image(file = "1_process/ws_manual.Rdata")
