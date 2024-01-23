#import a dataset from spreadsheet
download.file(url = "https://ndwonloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
#read csv poi ("cartella/ nome e estensione file")

#take a peak at the data
#head fa vedere le prime sei righe
head(surveys)
#csv2 se hanno ;
#read.tablen .tsv
#read.table(file, header = FALSE, sep = "", quote = "\"'",
        #   dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
         #  row.names, col.names, as.is = !stringsAsFactors, tryLogical = TRUE,
        #   na.strings = "NA", colClasses = NA, nrows = -1,
        #   skip = 0, check.names = TRUE, fill = !blank.lines.skip,
        #   strip.white = FALSE, blank.lines.skip = TRUE,
        #   comment.char = "#",
         #  allowEscapes = FALSE, flush = FALSE,
        #   stringsAsFactors = FALSE,
         #  fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)

#view(surveys)
str(surveys)
#qui è per contare
dim(surveys)
nrow(surveys)
ncol(surveys)
#x vedere le ultime righe anzichè head usiamo tail
tail(surveys)

#per avere i nomi delle colonne
names(surveys)
colnames(surveys)
rownames(surveys)

#summary per ogni colonna (val min massimo, media ,3 rd quart etc)
summary(surveys)
#Indexing and subsetting [row, column]
surveys[1, 6]

surveys[1, 6 | 2, 4]
#extract tutto il primo row
surveys[1, ]
#tutta colonna 1
surveys[,1]
surveys[1:3,5:6]
#49 è uguale a surveys[c(1, 2, 3),c(5, 6)]
surveys[1:4,5:10]

#remove first column [-la cosa che vuoi eliminare
surveys[,-1]

#give me this column
surveys[,"sex"]
summary(surveys[,"sex"])
# dollar $ is for what are you. looking for

surveys$plot_id
#in questo caso la risposta che ci torna
#indietro non è una tabella ma un vettore!!

#challenge
data.frame200 <- surveys[200,]
data.frame200
nrow(data.frame200)
tail(surveys)
tail(surveys)[6,]
nrow(surveys)
surveys[34786,]
surveys[34786/2,]
surveys_middle <- surveys[34786/2,]

#correzione
#66 ok
#67 ok
#68:72 --> surveys[nrow(surveys),]
#74 ok

