#import a dataset from spreadsheet
download.file(url = "https://ndwonloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
#read csv poi ("cartella/ nome e estensione file")

#take a peak at the data
#head fa vedere le prime sei righe
head(surveys)
