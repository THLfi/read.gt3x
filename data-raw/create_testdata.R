library(data.table)


## Personal dropbox sharing links to (1) GT3X file, (2) ActiLife raw data output CSV
gt3x.fpath <- "https://www.dropbox.com/s/pi7m9b75gqzl64g/TAS1H30182785%20%282019-09-17%29.gt3x?dl=1"
csv.fpath <- "https://www.dropbox.com/s/hhdqjntarqxgpuu/TAS1H30182785%20%282019-09-17%29RAW.csv?dl=1"

file_directory <- file.path(getwd(), "tests")
csv.destfile <- file.path(file_directory, "TAS1H30182785 (2019-09-17).csv")
gt3x.destfile <- file.path(file_directory, "TAS1H30182785 (2019-09-17).gt3x")

## Download file into the /tests dir
if (!file.exists(gt3x.destfile)) download.file(gt3x.fpath, gt3x.destfile)
if (!file.exists(csv.destfile)) download.file(csv.fpath, csv.destfile)
