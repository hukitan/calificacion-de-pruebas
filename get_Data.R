if (require(gsheet)) {
    library(gsheet)
} else {
    install.packages("gsheet", dep = TRUE)
    library(gsheet)
}

pittsburg <- gsheet2tbl("docs.google.com/spreadsheets/d/1uwcQvyIvxhIO0tNeykoPK8f1p7v7b9DIlUNf-1kozYc")
