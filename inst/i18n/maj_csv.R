
library(data.table)
library(stringr)
library(utils)
library(polyglotr)

source("inst/i18n/extract_labels.R")


# Extraction of labels contained in the “/R” subdirectory

extraction_labels <- c(extract_labels(folder = "R"))


# Updating csv with previously extracted labels WITHOUT translation

update_csv(labels = extraction_labels, lang = "fr", lang_csv = "fr", translation = FALSE)
update_csv(labels = extraction_labels, lang = "es", lang_csv = "es", translation = FALSE)
update_csv(labels = extraction_labels, lang = "de", lang_csv = "de", translation = FALSE)
update_csv(labels = extraction_labels, lang = "sq", lang_csv = "al", translation = FALSE)
update_csv(labels = extraction_labels, lang = "pl", lang_csv = "pl", translation = FALSE)
update_csv(labels = extraction_labels, lang = "pt", lang_csv = "pt", translation = FALSE)
update_csv(labels = extraction_labels, lang = "tr", lang_csv = "tr", translation = FALSE)
update_csv(labels = extraction_labels, lang = "mk", lang_csv = "mk", translation = FALSE)
# update_csv(labels = extraction_labels, lang = "ja", lang_csv = "ja", translation = FALSE, encoding = "ISO_2022,locale=ja,version=4")  # review encoding
update_csv(labels = extraction_labels, lang = "zh-CN", lang_csv = "cn", translation = FALSE, encoding = "GBK") #"UTF16_BigEndian" or "GB18030"
update_csv(labels = extraction_labels, lang = "ko", lang_csv = "kr", translation = FALSE, encoding = "korean") # "GBK", "korean"


# Updating csv with labels extracted previously WITH translation

update_csv(labels = extraction_labels, lang = "fr", lang_csv = "fr")
update_csv(labels = extraction_labels, lang = "es", lang_csv = "es")
update_csv(labels = extraction_labels, lang = "de", lang_csv = "de")
update_csv(labels = extraction_labels, lang = "sq", lang_csv = "al")
update_csv(labels = extraction_labels, lang = "pl", lang_csv = "pl")
update_csv(labels = extraction_labels, lang = "pt", lang_csv = "pt")
update_csv(labels = extraction_labels, lang = "tr", lang_csv = "tr")
update_csv(labels = extraction_labels, lang = "mk", lang_csv = "mk")
# update_csv(labels = extraction_labels, lang = "ja", lang_csv = "ja", encoding = "ISO_2022,locale=ja,version=4")  # review encoding
update_csv(labels = extraction_labels, lang = "zh-CN", lang_csv = "cn", encoding = "GBK") #"UTF16_BigEndian" or "GB18030"
update_csv(labels = extraction_labels, lang = "ko", lang_csv = "kr", encoding = "korean") # "GBK", "korean"
