#' Function to extract labels
#'
#' @param folder file directory
#'
#' @return an extraction of the labels contained in the directory files
#' @importFrom stringr str_subset str_extract_all str_remove_all
#' @export
#'
#' @examples extract_labels(folder = "R")
extract_labels <- function(folder = "R") {
  files <- list.files(folder)
  list_extractions <- lapply(
    X = files,
    FUN = function(file) {
      read_file <- readLines(file.path(folder, file))
      extraction <- str_extract_all(
        string = str_subset(read_file, "i18n"),
        pattern = 'i18n\\(".*?"\\)'
      ) |>
        unlist()
      extraction
    }
  )
  extract_labels <- unlist(list_extractions, recursive = TRUE)
  str_remove_all(unique(extract_labels), paste(c("i18n", "\"", "\\)", "\\("), collapse = "|"))
}



#' Update all csvs that are in inst/i18n
#'
#' @param labels results of label extractions
#' @param lang the language that you want to translate the text into. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param lang_csv the name of the csv file
#' @param translation TRUE or FALSE if you want to translate the language
#' @param ... other arguments passed to datamods::translate_labels
#'
#' @return all csvs updated
#' @importFrom data.table merge fwrite data.table fread unique
#' @export
#'
#' @examples update_csv(labels = extract_labels(folder = "R"))
#' new_csv_fr <- fread("inst/i18n/fr.csv")
update_csv <- function(labels,
                       lang,
                       lang_csv,
                       translation = TRUE,
                       ...) {
  old <- fread(file = sprintf("inst/i18n/%s.csv", lang_csv), encoding = "UTF-8", fill = TRUE)
  new <- merge(
    x = data.table(label = unique(labels)),
    y = old,
    by = "label",
    all.x = TRUE
  )

  if (isTRUE(translation)) {
    final <- rbind(
      new[!is.na(translation)],
      translate_labels(labels = new[is.na(translation)]$label, target_language = lang, ...),
      fill = TRUE
    )
  } else {
    final <- new
  }

  fwrite(final, file = sprintf("inst/i18n/%s.csv", lang_csv), row.names = FALSE, na = '', quote = TRUE)
}



#' Translate labels
#'
#' @param labels labels to translate
#' @param source_language the language that you want to translate the text into. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param target_language the language of the text that you want to translate. See polyglotr::google_supported_languages for the Table with the codes of available languages
#' @param encoding Name of encoding. See stringi::stri_enc_list() for a complete list
#'
#' @importFrom polyglotr google_translate
#' @importFrom stringr str_conv
#' @importFrom data.table data.table
#'
#' @return a data frame with translated labels
#' @export
#'
#' @examples translate_labels(labels = extract_labels(folder = "R"))
translate_labels <- function(labels,
                             source_language = "en",
                             target_language = "fr",
                             encoding = "UTF-8") {

  translation <- polyglotr::google_translate(
    text = labels,
    target_language = target_language,
    source_language = source_language
  )
  data.table(
    label = labels,
    translation = translation |>
      unlist() |>
      str_conv(encoding),
    comment = "Automatically translated"
  )
}

