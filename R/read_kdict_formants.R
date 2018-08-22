##' Read a tier from all TextGrids within the kdict root directory
##'
##' @param kdict_path path to where all kdict batch folder are (e.g. b001, b002, etc.)
##' @param tracker name(s) of formant tracker ('praat' and/or 'forest'; default is both)
##' @param .progress print out progress bar during read? (default is \code{FALSE})
##'
##' @export

read_kdict_formants <- function(kdict_path, tracker = c("praat", "forest"), .progress = FALSE) {

    stopifnot(dir.exists(kdict_path))
    stopifnot(all(tracker %in% c('praat', 'forest')))

    plan(multiprocess)

    list.files(
        path       = kdict_path,
        pattern    = "formants\\.(praat|forest)\\.csv",
        recursive  = TRUE,
        full.names = TRUE
    ) %>%
    future_map_dfr(
        .progress = .progress,

        .f = function(formant_csv) {
        batch_ref <- str_remove(basename(formant_csv), "\\.formants\\.(praat|forest)\\.csv")

        if(grepl("forest", formant_csv)) {

            read_csv(file = formant_csv, col_types = cols()) %>%
                select(time, f1, f2, f3) %>%
                cbind(tracker = "forest", source_file = batch_ref, stringsAsFactors = FALSE)

        } else {

            read_csv(file = formant_csv, na = "--undefined--", col_types = cols()) %>%
                select(time = `time(s)`, f1 = `F1(Hz)`, f2 = `F2(Hz)`, f3 = `F3(Hz)`) %>%
                cbind(tracker = "praat", source_file = batch_ref, stringsAsFactors = FALSE)

        }
    })

}
