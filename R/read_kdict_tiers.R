##' Read a tier from all TextGrids within the kdict root directory
##'
##' @param kdict_path path to where all kdict batch folder are (e.g. b001, b002, etc.)
##' @param tier_name name(s) of TextGrid tier ('ipa', 'vowels', 'formants', 'tags', or 'comments')
##' @param .progress print out progress bar during read? (default is \code{FALSE})
##'
##' @import phonpack
##'
##' @importFrom furrr future_map2_dfr
##' @importFrom purrr map
##'
##' @export

read_kdict_tiers <- function(kdict_path, tier_names, .progress = FALSE) {

    stopifnot(dir.exists(kdict_path))
    stopifnot(all(tier_names %in% c('ipa', 'vowels', 'formants', 'tags', 'comments')))

    plan(multiprocess)

    list.files(
        path       = kdict_path,
        pattern    = "[a-z|A-Z]\\.TextGrid",
        recursive  = TRUE,
        full.names = TRUE
    ) %>%
    map(normalizePath) %>%
    expand.grid(
        textgrid_path = .,
        tier_name = tier_names,
        stringsAsFactors = FALSE
    ) %>%
    future_map2_dfr(
        .x = .$textgrid_path,
        .y = .$tier_name,
        .f = ~ kdict_textgrid2df(.x, .y),
        .progress = .progress
    ) %>%
    I()

}

kdict_textgrid2df <- function(textgrid_path, tier_name) {
    textgrid_info <- str_match(string  = basename(textgrid_path),
                               pattern = "(.*?)_([A-Z|a-z]+)\\.TextGrid$")

    info_df <- tibble(
        annotator   = textgrid_info[,3], # kr, ns, etc.
        tier_name   = tier_name,
        source_file = textgrid_info[,2]  # b017_100, etc.
    )

    vowels_df <-
        readTextGrid(textgrid_path) %>%
        getTierByName(tier_name) %>%
        getTierIntervals(discard_empty = TRUE)

    empty_df <- tibble(xmin = NA, xmax = NA, text = NA)

    if(nrow(vowels_df) > 0) {
        cbind(vowels_df, info_df, stringsAsFactors = FALSE)
    } else {
        cbind(empty_df, info_df, stringsAsFactors = FALSE)
    }

}
