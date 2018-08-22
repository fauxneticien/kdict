##' Check that all kdict files are located in appropriate folders (e.g. b001_file01 inside folder b001_file01, not b001_file02)
##'
##' @param kdict_path path to where all kdict batch folder are (e.g. b001, b002, etc.)
##'
##' @export

check_files_locations <- function(kdict_path) {

    stopifnot(dir.exists(kdict_path))

    list.files(
        path       = kdict_path,
        recursive  = TRUE,
        full.names = TRUE
    ) %>%
    tibble(path = .) %>%
    mutate(
        basename  = basename(path),
        batch_ref = str_extract(basename, "b\\d+_(file)?\\d+") %>% str_remove("file"),
        directory = dirname(path) %>% str_extract("(?<=[\\|/])[^\\|/]+?$")
    ) %>%
    filter(!str_detect(basename, "(yml|pdf)$")) %>%
    filter(directory != batch_ref)

}
