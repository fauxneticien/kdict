#' Helper functions for wrangling kdict vowel- and word-level transcriptions
#'
#'
#' @section Wrangling functions:
#'
#' \describe{
#'    \item{`get_cons_context()`}{returns a vector of consonantal contexts for the
#'    vowels within a given transcription. Will return an error if vowels vector and
#'    transcription do not have identical vowels.}
#'
#'    \item{`read_vowel_defs()`}{returns the table of vowel definitions and features
#'    contained within the package.}
#'
#'    \item{`remove_diacritics()`}{removes all IPA diacritics input string.}
#' }
#'
#' @name wrangle_vowels
NULL

#' @rdname wrangle_vowels
#' @export
read_vowel_defs <- function() {
    readr::read_csv(system.file("extdata/vowels.csv", package = "kdict"), col_types = "c")
}

#' @rdname wrangle_vowels
#' @param ipa_string a string or a vector of strings from which all diacritics are to be removed
#' @export
remove_diacritics <- function(ipa_string) {

    diacritics <- read_csv(system.file("extdata/diacritics.csv", package = "kdict"), col_types = "ccc")

    reduce(
        .x = diacritics$unicode_hex,
        .f = ~ str_remove_all(.x, .y),
        .init = ipa_string
    )

}

#' @rdname wrangle_vowels
#' @param transcription kdict transcription from the 'ipa' tier, e.g. \code{'aleke'}
#' @param vowels a vector of kdict vowels from the 'vowel' tier, e.g. \code{c('a', 'e', 'e')}
#'
#' @examples
#' get_cons_context('ŋɐeelə', c('ɐe', 'e', 'ə')) # returns: c("ŋ_V", "V_l", "l_#")
#'
#' @export
get_cons_context <- function(transcription, vowels) {
    ########### data validation
    if(!vowels_identical(transcription, vowels)) return(rep("Error: transcription vowels and vowels tier vowels not identical!", length(vowels)))

    ########### data derivation
    debug_df <-
        tibble(vowel = vowels) %>%                                                                    # c("a", "ei, "e")   vowels
        mutate(
            v_index = 1:n(),                                                                          # c(1, 2, 3)         index of vowels
            n_chars = nchar(vowel),                                                                   # c(1, 2, 1)         num of chars in vowels
            c_sum   = cumsum(n_chars),                                                                # c(1, 3, 4)         cumulative sum of num chars
            v_nums  = ifelse(n_chars == 1, c_sum, vec_seq(c_sum - n_chars + 1, c_sum)),               # c("1", "23", "4")  string of vowels sequence
            v_regex = paste(".?", v_nums, ".?", sep = ""),                                            # c(".?1.?", ...)    regular expression, with optional char on either side
            ctx_raw = str_extract(number_vowels(transcription), v_regex),                             # c("1l", "l23k",..) result of search regex within transcription
            ctx     = str_replace(ctx_raw, as.character(v_nums), "_") %>% str_replace_all("\\d", "V"),    # c("_l", "l_k", ..) replace string of vowels sequence with "_"
            ctx     = case_when(
                v_index == 1            & str_detect(ctx, "^_")  ~ paste0("#", ctx),                  # if word-initial vowel, then add '#' to left of context, e.g. '_l' => '#_l'
                v_index == max(v_index) & str_detect(ctx, "_$")  ~ paste0(ctx, "#"),                  # if word-final vowel, then add '#' to right of context, e.g. 'k_' => 'k_#'
                TRUE                                             ~ ctx                                # if word-medial vowel, return context as-is
            )
        )

    # for debugging, add breakpoint below to view the derivation table
    # View(debug_df)

    # return just the contexts vector, e.g. c('#_l', 'l_k', 'k_#')
    debug_df$ctx
}

## Internal helpers
## () => 'a|e|...'
#' @export
get_vowels_regex <- function() {
    read_vowel_defs() %>%
    .$vowel.label %>%
    paste0(collapse = "|")
}

## 'aleike' => '1l23k4'
number_vowels <- function(transcription) {
    n_vowels <- str_count(transcription, get_vowels_regex())

    reduce(as.character(1:n_vowels), ~ str_replace(.x, get_vowels_regex(), .y), .init = transcription)
}

# vectorised version of base R seq(), which returns a string e.g. from 1, to 3 => '123'
vec_seq <- function(from, to, by = 1) {
    map2_chr(.x = from, .y = to, ~ seq(from = .x, to = .y, by = by) %>% paste0(collapse = ""))
}

# argument 1, 'aleike' => 'aeie' === 'aeie' <= c('a', 'ei', 'e'), argument 2
vowels_identical <- function(transcription, vowels) {
    identical(
        str_extract_all(transcription, get_vowels_regex()) %>% unlist(use.names = FALSE) %>% paste0(collapse = ""),
        vowels %>% paste0(collapse = "")
    )
}
