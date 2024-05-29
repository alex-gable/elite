#' Gets draft information, players' names, and player URLs for specified draft and season
#'
#' Returns a data frame of draft information, players' names, and player URLs for user supplied drafts
#' (NHL Entry Draft, CHL Import Draft, etc.) & seasons.
#'
#' @param draft_type The type of draft for which the user wants to scrape data.
#' Draft types must be typed exactly as they are found on EliteProspects (though case doesn't matter).
#' Draft types include -- but are not limited to -- NHL Entry Draft, NHL Expansion Draft, KHL Draft,
#' NWHL Draft, CWHL Draft, CHL Import Draft, and OHL U18 Priority Selection. Others may be found at the
#' bottom of the page at \url{https://www.eliteprospects.com/draft/nhl-entry-draft}.
#' @param draft_year Seasons for which the user wants to scrape data.
#' Must be of the form \code{2018}, \code{1996}, etc -- only  a single 4-digit number.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param ... Allows the user to supply other information to the function.
#' If you don't know what this means, then don't worry about it.
#' @examples
#' get_drafts("chl import draft", 2018)
#'
#' get_drafts(c("nhl entry draft", "khl draft"), c(1994, 2017:2018))
#'
#' @export
#' @import dplyr
#'
get_drafts <- function(draft_type = "nhl entry draft",
                       draft_year = get_current_year(mode = "draft"),
                       progress = TRUE,
                       ...) {

  validate_draft_type(draft_type)

  # TODO: move out checks to their own functions
  if (any(nchar(draft_year) > 4) || any(!stringr::str_detect(draft_year, "[0-9]{4,4}"))) {

    cat("\n")

    stop("\n\nMake sure your draft years are all 4-digit numbers in like 1994 and 2017\n\n")

  } else if (any(as.numeric(draft_year) > lubridate::year(Sys.time()))) {

    cat("\n")

    stop("\n\nMake sure your draft years are all actual draft years (not 2025)\n\n")

  }

  draft_types <- draft_type %>%
    tibble::as_tibble() %>%
    purrr::set_names("draft_type") %>%
    dplyr::mutate(draft_type = stringr::str_replace_all(draft_type, " ", "-"))

  draft_years <- draft_year %>%
    tibble::as_tibble() %>%
    purrr::set_names("draft_year")

  mydata <- tidyr::crossing(draft_types, draft_years)

  # if (progress) {

  #   pb <- progress::progress_bar$new(format = "get_drafts() [:bar] :percent ETA: :eta",
  #                                    clear = FALSE, total = nrow(mydata), show_after = 0)

  #   cat("\n")

  #   pb$tick(0)

  # }

  fetch_draft_insist <- purrr::insistently(fetch_draft, rate = purrr::rate_delay(pause = 0.1, max_times = 10))

  .get_draft <- function(draft_type, draft_year, ...) {

    tryCatch(fetch_draft_insist(draft_type, draft_year, ...),

      error = function(e) {
        cat("\n\nThere's an error:\n\n", sep = "")
        print(e)
        cat("\nHere's where it's from:\n\nDraft Type:\t", draft_type, "\nDraft Year:\t", draft_year, sep = "")
        cat("\n")
        tibble::tibble()
      },

      warning = function(w) {
        cat("\n\nThere's a warning:\n\n", sep = "")
        print(w)
        cat("\nHere's where it's from:\n\nDraft Type:\t", draft_type, "\nDraft Year:\t", draft_year, sep = "")
        cat("\n")
        tibble::tibble()
      }
    )

  }

  draft_data <- purrr::map2_dfr(mydata[["draft_type"]], mydata[["draft_year"]], .get_draft)

  cat("\n")

  return(draft_data)

}

#' Fetches draft information, players' names, and player URLs for specified draft and season
#'
#' @param draft_type The type of draft for which the user wants to scrape data.
#' Draft types must be typed exactly as they are found on EliteProspects (though case doesn't matter).
#' Draft types include -- but are not limited to -- NHL Entry Draft, NHL Expansion Draft, KHL Draft,
#' NWHL Draft, CWHL Draft, CHL Import Draft, and OHL U18 Priority Selection. Others may be found at the
#' bottom of the page at \url{https://www.eliteprospects.com/draft/nhl-entry-draft}.
#' @param draft_year Seasons for which the user wants to scrape data.
#' Must be of the form \code{2018}, \code{1996}, etc -- only  a single 4-digit number.
#' @param ... Allows the user to supply other information to the function.
#'
#' @return A data frame of draft information, players' names, and player URLs for a specified draft
#'
#' @export
#' @import dplyr
#'
fetch_draft <- function(draft_type, draft_year, ...) {

  # seq(7, 11, by = 0.001) %>%
  #   sample(1) %>%
  #   Sys.sleep()

  page <- stringr::str_c("https://www.eliteprospects.com/draft/", draft_type, "/", draft_year) %>% xml2::read_html()

  draft_league <- page %>%
    rvest::html_nodes(".plytitle") %>%
    rvest::html_text() %>%
    stringr::str_replace("[0-9]{4,4}", "") %>%
    stringr::str_squish()

  draft_pick_info <- page %>%
    rvest::html_nodes("#drafted-players td:nth-child(1)") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(round = ifelse(stringr::str_detect(value, "ROUND"), value, NA)) %>%
    tidyr::fill(round) %>%
    dplyr::filter(!stringr::str_detect(value, "ROUND")) %>%
    dplyr::mutate(value = stringr::str_replace(value, "#", "")) %>%
    dplyr::mutate(round = stringr::str_replace(round, "ROUND", "")) %>%
    dplyr::mutate_all(stringr::str_squish) %>%
    dplyr::rename(round = round, pick_number = value)

  draft_team <- page %>%
    rvest::html_nodes(".team a") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    purrr::set_names("draft_team")

  player_info <- page %>%
    rvest::html_nodes("#drafted-players .txt-blue a") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(position = stringr::str_split(value, "\\(", simplify = TRUE, n = 2)[, 2]) %>%
    dplyr::mutate(position = stringr::str_split(position, "\\)", simplify = TRUE, n = 2)[, 1]) %>%
    dplyr::mutate(name = stringr::str_split(value, "\\(", simplify = TRUE, n = 2)[, 1]) %>%
    dplyr::mutate_all(stringr::str_squish)

  player_url <- page %>%
    rvest::html_nodes("#drafted-players .txt-blue a") %>%
    rvest::html_attr("href") %>%
    tibble::as_tibble() %>%
    purrr::set_names("player_url")

  player_names_with_no_selection <- page %>%
    rvest::html_nodes("#drafted-players td.player") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    purrr::set_names("value")

  no_selection_info <- player_names_with_no_selection %>%
    dplyr::bind_cols(draft_pick_info) %>%
    dplyr::bind_cols(draft_team) %>%
    dplyr::anti_join(player_info, by = c("value" = "value"))

  everything_w_no_selection_info <- draft_pick_info %>%
    dplyr::bind_cols(draft_team) %>%
    dplyr::anti_join(no_selection_info, by = c("pick_number" = "pick_number", "round" = "round")) %>%
    dplyr::bind_cols(player_info) %>%
    dplyr::bind_cols(player_url)

  all_data <- everything_w_no_selection_info %>%
    dplyr::bind_rows(no_selection_info) %>%
    dplyr::mutate(draft_league = draft_league) %>%
    dplyr::mutate(draft_year = draft_year) %>%
    dplyr::mutate_at(dplyr::vars(pick_number, round), as.numeric) %>%
    dplyr::select(-c(value)) %>%
    dplyr::select(draft_league, draft_year, pick_number, round, draft_team,
                  name, position, ep_player_url = player_url) %>%
    dplyr::arrange(pick_number)

  # if (progress) {
  #   pb$tick()
  # }

  return(all_data)
}
