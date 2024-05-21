#' Gets teams and team URLs for specified league and season
#'
#' Returns a data frame of teams and their URLs for user supplied leagues & seasons.
#' Bear in mind that there are some cases in which teams that aren't part of the user-supplied league
#' are returned in the data frame. This is normal and is fixed later when using \code{get_player_stats_team()}.
#'
#' @param league Leagues from which the user wants to scrape data
#' @param season Seasons for which the user wants to scrape data. Must be of the form \code{2018}, \code{1965},
#' etc. for the 2017-18 and 1964-65 seasons, respectively.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param ... Allows the user to supply other information to the function.
#' If you don't know what this means, then don't worry about it.
#' @examples
#' get_teams("ohl", 2013)
#'
#' get_teams(c("SHL", "allsvenskan", "ncaa iii"), c(1994, 2017:2018), progress = FALSE)
#'
#' @export
#' @import dplyr
#'
get_teams <- function(league, season, progress = TRUE, ...) {

  if (progress) {
    bar <- list(type = "iterator",
                format = "Getting Team(s): {cli::pb_bar} {cli::pb_percent}",
                show_after = 0,
                clear = TRUE)
  } else {
    bar <- NULL
  }

  if (any(nchar(season) > 4) || any(!stringr::str_detect(season, "[0-9]{4,4}"))) {

    cat("\n")

    stop("\n\nMake sure your seasons are all 4-digit numbers
          \rlike 1994 (for 1993-94) and 2017 (for 2016-17)\n\n")

  } else if (any(as.numeric(season) > 1 + lubridate::year(Sys.time()))) {

    cat("\n")

    stop("\n\nMake sure your seasons are all actual
          \rseasons (not 2025, you silly goose)\n\n")

  }

  league <- stringr::str_replace_all(league, " ", "-")
  # season <- stringr::str_c(as.numeric(season) - 1, as.numeric(season), sep = "-")

  leagues <- league %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("league")

  seasons <- season %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("season")

  mydata <- tidyr::crossing(leagues, seasons)

  insistently_fetch_league_teams <- purrr::insistently(fetch_league_teams,
                                                       rate = purrr::rate_backoff(pause_base = 0.1, max_times = 5))

  try_fetch_teams <- function(league, season, ...) {

    # tryCatch(insistently_fetch_league_teams(league, season, ...),
    tryCatch(fetch_league_teams(league, season, ...),

      error = function(e) {
        cat("\n\nThere's an error:\n\n", sep = "")
        print(e)
        cat("\nHere's where it's from:\n\nLeague:\t", league, "\nSeason:\t", season, sep = "")
        cat("\n")
        tibble::tibble()
      },

      warning = function(w) {
        cat("\n\nThere's a warning:\n\n", sep = "")
        print(w)
        cat("\nHere's where it's from:\n\nLeague:\t", league, "\nSeason:\t", season, sep = "")
        cat("\n")
        tibble::tibble()
      }
    )

  }

  fetched_league_teams <- purrr::map2(mydata[["league"]], mydata[["season"]], try_fetch_teams, .progress = bar)

  league_team_data <- purrr::list_rbind(fetched_league_teams)

  cat("\n")

  return(league_team_data)

}

#' Fetches team URLs for a league and season
#'
#' @param league League for which the user wants to scrape data. Must be of the form \code{ohl}, \code{shl},
#' @param season Season for which the user wants to scrape data. Must be of the form \code{2018}, \code{1965},
#' @param ... Allows the user to supply other information to the function.
#'
#' @return A data frame of a teams' information for that season
#' @export
fetch_league_teams <- function(league, season, ...) {

  .get_page <- function(league, season_slug) {

    page_gotten <- httr::GET("https://www.eliteprospects.com/ajax/league.standings-and-player-stats",
                             #  path = "ajax/standings-and-player-stats",
                             query = list(league = league, season = season_slug))
    return(page_gotten)
  }

  mget_page <- memoise::memoise(.get_page, cache = cachem::cache_disk())

  season_slug <- stringr::str_c(season, season + 1, sep = "-")

  page <- mget_page(league, season_slug)

  team_nodes <- xml2::read_html(page) %>%
    rvest::html_nodes("td.team a")

  teams_df <- tibble::tibble(
    team = team_nodes %>% rvest::html_text() %>% stringr::str_trim(),
    team_url = team_nodes %>% rvest::html_attr("href")
  ) %>%
    dplyr::mutate(team_id = stringr::str_extract(team_url, "(?<=/team/)\\d+"),
                  team_url_ends_with_season = grepl(".*/[0-9]{4}-[0-9]{4}$", team_url),
                  team_url_ending_season = gsub(".*/([0-9]{4}-[0-9]{4})$", "\\1", team_url),
                  team_stats_url = stringr::str_c(ifelse(team_url_ends_with_season,
                                                         team_url,
                                                         stringr::str_c(team_url, "/", season_slug)),
                                                  "?tab=stats"),
                  league = league,
                  season = season,
                  season_slug = season_slug)

  ## temporary?
  teams_df <- teams_df %>%
    dplyr::select(team, ep_team_url = team_stats_url, league, season, ep_team_id =  team_id, season_slug)

  return(teams_df)
}
