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
  print(mydata)

  ## TODO: fix progress bar
  # if (progress) {

  #   pb <- progress::progress_bar$new(format = "get_teams() [:bar] :percent ETA: :eta",
  #                                    clear = FALSE, total = nrow(mydata), show_after = 0)
  #   cat("\n")

  #   pb$tick(0)
  # }

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

  # league_team_data <- purrr::map2_dfr(mydata[["league"]], mydata[["season"]], try_fetch_teams)
  fetched_league_teams <- purrr::map2(mydata[["league"]], mydata[["season"]], try_fetch_teams)
  # print(result_list)

  league_team_data <- purrr::list_rbind(fetched_league_teams)

  cat("\n")

  # print(league_team_data)

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
    dplyr::select(team, team_url = team_stats_url, league, season, team_id, season_slug)

  # if (progress) {
  #   pb$tick()
  # }

  return(teams_df)
}


z_fetch_league_teams <- function(league, season = get_current_year(), ...) {

  pause <- function(league, season, ...) {
    seq(3, 7, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
  }

  mpause <- memoise::memoise(pause)

  mpause()

  memoised_read_html <- memoise::memoise(xml2::read_html)

  page_url <- stringr::str_c("https://www.eliteprospects.com/league/", league, "/", season)
  print(paste0("\n",page_url))

  page <- memoised_read_html(page_url)

  league_name <- page %>%
    rvest::html_nodes("small") %>%
    rvest::html_text() %>%
    stringr::str_squish()

  team_urls_rosters <- page %>%
    rvest::html_nodes(".column-4 i+ a") %>%
    rvest::html_attr("href") %>%
    ifelse(stringr::str_detect(., "[0-9]{4,4}-[0-9]{4,4}"), ., stringr::str_c(., season, sep = "/")) %>%
    ifelse(stringr::str_detect(., "https"), stringr::str_c(., "?tab=stats"), .) %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("team_url") %>%
    dplyr::mutate(team_url = stringr::str_replace_all(team_url, "\\-\\-", ""))

  teams_rosters <- page %>%
    rvest::html_nodes(".column-4 i+ a") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("team")

  team_urls_standings <- page %>%
    rvest::html_nodes("#standings .team a") %>%
    rvest::html_attr("href") %>%
    ifelse(stringr::str_detect(., "[0-9]{4,4}-[0-9]{4,4}"), ., stringr::str_c(., season, sep = "/")) %>%
    ifelse(stringr::str_detect(., "https"), stringr::str_c(., "?tab=stats"), .) %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("team_url") %>%
    dplyr::mutate(team_url = stringr::str_replace_all(team_url, "\\-\\-", ""))

  teams_standings <- page %>%
    rvest::html_nodes("#standings .team a") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("team")

  teams <- teams_standings %>%
    dplyr::bind_rows(teams_rosters) %>%
    dplyr::mutate(team_lower = tolower(team)) %>%
    dplyr::distinct(team_lower, .keep_all = TRUE) %>%
    dplyr::select(team)

  team_urls <- team_urls_standings %>%
    dplyr::bind_rows(team_urls_rosters) %>%
    dplyr::mutate(team_url = stringr::str_replace_all(team_url, c("\\-\\/" = "/"))) %>%
    dplyr::distinct()

  season <- stringr::str_split(season, "-", simplify = TRUE, n = 2)[, 2] %>%
    stringr::str_sub(3, 4) %>%
    stringr::str_c(stringr::str_split(season, "-", simplify = TRUE, n = 2)[, 1], ., sep = "-")

  all_data <- teams %>%
    dplyr::bind_cols(team_urls) %>%
    dplyr::mutate(league = league_name) %>%
    dplyr::mutate(season = season)

  # if (exists("progress") && progress && exists("pb")) {
  #   pb$tick()
  # }

  return(all_data)

}
