#' Gets players' stats and URLs for specified team
#'
#' Returns a data frame of players' stats and player URLs for user supplied teams & seasons
#'
#' @param ... Function requires a \code{team_url}, \code{team}, \code{league}, and \code{season}.
#' Additional data may be supplied. All of this information comes directly from \code{get_teams()}, if desired.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @examples
#'
#' # The function works in conjunction with get_teams()
#' teams <- get_teams("ohl", 2018)
#' get_player_stats_team(teams)
#'
#' # All functions are easily pipeable too
#' get_teams(c("shl", "hockeyallsvenskan"), 2021:2023) %>%
#'   get_player_stats_team(progress = TRUE)
#'
#' # It's also easy to get player stats for only 1 team
#' get_teams("ncaa iii", 2018) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team()
#'
#' @export
get_player_stats_team <- function(..., progress = FALSE) {

  if (progress) {
    pb <- progress::progress_bar$new(format = "get_player_stats_team() [:bar] :percent ETA: :eta",
                                     clear = FALSE, total = nrow(...), show_after = 0)

    cat("\n")

    pb$tick(0)
  }

  # TODO: Implement insistently
  # TODO: use insistently and purrr::possibly() to retry on failure
  # get_player_stats_team_insist <- purrr::insistently(fetch_player_stats_team,
  #                                                    rate = purrr::rate_backoff(pause_base = 0.1, max_times = 5))

  get_team_player_stats <- function(team, ep_team_url, league, season, ep_team_id, season_slug, ...) {

    tryCatch(fetch_player_stats_team(team, ep_team_url, league, season, ep_team_id, season_slug, ...),

      error = function(e) {
        cat("\n\nThere's an error:\n\n", sep = "")
        print(e)
        cat("\nHere's where it's from:\n\nTeam:\t", team,
            "\nLeague:\t", league,
            "\nSeason:\t", season,
            "\nURL:\t", ep_team_url,
            sep = "")
        cat("\n")
        tibble::tibble()
      },

      warning = function(w) {
        cat("\n\nThere's a warning:\n\n", sep = "")
        print(w)
        cat("\nHere's where it's from:\n\nTeam:\t", team,
            "\nLeague:\t", league,
            "\nSeason:\t", season,
            "\nURL:\t", ep_team_url,
            sep = "")
        cat("\n")
        tibble::tibble()
      }
    )

  }

  # TODO: readd progress bar
  fetched_team_player_stats <- purrr::pmap(..., get_team_player_stats)
  player_stats_team <- purrr::list_rbind(fetched_team_player_stats)

  cat("\n")

  return(player_stats_team)
}

#' Used to player stats for a team
#'
#' @param team_url URL for the team's roster page
#' @param team Team name
#' @param league League name
#' @param season Season
#'
#' @return A tibble of player stats
fetch_player_stats_team <- function(team, ep_team_url, league, season, ep_team_id, season_slug, ...) {

  team_player_stats_page <- fetch_team_player_stats_page(ep_team_id, season_slug)
  skater_stats <- parse_skater_stats(team_player_stats_page)
  goalie_stats <- parse_goalie_stats(team_player_stats_page)

  dataframe <- dplyr::bind_rows(skater_stats, goalie_stats) |>
    dplyr::select(!starts_with("ep_"), ep_player_id, ep_player_slug, ep_player_url) %>%
    dplyr::mutate(team = team,
                  season = season,
                  ep_team_url = ep_team_url) %>%
    dplyr::mutate(dplyr::across(games_played:save_percentage_playoffs, as_numeric_quietly)) %>%
    dplyr::select(name, team, league, season, dplyr::everything())

  if (progress) {
    pb$tick()
  }

  return(dataframe)
}


#' @title Fetch team player stats page
#' @description Fetches a team player stats page from Elite Prospects. Caches the results to disk by default
#'
#' @param ep_team_id The team's Elite Prospects ID
#' @param season_slug The season slug
#'
#' @return A team player stats page as html text
fetch_team_player_stats_page <- function(ep_team_id, season_slug, ...) {

  .get_page <- function(ep_team_id, season_slug) {

    agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36" #nolint
    accept_header <- "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" #nolint
    page_gotten <- httr::GET(
      "https://www.eliteprospects.com/ajax/team.player-stats",
      query = list(teamId = ep_team_id, season = season_slug),
      httr::user_agent(agent),
      httr::content_type("text/html"),
      httr::add_headers(
        "Accept" = accept_header,
        "Accept-Encoding" = "gzip, deflate, br, zstd",
        "Accept-Language" = "en-US,en;q=0.9",
        "Cache-Control" = "max-age=0",
        "Connection" = "keep-alive",
        "DNT" = "1",
        "Host" = "www.eliteprospects.com",
        "Sec-Fetch-Site" = "none",
        "Sec-Fetch-Dest" = "document",
        "Sec-Fetch-User" = "?1",
        "Sec-Fetch-Mode" = "navigate",
        "Upgrade-Insecure-Requests" = "1",
        "sec-ch-ua" = "\"Chromium\";v=\"123\", \"Not:A-Brand\";v=\"8\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"macOS\""
      )
    )
  }

  # TODO: memoise on load, allow forgetting or custom age
  mget_page <- memoise::memoise(.get_page,
                                cache = cachem::cache_disk(max_age = 3600))


  page <- httr::content(mget_page(ep_team_id, season_slug),
                        as = "text", type = "text/html", encoding = "UTF-8")

  return(xml2::read_html(page))
}

#' @title Parse skater stats
#' @description Parses skater stats from a team player stats page
#'
#' @param team_player_stats_page A team player stats page as html text
#'
#' @return A tibble of skater stats for a given team
parse_skater_stats <- function(team_player_stats_page) {

  team_player_stats_page |>
    rvest::html_node('[class="table table-striped table-sortable skater-stats highlight-stats"]') |>
    html_table_with_links() |>
    dplyr::rename(
      idx = number,
      league = n,
      name = player,
      games_played = gp,
      goals = g,
      assists = a,
      points = tp,
      penalty_minutes = pim,
      plus_minus = x,
      blank = x_2,
      games_played_playoffs = gp_2,
      goals_playoffs = g_2,
      assists_playoffs = a_2,
      points_playoffs = tp_2,
      penalty_minutes_playoffs = pim_2,
      plus_minus_playoffs = x_3,
      ep_player_url = player_url
    ) |>
    dplyr::mutate(league = dplyr::na_if(league, "")) |>
    tidyr::fill(league, .direction = "down") |>
    dplyr::filter(!is.na(idx)) |>
    dplyr::select(-idx, -blank) |>
    dplyr::mutate(position = stringr::str_extract(name, "\\((.*)\\)$", 1),
                  name = stringr::str_remove(name, "\\s\\(.*\\)$")) %>%
    dplyr::mutate(ep_player_id = stringr::str_extract(ep_player_url, "(?<=player/)[0-9]+"),
                  ep_player_slug = stringr::str_extract(ep_player_url, "(?<=player/)([0-9]+/.+)", 1)) %>%
    dplyr::select(name, position, league, dplyr::everything()) %>%
    dplyr::mutate_all(stringr::str_squish)

}

#' @title Parse goalie stats
#' @description Parses goalie stats from a team player stats page
#'
#' @param team_player_stats_page A team player stats page as html text
#'
#' @return A tibble of goalie stats for a given team
parse_goalie_stats <- function(team_player_stats_page) {
  team_player_stats_page |>
    rvest::html_node('[class="table table-striped table-sortable goalie-stats highlight-stats"]') |>
    html_table_with_links() |>
    dplyr::rename(
      idx = number,
      league = n,
      name = player,
      games_played = gp,
      goals_against_average = gaa,
      save_percentage = sv_percent,
      wins = w,
      losses = l,
      ties = t,
      shutouts = so,
      time_on_ice = toi,
      saves = svs,
      blank = x,
      games_played_playoffs = gp_2,
      goals_against_average_playoffs = gaa_2,
      save_percentage_playoffs = sv_percent_2,
      ep_player_url = player_url

    ) |>
    dplyr::mutate(league = dplyr::na_if(league, "")) |>
    tidyr::fill(league, .direction = "down") |>
    dplyr::filter(!is.na(idx)) |>
    dplyr::select(-idx, -blank) |>
    dplyr::mutate(position = "G") |>
    dplyr::mutate(ep_player_id = stringr::str_extract(ep_player_url, "(?<=player/)[0-9]+"),
                  ep_player_slug = stringr::str_extract(ep_player_url, "(?<=player/)([0-9]+/.+)", 1)) |>
    dplyr::select(name, position, league, dplyr::everything()) |>
    dplyr::mutate_all(stringr::str_squish)
}
