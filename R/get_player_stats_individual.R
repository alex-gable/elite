#' Gets bio information and career statistics for specified player
#'
#' Returns a data frame of players, their bio information (age, birth place, etc.),
#' and career statistics for user supplied player URLs and names.
#'
#' @param ... Function requires a \code{player_url} and \code{name}.
#' Additional data may be supplied. All of this information comes directly from
#' \code{get_player_stats_team()} and \code{get_teams()} or \code{get_drafts()}, if desired.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param strip_redundancy Removes variables \code{name_}, \code{player_url_}, and \code{position_},
#' as they're the same as \code{name}, \code{player_url}, and \code{position}. Defaults to \code{TRUE}.
#' @examples
#'
#' # The function works in conjunction with get_teams() and get_player_stats_team()
#' teams <- get_teams("ohl", 2018)
#' stats_team <- get_player_stats_team(teams)
#' get_player_stats_individual(stats_team)
#'
#' # The function also works in conjunction with get_drafts()
#' drafts <- get_drafts("nhl entry draft", 2018)
#' get_player_stats_individual(drafts)
#'
#' # All functions are easily pipeable too
#' get_teams(c("shl", "allsvenskan"), 2009:2011) %>%
#'   get_player_stats_team(progress = TRUE) %>%
#'   get_player_stats_individual(strip_redundancy = FALSE)
#'
#' # It's also easy to get player stats & bio information for only 1 team
#' get_teams("ncaa iii", 2018) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team() %>%
#'   get_player_stats_individual()
#'
#' # Once you have your data, use tidyr::unnest() to view players' career statistics
#' get_teams("ncaa iii", 2015) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team() %>%
#'   get_player_stats_individual() %>%
#'   tidyr::unnest(player_statistics)
#'
#' @export
#' @import dplyr
#'
get_player_stats_individual <- function(..., progress = FALSE, strip_redundancy = TRUE) {

  if (progress) {
    bar <- list(type = "iterator",
                format = "{cli::pb_spin} Getting Stats for Individual(s): {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str}", #nolint
                show_after = 0,
                clear = TRUE)
  } else {
    bar <- NULL
  }

  # get_individual_player_stats_insist <- purrr::insistently(fetch_individual_player_stats, # nolint
  #                                                          rate = purrr::rate_delay(pause = 0.1, max_times = 10))

  get_individual_player_stats <- function(ep_player_url, ep_player_id, ...) {

    tryCatch(fetch_individual_player_stats(ep_player_url, ep_player_id, ...),

      error = function(e) {
        cat("\n\nThere's an error:\n\n", sep = "")
        print(e)
        cat("\nHere's where it's from:\n\nPlayer URL:\t", ep_player_url, "\nId:\t", ep_player_id, sep = "")
        cat("\n")
        tibble::tibble()
      },

      warning = function(w) {
        cat("\n\nThere's a warning:\n\n", sep = "")
        print(w)
        cat("\nHere's where it's from:\n\nPlayer URL:\t", ep_player_url, "\nId:\t", ep_player_id, sep = "")
        cat("\n")
        tibble::tibble()
      }
    )

  }

  player_stats_individual <- purrr::pmap_dfr(..., get_individual_player_stats, .progress = bar)

  mydata <- player_stats_individual

  return(mydata)
}


#' @rdname get_player_stats_individual
#'
#' @param player_url A character vector of player URLs.
#' @param player_id A vector of player id's.
#'
#' @return A data frame of players, their bio information (age, birth place, etc.),
#'
#' @export
#'
fetch_individual_player_stats <- function(ep_player_url, ep_player_id, ...) {

  if (is.na(ep_player_url)) {

    all_data <- build_empty_player_stats_row()

  } else {

    card_page <- fetch_player_card(ep_player_url)
    vitals <- get_player_vitals(card_page)

    # TODO: use this instead for playing stats
    stats_page <- fetch_player_stats_individual(ep_player_id)
    stats_table <- get_stats_page_table(stats_page) #, vitals)

    # TODO: allow skip vitals (need positition unless using column names or have player info already?)
    if (vitals[["position"]] == "G") {

      player_statistics <- stats_table %>%
        parse_individual_goalie_stats() %>%
        tidyr::nest_legacy()

    } else {

      player_statistics <- stats_table %>%
        parse_individual_skater_stats() %>%
        tidyr::nest_legacy()

    }

    all_data <- vitals %>%
      dplyr::bind_cols(player_statistics) %>%
      dplyr::rename(player_statistics = data) %>%
      dplyr::mutate(ep_player_id = ep_player_id,
                    ep_player_url = ep_player_url)

  }

  return(all_data)

}

get_player_vitals <- function(card_page_html) {

  player_vitals_df <- card_page_html %>%
    rvest::html_elements("section.plyr_details") %>%
    rvest::html_elements("div.table-view") %>%
    rvest::html_elements("ul.list-unstyled") %>%
    # rvest::html_elements("div.col-xs") %>%
    rvest::html_elements("li") %>%
    rvest::html_text2() %>%
    stringr::str_split_fixed("\n", 2) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(janitor::make_clean_names(dplyr::slice(., 1))) %>%
    dplyr::slice(-1) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(birthday = lubridate::mdy(date_of_birth, quiet = TRUE),
                  height = stringr::str_extract(height, "[0-9']+")) %>%
    dplyr::mutate(height_split = stringr::str_split(height, "'", 2, simplify = FALSE),
                  feet_tall = dplyr::coalesce(as_numeric_quietly(height_split[[1]][1]), 0),
                  inches_tall = dplyr::coalesce(as_numeric_quietly(height_split[[1]][2]), 0)) %>%
    dplyr::select(-height_split) %>%
    dplyr::mutate(height = (feet_tall * 12) + inches_tall,
                  weight = ifelse(stringr::str_detect(weight, "[0-9]"),
                                  stringr::str_split_1(weight, "lbs"),
                                  NA_real_)) %>%
    dplyr::mutate_all(~stringr::str_trim(., side = "both")) %>%
    dplyr::mutate_all(~dplyr::na_if(., "-")) %>%
    dplyr::mutate_all(~dplyr::na_if(., "")) %>%
    dplyr::select(-any_of(c("feet_tall", "inches_tall", "age", "youth_team", "date_of_birth", "highlights", "agency")))

  if ("shoots" %in% colnames(player_vitals_df)) {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(shot_handedness = shoots) %>%
      dplyr::select(-shoots)
  } else if ("catches" %in% colnames(player_vitals_df)) {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(shot_handedness = catches) %>%
      dplyr::select(-catches)
  } else {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(shot_handedness = NA_character_)
  }

  if ("cap_hit" %in% colnames(player_vitals_df)) {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(cap_hit = strip_currency_format(stringr::str_split_i(cap_hit, "\\s", 1)))
  } else {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(cap_hit = NA_real_)
  }

  if ("nhl_rights" %in% colnames(player_vitals_df)) {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(
        nhl_rights_team = stringr::str_split_i(nhl_rights, " / ", 1),
        nhl_rights_status = stringr::str_split_i(nhl_rights, " / ", 2)
      ) %>%
      dplyr::select(-nhl_rights)
  } else {
    player_vitals_df <- player_vitals_df %>%
      dplyr::mutate(
        nhl_rights_team = NA_character_,
        nhl_rights_status = NA_character_
      )
  }

  player_name <- card_page_html %>%
    rvest::html_element("section.plyr_details") %>%
    rvest::html_element("h1.plytitle") %>%
    rvest::html_text2() %>%
    stringr::str_split_i(" a.k.a. ", 1)

  player_vitals_df <- player_vitals_df %>%
    dplyr::mutate(name = player_name)

  return(player_vitals_df)

}


get_stats_page_table <- function(stats_page) {

  base_table <- stats_page %>%
    # rvest::html_children()
    rvest::html_elements("div.table-wizard") %>%
    rvest::html_element("table")

  stats_table_colnames <- base_table %>%
    rvest::html_elements("thead") %>%
    rvest::html_elements("tr") %>%
    rvest::html_elements("th") %>%
    rvest::html_attr("class") %>%
    janitor::make_clean_names()

  stats_for_player <- base_table %>%
    html_table_with_links(na_strings = c("-", "-*", "- *", "")) %>%
    # TODO: no idea why this is necessary but it needs double unnesting for some reason
    tidyr::unnest(cols = c(x), names_repair = janitor::make_clean_names) %>%
    # TODO: codify url columns
    magrittr::set_colnames(c(stats_table_colnames, "team_url", "league_url", "postseason_url")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ep_team_id = stringr::str_extract(team_url, "(?<=/team/)\\d+")) %>%
    dplyr::select(-ends_with("_url"))

  # shared transforms
  transformed_stats_for_player <- stats_for_player %>%
    dplyr::mutate(team_split = stringr::str_split(team, "\U201C", simplify = TRUE, n = 2),
                  team = stringr::str_trim(team_split[, 1]),
                  captaincy = dplyr::na_if(stringr::str_replace(team_split[, 2], "\U201D", ""), "")) %>%
    dplyr::select(season = season_sorted, team, captaincy, league,
                  everything(),
                  -c(separator, postseason, team_split)) %>%
    dplyr::mutate_all(stringr::str_squish) %>%
    dplyr::mutate(season_short = purrr::map_int(season,
                                                ~ as_numeric_quietly(stringr::str_split_i(.x, "-", 1)) + 1)) %>%
    # dplyr::mutate(birthday = player_vitals[["birthday"]]) %>%
    # TODO: pull out draft eligible age function/calculation
    # dplyr::mutate(draft_eligibility_date = stringr::str_c(as.character(season_short), "09-15", sep = "-")) %>%
    # dplyr::mutate(age = elite::get_years_difference(birthday, draft_eligibility_date)) %>%
    dplyr::mutate(across(.cols = c(starts_with(c("regular", "postseason")), -contains("wlt")),
                         .fns = ~ as_numeric_quietly(stringr::str_replace(.x, "\\*", "")))) %>%
    dplyr::mutate(across(.cols = contains("wlt"),
                         .fns = ~ stringr::str_replace(.x, "\\s?\\*", ""))) #%>%
  # dplyr::select(-c(draft_eligibility_date, birthday))

  return(transformed_stats_for_player)

}

parse_individual_skater_stats <- function(skater_stats_table) {

  skater_stats_parsed <- skater_stats_table %>%
    dplyr::mutate(goals_against_average = NA_real_,
                  save_percentage = NA_real_,
                  goals_against = NA_real_,
                  saves = NA_real_,
                  shutouts = NA_real_,
                  record = NA_character_,
                  time_on_ice = NA_real_,
                  goals_against_average_playoffs = NA_real_,
                  save_percentage_playoffs = NA_real_,
                  goals_against_playoffs = NA_real_,
                  saves_playoffs = NA_real_,
                  shutouts_playoffs = NA_real_,
                  record_playoffs = NA_character_,
                  time_on_ice_playoffs = NA_real_) %>%
    dplyr::select(
      team, league, captaincy, season, season_short, #age,
      games_played = regular_gp,
      goals = regular_g,
      assists = regular_a,
      points = regular_tp,
      penalty_minutes = regular_pim,
      plus_minus = regular_pm,
      goals_against_average,
      save_percentage,
      goals_against,
      saves,
      shutouts,
      record,
      time_on_ice,
      games_played_playoffs = postseason_gp,
      goals_playoffs = postseason_g,
      assists_playoffs = postseason_a,
      points_playoffs = postseason_tp,
      penalty_minutes_playoffs = postseason_pim,
      plus_minus_playoffs = postseason_pm,
      goals_against_average_playoffs,
      save_percentage_playoffs,
      goals_against_playoffs,
      saves_playoffs,
      shutouts_playoffs,
      record_playoffs,
      time_on_ice_playoffs,
      ep_team_id
    )

  return(skater_stats_parsed)
}

parse_individual_goalie_stats <- function(goalie_stats_table) {
  goalie_stats_parsed <- goalie_stats_table %>%
    dplyr::mutate(goals = NA_real_,
                  assists = NA_real_,
                  points = NA_real_,
                  penalty_minutes = NA_real_,
                  plus_minus = NA_real_,
                  goals_playoffs = NA_real_,
                  assists_playoffs = NA_real_,
                  points_playoffs = NA_real_,
                  penalty_minutes_playoffs = NA_real_,
                  plus_minus_playoffs = NA_real_) %>%
    dplyr::select(
      team, league, captaincy, season, season_short, #age,
      games_played = regular_gp,
      goals,
      assists,
      points,
      penalty_minutes,
      plus_minus,
      # goal_differential_maybe = regular_gd,
      goals_against_average = regular_gaa,
      save_percentage = regular_svp,
      goals_against = regular_ga,
      saves = regular_svs,
      shutouts = regular_so,
      record = regular_wlt,
      time_on_ice = regular_toi,
      games_played_playoffs = postseason_gp,
      goals_playoffs,
      assists_playoffs,
      points_playoffs,
      penalty_minutes_playoffs,
      plus_minus_playoffs,
      goals_against_average_playoffs = postseason_gaa,
      save_percentage_playoffs = postseason_svp,
      goals_against_playoffs = postseason_ga,
      saves_playoffs = postseason_svs,
      shutouts_playoffs = postseason_so,
      record_playoffs = postseason_wlt,
      time_on_ice_playoffs = postseason_toi,
      ep_team_id
    )

  return(goalie_stats_parsed)
}


#' @title Fetch an embed-stats page for a player for its player details and statistics
#' @description Fetches a player stats (embed) page from Elite Prospects. Caches the results to disk by default
#'
#' @param ep_player_url The players's Elite Prospects Url
#'
#' @return A player embed stats page as html text
fetch_player_card <- function(ep_player_url, ...) {

  .get_embed_page <- function(ep_player_url) {
    ep_player_embed_stats_url <- paste0(ep_player_url, "/embed-stats")

    agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36" #nolint
    accept_header <- "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" #nolint
    page_gotten <- httr::GET(
      ep_player_embed_stats_url,
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
  mget_embed_page <- memoise::memoise(.get_embed_page,
                                      cache = cachem::cache_disk(max_age = 3600))

  page <- httr::content(mget_embed_page(ep_player_url),
                        as = "text", type = "text/html", encoding = "UTF-8")

  return(xml2::read_html(page))
}

# TODO: make this line up with a full row
build_empty_player_stats_row <- function() {
  row_base <- tibble::tibble(shot_handedness = NA, birth_place = NA, birth_country = NA, birthday = NA,
                             height = NA, weight = NA, age = NA, name_ = NA, position_ = NA, player_url_ = NA)

  player_statistics <- NA %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("captaincy_") %>%
    tidyr::nest_legacy()

  row_base <- row_base %>%
    dplyr::bind_cols(player_statistics) %>%
    dplyr::rename(player_statistics = data)

  return(row_base)
}


#' @title Fetch team player stats page
#' @description Fetches a team player stats page from Elite Prospects. Caches the results to disk by default
#'
#' @param ep_team_id The team's Elite Prospects ID
#' @param season_slug The season slug
#'
#' @return A team player stats page as html text
#'
fetch_player_stats_individual <- function(ep_player_id, ...) { #nolint

  .get_page <- function(ep_player_id) {
    page_url <- paste0("https://www.eliteprospects.com/ajax/player.stats.default?playerId=",
                       ep_player_id,
                       "&amp;leagueType=league&amp;included=")

    agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36" #nolint
    accept_header <- "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" #nolint
    page_gotten <- httr::GET(
      page_url,
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

  page <- httr::content(mget_page(ep_player_id),
                        as = "text", type = "text/html", encoding = "UTF-8")

  return(xml2::read_html(page))
}
