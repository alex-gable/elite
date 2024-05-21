get_player_contracts <- function(ep_player_id, ...) { #nolint

  player_contract_page <- fetch_player_contract(ep_player_id, ...)

  player_contract_table <- get_player_contract_table(player_contract_page)

  player_contracts <- player_contract_table %>%
    dplyr::mutate(ep_player_id = ep_player_id)

  return(player_contracts)
}

fetch_player_contract <- function(ep_player_id, ...) { #nolint

  .get_page <- function(ep_player_id) {
    page_url <- paste0("https://www.eliteprospects.com/ajax/modal.player-contract?playerId=",
                       ep_player_id)

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

  # page <- httr::content(.get_page(ep_player_id),
  page <- httr::content(mget_page(ep_player_id),
                        as = "text", type = "text/html", encoding = "UTF-8")

  return(xml2::read_html(page))
}

get_player_contract_table <- function(player_contract_page) {

  player_contract_table <- player_contract_page %>%
    rvest::html_elements("div.table-wizard") %>%
    rvest::html_element("table") %>%
    rvest::html_table(na.strings = "") %>%
    # todo: still unsure why multiple table is interpreted and list is returned
    purrr::pluck(1) %>%
    magrittr::set_colnames(janitor::make_clean_names(colnames(.))) %>%
    dplyr::mutate(across(.cols = c(cap_hit, minors_salary),
                         .fns = ~ strip_currency_format(.x))) %>%
    dplyr::rename(clauses = clause, season_slug = season) %>%
    dplyr::mutate(season = as_numeric_quietly(stringr::str_split_i(season_slug, "-", 1))) %>%
    tibble::as_tibble()

  return(player_contract_table)
}
