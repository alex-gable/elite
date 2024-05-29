#' Get the current year
#'
#' @param mode A character string. Either "season" or "draft".
#'  If "season", the current year is the current season.
#' If "draft", the current year is the current draft year.
#'
#' @return An integer representing the current year.
#'
#' @examples
#' get_current_year("season")
#' get_current_year("draft")
#'
#' @export
get_current_year <- function(mode = "season") {

  if (mode == "season") {

    current_year <- lubridate::year(Sys.time())

  } else if (mode == "draft") {

    current_year <- lubridate::year(Sys.time()) - 1

  }

  return(current_year)
}

#' Convert a string to a slug. Adopted from the site's own function.
#'
#' @param str A character string.
#'
#' @return A character string.
#'
#' @examples
#'
#' ep_string_to_slug("Hello World")
#'
#' @export
ep_string_to_slug <- function(str) {
  str <- trimws(str) # trim
  str <- tolower(str) # convert to lowercase

  # remove accents, swap ñ for n, etc
  from <- "àáäâèéëêìíïîòóöôùúüûñç·/_,:;"
  to <- "aaaaeeeeiiiioooouuuunc------"

  for (i in seq_along(from)) {
    str <- gsub(pattern = substr(from, i, i), replacement = substr(to, i, i), x = str, fixed = TRUE)
  }

  str <- gsub("[^a-z0-9 -]", "", str) # remove invalid chars
  str <- gsub("\\s+", "-", str) # collapse whitespace and replace by -
  str <- gsub("-+", "-", str) # collapse dashes

  return(str)
}

#' Extends rvest::html_table to include links when present.
#'
#' @param html_table A rvest::html_node object containing a table, in HTML.
#'
#' @return A tibble with the same columns as the table, plus columns for each link in the table.
#' named with {column_name}_url.
#'
#' @examples
#' test_html_table <- rvest::read_html(
#'      <tr>
#'       <th>player</th>
#'       <th>playerLink</th>
#'       <th>team</th>
#'       <th>teamLink</th>
#'     </tr>
#'     <tr>
#'       <td class=\"player\">Player 1</td>
#'       <td class=\"playerLink\"><a href=\"https://example.com/player1\">Profile 1</a></td>
#'       <td class=\"team\">A</td>
#'       <td class=\"teamLink\"><a href=\"https://example.com/teamA\">Team A</a></td>
#'     </tr>
#'     <tr>
#'       <td class=\"player\">Player 2</td>
#'       <td class=\"playerLink\"><a href=\"https://example.com/player2\">Profile 2</a></td>
#'       <td class=\"team\">A</td>
#'       <td class=\"teamLink\">No Team</td>
#'     </tr>
#'     <tr>
#'       <td class=\"player\">Player 3</td>
#'       <td class=\"playerLink\">Missing Profile 3</td>
#'       <td class=\"team\">B</td>
#'       <td class=\"teamLink\"><a href=\"https://example.com/teamB\">Team B</a></td>
#'     </tr>
#'
#'   </table>") %>% rvest::html_node("table")
#'
#'
#' html_table_with_links(test_html_table)
#'
#' The result should be a data frame:
#'   player   | player_link       | team | team_link | playerLink_url              | teamLink_url
#'   Player 1 | Profile 1         | A    | Team A    | https://example.com/player1 | https://example.com/teamA
#'   Player 2 | Profile 2         | A    | No Team   | https://example.com/player2 | NA
#'   Player 3 | Missing Profile 3 | B    | Team B    | NA                          | https://example.com/teamB
#'
#' @export
html_table_with_links <- function(html_table, na_strings = "NA") {

  process_row <- function(row) {
    cells <- rvest::html_nodes(row, "td")
    link_cells <- purrr::keep(cells, ~length(rvest::html_nodes(.x, "a")) > 0)

    if (length(link_cells) == 0) {
      return(NULL)
    }

    link_cell_urls <- purrr::map(link_cells, ~rvest::html_attr(rvest::html_node(.x, "a"), "href"))

    link_cell_names <- paste0(rvest::html_attr(link_cells, "class"), "_url")

    urls <- setNames(link_cell_urls, link_cell_names)

    return(urls)
  }


  table_nodes <- rvest::html_nodes(html_table, "tr")[-1]

  html_table |>
    rvest::html_table(na.strings = na_strings) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    dplyr::mutate(parsed_row = purrr::map(table_nodes, process_row)) |>
    tidyr::unnest_wider(parsed_row)
}

#' Convert a character vector to numeric, suppressing warnings.
#'
#' @param x A character vector.
#'
#' @return A numeric vector.
#'
#' @examples
#'
#' as_numeric_quietly(c("1", NA, "", "-", "5"))
#'
#' @export
as_numeric_quietly <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Check if provided draft name is valid
#'
#' @param draft_type A character string specifying league and draft type.
#'
#' @examples validate_draft_type("nhl entry draft")
#'
#' @export
validate_draft_type <- function(draft_type) {

  valid_draft_types <- c("nhl entry draft",
                         "nhl expansion draft",
                         "nhl supplemental draft",
                         "wha amateur draft",
                         "khl draft",
                         "lnah draft",
                         "nwhl draft",
                         "cwhl draft",
                         "canada",
                         "bchl affiliate player draft",
                         "cchl bantam protected draft",
                         "cchl draft",
                         "chl import draft",
                         "mjahl draft",
                         "mjhl draft",
                         "gmhl draft",
                         "ohl priority selection",
                         "ohl u18 priority selection",
                         "qjhl draft",
                         "qjhl expansion draft",
                         "qjhl territorial draft",
                         "qmjhl american draft",
                         "qmjhl entry draft",
                         "qmjhl expansion draft",
                         "sijhl draft",
                         "sjhl bantam draft",
                         "whl bantam draft",
                         "whl us prospect draft",
                         "usa",
                         "nahl entry draft",
                         "nahl supplemental draft",
                         "na3hl draft",
                         "ncdc entry draft",
                         "ncdc futures draft",
                         "ushl entry draft",
                         "ushl futures draft")

  is_valid_draft_type <- draft_type %in% valid_draft_types

  stopifnot(is_valid_draft_type)
}


strip_currency_format <- function(currency_numeric) {
  as_numeric_quietly(stringr::str_remove_all(currency_numeric, "\\$|\\,"))
}
