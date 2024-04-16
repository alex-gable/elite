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
  to   <- "aaaaeeeeiiiioooouuuunc------"
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
#' # test_html_table <- read_html(
#'  "<table>
#'     <tr>
#'       <th>player</th>
#'       <th>playerLink</th>
#'     </tr>
#'     <tr>
#'       <td class="player">Player 1</td>
#'       <td class="playerLink"><a href="https://example.com/player1">Profile 1</a></td>
#'     </tr>
#'     <tr>
#'       <td class="player">Player 2</td>
#'       <td class="playerLink"><a href="https://example.com/player2">Profile 2</a></td>
#'     </tr>
#'     <tr>
#'       <td class="player">Player 3</td>
#'       <td class="playerLink">Missing Profile 3</td>
#'     </tr>
#'   </table>") %>% rvest::html_node("table")
#'
#'
#' html_table_with_links(test_html_table)
#'
#' The result should be a data frame:
#'   player  | playerLink_text      | playerLink_url
#'   Player 1| Profile 1 | https://example.com/player1
#'   Player 2| Profile 2 | https://example.com/player2
#'   Player 3| Missing Profile 3 | NA
#'
#' @export
html_table_with_links <- function(html_table) {
  extract_link_data <- function(cell) {
    link <- rvest::html_node(cell, "a")
    if (!is.null(link)) {
      list(text = rvest::html_text(link), url = rvest::html_attr(link, "href"))
    } else {
      list(text = rvest::html_text(cell), url = NA_character_)
    }
  }

  process_row_new <- function(row) {
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
    rvest::html_table() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    dplyr::mutate(parsed_row = purrr::map(table_nodes, process_row_new)) |>
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
