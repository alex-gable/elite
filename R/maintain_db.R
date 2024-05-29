get_default_db_path <- function() {
  # TODO allow for different path using options
  get_tmp_dir <- function() {
    tmp_wd <- paste0(getwd(), "/tmp")

    if (!dir.exists(tmp_wd)) {
      dir.create(tmp_wd)
    }

    return(tmp_wd)
  }

  db_path <- get_tmp_dir()

  db_file_path <- paste0(db_path, "/elite.duckdb")

  return(db_file_path)
}

elite_db_connect <- function(ro = FALSE) {
  db_file <- get_default_db_path()

  cxn <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = ro)

  return(cxn)
}

elite_db_disconnect <- function(duckdb_connection) {
  DBI::dbDisconnect(duckdb_connection)
  duckdb::duckdb_shutdown(duckdb_connection@driver)
}

elite_db_create <- function() {
  if (file.exists(get_default_db_path())) {
    cat(paste("Database already exists at:", get_default_db_path()), sep = "\n")
    return(TRUE)
  }

  cr_con <- elite_db_connect()
  res1 <- DBI::dbGetQuery(cr_con, "select 1;")

  check_result <- res1$`1` == 1

  if (file.exists(get_default_db_path())) {
    cat(paste("Database created at", get_default_db_path()), sep = "\n")
  }

  elite_db_disconnect(cr_con)
  return(check_result)
}

create_elite_tables <- function() {
  tables <- c(
    "teams",
    "team_seasons",
    "team_player_stats",
    "teams_individual_stats",
    "player_statistics",
    "drafts",
    "contracts"
  )

  purrr::walk(tables, create_elite_table)

  tc_con <- elite_db_connect()
  tables_created <- purrr::map_lgl(tables, ~DBI::dbExistsTable(tc_con, .x))
  names(tables_created) <- tables

  elite_db_disconnect(tc_con)
  suppressMessages(gc())

  return(tables_created)

}

create_elite_table <- function(table_name) {
  ct_con <- elite_db_connect()
  table_spec <- get_table_spec(table_name)

  if (DBI::dbExistsTable(ct_con, table_name)) {
    elite_db_disconnect(ct_con)
    return(TRUE)
    # TODO: check spec
  }

  DBI::dbCreateTable(ct_con,
                     name = table_name,
                     fields = table_spec$columns)

  check_table <- DBI::dbExistsTable(ct_con, table_name)

  elite_db_disconnect(ct_con)
  return(check_table)
}

get_table_spec <- function(table) {
  # a named list of named character vectors, used by dbCreateTable to create the table
  table_columns <- list(
    "teams" = c(
      team = "TEXT",
      league = "TEXT",
      ep_team_url = "TEXT",
      ep_team_id = "INTEGER"
    ),
    "team_seasons" = c(
      ep_team_id = "INTEGER",
      season = "INTEGER",
      season_slug = "TEXT",
      ep_team_url = "TEXT"
    ),
    "team_player_stats" = c(
      name = "TEXT",
      team = "TEXT",
      league = "TEXT",
      season = "INTEGER",
      games_played = "INTEGER",
      goals = "INTEGER",
      assists = "INTEGER",
      points = "INTEGER",
      penalty_minutes = "INTEGER",
      plus_minus = "INTEGER",
      games_played_playoffs = "INTEGER",
      goals_playoffs = "INTEGER",
      assists_playoffs = "INTEGER",
      points_playoffs = "INTEGER",
      penalty_minutes_playoffs = "INTEGER",
      plus_minus_playoffs = "INTEGER",
      goals_against_average = "REAL",
      save_percentage = "REAL",
      wins = "INTEGER",
      losses = "INTEGER",
      ties = "INTEGER",
      shutouts = "INTEGER",
      time_on_ice = "INTEGER",
      saves = "INTEGER",
      goals_against_average_playoffs = "REAL",
      save_percentage_playoffs = "REAL"
    ),
    "teams_individual_stats" = c(
      ep_player_id = "INTEGER",
      name = "TEXT",
      place_of_birth = "TEXT",
      nation = "TEXT",
      position = "TEXT",
      height = "INTEGER",
      weight = "INTEGER",
      contract = "TEXT",
      cap_hit = "INTEGER",
      drated = "TEXT",
      birthday = "DATE",
      shot_handedness = "TEXT",
      nhl_rights_team = "TEXT",
      nhl_rights_status = "TEXT",
      status = "TEXT"
    ),
    player_statistics = c(
      team = "TEXT",
      league = "TEXT",
      captaincy = "TEXT",
      season = "TEXT",
      season_short = "INTEGER",
      age = "REAL",
      games_played = "INTEGER",
      goals = "INTEGER",
      assists = "INTEGER",
      points = "INTEGER",
      penalty_minutes = "INTEGER",
      plus_minus = "INTEGER",
      goals_against_average = "REAL",
      save_percentage = "REAL",
      goals_against = "INTEGER",
      saves = "INTEGER",
      shutouts = "INTEGER",
      record = "TEXT",
      time_on_ice = "INTEGER",
      games_played_playoffs = "INTEGER",
      goals_playoffs = "INTEGER",
      assists_playoffs = "INTEGER",
      points_playoffs = "INTEGER",
      penalty_minutes_playoffs = "INTEGER",
      plus_minus_playoffs = "INTEGER",
      goals_against_average_playoffs = "REAL",
      save_percentage_playoffs = "REAL",
      goals_against_playoffs = "INTEGER",
      saves_playoffs = "INTEGER",
      shutouts_playoffs = "INTEGER",
      record_playoffs = "TEXT",
      time_on_ice_playoffs = "INTEGER"
    ),
    "drafts" = c(
      draft_league = "TEXT",
      draft_year = "INTEGER",
      pick_number = "INTEGER",
      round = "INTEGER",
      draft_team = "TEXT",
      name = "TEXT",
      position = "TEXT",
      ep_player_url = "TEXT"
    ),
    "contracts" = c(
      season_slug = "TEXT",
      clauses = "TEXT",
      cap_hit = "INTEGER",
      minors_salary = "INTEGER",
      season = "INTEGER",
      ep_player_id = "INTEGER"
    )
  )

  full_table_columns <- c(
    table_columns[[table]],
    updated_at = "TIMESTAMP"
  )

  table_pks <- list(
    "teams" = "ep_team_id",
    "team_seasons" = c("ep_team_id", "season"),
    "team_player_stats" = c("ep_team_id", "ep_player_id", "season"),
    "team_individual_stats" = "ep_player_id",
    "player_statistics" = c("ep_player_id", "ep_team_id", "season"),
    "drafts" = c("draft_league", "draft_year", "round", "pick_number"),
    "contracts" = c("ep_player_id", "season_slug")
  )

  spec <- list(
    "columns" = full_table_columns,
    "pks" = table_pks[[table]]
  )

  return(spec)

}

write_elite_table <- function(data, table_name, distinct = TRUE) {
  data$updated_at <- lubridate::now(tzone = "UTC")

  table_spec <- get_table_spec(table_name) |> names()

  data <- data |>
    dplyr::select(all_of(table_spec$columns))

  if (distinct) {
    data <- data |> dplyr::distinct()
  }

  wt_con <- elite_db_connect()

  DBI::dbWriteTable(wt_con, table_name, data, append = TRUE)

  elite_db_disconnect(wt_con)
}