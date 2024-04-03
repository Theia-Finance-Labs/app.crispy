download_db_tables_postgres <- function(tables, folder_path, dbname, host, port, user, password) {
  # Example function call
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password,
    sslmode = "require"
  )

  # Ensure the directory exists
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }

  for (table_name in tables) {
    query <- sprintf("SELECT * FROM public.\"%s\"", table_name)
    data <- DBI::dbGetQuery(conn, query)
    file_path <- file.path(folder_path, paste0(table_name, ".csv"))
    readr::write_csv(data, file = file_path)
  }
}
