download_db_tables_postgres <- function(tables, folder_path) {  
    # Example function call
    conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRES_DB"),
    host = Sys.getenv("POSTGRES_HOST"),
    port = Sys.getenv("POSTGRES_PORT"),
    user = Sys.getenv("ST_POSTGRES_USERNAME"),
    password = Sys.getenv("POSTGRES_PASSWORD")
    )

  lapply(tables, function(table_name) {
    query <- sprintf("SELECT * FROM %s", table_name)
    data <- DBI::dbGetQuery(conn, query)
    readr::write_csv(data, file = file.path(folder_path, paste0(table_name, ".csv")))
  })
}
