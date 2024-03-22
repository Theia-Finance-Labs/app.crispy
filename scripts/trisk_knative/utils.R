download_db_tables_postgres <- function(tables, folder_path) {  
    # Example function call
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("POSTGRES_DB"),
      host = Sys.getenv("POSTGRES_HOST"),
      port = Sys.getenv("POSTGRES_PORT"),
      user = Sys.getenv("POSTGRES_USERNAME"),
      password = Sys.getenv("POSTGRES_PASSWORD"),
      sslmode="require"
      
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
