trigger_trisk_api_computation <- function(trisk_run_params, trisk_api_service) {
  # Define the URL
  # by defaylt trisk_api_service should be equalt to "trisk-api-service"
  url <- paste0("http://", trisk_api_service, ":80/compute_trisk")

  # Define the body of the request
  body <- list(
    trisk_run_params = trisk_run_params
  )
  # Convert the body to JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Define the headers, including Host
  headers <- c(
    `Host` = paste0("trisk-api.default.", trisk_api_service),
    `Content-Type` = "application/json"
  )

  # Make the POST request with a 6-minute timeout
  response <- httr::POST(url, body = body_json, httr::add_headers(.headers = headers), httr::timeout(360))

  # Check the response
  status_code <- httr::status_code(response)

  if (status_code != 200) {
    stop("The request failed with status code ", status_code)
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  # jsonlite::fromJSON has to be doubled bc of this content structure
  run_id <- jsonlite::fromJSON(jsonlite::fromJSON(content))$trisk_run_id

  return(run_id)
}


get_data_from_postgres <- function(
    table_name,
    dbname,
    host_db,
    db_port,
    db_user,
    db_password,
    query_filter = NULL,
    default_tibble = tibble::tibble()) {
  # Create a connection string
  conn <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = dbname,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password
  )

  if (DBI::dbExistsTable(conn, table_name)) {
    # Construct the SQL query
    if (is.null(query_filter)) {
      query <- paste("SELECT * FROM", table_name)
    } else {
      query <- paste("SELECT * FROM", table_name, "WHERE", query_filter)
    }


    # Execute the query and fetch results
    table_data <- DBI::dbGetQuery(conn, query)
    table_data <- tibble::as_tibble(table_data)
  } else {
    table_data <- default_tibble
  }
  # Close the connection
  DBI::dbDisconnect(conn)

  return(table_data)
}


get_possible_trisk_combinations_from_api <- function(trisk_api_service){
  # Define the URL
  # by defaylt trisk_api_service should be equalt to "trisk-api-service"
  url <- paste0("http://", trisk_api_service, ":80/get_possible_trisk_combinations")
  # Make the POST request with a 6-minute timeout
  response <- httr::GET(url, httr::timeout(360))
  content <- httr::content(response, "text", encoding = "UTF-8")
  possible_trisk_combinations <- jsonlite::fromJSON(jsonlite::fromJSON(content))
}


download_files_from_s3 <- function(
    local_folder_path,
    s3_url,
    s3_bucket,
    s3_folder_path,
    s3_access_key,
    s3_secret_key,
    s3_region) {
  # Configure the S3 client to use DigitalOcean Spaces
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = s3_access_key,
    "AWS_SECRET_ACCESS_KEY" = s3_secret_key,
    "AWS_S3_ENDPOINT" = s3_url,
    "AWS_DEFAULT_REGION" = s3_region
  )

  # Check and create the local directory if it doesn't exist
  if (!dir.exists(local_folder_path)) {
    dir.create(local_folder_path, recursive = TRUE)
  }

  # List all files in the folder
  response <- aws.s3::get_bucket(
    bucket = s3_bucket,
    prefix = s3_folder_path,
    delimiter = "/",
    parse_response = TRUE
  )


  for (i in 1:length(response)) {
    file_key <- response[i]$Contents$Key
    if (grepl("\\.csv$", file_key)) {
      # Download file
      aws.s3::save_object(
        file = paste0(local_folder_path, "/", basename(file_key)),
        object = file_key,
        bucket = s3_bucket
      )
    }
  }
}
