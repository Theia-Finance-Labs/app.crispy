

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
