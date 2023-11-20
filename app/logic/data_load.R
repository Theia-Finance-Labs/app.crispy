box::use(
    app/logic/constant[backend_crispy_data_path, backend_crispy_test_data_path],
    arrow[read_parquet],
    dplyr[filter]
)

backend_data_load <- function(){
  if (Sys.getenv("SHINY_APP_MODE") == "testing") {
    # Load a test dataset
    backend_crispy_data <- arrow::read_parquet(backend_crispy_test_data_path) 
  } else {
    # Load the main dataset for normal runs
    backend_crispy_data <- arrow::read_parquet(backend_crispy_data_path) 
  }

  backend_data_load <- backend_crispy_data |>
      filter(.data$term == 5) |>
      filter(.data$ald_sector %in% use_ald_sector)

  return(backend_crispy_data)

}