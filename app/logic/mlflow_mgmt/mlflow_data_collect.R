read_csv_from_zipped_artifacts <-
  function(tracking_uri,
           experiment_name,
           run_id,
           csv_filename) {
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    artifacts_path <-
      mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
    f_conn <-
      unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
    artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
    return(artifact)
  }
