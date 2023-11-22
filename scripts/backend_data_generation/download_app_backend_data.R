box::use(
  scripts / constant[
    mlflow_python_bin, mlflow_bin, mlflow_uri, exp_name, artifact_names, mlflow_download_dir
  ],
  app / logic / constant[backend_crispy_test_data_path, max_crispy_granularity],
  app / logic / mlflow_mgmt / mlflow_data_collect[download_mlflow_search_result]
)

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(
    filter = "tags.LOG_STATUS = 'SUCCESS'",
    experiment_ids = as.character(experiment_id)
  )


### CRISPY COLLECT

download_mlflow_search_result(mlflow_uri, exp_name, all_runs, mlflow_download_dir)

multi_crispy_data <- stress.test.plot.report::main_load_multi_crispy_data(
  crispy_outputs_dir = mlflow_download_dir,
  max_crispy_granularity = max_crispy_granularity
)
multi_crispy_data |> arrow::write_parquet(backend_crispy_test_data_path)
