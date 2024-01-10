# GENERATE SYNTHETIC DATA CONSTANTS


trisk_input_path <- fs::path("workspace", "st_inputs_sampled")
trisk_output_path <- fs::path("workspace", "st_outputs")

abcd_stress_test_input_path <- fs::path(trisk_input_path, "abcd_stress_test_input", ext = "csv")
prewrangled_financial_data_stress_test_path <- fs::path(trisk_input_path, "prewrangled_financial_data_stress_test", ext = "csv")


# BACKEND DATA GENERATION CONSTANTS

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

mlflow_uri <- "http://localhost:5000"
exp_name <- "app_crispys_test"

artifact_names <- c("crispy_output", "company_trajectories")


mlflow_download_dir <- fs::path("workspace", "app_crispys")
