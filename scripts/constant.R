# GENERATE SYNTHETIC DATA CONSTANTS


trisk_input_path <- fs::path("workspace", "synthetic_trisk_inputs")
trisk_output_path <- fs::path("workspace", "synthetic_crispy")

abcd_stress_test_input_path <- fs::path(trisk_input_path, "abcd_stress_test_input", ext = "csv")
prewrangled_financial_data_stress_test_path <- fs::path(trisk_input_path, "prewrangled_financial_data_stress_test", ext = "csv")


# BACKEND DATA GENERATION CONSTANTS

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

mlflow_uri <- "http://localhost:5000"
exp_name <- "fake_data_test2"

artifact_names <- c("crispy_output", "company_trajectories")


mlflow_download_dir <- fs::path("workspace", "synthetic_crispy_from_mlflow")
