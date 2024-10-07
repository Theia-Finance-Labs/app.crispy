box::use(
    app/logic/load_multi_crispy[main_load_multi_crispy_data]
)
#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param granularity granularity
#' @param trisk_start_year (default) sets to the earliest year of multi_cripy_data
#' @param multi_crispy_data multi_crispy_data
#' @param portfolio_data portfolio_data
#' @param filter_outliers filter_outliers
#'
#' @export
#'
load_input_plots_data_from_tibble <-
  function(multi_crispy_data,
           portfolio_data = NULL,
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           filter_outliers = FALSE) {

    raw_crispy_columns <- colnames(multi_crispy_data)

    multi_crispy_data <- multi_crispy_data |>
      main_load_multi_crispy_data(
        granularity = granularity,
        filter_outliers = filter_outliers
              )

    # start year must be unique
    
    if("start_year" %in% names(multi_crispy_data)){
trisk_start_year <- unique(multi_crispy_data$start_year)[1]
    }
    else{
      trisk_start_year <- 2022
    }
    
    if (!is.null(portfolio_data)){
      portfolio_data <- portfolio_data |>
        main_load_portfolio_data(
          granularity = granularity,
          trisk_start_year = trisk_start_year)
    } else{
      portfolio_data <- load_portfolio_data(portfolio_data_path=NULL)

      # drop_cols contains column that could be used as granularity but are dropped
      # TODO TEST columns that could be used as granularity are columns who are shared between dataframse
      shared_index  <-  dplyr::intersect(raw_crispy_columns, colnames(portfolio_data))
      drop_cols <- dplyr::setdiff(shared_index, granularity)
      portfolio_data <- portfolio_data |>
        dplyr::select_at(colnames(portfolio_data)[!colnames(portfolio_data) %in% drop_cols])
    }

    portfolio_crispy_merge_cols <- dplyr::intersect(
      c(granularity, "term"),
      dplyr::intersect(
        colnames(multi_crispy_data), colnames(portfolio_data)))
    stopifnot(length(portfolio_crispy_merge_cols) > 0)

    analysis_data <-
      main_load_analysis_data(
        portfolio_data = portfolio_data,
        multi_crispy_data = multi_crispy_data,
        portfolio_crispy_merge_cols = portfolio_crispy_merge_cols
      )

    return(analysis_data)
  }

#' main load analysis data
#'
#'
#'
#' @param portfolio_data portfolio_data
#' @param portfolio_crispy_merge_cols portfolio_crispy_merge_cols
#' @param multi_crispy_data multi_crispy_data
#'
#'
#' @export
main_load_analysis_data <-
  function(multi_crispy_data,
           portfolio_data,
           portfolio_crispy_merge_cols) {

    stopifnot(unique(portfolio_data$asset_type) %in% c("equity", "fixed_income"))

    analysis_data <-
      create_analysis_data(portfolio_data, multi_crispy_data, portfolio_crispy_merge_cols) |>
        aggregate_equities() |>
        compute_analysis_metrics() 

    return(analysis_data)
  }


#' Title
#'
#' @param portfolio_data portfolio_data
#' @param multi_crispy_data multi_crispy_data
#' @param portfolio_crispy_merge_cols portfolio_crispy_merge_cols
#'
create_analysis_data <-
  function(portfolio_data,
           multi_crispy_data,
           portfolio_crispy_merge_cols) {
    # If no portfolio is provided, fill the merging columns
    # with the ones available in crispy, in order to get a full crispy output
    if (nrow(portfolio_data) == 0) {
      merge_cols_values <- multi_crispy_data |>
        dplyr::distinct_at(portfolio_crispy_merge_cols)
      portfolio_data <- dplyr::full_join(portfolio_data, merge_cols_values)
    }

    analysis_data <-
      dplyr::inner_join(
        portfolio_data,
        multi_crispy_data,
        by = portfolio_crispy_merge_cols,
        relationship = "many-to-many"
      )

    facts <- c(
      "net_present_value_baseline",  "net_present_value_shock", "pd_baseline", "pd_shock",
      "exposure_value_usd", "loss_given_default", "pd_portfolio")


    stopifnot(all(facts %in% names(analysis_data))) 

    return(analysis_data)
  }

#' Compute Analysis Metrics
#'
#' @description Function computing financial metrics to use for analysis
#' @param analysis_data analysis_data
#'
compute_analysis_metrics <- function(analysis_data) {
  analysis_data <- analysis_data |>
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      crispy_perc_value_change = .data$net_present_value_difference / .data$net_present_value_baseline,
      crispy_value_loss = .data$crispy_perc_value_change * .data$exposure_value_usd,
      exposure_at_default = .data$exposure_value_usd * .data$loss_given_default,
      # exposure_at_default_baseline = .data$net_present_value_baseline * .data$loss_given_default,
      # exposure_at_default_shock = .data$net_present_value_shock * .data$loss_given_default,

      # pd_difference_portfolio = .data$pd_portfolio - .data$pd_shock,
      pd_difference = .data$pd_shock - .data$pd_baseline,
      # crispy_perc_pd_change = .data$pd_difference / .data$pd_baseline,

      expected_loss_portfolio = - .data$exposure_at_default * .data$pd_portfolio,
      expected_loss_baseline = - .data$exposure_at_default * .data$pd_baseline,
      expected_loss_shock = - .data$exposure_at_default * .data$pd_shock,
      expected_loss_difference = - .data$exposure_at_default * .data$pd_difference
    )



  return(analysis_data)
}


#' 
#'
#' @description This will do the average of all values of equities over the terms (ie the term disappears and value was just dupicated)
#' @param analysis_data analysis_data
#'
aggregate_equities <- function(analysis_data) {
    facts <- c(
      "net_present_value_baseline",  "net_present_value_shock", "pd_baseline", "pd_shock",
      "exposure_value_usd", "loss_given_default", "pd_portfolio")

    equities_subgroup_analysis <- analysis_data |> 
            dplyr::filter(asset_type=="equity") |>
            dplyr::group_by_at(colnames(analysis_data)[!colnames(analysis_data) %in% c(facts, "term")]) |>
            dplyr::summarise(
              exposure_value_usd=stats::median(.data$exposure_value_usd, na.rm=T),
              net_present_value_baseline=stats::median(.data$net_present_value_baseline, na.rm=T),
              net_present_value_shock=stats::median(.data$net_present_value_shock, na.rm=T),
              pd_baseline=stats::median(.data$pd_baseline, na.rm=T),
              pd_shock=stats::median(.data$pd_shock, na.rm=T),
              loss_given_default=stats::median(.data$loss_given_default, na.rm=T),
              pd_portfolio=stats::median(.data$pd_portfolio, na.rm=T)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(
              term=NA,
              loss_given_default=NA
            )
    non_equities_subgroup_analysis  <- analysis_data |>
    dplyr::filter((asset_type != "equity") | is.na(asset_type))

    analysis_data <- dplyr::bind_rows(equities_subgroup_analysis,non_equities_subgroup_analysis)
    return(analysis_data)
}



#' Title
#'
#' @param portfolio_data portfolio_data
#' @param granularity granularity
#' @param param_cols param_cols
#' @param trisk_start_year trisk_start_year
#'
#'
#' @export
main_load_portfolio_data <-
  function(portfolio_data,
           granularity,
           trisk_start_year,
           param_cols = c("portfolio_id", "term", "asset_type")
           ) {
    group_cols <- unique(c(granularity, param_cols))

    portfolio_data <- portfolio_data |>
      map_portfolio_maturity_to_term(
        trisk_start_year = trisk_start_year
      ) |>
      aggregate_portfolio_facts(group_cols = group_cols)

    return(portfolio_data)
  }



#' Title
#'
#' @param portfolio_data_path
#'
load_portfolio_data <- function(portfolio_data_path=NULL) {
  if (!is.null(portfolio_data_path)) {
    portfolio_data <- readr::read_csv(
      portfolio_data_path,
      col_types = readr::cols_only(
        portfolio_id = "c",
        company_id = "c",
        company_name = "c",
        asset_type = "c",
        ald_sector = "c",
        ald_business_unit = "c",
        ald_location = "c",
        exposure_value_usd = "d",
        expiration_date = "c",
        loss_given_default = "d",
        pd_portfolio = "d"
      )
    ) %>%
      convert_date_column(colname="expiration_date")

  } else {
    portfolio_data <- tibble::tibble(
      portfolio_id = character(),
      asset_type = character(),
      company_id = character(),
      company_name = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      exposure_value_usd = double(),
      expiration_date = date(),
      loss_given_default = double(),
      pd_portfolio = double()
    )
  }

  return(portfolio_data)
}


#' Convert column year to term. 1 term is equal to 2.5 years
#'
#' @param trisk_start_year trisk_start_year
#' @param portfolio_data portfolio_data
#'
map_portfolio_maturity_to_term <-
  function(portfolio_data,
           trisk_start_year) {
    start_year_exists <- !(is.na(trisk_start_year) | is.null(trisk_start_year))

    if (start_year_exists) {
      start_date <- as.Date(paste0(trisk_start_year, "-01-01"))

      #.TODO ADD A CHECK VALIDATING THE CONVERSION TO DATE WITH NO NAs
      portfolio_data <- portfolio_data |>
        dplyr::mutate(
          expiration_date = as.Date(.data$expiration_date),
          portfolio_maturity_month = lubridate::interval(.env$start_date, .data$expiration_date) %/% months(1),
          term = ceiling((.data$portfolio_maturity_month / 12) * 0.4) # 1 term is equal to 2.5 years
        ) |>
        dplyr::select(-c(portfolio_maturity_month))
    } else {
      # replace term by 1 if there's no start year,
      # which is a consequence of the crispy data being empty
      portfolio_data <- portfolio_data |>
        dplyr::mutate(term = 1)
    }

    # replace term by 1 if asset type is equity
    portfolio_data <- portfolio_data |>
      dplyr::mutate(
        term = dplyr::if_else(is.na(.data$term), 1, .data$term),
        portfolio_id=dplyr::if_else(is.na(.data$portfolio_id), "1", .data$portfolio_id))

    return(portfolio_data)
  }



#' Title
#'
#' @param portfolio_data portfolio_data
#' @param group_cols group_cols
#'
#'
aggregate_portfolio_facts <- function(portfolio_data, group_cols) {
  portfolio_data <- portfolio_data |>
    dplyr::group_by_at(group_cols) |>
    dplyr::summarize(
      exposure_value_usd = sum(.data$exposure_value_usd),
      loss_given_default = stats::median(loss_given_default, na.rm = T),
      pd_portfolio = stats::median(.data$pd_portfolio, na.rm = T),
      .groups = "drop"
    )

  return(portfolio_data)
}


#' Validate and conversion to date
#'
#' Define a function to check date format and convert
#'
#' @param data data
#' @param date_col_name date_col_name
#'
convert_date_column <- function(data_frame, colname) {
  # Check if the column exists in the dataframe
  if (!colname %in% names(data_frame)) {
    stop("Column not found in the dataframe")
  }

  # Use lubridate::ymd() to parse dates; invalid dates become NA
  data_frame[[colname]] <- lubridate::ymd(data_frame[[colname]], quiet = TRUE)

  # Check if there were any parsing failures (i.e., NAs introduced)
  if (any(is.na(data_frame[[colname]]))) {
    warning("Some dates were not in the correct 'YYYY-mm-dd' format and have been converted to NA")
  }

  return(data_frame)
}

