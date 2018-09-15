#' get_grad_data
#'
#' get_grad_data returns a data frame of cohort graduation data from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids and
#'    "Class of" school years.
#'
#'    All the cohort data, through class of 2016, that can be pulled by this function is available
#'    in the data frame `nrc_cohort_data` that comes with the nrc package; View(nrc_cohort_data).
#'    This function was used to create that data frame.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param class_of_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the class of 2015-2016 school year.
#'
#' @return returns a data frame of cohort graduation data.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Get cohort grad data for CCSD schools for 2015-2016
#' ccsd_ids <- nrc_orgs$id[nrc_orgs$parent_id == 64827 | nrc_orgs$id == 64827]
#' ccsd_grad_2016 <- get_grad_data(ccsd_ids, 2016)
#'
#' # Get grad data for the state for all years.
#' state_grad <- get_grad_data(64825, 2011:2016)

get_grad_data <- function(org_ids, class_of_years = 2017) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2011:2017

  if (is.null(class_of_years)) return(stop("Invalid class_of_years provided. Must be 2011 through 2017."))

  for (year in class_of_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid class_of_years provided. Must be 2011 through 2017."))
    }
  }

  # Add the nrc year codes to the scope (e20 is the code for the cohort report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% (class_of_years + 1)]
  scope <- paste(c('e20', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)

  # Scores parameter (Will include all the cohort grad data reporting fields)
  scores <- 'scores=880,890,881,891,882,892,883,893,885,894,886,895,887,896,888,897,889,898,899,908,917,926,935,944,953,900,909,918,927,936,945,954,901,910,919,928,937,946,955,902,911,920,929,938,947,956,903,912,921,930,939,948,957,904,913,922,931,940,949,958,905,914,923,932,941,950,959,906,915,924,933,942,951,960,907,916,925,934,943,952,961,962,971,980,989,998,963,972,981,990,999,964,973,982,991,1000,965,974,983,992,1001,966,975,984,993,1002,967,976,985,994,1003,968,977,986,995,1004,969,978,987,996,1005,970,979,988,997,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,1020,1036,1021,1022,1023,1024,1025,1037'

  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&fields=309,310,311,313,318,320')

  # Download the csv data from the target url.
  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::select(name = Name,
           accountability_year = `Accountability Year`,
           class_of = `Graduating Class of`,
           school_levels = `School Levels`,
           organization_id = `Organization ID`,
           organization_level = `Organization Level`,
           state_id = identifier,

           grad_count_female = `Graduates Female`,
           completer_count_female = `Completers Female`,
           transfer_out_count_female = `Transfer Outs Female`,
           dropout_count_female = `Dropouts Female`,
           nongrad_count_female = `Non-Graduates Female`,
           other_transfer_count_female = `Other Transfers Female`,
           missing_end_status_count_female = `Missing End Status Female`,
           grad_rate_female = `Graduation Rate Female`,

           total_count_male = `Total Male`,
           grad_count_male = `Graduates Male`,
           completer_count_male = `Completers Male`,
           transfer_out_count_male = `Transfer Outs Male`,
           dropout_count_male = `Dropouts Male`,
           nongrad_count_male = `Non-Graduates Male`,
           other_transfer_count_male = `Other Transfers Male`,
           missing_end_status_count_male = `Missing End Status Male`,
           grad_rate_male = `Graduation Rate Male`,

           total_count_american_indian = `Total Am Indian/AK Nat`,
           grad_count_american_indian = `Graduates Am Indian/AK Nat`,
           completer_count_american_indian = `Completers Am Indian/AK Nat`,
           transfer_out_count_american_indian = `Transfer Outs Am Indian/AK Nat`,
           dropout_count_american_indian = `Dropouts Am Indian/AK Nat`,
           nongrad_count_american_indian = `Non-Graduates Am Indian/AK Nat`,
           other_transfer_count_american_indian = `Other Transfers Am Indian/AK Nat`,
           missing_end_status_count_american_indian = `Missing End Status Am Indian/AK Nat`,
           grad_rate_american_indian = `Graduation Rate Am Indian/AK Nat`,

           total_count_asian = `Total Asian`,
           grad_count_asian = `Graduates Asian`,
           completer_count_asian = `Completers Asian`,
           transfer_out_count_asian = `Transfer Outs Asian`,
           dropout_count_asian = `Dropouts Asian`,
           nongrad_count_asian = `Non-Graduates Asian`,
           other_transfer_count_asian = `Other Transfers Asian`,
           missing_end_status_count_asian = `Missing End Status Asian`,
           grad_rate_asian = `Graduation Rate Asian`,

           total_count_black = `Total Black`,
           grad_count_black = `Graduates Black`,
           completer_count_black = `Completers Black`,
           transfer_out_count_black = `Transfer Outs Black`,
           dropout_count_black = `Dropouts Black`,
           nongrad_count_black = `Non-Graduates Black`,
           other_transfer_count_black = `Other Transfers Black`,
           missing_end_status_count_black = `Missing End Status Black`,
           grad_rate_black = `Graduation Rate Black`,

           total_count_hispanic = `Total Hispanic`,
           grad_count_hispanic = `Graduates Hispanic`,
           completer_count_hispanic = `Completers Hispanic`,
           transfer_out_count_hispanic = `Transfer Outs Hispanic`,
           dropout_count_hispanic = `Dropouts Hispanic`,
           nongrad_count_hispanic = `Non-Graduates Hispanic`,
           other_transfer_count_hispanic = `Other Transfers Hispanic`,
           missing_end_status_count_hispanic = `Missing End Status Hispanic`,
           grad_rate_hispanic = `Graduation Rate Hispanic`,

           total_count_multiracial = `Total Two or More Races`,
           grad_count_multiracial = `Graduates Two or More Races`,
           completer_count_multiracial = `Completers Two or More Races`,
           transfer_out_count_multiracial = `Transfer Outs Two or More Races`,
           dropout_count_multiracial = `Dropouts Two or More Races`,
           nongrad_count_multiracial = `Non-Graduates Two or More Races`,
           other_transfer_count_multiracial = `Other Transfers Two or More Races`,
           missing_end_status_count_multiracial = `Missing End Status Two or More Races`,
           grad_rate_multiracial = `Graduation Rate Multi-race`,

           total_count_pacific_islander = `Total Pacific Islander`,
           grad_count_pacific_islander = `Graduates Pacific Islander`,
           completer_count_pacific_islander = `Completers Pacific Islander`,
           transfer_out_count_pacific_islander = `Transfer Outs Pacific Islander`,
           dropout_count_pacific_islander = `Dropouts Pacific Islander`,
           nongrad_count_pacific_islander = `Non-Graduates Pacific Islander`,
           other_transfer_count_pacific_islander = `Other Transfers Pacific Islander`,
           missing_end_status_count_pacific_islander = `Missing End Status Pacific Islander`,
           grad_rate_pacific_islander = `Graduation Rate Pacific Islander`,

           total_count_white = `Total White`,
           grad_count_white = `Graduates White`,
           completer_count_white = `Completers White`,
           transfer_out_count_white = `Transfer Outs White`,
           dropout_count_white = `Dropouts White`,
           nongrad_count_white = `Non-Graduates White`,
           other_transfer_count_white = `Other Transfers White`,
           missing_end_status_count_white = `Missing End Status White`,
           grad_rate_white = `Graduation Rate White`,

           total_count_cte = `Total CTE`,
           grad_count_cte = `Graduates CTE`,
           completer_count_cte = `Completers CTE`,
           transfer_out_count_cte = `Transfer Outs CTE`,
           dropout_count_cte = `Dropouts CTE`,
           nongrad_count_cte = `Non-Graduates CTE`,
           other_transfer_count_cte = `Other Transfers CTE`,
           missing_end_status_count_cte = `Missing End Status CTE`,
           grad_rate_cte = `Graduation Rate CTE`,

           total_count_frl = `Total FRL`,
           grad_count_frl = `Graduates FRL`,
           completer_count_frl = `Completers FRL`,
           transfer_out_count_frl = `Transfer Outs FRL`,
           dropout_count_frl = `Dropouts FRL`,
           nongrad_count_frl = `Non-Graduates FRL`,
           other_transfer_count_frl = `Other Transfers FRL`,
           missing_end_status_count_frl = `Missing End Status FRL`,
           grad_rate_frl = `Graduation Rate FRL`,

           total_count_iep = `Total IEP`,
           grad_count_iep = `Graduates IEP`,
           completer_count_iep = `Completers IEP`,
           transfer_out_count_iep = `Transfer Outs IEP`,
           dropout_count_iep = `Dropouts IEP`,
           nongrad_count_iep = `Non-Graduates IEP`,
           other_transfer_count_iep = `Other Transfers IEP`,
           missing_end_status_count_iep = `Missing End Status IEP`,
           grad_rate_iep = `Graduation Rate IEP`,

           total_count_ell = `Total ELL`,
           grad_count_ell = `Graduates ELL`,
           completer_count_ell = `Completers ELL`,
           transfer_out_count_ell = `Transfer Outs ELL`,
           dropout_count_ell = `Dropouts ELL`,
           nongrad_count_ell = `Non-Graduates ELL`,
           other_transfer_count_ell = `Other Transfers ELL`,
           missing_end_status_count_ell = `Missing End Status ELL`,
           grad_rate_ell = `Graduation Rate ELL`,

           total_count_migrant = `Total Migrant`,
           grad_count_migrant = `Graduates Migrant`,
           completer_count_migrant = `Completers Migrant`,
           transfer_out_count_migrant = `Transfer Outs Migrant`,
           dropout_count_migrant = `Dropouts Migrant`,
           nongrad_count_migrant = `Non-Graduates Migrant`,
           other_transfer_count_migrant = `Other Transfers Migrant`,
           missing_end_status_count_migrant = `Missing End Status Migrant`,
           grad_rate_migrant = `Graduation Rate Migrant`,

           total_count_all = `Total`,
           grad_count_all = `Graduates Total`,
           completer_count_all = `Completers Total`,
           transfer_out_count_all = `Transfer Outs Total`,
           dropout_count_all = `Dropouts Total`,
           nongrad_count_all = `Non-Graduates Total`,
           other_transfer_count_all = `Other Transfers Total`,
           missing_end_status_count_all = `Missing End Status Total`,
           grad_rate_all = `Graduation Rate Total`,

           adjusted_diploma_count_all = `Adjusted Diploma #`,
           adjusted_diploma_rate_all = `Adjusted Diploma %`,
           adult_diploma_count_all = `Adult Diploma #`,
           adult_diploma_rate_all = `Adult Diploma %`,
           advanced_diploma_count_all = `Advanced Diploma #`,
           advanced_diploma_rate_all = `Advanced Diploma %`,
           certificate_of_attendance_count_all = `Certificate of Attendance and HSE #`,
           certificate_of_attendance_rate_all = `Certificate of Attendance and HSE %`,
           standard_diploma_count_all = `Standard Diploma #`,
           standard_diploma_rate_all = `Standard Diploma %`,
           hse_count_all = `HSE #`,
           hse_rate_all = `HSE %`) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name))

  # Convert the pertinent columns to numeric.
  results_df[,8:ncol(results_df)] <- sapply(results_df[, 8:ncol(results_df)], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  return(results_df)


}
