#' get_cte
#'
#' get_cte returns a data frame of accountability career annd techinical education data  from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids.
#'
#'    All of the data that can be pulled by this function is already available in a data frame
#'    `nrc_cte` that comes with the nrc package; View(nrc_cte). This function was
#'    used to create that data frame.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#'
#' @return returns a data frame of accountability cte data.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' all_nde_cte <- get_cte(nrc_orgs$id)

get_cte <- function(org_ids, spring_years = 2008:2017) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2008:2017

  if (is.null(spring_years)) return(stop("Invalid spring_years provided. Must be 2008 through 2017."))

  for (year in spring_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid spring_years provided. Must be 2008 through 2017."))
    }
  }

  # Add the nrc year codes to the scope (e11 is the code for the cte report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e11', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores = '735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759'


  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&fields=309,310,311,313,318,320')


  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::select(name = Name,
                  accountability_year = `Accountability Year`,
                  school_levels = `School Levels`,
                  organization_id = `Organization ID`,
                  organization_level = `Organization Level`,
                  state_id = identifier,

                  cte_ada_rate_all = `CTE Average Daily Attendance - All Students`,
                  cte_ada_rate_american_indian = `CTE Average Daily Attendance - American Indian/Alaskan Native`,
                  cte_ada_rate_asian = `CTE Average Daily Attendance - Asian`,
                  cte_ada_rate_hispanic = `CTE Average Daily Attendance - Hispanic`,
                  cte_ada_rate_black = `CTE Average Daily Attendance - Black/African American`,
                  cte_ada_rate_white = `CTE Average Daily Attendance - White`,
                  cte_ada_rate_pacific_islander = `CTE Average Daily Attendance - Pacific Islander`,
                  cte_ada_rate_multiracial = `CTE Average Daily Attendance - Two or More Races`,
                  cte_ada_rate_iep = `CTE Average Daily Attendance - IEP`,
                  cte_ada_rate_ell = `CTE Average Daily Attendance - ELL`,
                  cte_ada_rate_frl = `CTE Average Daily Attendance - FRL`,
                  cte_enrollment_count = `CTE - CTE Enrollment`,
                  cte_course_completer_count = `CTE - # Course Completers`,
                  cte_program_completer_count = `CTE - # Program Completers`,
                  cte_standard_diploma_count = `CTE - Standard Diploma #`,
                  cte_standard_diploma_rate = `CTE - Standard Diploma %`,
                  cte_advanced_diploma_count = `CTE - Advanced Diploma #`,
                  cte_advanced_diploma_rate = `CTE - Advanced Diploma %`,
                  cte_adult_diploma_count = `CTE - Adult Diploma #`,
                  cte_adult_diploma_rate = `CTE - Adult Diploma %`,
                  cte_adjusted_diploma_count = `CTE - Adjusted Diploma #`,
                  cte_adjusted_diploma_rate = `CTE - Adjusted Diploma %`,
                  cte_certificate_of_attendance_count = `CTE - Certificate of Attendance #`,
                  cte_certificaate_of_attendance_rate = `CTE - Certificate of Attendance %`,
                  cte_drop_out_rate = `CTE - % Pupils Enrolled in CTE Program Prior to Dropping Out of School`
    ) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name))

  # Convert the pertinent columns to numeric.
  results_df[,7:ncol(results_df)] <- sapply(results_df[, 7:ncol(results_df)], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  return(results_df)
}
