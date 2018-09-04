#' get_sbac
#'
#' get_sbac retrieves SBAC results from the Nevada Report Card (NRC) API
#'
#'    WARNING: There are no ids provided with the data extract and there are instances of school
#'    names that match across school districts.
#'
#'    All of the data, through 2017, that can be pulled by this function is already available in
#'    a data frame `nrc_sbac` that comes with the nrc package; View(nrc_sbac). This function along
#'    with `create_nrc_sbac` was used to create that data frame. That data includes state_id,
#'    org_id, and type.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#' @param grades a numeric vector of grades to be included. Can be numbers 3-8
#'
#' @return returns a data frame of school and/or district SBAC results.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Get district SBAC results for all grades.
#' get_sbac(nrc_orgs$id[nrc_orgs$type == 'D'], spring_years = 2016, grades = 3:8)


get_sbac <- function(org_ids, spring_years = 2016:2017, grades = 3:8) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2016:2018

  if (is.null(spring_years)) return(stop("Invalid spring_years provided. Must be 2016 through 2018."))

  for (year in spring_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid spring_years provided. Must be 2016 through 2018."))
    }
  }

  # Check that valid grades were provided
  valid_grades <- 3:8

  if (is.null(grades)) return(stop("Invalid grades provided. Must be 3 through 8."))

  for (grade in grades) {
    if (!grade %in% valid_grades) {
      return(stop("Invalid grades provided. Must be 3 through 8."))
    }
  }

  # Convert the grades to nrc grade codes.
  nrc_grades <- paste0('g', grades)

  # Add the nrc year codes to the scope (e24 is the code for the sbac data).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e24', nrc_years, nrc_grades), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores <- 'MA_level,MA_pass,MA_SS,N_MA,RD_level,RD_pass,RD_SS,N_RD'


  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores)


  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::select(name = Group,
           year = Year,
           grade = Grade,
           grade_enrolled = `Number Enrolled`,

           math_test_count = `Mathematics - Number Tested`,
           math_ss = `Mathematics - Mean Scale Score`,
           math_prof_rate = `Mathematics - % Proficient`,
           math_emergent_rate = `Mathematics - % Emergent/Developing`,
           math_approaches_rate = `Mathematics - % Approaches Standard`,
           math_meets_rate = `Mathematics - % Meets Standard`,
           math_exceeds_rate = `Mathematics - % Exceeds Standard`,

           read_test_count = `Reading - Number Tested`,
           read_ss = `Reading - Mean Scale Score`,
           read_prof_rate = `Reading - % Proficient`,
           read_emergent_rate = `Reading - % Emergent/Developing`,
           read_approaches_rate = `Reading - % Approaches Standard`,
           read_meets_rate = `Reading - % Meets Standard`,
           read_exceeds_rate = `Reading - % Exceeds Standard`) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name))

  # Convert the pertinent columns to numeric.
  results_df[,4:ncol(results_df)] <- sapply(results_df[, 4:ncol(results_df)], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  return(results_df)
}
