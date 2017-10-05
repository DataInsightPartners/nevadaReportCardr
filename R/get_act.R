#' get_act
#'
#' get_act retrieves ACT results from the Nevada Report Card (NRC) API
#'
#'    WARNING: There are no ids provided with the data extract and there are instances of school
#'    names that match across school districts.
#'
#'    All of the data, through 2017, that can be pulled by this function is already available in
#'    a data frame `nrc_act` that comes with the nrc package; View(nrc_act). This function along
#'    with `create_nrc_act` was used to create that data frame. That data includes state_id,
#'    org_id, and type.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2016:2017 would be provided to get both the 2015-2016 and 2016-2017 school years.
#'
#' @return returns a data frame of school, district and/or state ACT results.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Get district ACT results.
#' get_act(nrc_orgs$id[nrc_orgs$type == 'D'], spring_years = 2016)


get_act <- function(org_ids, spring_years = 2016:2017) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2016:2017

  if (is.null(spring_years)) return(stop("Invalid spring_years provided. Must be 2016 or 2017."))

  for (year in spring_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid spring_years provided. Must be 2016 or 2017."))
    }
  }

  # ACT results are only available for grade 11 from Nevada Report Card.
  nrc_grades <- 'g11'

  # Add the nrc year codes to the scope (e25 is the code for the ACT data).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e25', nrc_years, nrc_grades), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores <- 'AC_SS,N_AC,N_MA,MA_SS,N_EN,EN_SS,N_RD,RD_SS,N_SC,SC_SS,N_WR,WR_SS'


  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores)


  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::select(name = Group,
                  year = Year,
                  grade = Grade,
                  grade_enrolled = `Number Enrolled`,

                  all_test_count = `Number Tested All`,
                  avg_composite_score = `Mean Composite Score`,

                  math_test_count = `Mathematics - Number Tested`,
                  avg_math_score = `Mathematics - Mean Scale Score`,

                  english_test_count = `English - Number Tested`,
                  avg_english_score = `English - Mean Scale Score`,

                  reading_test_count = `Reading - Number Tested`,
                  avg_reading_score = `Reading - Mean Scale Score`,

                  science_test_count = `Science - Number Tested`,
                  avg_science_score = `Science - Mean Scale Score`,

                  writing_test_count = `Writing - Number Tested`,
                  avg_writing_score = `Writing - Mean Scale Score`)

  # Convert the pertinent columns to numeric.
  results_df[,4:ncol(results_df)] <- sapply(results_df[, 4:ncol(results_df)], as.numeric)

  return(results_df)
}
