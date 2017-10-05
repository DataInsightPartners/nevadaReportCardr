#' get_fiscal
#'
#' get_fiscal returns a data frame of accountability fiscal data from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids.
#'
#'    WARNING! According to NRC, this data is reported for the year prior to the accountability year.
#'    A column has been added to the API data to indicate report year which is the spring year
#'    prior to the accountability year.
#'
#'    The data starts in 2007-2008 because footnote on NRC stated that is when district totals began
#'    to exclude district sponsored charter school data.
#'
#'    All of the data that can be pulled by this function is already available in a data frame
#'    `nrc_fiscal` that comes with the nrc package; View(nrc_fiscal). This function was
#'    used to create that data frame.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#'
#' @return returns a data frame of accountability fiscal data.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' all_nde_fiscal <- get_fiscal(nrc_orgs$id)

get_fiscal <- function(org_ids, spring_years = 2008:2017) {

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

  # Add the nrc year codes to the scope (e9 is the code for the fiscal report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e9', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores = '606,607,608,609,610,611,612,613,614,615,616,617,618,619'


  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&fields=309,310,311,313,318,320')


  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::mutate(report_year = substr(`Accountability Year`, 1, 4)) %>%
    dplyr::select(name = Name,
                  accountability_year = `Accountability Year`,
                  report_year,
                  school_levels = `School Levels`,
                  organization_id = `Organization ID`,
                  organization_level = `Organization Level`,
                  state_id = identifier,

                  instruction_dollars = `Instruction $`,
                  instruction_rate = `Instruction %`,

                  instruction_support_dollars = `Instruction Support $`,
                  instruction_support_rate = `Instruction Support %`,

                  operations_dollars = `Operations $`,
                  operations_rate = `Operations %`,

                  leadership_dollars = `Leadership $`,
                  leadership_rate = `Leadership %`,

                  total_expenditures_dollars = `Total Expenditures $`,

                  federal_rate = Federal,
                  state_rate = State,
                  local_rate = Local,
                  other_rate = Others
    ) %>%
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
