#' get_technology
#'
#' get_technology returns a data frame of accountability technology data from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids.
#'
#'    All of the data that can be pulled by this function is already available in a data frame
#'    `nrc_technology` that comes with the nrc package; View(nrc_technology). This function was
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

get_technology <- function(org_ids, spring_years = 2012:2017) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2012:2017

  if (is.null(spring_years)) return(stop("Invalid spring_years provided. Must be 2012 through 2017."))

  for (year in spring_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid spring_years provided. Must be 2012 through 2017."))
    }
  }

  # Add the nrc year codes to the scope (e8 is the code for the technology report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e8', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores = '809,810,811,812,813,814,815'


  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&fields=309,310,311,313,318,320')


  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::mutate(report_year = substr(`Accountability Year`, 1, 4)) %>%
    dplyr::select(name = Name,
                  accountability_year = `Accountability Year`,
                  school_levels = `School Levels`,
                  organization_id = `Organization ID`,
                  organization_level = `Organization Level`,
                  state_id = identifier,

                  stu_comp_ratio_5yr_less = `Student/Computer Ratio Computers 5 Yrs. Old of Newer`,
                  stu_comp_ratio_5yr_older = `Student/Computer Ratio Computers Older Than 5 Yrs.`,
                  comp_5yr_less_rate = `% of All Computers 5 Yrs Old or Newer`,

                  stu_mobile_ratio = `Student/Mobile Learning Device Ratio`,
                  interactive_projection_systems = `% of Classrooms with Interactive Projection Systems`,

                  technician_comp_ratio = `Ratio of IT Technicians per Computer`,
                  tech_coach_stu_ratio = `Ratio of Technology Coaches per Student`) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name),

                  stu_count = ifelse(stringr::str_detect(stu_comp_ratio_5yr_less, ':'),
                                     stringr::str_sub(stu_comp_ratio_5yr_less,
                                                      1, stringr::str_locate(stu_comp_ratio_5yr_less, ':')[1] - 1)),

                  comp_5yr_less_count = ifelse(stringr::str_detect(stu_comp_ratio_5yr_less, ':'),
                                               stringr::str_sub(stu_comp_ratio_5yr_less,
                                                                stringr::str_locate(stu_comp_ratio_5yr_less, ':')[1] + 1)),

                  comp_5yr_older_count = ifelse(stringr::str_detect(stu_comp_ratio_5yr_older, ':'),
                                               stringr::str_sub(stu_comp_ratio_5yr_older,
                                                                stringr::str_locate(stu_comp_ratio_5yr_older, ':')[1] + 1)),

                  mobile_count = ifelse(stringr::str_detect(stu_mobile_ratio, ':'),
                                                stringr::str_sub(stu_mobile_ratio,
                                                                 stringr::str_locate(stu_mobile_ratio, ':')[1] + 1)),

                  total_comp_count = as.numeric(comp_5yr_less_count) + as.numeric(comp_5yr_older_count),
                  total_device_count = as.numeric(mobile_count) + as.numeric(total_comp_count),
                  stu_per_comp_5yr_less = as.numeric(stu_count) / as.numeric(comp_5yr_less_count),
                  stu_per_comp_5yr_older = as.numeric(stu_count) / as.numeric(comp_5yr_older_count),
                  stu_per_comp = as.numeric(stu_count) / total_comp_count,
                  stu_per_mobile = as.numeric(stu_count) / as.numeric(mobile_count),
                  stu_per_device = as.numeric(stu_count) / total_device_count)

  # Convert the pertinent columns to numeric.
  results_df[,grepl('_count', names(results_df))] <- sapply(results_df[, grepl('_count', names(results_df))], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  return(results_df)
}
