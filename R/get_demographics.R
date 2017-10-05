#' get_demographics
#'
#' get_demographics returns a data frame of accountability demographics from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids.
#'
#'    All of the data that can be pulled by this function is already available in a data frame
#'    `nrc_demographics` that comes with the nrc package; View(nrc_demographics). This function was
#'    used to create that data frame.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#'
#' @return returns a data frame of accountability demographics.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' all_nde_demographics <- get_demographics(nrc_orgs$id)

get_demographics <- function(org_ids, spring_years = 2004:2017) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  # Check that valid years were provided
  valid_years <- 2004:2017

  if (is.null(spring_years)) return(stop("Invalid spring_years provided. Must be 2004 through 2017."))

  for (year in spring_years) {
    if (!year %in% valid_years) {
      return(stop("Invalid spring_years provided. Must be 2004 through 2017."))
    }
  }

  # Add the nrc year codes to the scope (e7 is the code for the demographics report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e7', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores = '1026,566,567,568,569,570,571,572,573,574,575,805,576,577,806,586,587,588,589,578,579,580,1038,1040,1042,581,582,583,584,1039,1041,1043,585'


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

           total_enrollment = `Total Enrollment`,
           american_indian_count = `American Indian / Alaskan Native #`,
           american_indian_rate = `American Indian / Alaskan Native %`,
           asian_count = `Asian #`,
           asian_rate = `Asian %`,
           hispanic_count = `Hispanic #`,
           hispanic_rate = `Hispanic %`,
           black_count = `Black #`,
           black_rate = `Black %`,
           white_count = `White #`,
           white_rate = `White %`,
           pacific_islander_count = `Pacific Islander #`,
           pacific_islander_rate = `Pacific Islander %`,
           multiracial_count = `Two or More Races #`,
           multiracial_rate = `Two or More Races %`,
           male_count = `Male #`,
           male_rate = `Male %`,
           female_count = `Female #`,
           female_rate = `Female %`,
           iep_count = `IEP #`,
           iep_rate = `IEP %`,
           ell_count = `ELL #`,
           ell_rate = `ELL %`,
           frl_count = `FRL Eligible #`,
           frl_rate = `FRL Eligible %`,
           frb_count = `FRB Eligible #`,
           frb_rate = `FRB Eligible %`,
           migrant_count = `Migrant #`,
           migrant_rate = `Migrant %`
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
