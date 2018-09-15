#' get_sbac_subgroup
#'
#' get_sbac_subgroup retrieves SBAC subgroup results from the Nevada Report Card (NRC) API
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#' @param grades a numeric vector of grades to be included. Can be numbers 3-8
#' @param ethnicities a character vector of ethnicities to filter by
#'     Possible Values: 'A', 'B', 'C', 'H', 'I', 'M', 'P'
#'     Corresponding to: Asian, Black, Caucasian, Hispanic, Indian (Native American), Multiracial,
#'     Pacific Islander
#'     NULL will not filter by ethnicity.
#'
#' @param gender a character of either 'M' or 'F' to filter by male or female.
#'    NULL will not filter by gender.
#' @param iep a character of either 'Y' or 'N' to filter by IEP or Not IEP.
#'    NULL will not filter by IEP.
#' @param frl a character of either 'Y' or 'N' to filter by FRL or Not FRL.
#'    NULL will not filter by FRL.
#' @param ell a character of either 'Y' or 'N' to filter by ELL or Not ELL.
#'    NULL will not filter by ELL.
#' @param condition string of either 'AND' or 'OR' that sets the condition on how the other
#'    parameters are combined. (e.g. FRL AND ELL or FRL OR ELL)
#'
#' @return returns a data frame of sbac results or the requested organizations,
#'    and grades for the filtered subgroup.
#'
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Get district results for African American male students.
#' district_ids <- nrc_orgs$id[nrc_orgs$type == 'D']
#' african_american_male <- get_sbac_subgroup(org_ids = district_ids, ethnicities = 'b',gender = 'm')
#' View(african_american_male)
#'
#' # Get state results for students who are FRL and ELL
#' state_id <- nrc_orgs$id[nrc_orgs$type == 'S']
#' state_frl_ell <- get_sbac_subgroup(org_ids = state_id, frl = 'Y', ell = 'Y')


get_sbac_subgroup <- function(org_ids, spring_years = 2016:2018, grades = 3:8, ethnicities = NULL, gender = NULL, iep = NULL, frl = NULL, ell = NULL, condition = 'and') {

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
      return(stop("Invalid spring_years provided. Must be 2016 or 2017."))
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

  # Create empty character vectors to hold the filter data.
  filter_data <- character()
  filter_keys <- character()
  # Check that valid grades were provided.
  if (is.null(grades)) return(stop("Invalid grades provided. Must be 3 through 8."))

  valid_grades <- 3:8
  for (grade in grades) {
    if (!grade %in% valid_grades)
      return(stop("Invalid grades provided. Must be 3 through 8."))
  }


  # Check that valid ethnicities were provided.
  if (!is.null(ethnicities)) {
    ethnicities <- toupper(ethnicities)
    valid_ethnicities <- c('A', 'B', 'C', 'H', 'I', 'M', 'P')
    for (ethncitiy in ethnicities) {
      if (!ethncitiy %in% valid_ethnicities) {
        return(stop("Invalid ethnicity code included.
                    Only 'A', 'B', 'C', 'H', 'I', 'M', 'P', or NULL accepted."))
      }
    }
    filter_data <- c(filter_data, paste0('ethnicity_', ethnicities))
    filter_keys <- c(filter_keys, paste0('ethnicity.', ethnicities))
  }


  # Check that a valid gender was provided
  if (!is.null(gender)) {
    gender <- toupper(gender)
    valid_genders <- c('M', 'F')
    if (!toupper(gender) %in% valid_genders) {
      return(stop("Invalid gender code provided. Only 'M', 'F', or NULL accepted."))
    }
    filter_data <- c(filter_data, paste0('gender_', gender))
    filter_keys <- c(filter_keys, paste0('gender.', gender))
  }

  # Check that a valid iep was provided
  if (!is.null(iep)) {
    iep <- toupper(iep)
    valid_iep <- c('Y', 'N')
    if (!iep %in% valid_iep) {
      return(stop("Invalid IEP choice provided. Only 'Y', 'N' or NULL accepted."))
    }
    filter_data <- c(filter_data, paste0('iep_', iep))
    filter_keys <- c(filter_keys, paste0('iep.', iep))
  }

  # Check that a valid frl was provided
  if (!is.null(frl)) {
    frl <- toupper(frl)
    valid_frl <- c('Y', 'N')
    if (!frl %in% valid_frl) {
      return(stop("Invalid FRL choice provided. Only 'Y', 'N' or NULL accepted."))
    }
    filter_data <- c(filter_data, paste0('frl_', frl))
    filter_keys <- c(filter_keys, paste0('frl.', frl))
  }

  # Check that a valid ell was provided
  if (!is.null(ell)) {
    valid_ell <- c('Y', 'N')
    if (!toupper(ell) %in% valid_ell) {
      return(stop("Invalid ELL choice provided. Only 'Y', 'N' or NULL accepted."))
    }
    filter_data <- c(filter_data, paste0('lep_', ell))
    filter_keys <- c(filter_keys, paste0('lep.', ell))
  }

  # Collapse the filter vectors into a comma separated string.
  filter_data <- paste(filter_data, collapse = ',')
  filter_keys <- paste(filter_keys, collapse = '.')

  # Check that a valid condition was provided
  if (is.null(condition)) return(stop("Invalid condition provided. Only 'AND' or 'OR' accepted."))

  condition <- tolower(condition)
  valid_conditions <- c('and', 'or')
  if (!condition %in% valid_conditions) return(stop("Invalid condition provided. Only 'AND' or 'OR' accepted."))



  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)
  # scores parameter (% in achievement levels, % proficient, avg. scale score, number tested)
  scores <- 'MA_level,MA_pass,MA_SS,N_MA,RD_level,RD_pass,RD_SS,N_RD'


  # Add the nrc year codes to the scope (e24 is the code for the sbac data).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e24', nrc_years), collapse = '.')
  # Add grades to the scope.
  for (grade in grades) {
    scope <- paste0(scope, '.g', grade)
  }

  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&filterdata=', filter_data, '&filterkeys=', filter_keys, '&filterrelation=', condition)


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
           read_exceeds_rate = `Reading - % Exceeds Standard`)

  # Convert the pertinent columns to numeric.
  results_df[,4:ncol(results_df)] <- sapply(results_df[, 4:ncol(results_df)], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  # Add metadata to the data frame that indicates what the filters were.
  results_df$filter <- filter_data
  results_df$condition <- condition

  return(results_df)

}
