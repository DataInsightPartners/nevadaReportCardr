#' get_profile
#'
#' get_profile returns the school profile from the Nevada Report Card (NRC) API given the school's
#'     organization id and a spring school year.
#'
#' @param org_id   A single NRC organization id. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_year a single spring school year.
#'     For example, 2016 would be submitted for the 2015-2016 school year
#'
#' @return a dataframe of the accountability profile data for the provided entity and school year.
#' @export
#'
#' @examples
#' # Get the 2013-2014 profile for Forbuss ES
#' forbuss_org_id <- get_org_id('forbuss')$org_id
#' forbuss_profile <- get_profile(org_id = forbuss_org_id, spring_year = 2014)

get_profile <- function(org_id, spring_year = 2018) {

  # Check that a valid org_id was provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_id)) return(stop("Invalid org_ids provided. Must be an id from nrc_orgs."))

  if (length(org_id) != 1) return(stop("Invalid org_ids provided. Only a single id from nrc_orgs may be provided."))

  if (!id %in% valid_ids) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  # Convert the school year to the year code used in NRC.
  nrc_year <- nrc_scopes$code[nrc_scopes$value == spring_year]

  # Create a vector of the uri parameters
  parameters <- structure(c("profile_1", "profile", nrc_year, org_id),
                          .Names = c("report", "reportID","scope","organization"))

  # Get the school profile data (returned as a json list).
  #  Wrapped in a try fucntion to prevent an error when there is no data for the given year.
  prof.json.lst <- try(RCurl::getForm('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/Profile',
                               .params = parameters), silent = TRUE)

  # If there was no data, return 'No Data.'
  if (class(prof.json.lst) == 'try-error') {return('No Data.')}

  # Transform from JSON to list to named vector to transposed vector to data frame.
  prof.df <- dplyr::as_data_frame(t(unlist(RJSONIO::fromJSON(prof.json.lst))))
  prof.df$org_id <- org_id
  prof.df$state_id <- nrc_orgs$state_id[nrc_orgs$id == org_id]
  prof.df$year <- spring_year

  return(prof.df)
}


# profiles <- dplyr::data_frame(name = character())
#
#   for (id in nrc_orgs$id) {
#     profile <- get_profile(id, 2018)
#     if (profile != 'No Data.') {
#       profiles <- dplyr::bind_rows(profiles, profile)
#     }
#   }

