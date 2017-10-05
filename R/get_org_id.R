#' get_org_id
#'
#' get_org_id returns the Nevada Report Card (NRC) organization ids and names for all organizations
#'     whose name matches the string provided as the org_name parameter. The organization id is the
#'     unique id used by NRC, it is different than the state id used by the Nevada Department
#'     of Education.
#'
#'     The nrc package comes with a data frame `nrc_orgs` that contains all the districts and
#'     schools that hava a org_id. That is the data set this function pulls information from.  You
#'     can inspect it by executing: `View(nrc_orgs)`
#'
#' @param org_name   a string that is part of an organization's name (district or school).
#'
#' @return returns a data frame of ids and names for all the organizations that matched
#'         the org_name that was provided and prints them to the console.
#' @export
#'
#' @examples
#' # Note the function ignores case (capitalization doesn't matter)
#' get_org_id('forbuss')
#' get_org_id('Forbuss')
#'
#' get_org_id('churchill')


get_org_id <- function(name) {
  result <- nrc_orgs[grepl(name, nrc_orgs$name, ignore.case = TRUE), c('id', 'name', 'state_id')]
  result <- dplyr::rename(result, org_id = id)
  print(as.data.frame(result), row.names = FALSE)
}
