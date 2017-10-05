#' get_collection_id
#'
#' get_collection_id returns the Nevada Report Card (NRC) collection id that is used to reference a
#'    collection of schools and/or districts when requesting data (e.g. assessment data, personnel data, etc.).
#'    This function is referenced internally by most of the other functions in the nrc package.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package, View(nrc_orgs).
#'
#' @return returns a collection id for the set of organization ids provided.
#'
#' @export
#'
#' @examples
#' # Get the collection id for all organizations (c18172)
#' get_collection_id(unique(nrc_orgs$id))
#'
#' # Get the collection id for organizations in Clark County
#' get_collection_id(nrc_orgs$id[nrc_orgs$parent_id == 64827])
#'
#' # Get the collection id for all the districts in Nevadda
#' get_collection_id(nrc_orgs$id[nrc_orgs$type == 'D'])

get_collection_id <- function(org_ids) {

  # Check that valid org_ids were provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  for (id in org_ids) {
    if (!id %in% valid_ids) {
      return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))
    }
  }

  r = RCurl::dynCurlReader()
  ids <- paste0("=", paste(org_ids, collapse = ','))
  RCurl::curlPerform(postfields = ids,
                     url = 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationCollectionHash',
                     verbose = FALSE,
                     post = 1L,
                     writefunction = r$update)
  paste0("c", r$value())
}
