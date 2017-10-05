#' update_org_labels
#'
#' create_org_labels creats a copy of the nrc_orgs dataset in the global environment named
#'     `temp_nrc_orgs`.  It then updates the label_name for an organization given that
#'     organizations id.
#'
#'     You can update a label name and then assign the temp data set back to `nrc_orgs` and
#'     then use the command `devtools::use_data(nrc_orgs, overwrite == TRUE)` to overwrite the
#'     set that is embedded in the `nrc` package.  Then `CTRL + SHIFT + B` to rebuild the
#'     package with the updated nrc_orgs data.
#'
#' @return Updates the nrc_org data set provided with the `nrc` package.
#'
#' @export
#' @importFrom dplyr "%>%"


update_org_labels <- function(org_id, label_name) {

  # Check that a valid org_id was provided
  valid_ids <- nrc_orgs$id

  if (is.null(org_ids)) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  if (length(org_id) != 1) return(stop("Invalid org_ids provided. Only a single id from nrc_orgs may be provided."))

  if (!id %in% valid_ids) return(stop("Invalid org_ids provided. Must be an id or ids from nrc_orgs."))

  temp <- get0("temp_nrc_orgs", ifnotfound = nrc_orgs)

  temp$label_name[temp$id == org_id] <- label_name
  assign("temp_nrc_orgs", temp, envir = .GlobalEnv)


}
