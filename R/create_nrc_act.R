#' create_nrc_act
#'
#' create_nrc_act creates a data frame with all ACT results and includes state_id, org_id, and
#'    type. Those fields are not included in the ACT api output and are included by combining it
#'    with the organization api data.
#'
#'    All of the data, through 2017, that can be pulled by this function is already available in
#'    a data frame `nrc_act` that comes with the nrc package; View(nrc_act). This function was
#'    used to create that data frame.
#'
#' @return returns a data frame of school and/or district ACT results.
#'
#' @export
#' @importFrom dplyr "%>%"


create_nrc_act <- function() {

  act_results <- get_act(nrc_orgs$id, spring_years = 2016:2017)

  # Filter out results that were supressed due to low n counts.
  act_results <- filter(act_results, !is.na(avg_composite_score)) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name. Rename 'State Public Charter Schools' to 'State Public Schools'
    #   to match what is in nrc_orgs.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name),
                  name = ifelse(name == 'State Public Charter Schools', 'State Public Schools', name))

  # Join with the organization data. After reviewing the data, Davidson gets duplicated if
  #  one of the instances of it in the organization data is not excluded (id = 65509).
  nrc_act <- left_join(act_results, filter(nrc_orgs, id != 65509) %>%
                         rename(org_id = id),
                       by = c('name' = 'name'))

  # Check to make sure that no duplicates have been created by adding in the organization metadata.
  if (nrow(act_results) != nrow(nrc_act)) {
    return(stop("Duplicates were created when adding in organization metadata. Review act and nrc_orgs data to fix."))
  }

  return(nrc_act)
}


