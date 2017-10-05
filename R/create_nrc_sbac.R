#' create_nrc_sbac
#'
#' create_nrc_sbac creates a data frame with all sbac results and includes state_id, org_id, and
#'    type. Those fields are not included in the sbac api output and are included by combining it
#'    with the organization api.
#'
#'    All of the data, through 2017, that can be pulled by this function is already available in
#'    a data frame `nrc_sbac` that comes with the nrc package; View(nrc_sbac). This function was
#'    used to create that data frame.
#'
#' @return returns a data frame of school and/or district SBAC results.
#'
#' @export
#' @importFrom dplyr "%>%"


create_nrc_sbac <- function() {

  # Subset all the districts in order to loop through them individually to pull SBAC results.
  # This will prevent running into duplicate school names across districts.
  districts <- nrc_orgs[nrc_orgs$type == 'D',]

  # Initialize an empty data frame to hold the SBAC results as they are retrieved by distirct.
  nrc_sbac <- data_frame(name = character())

  # Looop through each district to pull SBAC results from the district and its schools.
  for (district_id in districts$id) {

    # Subset meta data (org_id, state_id, and type) about the district and its schools.
    #   This is data not available from the assessment API and needs to be added.
    #   != 18427 excludes the instance of Doral Academy not assosciated with demographics data.
    dist_meta <- filter(nrc_orgs, (parent_id == district_id | id == district_id) & state_id != 18427) %>%
      select(org_id = id,
             state_id,
             name,
             type,
             district,
             charter)
    # Retrive the SBAC data from the API for the current district and schools.
    dist_sbac <- get_sbac(dist_meta$org_id)

    # Join the SBAC data and meta data.
    dist_sbac <- inner_join(dist_sbac, dist_meta,
                            by = c('name' = 'name'))

    # Append the results to the data frame that will ultimately be returned by the main function.
    nrc_sbac <- bind_rows(nrc_sbac, dist_sbac)
  }

  # Pull the state meta data (org_id, state_id, and type) to be joined to the state SBAC results.
  state_meta <- filter(nrc_orgs, id == 64825) %>%
    select(org_id = id,
           state_id,
           name,
           type,
           district,
           charter)

  # Retrieve the state SBAC results.
  state_sbac <- get_sbac(state_meta$org_id)
  # Join the state SBAC results and meta data.
  state_sbac <- inner_join(state_sbac, state_meta, by = c('name' = 'name'))
  # Combine the state results with all the district and school results.
  nrc_sbac <- bind_rows(nrc_sbac, state_sbac)

  return(nrc_sbac)
}

