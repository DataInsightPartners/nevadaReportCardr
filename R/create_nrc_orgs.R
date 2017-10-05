#' create_nrc_orgs
#'
#' create_nrc_orgs creates a data frame with all the orgnaizations from the Nevada Report Card (NRC)
#'    API. This provides an organization id, state id, name, and type for each school, district,
#'    and the state.
#'
#'    This data is then joined with the demographic data in order to pull in the levels the school
#'    services (i.e. 1 2 3 for ES, MS, and HS)
#'
#'    All of the data that can be pulled by this function is already available in
#'    a data frame `nrc_orgs` that comes with the nrc package; View(nrc_orgs). This function was
#'    used to create that data frame.
#'
#' @return returns a data frame of all NRC organizations: schools, dirstricts, and the state.
#'
#' @importFrom dplyr "%>%"


create_nrc_orgs <- function() {
  # NRC API url to pull organization information.
  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/Organizations'

  # Retrieve that data from the NRC API.
  results_json <- RCurl::getURL(api_url)

  # Use fromJSON to convert from JSON to list. Then use ldply to loop through each list item
  #   and convert to data frame and combine all items to a single data frame.
  orgs_df <- plyr::ldply(RJSONIO::fromJSON(results_json),
                         function(x) dplyr::as_data_frame(x))

  # Format the column names and add a district column.
  orgs_df <- dplyr::select(orgs_df,
                    id,
                    parent_id,
                    state_id = code,
                    name,
                    type) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name),
                  district = ifelse(type == 'B', orgs_df$name[orgs_df$id == parent_id], name),
                  district = ifelse(stringr::str_detect(district, ' - [0-9]'),
                                    stringr::str_sub(district, 1, stringr::str_locate(district, ' - [0-9]')[1] - 1), district))

  # Add in the grade levels.
  orgs_df <- dplyr::left_join(orgs_df,
                              dplyr::filter(nrc_demographics, accountability_year == '2016-2017') %>%
                                dplyr::rowwise() %>%
                                dplyr::mutate(state_id = ifelse(nchar(state_id) == 1 | nchar(state_id) == 4,
                                                        paste0('0',state_id), as.character(state_id))) %>%
                                dplyr::select(school_levels, organization_level, state_id))

  # Equipo & SLAM were not designated a charter school.
  orgs_df$organization_level[orgs_df$state_id == 18433] <- 'State Public Charter Schools'
  orgs_df$organization_level[orgs_df$state_id == 18434] <- 'State Public Charter Schools'

  # Create a column to indicate charter school.
  orgs_df$charter <- ifelse(grepl('charter', orgs_df$organization_level, ignore.case = TRUE),1, 0)

  return(orgs_df)
}







