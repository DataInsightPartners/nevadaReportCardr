#' get_student_data
#'
#' get_student_data returns a data frame of student accountability data (ADA, Avg class size,
#'    student/teacher ratio, retention rates, credit deficient, transiency, etc.) from the
#'    Nevada Report Card (NRC) API given a numeric vector of NRC organization ids and
#'    spring school years.
#'
#' @param org_ids   A numeric vector of NRC organization ids. You can look them up for a school or
#'     district by using get_org_id(name). These can be viewed by looking at the
#'     nrc_orgs data frame that comes with the nrc package.
#' @param spring_years a numeric vector of spring school years.
#'     For example, 2016 would be submitted for the 2015-2016 school year and
#'     2015:2016 would be provided to get both the 2014-2015 and 2015-2016 school years.
#'
#' @return returns a data frame of accountability data.
#'
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Get student data for CCSD schools for 2015-2016
#' ccsd_ids <- nrc_orgs$id[nrc_orgs$parent_id == 64827 | nrc_orgs$id == 64827]
#' ccsd_student_2016 <- get_student_data(ccsd_ids, 2016)
#'
#' # Get student data for the state for all years.
#' state_student <- get_student_data(64825, 2004:2016)

get_student_data <- function(org_ids, spring_years = 2017) {

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

  # Add the nrc year codes to the scope (e10 is the code for the students report).
  nrc_years <- nrc_scopes$code[nrc_scopes$value %in% spring_years]
  scope <- paste(c('e10', nrc_years), collapse = '.')

  # Get the collection id for the group of schools and districts provided.
  col_id <- get_collection_id(org_ids)

  # Scores parameter (Will include teacher ada, substitute teachers, and staffing)
  scores <- '620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,658,659,660,661,662,663,664,665,666,816,818,820,822,817,819,821,823,849,671,672,673,674,675,676,677,678,679,1030,1031,1032,1033,1034,1035,730,731,732,733,734'

  api_url <- 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization='
  target <- paste0(api_url, col_id, '&scope=', scope, '&scores=', scores, '&fields=309,310,311,313,318,320')

  # Download the csv data from the target url.
  resultsText <- RCurl::getURL(target)

  # Populate a data frame from the csv results and format the column headers.
  results_df <- dplyr::as_data_frame(readr::read_csv(resultsText)) %>%
    dplyr::select(name = Name,
           accountability_year = `Accountability Year`,
           school_levels = `School Levels`,
           organization_id = `Organization ID`,
           organization_level = `Organization Level`,
           state_id = identifier,

           ada_rate_all = `Average Daily Attendance - All Students`,
           ada_rate_american_indian = `Average Daily Attendance - American Indian/Alaskan Native`,
           ada_rate_asian = `Average Daily Attendance - Asian`,
           ada_rate_hispanic = `Average Daily Attendance - Hispanic`,
           ada_rate_black = `Average Daily Attendance - Black/African American`,
           ada_rate_white = `Average Daily Attendance - White`,
           ada_rate_pacific_islander = `Average Daily Attendance - Pacific Islander`,
           ada_rate_multiracial = `Average Daily Attendance - Two or More Races`,
           ada_rate_iep = `Average Daily Attendance - IEP`,
           ada_rate_ell = `Average Daily Attendance - ELL`,
           ada_rate_frl = `Average Daily Attendance - FRL`,
           student_teacher_ratio_all = `Student/Teacher Ratio - All Schools*`,
           student_teacher_ratio_kindergarten = `Student/Teacher Ratio - Kindergarten+`,
           student_teacher_ratio_grade01 = `Student/Teacher Ratio - 1st Grade`,
           student_teacher_ratio_grade02 = `Student/Teacher Ratio - 2nd Grade`,
           student_teacher_ratio_grade03 = `Student/Teacher Ratio - 3rd Grade`,
           student_teacher_ratio_grade04 = `Student/Teacher Ratio - 4th Grade`,
           student_teacher_ratio_grade05 = `Student/Teacher Ratio - 5th Grade`,
           student_teacher_ratio_grade06 = `Student/Teacher Ratio - 6th Grade`,
           student_teacher_ratio_grade07 = `Student / Teacher Ratio - 7th Grade`,
           student_teacher_ratio_grade08 = `Student / Teacher Ratio - 8th Grade`,
           avg_class_size_english = `Average Class Size - English`,
           avg_class_size_math = `Average Class Size - Mathematics`,
           avg_class_size_science = `Average Class Size - Science`,
           avg_class_size_social_studies = `Average Class Size - Social Studies`,
           retention_count_kindergarten = `Retention by Grade K #`,
           retention_rate_kindergartne = `Retention by Grade K #`,
           retention_count_grade01 = `Retention by Grade 1 #`,
           retention_rate_grade01 = `Retention by Grade 1%`,
           retention_count_grade02 = `Retention by Grade 2 #`,
           retention_rate_grade02 = `Retention by Grade 2 %`,
           retention_count_grade03 = `Retention by Grade 3 #`,
           retention_rate_grade03 = `Retention by Grade 3 %`,
           retention_count_grade04 = `Retention by Grade 4 #`,
           retention_rate_grade04 = `Retention by Grade 4 %`,
           retention_count_grade05 = `Retention by Grade 5 #`,
           retention_rate_grade05 = `Retention by Grade 5 %`,
           retention_count_grade06 = `Retention by Grade 6 #`,
           retention_rate_grade06 = `Retention by Grade 6 %`,
           retention_count_grade07 = `Retention by Grade 7 #`,
           retention_rate_grade07 = `Retention by Grade 7 %`,
           retention_count_grade08 = `Retention by Grade 8 #`,
           retention_rate_grade08 = `Retention by Grade 8 %`,
           credit_deficient_count_grade09 = `Credit Deficient - Grade 9 #`,
           credit_deficient_rate_grade09 = `Credit Deficient - Grade 9 %`,
           credit_deficient_count_grade10 = `Credit Deficient - Grade 10 #`,
           credit_deficient_rate_grade10 = `Credit Deficient - Grade 10 %`,
           credit_deficient_count_grade11 = `Credit Deficient - Grade 11 #`,
           credit_deficient_rate_grade11 = `Credit Deficient - Grade 11 %`,
           credit_deficient_count_grade12 = `Credit Deficient - Grade 12 #`,
           credit_deficient_rate_grade12 = `Credit Deficient - Grade 12 %`,
           transiency_rate = `Transiency Rate`,
           disc_voilence_to_others_count = `Discipline - Violence to Other Students`,
           disc_violence_to_staff_count = `Discipline - Violence to School Staff`,
           disc_possession_weapons_count = `Discipline - Possession of Weapons`,
           disc_distribution_controlled_substance_count = `Discipline - Distribution of Controlled Substances`,
           disc_possession_controlled_substance_count = `Discipline - Possession or Use of Controlled Substances`,
           disc_possession_alcoholic_beverages_count = `Discipline - Possession or Use of Alcoholic Beverages`,
           disc_bullying_cyberbullying_count = `Discipline - Bullying, Cyber Bullying, Harassment & Intimidation`,
           disc_habitual_problem_expulsion_count = `Discipline - Habitual Disciplinary Problems (Expulsion Only)`,
           disc_habitual_truant_count = `Discipline - Habitual Truants (no suspension or expulsion)`,
           disc_bullying_reported_count = `Discipline - Bullying Incidents Reported`,
           disc_bullying_confirmed_count = `Discipline - Bullying Incidents Determined to be so after an Investigation`,
           disc_bullying_sus_exp_count = `Discipline - Bullying Incidents Suspension/Expulsion`,
           disc_cyberbullying_reported_count = `Discipline - Cyber Bullying Incidents Reported`,
           disc_cyberbullying_confirmed_count = `Discipline - Cyber Bullying Incidents Determined to be so after an Investigation`,
           disc_cyberbullying_sus_exp = `Discipline - Cyber Bullying Incidents Suspension/Expulsion`,
           prof_failure_count = `Proficiency Failures #`,
           prof_failure_rate = `Proficiency Failures %`,
           nshe_enrolled_py_count = `# Enrolled`,
           nshe_remediated_py_count = `# Remediated`,
           nshe_remediated_py_rate = `% Remediated`) %>%
    dplyr::rowwise() %>%
    # Remove the dash and id number from the name.
    dplyr::mutate(name = ifelse(stringr::str_detect(name, ' - [0-9]'),
                                stringr::str_sub(name, 1, stringr::str_locate(name, ' - [0-9]')[1] - 1), name))

  # Convert the pertinent columns to numeric.
  results_df[,grepl('_count', names(results_df))] <- sapply(results_df[, grepl('count', names(results_df))], as.numeric)
  results_df[,grepl('rate', names(results_df))] <- sapply(results_df[, grepl('rate', names(results_df))], as.numeric)
  results_df[,grepl('avg', names(results_df))] <- sapply(results_df[, grepl('avg', names(results_df))], as.numeric)

  # Transform the rate columns from a whole number to a decimal.
  results_df[,grepl('rate', names(results_df))] <- results_df[,grepl('rate', names(results_df))] / 100

  return(results_df)

}
