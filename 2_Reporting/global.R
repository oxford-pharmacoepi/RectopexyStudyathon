# load packages -----
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(IncidencePrevalence)
library(snakecase)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#d8323c", #  "#D8DEE9",
    dark_hover_bg = "#b72818", #"#81A1C1",
    dark_color ="white"# "#2E3440"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)
# functions ----
nice.num3<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
nice.num1<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

# read results from data folder ----
results<-list.files(here("data"), recursive = TRUE,
                    full.names = TRUE)

# cdm snapshot ------
cdm_snapshot_files<-results[stringr::str_detect(results, ".csv")]
cdm_snapshot_files<-results[stringr::str_detect(results, "cdm_snapshot")]
cdm_snapshot <- list()
for(i in seq_along(cdm_snapshot_files)){
  cdm_snapshot[[i]]<-readr::read_csv(cdm_snapshot_files[[i]], 
                                     show_col_types = FALSE) %>% 
    select("cdm_name", "person_count", "observation_period_count" ,
           "vocabulary_version")
}
cdm_snapshot <- dplyr::bind_rows(cdm_snapshot)
cdm_snapshot <- cdm_snapshot %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version")

# cohort_count -----
cohort_count_files<-results[stringr::str_detect(results, ".csv")]
cohort_count_files<-results[stringr::str_detect(results, "cohort_count")]
cohort_count <- list()
for(i in seq_along(cohort_count_files)){
  cohort_count[[i]]<-readr::read_csv(cohort_count_files[[i]], 
                                 show_col_types = FALSE) 
}
cohort_count <- dplyr::bind_rows(cohort_count)


# cohort_intersection -----
cohort_intersection_files<-results[stringr::str_detect(results, ".csv")]
cohort_intersection_files<-results[stringr::str_detect(results, "cohort_intersection")]
cohort_intersection <- list()
for(i in seq_along(cohort_intersection_files)){
  cohort_intersection[[i]]<-readr::read_csv(cohort_intersection_files[[i]], 
                                     show_col_types = FALSE) 
}
cohort_intersection <- dplyr::bind_rows(cohort_intersection)

# index_codes -----
index_codes_files<-results[stringr::str_detect(results, ".csv")]
index_codes_files<-results[stringr::str_detect(results, "index_codes")]
index_codes <- list()
for(i in seq_along(index_codes_files)){
  index_codes[[i]]<-readr::read_csv(index_codes_files[[i]], 
                                    show_col_types = FALSE) 
}
if(length(index_codes_files) >=1){
  index_codes <- dplyr::bind_rows(index_codes) %>% 
    filter(!is.na(estimate)) 
}


# rectal prolapse patient_characteristics -----
rp_patient_characteristics_files<-results[stringr::str_detect(results, ".csv")]
rp_patient_characteristics_files<-results[stringr::str_detect(results, "rectal_prolapse_patient_characteristics")]
rp_patient_characteristics <- list()
for(i in seq_along(rp_patient_characteristics_files)){
  rp_patient_characteristics[[i]]<-readr::read_csv(rp_patient_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_patient_characteristics <- dplyr::bind_rows(rp_patient_characteristics)


# rectal prolapse large_scale_characteristics -----
rp_large_scale_characteristics_files<-results[stringr::str_detect(results, ".csv")]
rp_large_scale_characteristics_files<-results[stringr::str_detect(results, "rectal_prolapse_large_scale_characteristics")]
rp_large_scale_characteristics <- list()
for(i in seq_along(rp_large_scale_characteristics_files)){
  rp_large_scale_characteristics[[i]]<-readr::read_csv(rp_large_scale_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_large_scale_characteristics <- dplyr::bind_rows(rp_large_scale_characteristics)




# rectal prolapse incidence -----
rp_incidence_files<-results[stringr::str_detect(results, ".csv")]
rp_incidence_files<-rp_incidence_files[stringr::str_detect(rp_incidence_files, "rectal_prolapse_incidence")]
rp_incidence_files<-rp_incidence_files[stringr::str_detect(rp_incidence_files, "attrition", negate = TRUE)]
rp_incidence <- list()
for(i in seq_along(rp_incidence_files)){
  rp_incidence[[i]]<-readr::read_csv(rp_incidence_files[[i]], 
                                                   show_col_types = FALSE) 
}
rp_incidence <- dplyr::bind_rows(rp_incidence)



# rectal prolapse prevalence -----
rp_prevalence_files<-results[stringr::str_detect(results, ".csv")]
rp_prevalence_files<-rp_prevalence_files[stringr::str_detect(rp_prevalence_files, "rectal_prolapse_prevalence")]
rp_prevalence_files<-rp_prevalence_files[stringr::str_detect(rp_prevalence_files, "attrition", negate = TRUE)]
rp_prevalence <- list()
for(i in seq_along(rp_prevalence_files)){
  rp_prevalence[[i]]<-readr::read_csv(rp_prevalence_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_prevalence <- dplyr::bind_rows(rp_prevalence)



# rectopexy patient_characteristics -----
rt_patient_characteristics_files<-results[stringr::str_detect(results, ".csv")]
rt_patient_characteristics_files<-results[stringr::str_detect(results, "rectopexy_patient_characteristics")]
rt_patient_characteristics <- list()
for(i in seq_along(rt_patient_characteristics_files)){
  rt_patient_characteristics[[i]]<-readr::read_csv(rt_patient_characteristics_files[[i]], 
                                                   show_col_types = FALSE) 
}
rt_patient_characteristics <- dplyr::bind_rows(rt_patient_characteristics)


# rectopexy large_scale_characteristics index -----
rt_large_scale_characteristics_files_index<-results[stringr::str_detect(results, "rectopexy_large_scale_characteristics_index")]
rt_large_scale_characteristics_index <- list()
for(i in seq_along(rt_large_scale_characteristics_files_index)){
  rt_large_scale_characteristics_index[[i]]<-readr::read_csv(rt_large_scale_characteristics_files_index[[i]], 
                                                       show_col_types = FALSE) 
}
rt_large_scale_characteristics_index <- dplyr::bind_rows(rt_large_scale_characteristics_index)

# rectopexy large_scale_characteristics post -----
rt_large_scale_characteristics_files_post<-results[stringr::str_detect(results, "rectopexy_large_scale_characteristics_post")]
rt_large_scale_characteristics_post <- list()
for(i in seq_along(rt_large_scale_characteristics_files_post)){
  rt_large_scale_characteristics_post[[i]]<-readr::read_csv(rt_large_scale_characteristics_files_post[[i]], 
                                                             show_col_types = FALSE) 
}
rt_large_scale_characteristics_post <- dplyr::bind_rows(rt_large_scale_characteristics_post)


# rectopexy incidence -----
rt_incidence_files<-results[stringr::str_detect(results, ".csv")]
rt_incidence_files<-rt_incidence_files[stringr::str_detect(rt_incidence_files, "rectopexy_incidence")]
rt_incidence_files<-rt_incidence_files[stringr::str_detect(rt_incidence_files, "attrition", negate = TRUE)]
rt_incidence <- list()
for(i in seq_along(rt_incidence_files)){
  rt_incidence[[i]]<-readr::read_csv(rt_incidence_files[[i]], 
                                     show_col_types = FALSE) 
}
rt_incidence <- dplyr::bind_rows(rt_incidence)





# rectopexy complications: survival_estimates ----
rt_survival_estimates_files<-results[stringr::str_detect(results, ".csv")]
rt_survival_estimates_files<-rt_survival_estimates_files[stringr::str_detect(rt_survival_estimates_files, "rectopexy_survival_estimates")]
rt_survival_estimates <- list()
for(i in seq_along(rt_survival_estimates_files)){
  rt_survival_estimates[[i]]<-readr::read_csv(rt_survival_estimates_files[[i]], 
                                     show_col_types = FALSE) 
}
rt_survival_estimates <- dplyr::bind_rows(rt_survival_estimates)
# rectopexy complications: survival_events ----
rt_survival_events_files<-results[stringr::str_detect(results, ".csv")]
rt_survival_events_files<-rt_survival_events_files[stringr::str_detect(rt_survival_events_files, "rectopexy_survival_events")]
rt_survival_events <- list()
for(i in seq_along(rt_survival_events_files)){
  rt_survival_events[[i]]<-readr::read_csv(rt_survival_events_files[[i]], 
                                              show_col_types = FALSE) 
}
rt_survival_events <- dplyr::bind_rows(rt_survival_events)
# rectopexy complications: survival_summary ----
rt_survival_summary_files<-results[stringr::str_detect(results, ".csv")]
rt_survival_summary_files<-rt_survival_summary_files[stringr::str_detect(rt_survival_summary_files, "rectopexy_survival_summary")]
rt_survival_summary <- list()
for(i in seq_along(rt_survival_summary_files)){
  rt_survival_summary[[i]]<-readr::read_csv(rt_survival_summary_files[[i]], 
                                           show_col_types = FALSE) 
}
rt_survival_summary <- dplyr::bind_rows(rt_survival_summary)

# combined survival results: 90 day -----
rt_90_day <-rt_survival_summary %>%
  filter(estimate_type == "Survival summary") %>% 
  filter(variable_type %in%  c("number_records", "events")) %>% 
  pivot_wider(names_from = variable_type,
              values_from = estimate) %>% 
  select(!c("group_name", "estimate_type", "analysis_type",
            "result_type", "variable", "outcome")) %>% 
  left_join(rt_survival_estimates %>%  
               filter(time == 90) %>% 
  filter(estimate_type == "Cumulative failure probability") %>% 
  pivot_wider(names_from = variable_type,
              values_from = estimate)) 

rt_90_day<-rt_90_day %>% 
  mutate(cumulative_incidence = paste0(estimate*100, "% (",
                                       estimate_95CI_lower*100, "% to ",
                                       estimate_95CI_upper, "%)")) %>% 
  select(c("cdm_name","group_level","strata_name","strata_level",       
           "variable_level", "number_records", "events",    
           "cumulative_incidence"))

 





library(CohortSurvival)
rt_survival_estimates %>% 
  filter(strata_level == "Overall") %>% 
plotSurvival(facet = c("cdm_name", "group_level",
                       "outcome"), ribbon = FALSE,
             colour = c("strata_name",
             "strata_level"))
