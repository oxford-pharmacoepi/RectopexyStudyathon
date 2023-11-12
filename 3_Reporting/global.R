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
library(CohortSurvival)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
# library(TreatmentPatterns)
library(ggplot2)

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
index_codes <- dplyr::bind_rows(index_codes)

# patient_characteristics -----
patient_characteristics_files<-results[stringr::str_detect(results, ".csv")]
patient_characteristics_files<-results[stringr::str_detect(results, "patient_characteristics")]
patient_characteristics <- list()
for(i in seq_along(patient_characteristics_files)){
  patient_characteristics[[i]]<-readr::read_csv(patient_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
patient_characteristics <- dplyr::bind_rows(patient_characteristics)


# large_scale_characteristics -----
large_scale_characteristics_files<-results[stringr::str_detect(results, ".csv")]
large_scale_characteristics_files<-results[stringr::str_detect(results, "large_scale_characteristics")]
large_scale_characteristics <- list()
for(i in seq_along(large_scale_characteristics_files)){
  large_scale_characteristics[[i]]<-readr::read_csv(large_scale_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
large_scale_characteristics <- dplyr::bind_rows(large_scale_characteristics)







