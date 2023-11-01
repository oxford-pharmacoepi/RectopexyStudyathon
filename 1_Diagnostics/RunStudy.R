# cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_schema,
                                  write_schema = c(schema = write_schema,
                                                   prefix = study_prefix),
                                  cdm_name = db_name)


# cdm snapshot ----
cli::cli_text("- Getting cdm snapshot")
write_csv(snapshot(cdm), here("Results", paste0(
  "cdm_snapshot_", cdmName(cdm), ".csv"
)))

# cohort generation ----
cli::cli_text("- Getting cohorts")
source(here("Cohorts", "InstantiateCohorts.R"))


# cohort counts ----
study_cohort_counts <- cohort_count(cdm[["study_cohorts"]]) %>% left_join(cohort_set(cdm[["study_cohorts"]]))
write_csv(study_cohort_counts,
          here("Results", paste0(
  "cohort_count_", cdmName(cdm), ".csv"
)))

# cohort overlap  ----
cohort_intersection <- cdm[["study_cohorts"]] %>%
  inner_join(cdm[["study_cohorts"]], by = "subject_id") %>%
  select(subject_id, cohort_definition_id_x = cohort_definition_id.x,
         cohort_definition_id_y = cohort_definition_id.y) %>%
  distinct()  %>%
  group_by(cohort_definition_id_x, cohort_definition_id_y) %>%
  summarize(intersect_count = as.integer(n())) %>%
  collect() %>%
  arrange(cohort_definition_id_x) %>%
  mutate(cdm_name = db_name)
write_csv(cohort_intersection,
          here("Results", paste0(
            "cohort_intersection_", cdmName(cdm), ".csv"
          )))

# codes in source  ----
cohort_codes <- codesFromCohort(path = here("Cohorts"), cdm = cdm)
code_use <- list()
cli::cli_text("- Getting code use in source")
for(i in seq_along(cohort_codes)){
cli::cli_text("-- For cohort {i} of {length(cohort_codes)}")
code_use[[i]] <- summariseCodeUse(cohort_codes[[i]],
                                  cdm = cdm,
                                  byYear = TRUE,
                                  bySex = TRUE,
                                  ageGroup = list(c(0,18),c(19,150))) %>%
  mutate(cohort_name = names(cohort_codes)[i])
}
code_use <- bind_rows(code_use)
write_csv(code_use,
          here("Results", paste0(
            "code_use_", cdmName(cdm), ".csv"
          )))

# index events  ----
cli::cli_text("- Getting index event codes")
index_codes<- list()

for(i in seq_along(cohort_codes)){
  cli::cli_text("-- For cohort {i} of {length(cohort_codes)}")
  index_codes[[i]] <- summariseCohortCodeUse(cohort_codes[[i]],
                                                           cohortTable = "study_cohorts",
                                                           timing = "entry",
                                                  cdm = cdm,
                                                  byYear = TRUE,
                                                  bySex = TRUE,
                                                  ageGroup = list(c(0,18),c(19,150))) %>%
    mutate(cohort_name = names(cohort_codes)[i])

}
index_codes <- bind_rows(index_codes)
write_csv(index_codes,
          here("Results", paste0(
            "index_codes_", cdmName(cdm), ".csv"
          )))

# end -----
cli::cli_alert_success("- Cohort diagnostics finished")
