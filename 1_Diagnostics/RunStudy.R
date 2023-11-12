# start -----
start_time <- Sys.time()
# cdm reference ----
cli::cli_text("- Creating CDM reference ({Sys.time()})")
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_schema,
                                  write_schema = c(schema = write_schema,
                                                   prefix = study_prefix),
                                  cdm_name = db_name)


# cdm snapshot ----
cli::cli_text("- Getting cdm snapshot ({Sys.time()})")
write_csv(snapshot(cdm), here("Results", paste0(
  "cdm_snapshot_", cdmName(cdm), ".csv"
)))

# import concepts ------
cli::cli_text("- Importing concepts ({Sys.time()})")
study_cs <- codesFromConceptSet(
  path = here("ConceptSets"),
  cdm = cdm)

# instantiate concept cohorts -------
cli::cli_text("- Instantiating concept based cohorts ({Sys.time()})")
cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                conceptSet = study_cs,
                                limit = "all",
                                end = 1,
                                name = "study_cohorts",
                                overwrite = TRUE)

# cohort counts ----
cli::cli_text("- Instantiating cohorts ({Sys.time()})")
rp_cohort_counts <- cohort_count(cdm[["study_cohorts"]]) %>%
  left_join(cohort_set(cdm[["study_cohorts"]]),
            by = "cohort_definition_id") %>%
  mutate(cdm_name = db_name)
write_csv(rp_cohort_counts,
          here("Results", paste0(
            "cohort_count_", cdmName(cdm), ".csv"
          )))

# cohort intersection ----
cli::cli_text("- Getting cohort intersections ({Sys.time()})")

# add cohort names
cdm[["study_cohorts"]] <- cdm[["study_cohorts"]] %>%
  left_join(attr(cdm[["study_cohorts"]], "cohort_set") %>%
              select("cohort_definition_id", "cohort_name"),
            by = "cohort_definition_id") %>%
  computeQuery()

# for cohort timings we look at first cohort entry per person
cdm[["study_cohorts_first"]] <- cdm[["study_cohorts"]] %>%
  dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
  dplyr::filter(
    .data$cohort_start_date == min(.data$cohort_start_date,
                                   na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  computeQuery()

cohort_intersection <- cdm[["study_cohorts_first"]]  %>%
  inner_join(cdm[["study_cohorts"]],
             by = "subject_id") %>%
  select(subject_id,
         cohort_definition_id_1 = cohort_definition_id.x,
         cohort_definition_id_2 = cohort_definition_id.y,
         cohort_name_1 = cohort_name.x,
         cohort_name_2 = cohort_name.y) %>%
  distinct()  %>%
  group_by(cohort_definition_id_1, cohort_name_1,
           cohort_definition_id_2, cohort_name_2) %>%
  tally(name = "intersect_count") %>%
  mutate(intersect_count = as.integer(intersect_count)) %>%
  collect() %>%
  arrange(cohort_definition_id_1) %>%
  mutate(cdm_name = db_name)

# adding code timings
cdm$study_cohorts_timings <- cdm[["study_cohorts_first"]] %>%
  inner_join(cdm[["study_cohorts_first"]],
             by = "subject_id") %>%
  computeQuery() %>%
  rename("cohort_start_date_x" = "cohort_start_date.x",
         "cohort_name_x" = "cohort_name.x",
         "cohort_start_date_y" = "cohort_start_date.y",
         "cohort_name_y" = "cohort_name.y") %>%
  mutate(diff_days = !!datediff("cohort_start_date_x",
                               "cohort_start_date_y",
                                interval = "day")) %>%
  computeQuery()

cohort_timings <- cdm[["study_cohorts_timings"]] %>%
  mutate(comparison = paste0(cohort_definition_id.x, ";",
                             cohort_name_x, ";",
                             cohort_definition_id.y, ";",
                             cohort_name_y)) %>%
  PatientProfiles::summariseResult(group=list("comparison"),
                                   variables = list(diff_days="diff_days"),
                                   functions = list(diff_days=c("min", "q25",
                                                               "median","q75",
                                                               "max"))) %>%
  mutate(cdm_name = db_name)

cohort_timings <- cohort_timings %>%
  filter(variable == "diff_days") %>%
  tidyr::separate(group_level,
                  into = c("cohort_definition_id_1",
                           "cohort_name_1",
                           "cohort_definition_id_2",
                           "cohort_name_2"),
                  sep = ";") %>%
  select("cdm_name",
         "cohort_definition_id_1",
         "cohort_name_1",
         "cohort_definition_id_2",
         "cohort_name_2",
         "estimate_type",
         "estimate") %>%
  pivot_wider(names_from = estimate_type,
              values_from = estimate,
              names_glue = "{estimate_type}_days")

# join intersections and timings
cohort_intersection <- cohort_intersection %>%
  mutate(cohort_definition_id_1 = as.integer(cohort_definition_id_1),
         cohort_definition_id_2 = as.integer(cohort_definition_id_2)) %>%
  left_join(cohort_timings %>%
              mutate(cohort_definition_id_1 = as.integer(cohort_definition_id_1),
                     cohort_definition_id_2 = as.integer(cohort_definition_id_2)))
write_csv(cohort_intersection %>%
            filter(intersect_count >= 5),
          here("Results", paste0(
            "cohort_intersection_", cdmName(cdm), ".csv"
          )))

# index events  ----
cli::cli_text("- Getting index event codes ({Sys.time()})")
index_codes<- list()
non_empty_cohorts <- sort(cohort_count(cdm[["study_cohorts"]]) %>%
  filter(number_records > 0) %>%
  pull("cohort_definition_id"))

for(i in seq_along(non_empty_cohorts)){
  working_cohort_id <- non_empty_cohorts[i]
  working_cohort <- cohort_set(cdm[["study_cohorts"]]) %>%
    filter(cohort_definition_id == working_cohort_id) %>%
    pull("cohort_name")
  cli::cli_text("-- For {working_cohort} ({i} of {length(non_empty_cohorts)})")

  index_codes[[i]] <- summariseCohortCodeUse(study_cs[working_cohort] ,
                                             cohortTable = "study_cohorts",
                                             cohortId = working_cohort_id,
                                             timing = "entry",
                                             cdm = cdm,
                                             byYear = TRUE,
                                             bySex = TRUE,
                                             ageGroup = list(c(0,17),
                                                             c(18,24),
                                                             c(25,34),
                                                             c(35,44),
                                                             c(45,54),
                                                             c(55,64),
                                                             c(65,74),
                                                             c(75,150))) %>%
    mutate(cohort_name = working_cohort)

}
index_codes <- bind_rows(index_codes) %>%
  mutate(cdm_name = db_name)
write_csv(index_codes,
          here("Results", paste0(
            "index_codes_", cdmName(cdm), ".csv"
          )))

# cohort characteristics ----
cli::cli_text("- Getting patient characteristics ({Sys.time()})")
chars <- PatientProfiles::summariseCharacteristics(cdm$study_cohorts,
                                                   ageGroup = list(c(0,17),
                                                                   c(18,24),
                                                                   c(25,34),
                                                                   c(35,44),
                                                                   c(45,54),
                                                                   c(55,64),
                                                                   c(65,74),
                                                                   c(75,150)))
write_csv(chars,
          here("Results", paste0(
            "patient_characteristics_", cdmName(cdm), ".csv"
          )))

# large scale characteristics ----
cli::cli_text("- Getting large scale characteristics ({Sys.time()})")
lsc <- PatientProfiles::summariseLargeScaleCharacteristics(cdm$study_cohorts,
                                          eventInWindow = c("drug_exposure",
                                                            "condition_occurrence",
                                                            "procedure_occurrence",
                                                            "device_exposure"))
write_csv(lsc,
          here("Results", paste0(
            "large_scale_characteristics_", cdmName(cdm), ".csv"
          )))

# zip all results -----
cli::cli_text("- Zipping results ({Sys.time()})")
files_to_zip <- list.files(here("Results"))
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here("Results"), "/Results_", db_name, ".zip"
)),
files = files_to_zip,
root = here("Results"))

dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
cli::cli_alert_success("Cohort diagnostics finished")
cli::cli_alert_success(glue::glue(
  "Diagnostics run in {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
))
