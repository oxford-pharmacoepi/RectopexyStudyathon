start_time <- Sys.time()
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

# import concepts ------
cli::cli_text("- Importing concepts")
study_cs <- codesFromConceptSet(
  path = here("Cohorts", "ConceptSets"),
  cdm = cdm)

# summarise code use -------
cli::cli_text("- Getting code use in database")
code_use <- summariseCodeUse(study_cs,
                             cdm = cdm,
                             byYear = TRUE,
                             bySex = TRUE,
                             ageGroup = list(c(0,17),
                                             c(18,24),
                                             c(25,34),
                                             c(35,44),
                                             c(45,54),
                                             c(65,74),
                                             c(75,150))) %>%
  mutate(cdm_name = db_name)

write_csv(code_use,
          here("Results", paste0(
            "code_use_", cdmName(cdm), ".csv"
          )))

# instantiate concept cohorts -------
cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                conceptSet = study_cs,
                                limit = "all",
                                end = 1,
                                name = "study_cohorts",
                                overwrite = TRUE)

# cohort counts ----
cli::cli_text("- Instantiating cohorts")
rp_cohort_counts <- cohort_count(cdm[["study_cohorts"]]) %>%
  left_join(cohort_set(cdm[["study_cohorts"]]),
            by = "cohort_definition_id") %>%
  mutate(cdm_name = db_name)
write_csv(rp_cohort_counts,
          here("Results", paste0(
            "cohort_count_", cdmName(cdm), ".csv"
          )))

# cohort intersection ----
cdm[["study_cohorts"]] <- cdm[["study_cohorts"]] %>%
  left_join(attr(cdm[["study_cohorts"]], "cohort_set") %>%
              select("cohort_definition_id", "cohort_name"),
            by = "cohort_definition_id") %>%
  computeQuery()

cohort_intersection <- cdm[["study_cohorts"]]  %>%
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
write_csv(cohort_intersection,
          here("Results", paste0(
            "cohort_intersection_", cdmName(cdm), ".csv"
          )))

# cohort timings ----


# index events  ----
cli::cli_text("- Getting index event codes")
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
cli::cli_text("- Getting patient characteristics")
chars <- summariseCharacteristics(cdm$study_cohorts)
write_csv(chars,
          here("Results", paste0(
            "patient_characteristics_", cdmName(cdm), ".csv"
          )))

# large scale characteristics ----
cli::cli_text("- Getting large scale characteristics")
lsc <- summariseLargeScaleCharacteristics(cdm$study_cohorts,
                                          eventInWindow = c("drug_exposure",
                                                            "condition_occurrence",
                                                            "observation",
                                                            "measurement",
                                                            "procedure_occurrence",
                                                            "visit_occurrence"))
write_csv(lsc,
          here("Results", paste0(
            "large_scale_characteristics_", cdmName(cdm), ".csv"
          )))

# period prevalence in 100k sample of database ----
cli::cli_text("- Getting period prevalence")
cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
                                    cohortDateRange = as.Date(c("2000-01-01", NA)),
                                    sex = c("Both", "Male", "Female"),
                                    ageGroup =  list(c(0,17),
                                                     c(18,24),
                                                     c(25,34),
                                                     c(35,44),
                                                     c(45,54),
                                                     c(65,74),
                                                     c(75,150)),
                                    requirementInteractions = FALSE)
period_prev <- estimatePeriodPrevalence(cdm,
                         denominatorTable = "denominator",
                         outcomeTable = "study_cohorts")

write_csv(period_prev,
          here("Results", paste0(
            "period_prev_", cdmName(cdm), ".csv"
          )))

# zip all results -----
cli::cli_text("- Zipping results")
files_to_zip <- list.files(here("Results"))
files_to_zip <- files_to_zip[str_detect(files_to_zip, "readme.md", negate = TRUE)]

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
