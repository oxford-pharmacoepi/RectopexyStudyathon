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
write_csv(snapshot(cdm), here("results", paste0(
  "cdm_snapshot_", cdmName(cdm), ".csv"
)))

# import concepts ------
cli::cli_text("- Importing concepts ({Sys.time()})")
study_cs <- codesFromConceptSet(
  path = here("ConceptSets"),
  cdm = cdm)

#placeholder cohorts - add a cohort that will exist in 100k to test code
study_cs[["rectal_prolapse_placeholder_for_testing"]] <-  c(317009, 201826)
study_cs[["rectopexy_placeholder_for_testing"]] <- c(317009, 201826)


study_cs[["surgical_infection"]] <- c(42538804,43530818,43530819,43530820,4334801,
                        42536181,4237450,4145549,4308542,4308837,
                        437474,44782822,2101889)

study_cs[["pain"]] <- c(43530621,439502,43531612,761087,4063577,76458,4058670,
          4189790,200219,45757641,442764,4033938,4145869)

study_cs[["diarrhoea_broad"]] <- c(37016247,196523,4012367,4031502,4048884,4057829,4091519,4104544,
                     4105136,4121590,4145808,4134607,4170302,4249551,4261727,4318832,
                     4318833,4320650,4340521,4341247,4301078,4142055)

study_cs[["diarrhoea_narrow"]]  <- c(4057829, 4121590)

study_cs[["constipation"]]  <- c(45757425,45757556,75860,79061,201905,4026011,4057824,
                  4070750,4135220,4218933,4238495,4328435,4333213,4333890,
                  4341090,4340520,4008552,40757904,3001535,3017847,3045964,
                  4305269,4030428)

# study_cs[["haemorrhage_broad"]] <- c(36712677,37017589,40488840,44806340,437312,443452,4189790,
#                        4212456,4245614,4026112,197925,4096781,443530,4310668,4338544,
#                        36717237,44784367,44784432,46273183,192671,197925,4002836,
#                        4028772,4028773,4227086,4322061,4341790,4342904,4343234,
#                        4179955,4209886,4311808,43021552,46271322,4078223,4080814,
#                        4149197,4168088,4201120,4303294,44782862,44784306,2109110,
#                        2109176,2109184,2109198,2109267)

study_cs[["haemorrhage_narrow"]] <- c(4189790,4212456,4245614,197925,4096781,
                        197925,4002836,4343234,46271322)


study_cs[["obstruction_broad"]] <- c(192953,193518,4026000,4026001,4129389,4299644,4340367,
                       4340932,4342762,4342765,36716701,4057365,4057809,
                       4090128,4090129,4249110,4340928,35624282,
                       35624283,192357,36716690,37209651,37309629)


study_cs[["obstruction_narrow"]] <- c(4299644,4340367,4340932,4342762,4342765,36716701,
                        4057809,4090128,4249110,4340928,35624282,192357,
                        37209651)

# instantiate concept cohorts -------
cli::cli_text("- Instantiating concept based cohorts ({Sys.time()})")
cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                conceptSet = study_cs,
                                limit = "all",
                                end = 1,
                                name = "study_cohorts",
                                overwrite = TRUE)

# subset cdm to cohorts ------
cli::cli_text("- Subsetting cdm to DUS cohorts ({Sys.time()})")
id_to_subset <- cohort_set(cdm$study_cohorts) %>%
  filter(cohort_name %in% c("rectal_prolapse_broad_cs",
                            "rectal_prolapse_placeholder_for_testing",
                            "rectopexy_broad_cs",
                            "rectopexy_placeholder_for_testing")) %>%
  pull(cohort_definition_id)

cdm <- cdm_subset_cohort(
  cdm = cdm,
  cohort_table = "study_cohorts",
  cohort_id = id_to_subset
)
cdm$person <- cdm$person %>% computeQuery()
cdm$observation_period <- cdm$observation_period %>% computeQuery()
cdm$condition_occurrence <- cdm$condition_occurrence %>% computeQuery()
cdm$drug_exposure <- cdm$drug_exposure %>% computeQuery()
cdm$procedure_occurrence <- cdm$procedure_occurrence %>% computeQuery()
cdm$device_exposure <- cdm$device_exposure %>% computeQuery()

# cohort counts ----
cli::cli_text("- Instantiating cohorts ({Sys.time()})")
rp_cohort_counts <- cohort_count(cdm[["study_cohorts"]]) %>%
  left_join(cohort_set(cdm[["study_cohorts"]]),
            by = "cohort_definition_id") %>%
  mutate(cdm_name = db_name)
write_csv(rp_cohort_counts,
          here("results", paste0(
            "cohort_count_", cdmName(cdm), ".csv"
          )))

# cohort intersection ----
cli::cli_text("- Getting cohort intersections ({Sys.time()})")

# add cohort names
cdm[["study_cohorts"]] <- cdm[["study_cohorts"]] %>%
  left_join(attr(cdm[["study_cohorts"]], "cohort_set") %>%
              select("cohort_definition_id", "cohort_name"),
            by = "cohort_definition_id") %>%
  CDMConnector::computeQuery(
    name = "study_cohorts_names",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

# for cohort timings we look at first cohort entry per person
cdm[["study_cohorts_first"]] <- cdm[["study_cohorts"]] %>%
  dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
  dplyr::filter(
    .data$cohort_start_date == min(.data$cohort_start_date,
                                   na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  CDMConnector::computeQuery(
    name = "study_cohorts_first",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

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
  rename("cohort_start_date_x" = "cohort_start_date.x",
         "cohort_name_x" = "cohort_name.x",
         "cohort_definition_id_x" = "cohort_definition_id.x",
         "cohort_start_date_y" = "cohort_start_date.y",
         "cohort_name_y" = "cohort_name.y",
         "cohort_definition_id_y" = "cohort_definition_id.y") %>%
  mutate(diff_days = !!datediff("cohort_start_date_x",
                                "cohort_start_date_y",
                                interval = "day")) %>%
  CDMConnector::computeQuery(
    name = "study_cohorts_first_timings",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

cdm$study_cohorts_timings <- cdm$study_cohorts_timings %>%
  mutate(comparison = as.character(paste0(as.character(cohort_definition_id_x),
                             as.character(";"),
                             as.character(cohort_name_x),
                             as.character(";"),
                             as.character(cohort_definition_id_y),
                             as.character(";"),
                             as.character(cohort_name_y)))) %>%
  CDMConnector::computeQuery(
    name = "study_cohorts_first_timings2",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

cohort_timings <- cdm[["study_cohorts_timings"]] %>%
  collect() %>%
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
                     cohort_definition_id_2 = as.integer(cohort_definition_id_2)),
            by = c("cohort_definition_id_1", "cohort_name_1", "cohort_definition_id_2",
                         "cohort_name_2", "cdm_name"))
write_csv(cohort_intersection %>%
            filter(intersect_count >= 5),
          here("results", paste0(
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
          here("results", paste0(
            "index_codes_", cdmName(cdm), ".csv"
          )))

# rectal prolapse cohort -----
cli::cli_text("- Instantiating rectal prolapse cohort ({Sys.time()})")

study_cs_rp <- list("rectal_prolapse_broad_cs" = study_cs[["rectal_prolapse_broad_cs"]],
                    "rectal_prolapse_narrow_cs" = study_cs[["rectal_prolapse_narrow_cs"]],
                    "rectal_prolapse_placeholder_for_testing" = study_cs[["rectal_prolapse_placeholder_for_testing"]])

cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                              conceptSet = study_cs_rp,
                                              limit = "first",
                                              end = "observation_period_end_date",
                                              name = "study_cohorts_rp",
                                              overwrite = TRUE)

# rectal prolapse: incidence and prevalence -----
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator",
                                    ageGroup = list(c(18,150),
                                                    c(18,24),
                                                    c(25,34),
                                                    c(35,44),
                                                    c(45,54),
                                                    c(55,64),
                                                    c(65,74),
                                                    c(75,150)),
                                    cohortDateRange = as.Date(c("2013-01-01",
                                                                "2022-12-31")),
                                    sex = c("Both", "Male", "Female"),
                                    daysPriorObservation = c(0, 365),
                                    overwrite = TRUE)

rp_inc_gpop <- estimateIncidence(cdm,
                              denominatorTable = "denominator",
                              outcomeTable = "study_cohorts_rp",
                              interval = "years",
                              completeDatabaseIntervals = TRUE)

write_csv(rp_inc_gpop,
          here("results", paste0(
            "rectal_prolapse_incidence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(incidenceAttrition(rp_inc_gpop),
          here("results", paste0(
            "rectal_prolapse_incidence_attrition_general_population_", cdmName(cdm), ".csv"
          )))

prev_gpop <- estimatePeriodPrevalence(cdm,
                                      denominatorTable = "denominator",
                                      outcomeTable = "study_cohorts_rp",
                                      interval = "years",
                                      completeDatabaseIntervals = TRUE,
                                      fullContribution = TRUE)
write_csv(prev_gpop,
          here("results", paste0(
            "rectal_prolapse_prevalence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(prevalenceAttrition(prev_gpop),
          here("results", paste0(
            "rectal_prolapse_prevalence_attrition_general_population_", cdmName(cdm), ".csv"
          )))

# rectal prolapse: cohort characteristics ----
cli::cli_text("- Getting patient characteristics for rectal prolapse ({Sys.time()})")
cdm$study_cohorts_rp <- cdm$study_cohorts_rp %>%
  filter(cohort_start_date >= "2013-01-01",
         cohort_end_date <= "2022-12-31") %>%
  record_cohort_attrition("Within study period")
cdm$study_cohorts_rp <- cdm$study_cohorts_rp %>%
  addAge() %>%
  filter(age >= 18) %>%
  record_cohort_attrition("Age 18 or over")

rp_chars <- PatientProfiles::summariseCharacteristics(cdm$study_cohorts_rp,
                                                   ageGroup = list(c(18,24),
                                                                   c(25,34),
                                                                   c(35,44),
                                                                   c(45,54),
                                                                   c(55,64),
                                                                   c(65,74),
                                                                   c(75,150)))
write_csv(rp_chars,
          here("results", paste0(
            "rectal_prolapse_patient_characteristics_", cdmName(cdm), ".csv"
          )))

# rectal prolapse: large scale characteristics ----
cli::cli_text("- Getting large scale characteristics for rectal prolapse ({Sys.time()})")
rp_lsc <- PatientProfiles::summariseLargeScaleCharacteristics(cdm$study_cohorts_rp,
                                          eventInWindow = c("condition_occurrence"),
                                          window = list(c(-Inf, 0),
                                                        c(0, 0)))
write_csv(rp_lsc,
          here("results", paste0(
            "rectal_prolapse_large_scale_characteristics_", cdmName(cdm), ".csv"
          )))

# rectopexy cohort ----
cli::cli_text("- Instantiating rectopexy cohort ({Sys.time()})")

study_cs_rt <- list("rectopexy_broad_cs" = study_cs[["rectopexy_broad_cs"]],
                    "rectopexy_narrow_cs" = study_cs[["rectopexy_narrow_cs"]],
                    "rectopexy_placeholder_for_testing" = study_cs[["rectopexy_placeholder_for_testing"]])

cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                              conceptSet = study_cs_rt,
                                              limit = "first",
                                              end = "observation_period_end_date",
                                              name = "study_cohorts_rt",
                                              overwrite = TRUE)

# rectopexy: incidence ----
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator",
                                    ageGroup = list(c(18,150),
                                                    c(18,24),
                                                    c(25,34),
                                                    c(35,44),
                                                    c(45,54),
                                                    c(55,64),
                                                    c(65,74),
                                                    c(75,150)),
                                    cohortDateRange = as.Date(c("2013-01-01",
                                                                "2022-12-31")),
                                    sex = c("Both", "Male", "Female"),
                                    daysPriorObservation = c(0, 365),
                                    overwrite = TRUE)

rt_inc_gpop <- estimateIncidence(cdm,
                                 denominatorTable = "denominator",
                                 outcomeTable = "study_cohorts_rt",
                                 interval = "years",
                                 completeDatabaseIntervals = TRUE)

write_csv(rt_inc_gpop,
          here("results", paste0(
            "rectopexy_incidence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(incidenceAttrition(rt_inc_gpop),
          here("results", paste0(
            "rectopexy_incidence_attrition_general_population_", cdmName(cdm), ".csv"
          )))


# rectopexy: cohort characteristics ----
cli::cli_text("- Getting patient characteristics for rectopexy ({Sys.time()})")
cdm$study_cohorts_rt <- cdm$study_cohorts_rt %>%
  filter(cohort_start_date >= "2013-01-01",
         cohort_end_date <= "2022-12-31") %>%
  record_cohort_attrition("Within study period")
cdm$study_cohorts_rt <- cdm$study_cohorts_rt %>%
  addAge() %>%
  filter(age >= 18) %>%
  record_cohort_attrition("Age 18 or over")

rt_chars <- PatientProfiles::summariseCharacteristics(cdm$study_cohorts_rt,
                                                      ageGroup = list(c(18,24),
                                                                      c(25,34),
                                                                      c(35,44),
                                                                      c(45,54),
                                                                      c(55,64),
                                                                      c(65,74),
                                                                      c(75,150)))
write_csv(rt_chars,
          here("results", paste0(
            "rectopexy_patient_characteristics_", cdmName(cdm), ".csv"
          )))


# rectopexy: large scale characteristics ----
cli::cli_text("- Getting large scale characteristics for rectal prolapse ({Sys.time()})")
rt_lsc <- PatientProfiles::summariseLargeScaleCharacteristics(cdm$study_cohorts_rt,
                                                              eventInWindow = c("procedure_occurrence",
                                                                                "condition_occurrence",
                                                                                "drug_exposure",
                                                                                "device_exposure"),
                                                              window = list(c(-Inf, -1),
                                                                            c(0, 0),
                                                                            c(1, 365)))
write_csv(rt_lsc,
          here("results", paste0(
            "rectopexy_large_scale_characteristics_", cdmName(cdm), ".csv"
          )))

# rectopexy: complications ----
surv_outcome_id <- cohort_set(cdm$study_cohorts) %>%
  filter(str_detect(cohort_name, "rectal", negate = TRUE)) %>%
  filter(str_detect(cohort_name, "rectopexy", negate = TRUE)) %>%
  filter(str_detect(cohort_name, "mesh", negate = TRUE)) %>%
  left_join(cohort_count(cdm$study_cohorts),
            by = "cohort_definition_id") %>%
  filter(number_records > 5) %>%
  pull(cohort_definition_id)

surv <- estimateSingleEventSurvival(cdm = cdm,
                            targetCohortTable = "study_cohorts_rp",
                            outcomeCohortTable = "study_cohorts",
                            outcomeCohortId = surv_outcome_id,
                            timeGap = 90,
                            followUpDays = 90)

write_csv(surv %>%
            dplyr::filter(time==90),
          here("results", paste0(
            "rectopexy_survival_estimates_", cdmName(cdm), ".csv"
          )))
write_csv(attr(surv, "events"),
          here("results", paste0(
            "rectopexy_survival_events_", cdmName(cdm), ".csv"
          )))
write_csv(attr(surv, "summary"),
          here("results", paste0(
            "rectopexy_survival_summary_", cdmName(cdm), ".csv"
          )))

# zip all results -----
cli::cli_text("- Zipping results ({Sys.time()})")
files_to_zip <- list.files(here("results"))
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here("results"), "/results_", db_name, ".zip"
)),
files = files_to_zip,
root = here("results"))

dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
cli::cli_alert_success("Cohort diagnostics finished")
cli::cli_alert_success(glue::glue(
  "Sturdy code ran in {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
))
