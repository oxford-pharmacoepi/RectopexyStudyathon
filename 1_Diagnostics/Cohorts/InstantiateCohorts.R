
study_cs<- CodelistGenerator::codesFromConceptSet(path = here("Cohorts", "ConceptSets"),
                                       cdm = cdm)

# Summarise code use -------
code_use <- list()
cli::cli_text("- Getting code use in source")
for(i in seq_along(study_cs)){
  cli::cli_text("-- For {names(study_cs)[i]} ({i} of {length(study_cs)})")
  code_use[[i]] <- summariseCodeUse(study_cs[[i]],
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
    mutate(concept_set = names(study_cs)[i])
}
code_use <- bind_rows(code_use)
write_csv(code_use,
          here("Results", paste0(
            "code_use_", cdmName(cdm), ".csv"
          )))

# Instantiate rectal prolapse cohorts-------
rp_cs <- list(rectal_prolapse_narrow_cs= study_cs[["rectal_prolapse_narrow_cs"]],
              rectal_prolapse_broad_cs= study_cs[["rectal_prolapse_broad_cs"]])
cdm <-  generateConceptCohortSet(cdm,
                                 conceptSet = rp_cs,
                                 limit = "all",
                                 end = 1,
                                 name = "rectal_prolapse",
                                 overwrite = TRUE)

# keep first ever
cdm$rectal_prolapse <- cdm$rectal_prolapse %>%
  mutate()

# keep if in study period

# first ever occurrence -----

# within study period -----
