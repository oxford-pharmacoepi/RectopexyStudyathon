# server shiny ----
server <- function(input, output, session) {

 ## cdm snapshot ----
  output$tbl_cdm_snaphot <- renderText(kable(cdm_snapshot) %>%
                                          kable_styling("striped", full_width = F) )
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(cdm_snapshot)
      gtsave(x, file)
    }
  )
  
  # code use ----
  getCodeUse <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    code_use <- code_use %>% 
      select(c("cdm_name", "codelist_name",
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_name",
               "source_concept_name",  "source_concept_id" ,   "domain_id",
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    names(code_use)<-stringr::str_replace_all(names(code_use), "_", " ")
    code_use
      
  })
  
  output$dt_code_use  <- DT::renderDataTable({
    table_data <- getCodeUse()
    
    datatable(table_data, 
              filter = "top",
              rownames= FALSE) 
  })

  # cohort_count ----
  get_cohort_count <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    working_cohort_count <- cohort_count  %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(cohort_name %in%  input$cd_cohort) %>% 
      select(cohort_name, cdm_name, number_records, number_subjects) %>% 
      pivot_wider(names_from = cdm_name, 
                  values_from = c(number_records, number_subjects),
                  names_glue = "{cdm_name}: {.value}",
                  names_vary = "slowest")
    if(isFALSE(input$cd_cc_records)){
      working_cohort_count<-working_cohort_count %>%
      select(!matches("number_record"))
    }
    
    if(isFALSE(input$cd_cc_subjects)) {
      working_cohort_count<-working_cohort_count %>%
        select(!matches("number_subj"))
    }
    
    working_cohort_count
  })
  
  output$dt_cohort_count  <- DT::renderDataTable({
    table_data <- get_cohort_count()
    
    datatable(table_data, rownames= FALSE) 
  })  
 
  # index_codes ----
  get_index_codes <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    index_codes <- index_codes %>% 
    filter(cdm_name %in% input$cd_cdm,
           cohort_name %in%  input$cd_cohort,
           group_name %in%  input$cd_index_group_name,
           strata_name %in%  input$cd_index_strata_name) %>% 
      select(c("cdm_name", "cohort_name" ,
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_id",
               "source_concept_name",  "source_concept_id" ,   "domain_id",
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    
    names(index_codes)<-stringr::str_replace_all(names(index_codes), "_", " ")
    
    index_codes
  })
  
  output$dt_index_codes  <- DT::renderDataTable({
    table_data <- get_index_codes()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  # cohort_intersection ----
  get_cohort_intersection <- reactive({
 
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    cohort_intersection <- cohort_intersection %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(cohort_name_1 %in%  input$cd_cohort) %>%   
      filter(cohort_name_2 %in%  input$cd_cohort) %>%
      select(!c("cohort_definition_id_1", 
               "cohort_definition_id_2"))
    names(cohort_intersection)<-stringr::str_replace_all(names(cohort_intersection), "_", " ")
    
    cohort_intersection
  })
  
  output$dt_cohort_intersection  <- DT::renderDataTable({
    table_data <- get_cohort_intersection()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  
  
  # patient_characteristics ----
  get_patient_characteristics <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    patient_characteristics <- patient_characteristics %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(group_level %in%  
               stringr::str_replace_all(
                 stringr::str_to_sentence(input$cd_cohort),
                 "_", " ")
               ) 
    patient_characteristics
  })
  
  output$gt_patient_characteristics  <- render_gt({
    PatientProfiles::gtCharacteristics(get_patient_characteristics())
  })   
  
  
  
  
  
  
  # large_scale_characteristics ----
  get_large_scale_characteristics <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    large_scale_characteristics <- large_scale_characteristics %>% 
      filter(cdm_name %in% input$cd_cdm,
             group_level %in%  input$cd_cohort,
             variable_level %in%  input$cd_index_time_window,
             table_name %in%  input$cd_lsc_domain) %>% 
      select(!c("result_type","group_name",
                "strata_name", "strata_level",
                "type", "analysis")) %>% 
      pivot_wider(names_from = estimate_type, 
                  values_from = estimate) %>% 
      rename("concept_id" = "concept",
             "concept_name" = "variable",
             "time_window" = "variable_level",
             "domain" = "table_name") %>% 
      relocate("time_window", .after = "domain") %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(group_level %in%  input$cd_cohort) %>% 
      mutate(percentage = round(percentage, 2)) %>% 
      mutate(count_percentage = paste0(count, " (", percentage, "%)"))
    names(large_scale_characteristics)<-stringr::str_replace_all(names(large_scale_characteristics), "_", " ")
    
    large_scale_characteristics
  })
  
  output$dt_large_scale_characteristics  <- DT::renderDataTable({
    table_data <- get_large_scale_characteristics()
    datatable(table_data, rownames= FALSE) 
  })   

  
  
  
}

