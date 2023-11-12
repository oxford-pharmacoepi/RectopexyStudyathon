# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "dbs",
        menuSubItem(
          text = "Database details",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Study diagnostics",
        tabName = "cd",
        menuSubItem(
          text = "Cohort diagnostics",
          tabName = "cohort_diagnostics"
        )
      ),
      menuItem(
        text = "Study results",
        tabName = "cd",
        menuSubItem(
          text = "Study results",
          tabName = "study_results"
        )
      )
)
),

  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
  # background  ------
      tabItem(
        tabName = "background",
        h3("Characterising rectal prolapse and rectopexy in the United Kingdom: population characteristics, incidence and surgical procedures"),
        tags$hr(),
        a(img(src="logo.png", align = "right",
              height="2%", width="20%"), href="https://www.ohdsi-europe.org/index.php/national-nodes/uk",
          target="_blank")
      ),
  # cdm snapshot ------
      tabItem(
        tabName = "cdm_snapshot",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_cdm_snaphot_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
      ),
  # cohort diagnostics -----
  tabItem(
    tabName = "cohort_diagnostics",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "cd_cdm",
        label = "Database",
        choices = sort(unique(cohort_count$cdm_name)),
        selected = sort(unique(cohort_count$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "cd_cohort",
        label = "Cohort",
        choices = sort(unique(cohort_count$cohort_name)),
        selected = sort(unique(cohort_count$cohort_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tags$style(HTML("
                  .tabbable > .nav > li > a {font-weight: bold; background-color: D3D4D8;  color:black}
                  ")),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Cohort counts",
        tags$hr(),
        prettySwitch(
          inputId = "cd_cc_subjects",
          label = "Number of subjects",
          fill = TRUE, 
          value = TRUE
        ),
        prettySwitch(
          inputId = "cd_cc_records",
          label = "Number of records",
          fill = TRUE, 
          value = TRUE
        ),
        tags$hr(),
        DT::dataTableOutput("dt_cohort_count") %>% 
          withSpinner()
      ),
      tabPanel(
        "Index codes",
        tags$hr(),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_index_group_name",
            label = "Group name",
            choices = sort(unique(index_codes$group_name)),
            selected = sort(unique(index_codes$group_name)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_index_strata_name",
            label = "Strata name",
            choices = sort(unique(index_codes$strata_name)),
            selected = sort(unique(index_codes$strata_name)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tags$hr(),
        DT::dataTableOutput("dt_index_codes") %>% 
          withSpinner()
      ),
      tabPanel(
        "Cohort demographics",
        tags$hr(),
        gt_output("gt_patient_characteristics") %>% 
          withSpinner()
      ),
      tabPanel(
        "Cohort large scale characteristics",
        tags$hr(),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_lsc_domain",
            label = "Domain",
            choices = sort(unique(large_scale_characteristics$table_name)),
            selected = sort(unique(large_scale_characteristics$table_name)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cd_index_time_window",
            label = "Time window",
            choices = sort(unique(large_scale_characteristics$variable_level)),
            selected = sort(unique(large_scale_characteristics$variable_level)),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tags$hr(),
        DT::dataTableOutput("dt_large_scale_characteristics") %>% 
          withSpinner()
      ),
      tabPanel(
        "Cohort intersection",
        tags$hr(),
        DT::dataTableOutput("dt_cohort_intersection") %>% 
          withSpinner(),
        tags$h5("Note, for cohort intersection only the first entry per cohort per individual is considered.")
      )
    )
  )
  
  
  
  # end -----
    )
  )
)


