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
        text = "Study results: rectal prolapse",
        tabName = "study_results",
        menuSubItem(
          text = "Population incidence",
          tabName = "rp_incidence"
        ),
        menuSubItem(
          text = "Population prevalence",
          tabName = "rp_prevalence"
        ),
        menuSubItem(
          text = "Patient characteristics",
          tabName = "rp_chars"
        )
      ),
      menuItem(
        text = "Study results: rectopexy",
        tabName = "study_results",
        menuSubItem(
          text = "Population incidence",
          tabName = "rt_incidence"
        ),
        menuSubItem(
          text = "Patient characteristics",
          tabName = "rt_chars"
        ),
        menuSubItem(
          text = "Post-operative complications",
          tabName = "rt_complications"
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
        "Cohort intersection",
        tags$hr(),
        DT::dataTableOutput("dt_cohort_intersection") %>% 
          withSpinner(),
        tags$h5("Note, for cohort intersection only the first entry per cohort per individual is considered.")
      )
    )
  ),
  # rp_incidence ----
  tabItem(
    tabName = "rp_incidence",
    h3("Incidence estimates"),
    p("Incidence estimates are shown below, please select configuration to filter them:"),
    p("Database and study outcome"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_cdm_name",
        label = "CDM name",
        choices = unique(rp_incidence$cdm_name),
        selected = unique(rp_incidence$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_outcome_cohort_name",
        label = "Outcome name",
        choices = unique(rp_incidence$outcome_cohort_name),
        selected = unique(rp_incidence$outcome_cohort_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Denominator population settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_denominator_age_group",
        label = "Age group",
        choices = unique(rp_incidence$denominator_age_group),
        selected = unique(rp_incidence$denominator_age_group)[1],
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_denominator_sex",
        label = "Sex",
        choices = unique(rp_incidence$denominator_sex),
        selected = "Both",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_denominator_days_prior_observation",
        label = "Days prior observation",
        choices = unique(rp_incidence$denominator_days_prior_observation),
        selected = unique(rp_incidence$denominator_days_prior_observation)[1],
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Analysis settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_analysis_outcome_washout",
        label = "Outcome washout",
        choices = unique(rp_incidence$analysis_outcome_washout),
        selected = unique(rp_incidence$analysis_outcome_washout),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_analysis_repeated_events",
        label = "Repeated events",
        choices = unique(rp_incidence$analysis_repeated_events),
        selected = unique(rp_incidence$analysis_repeated_events),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_analysis_complete_database_intervals",
        label = "Complete period",
        choices = unique(rp_incidence$analysis_complete_database_intervals),
        selected = unique(rp_incidence$analysis_complete_database_intervals),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Dates"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_analysis_interval",
        label = "Interval",
        choices = unique(rp_incidence$analysis_interval),
        selected = "years",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_incidence_estimates_incidence_start_date",
        label = "Incidence start date",
        choices = as.character(unique(rp_incidence$incidence_start_date)),
        selected = as.character(unique(rp_incidence$incidence_start_date)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table of estimates",
        downloadButton("rp_incidence_estimates_download_table", "Download current estimates"),
        DTOutput("rp_incidence_estimates_table") %>% withSpinner()
      ),
      tabPanel(
        "Plot of estimates",
        p("Plotting options"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_incidence_estimates_plot_x",
            label = "x axis",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = "incidence_start_date",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_incidence_estimates_plot_facet",
            label = "Facet by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = c("outcome_cohort_name"),
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_incidence_estimates_plot_colour",
            label = "Colour by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
            selected = "cdm_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        plotlyOutput(
          "rp_incidence_estimates_plot",
          height = "800px"
        ) %>%
          withSpinner(),
        h4("Download figure"),
        div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("rp_incidence_estimates_download_height", "", 10, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("rp_incidence_estimates_download_width", "", 20, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block; margin-right:",
          textInput("rp_incidence_estimates_download_dpi", "", 300, width = "50px")
        ),
        downloadButton("rp_incidence_estimates_download_plot", "Download plot")
      )
    )
  ),
  
  
  # rp_prevalence ----
  tabItem(
    tabName = "rp_prevalence",
    h3("rp_prevalence estimates"),
    p("rp_prevalence estimates are shown below, please select configuration to filter them:"),
    p("Database and study outcome"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_cdm_name",
        label = "CDM name",
        choices = unique(rp_prevalence$cdm_name),
        selected = unique(rp_prevalence$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_outcome_cohort_name",
        label = "Outcome name",
        choices = unique(rp_prevalence$outcome_cohort_name),
        selected = unique(rp_prevalence$outcome_cohort_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Denominator population settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_denominator_age_group",
        label = "Age group",
        choices = unique(rp_prevalence$denominator_age_group),
        selected = unique(rp_prevalence$denominator_age_group)[1],
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_denominator_sex",
        label = "Sex",
        choices = unique(rp_prevalence$denominator_sex),
        selected = "Both",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_denominator_days_prior_observation",
        label = "Days prior observation",
        choices = unique(rp_prevalence$denominator_days_prior_observation),
        selected = unique(rp_prevalence$denominator_days_prior_observation),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Analysis settings"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_analysis_type",
        label = "rp_prevalence type",
        choices = unique(rp_prevalence$analysis_type),
        selected = unique(rp_prevalence$analysis_type),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_analysis_time_point",
        label = "Time point",
        choices = unique(rp_prevalence$analysis_time_point),
        selected = unique(rp_prevalence$analysis_time_point),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_analysis_complete_database_intervals",
        label = "Complete period",
        choices = unique(rp_prevalence$analysis_complete_database_intervals),
        selected = unique(rp_prevalence$analysis_complete_database_intervals),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_analysis_full_contribution",
        label = "Full contribution",
        choices = unique(rp_prevalence$analysis_full_contribution),
        selected = unique(rp_prevalence$analysis_full_contribution),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    p("Dates"),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_analysis_interval",
        label = "Interval",
        choices = unique(rp_prevalence$analysis_interval),
        selected = "years",
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_prevalence_estimates_prevalence_start_date",
        label = "prevalence start date",
        choices = as.character(unique(rp_prevalence$prevalence_start_date)),
        selected = as.character(unique(rp_prevalence$prevalence_start_date)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table of estimates",
        downloadButton("rp_prevalence_estimates_download_table", "Download current estimates"),
        DTOutput("rp_prevalence_estimates_table") %>% withSpinner()
      ),
      tabPanel(
        "Plot of estimates",
        p("Plotting options"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_prevalence_estimates_plot_x",
            label = "x axis",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "analysis_type", "analysis_time_point", 
                        "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "prevalence_start_date",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_prevalence_estimates_plot_facet",
            label = "Facet by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_type", "analysis_outcome_lookback_days", "analysis_time_point", "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "outcome_cohort_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "rp_prevalence_estimates_plot_colour",
            label = "Colour by",
            choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_type", "analysis_outcome_lookback_days", "analysis_time_point", "analysis_complete_database_intervals", "analysis_full_contribution", "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"),
            selected = "cdm_name",
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        plotlyOutput(
          "rp_prevalence_estimates_plot",
          height = "800px"
        ) %>%
          withSpinner(),
        h4("Download figure"),
        div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("rp_prevalence_estimates_download_height", "", 10, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block;",
          textInput("rp_prevalence_estimates_download_width", "", 20, width = "50px")
        ),
        div("cm", style = "display: inline-block; margin-right: 25px;"),
        div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        div(
          style = "display: inline-block; margin-right:",
          textInput("rp_prevalence_estimates_download_dpi", "", 300, width = "50px")
        ),
        downloadButton("rp_prevalence_estimates_download_plot", "Download plot")
      )
    )
  ),
  
  # rp_chars -----
  tabItem(
    tabName = "rp_chars",
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_chars_cdm",
        label = "Database",
        choices = sort(unique(rp_patient_characteristics$cdm_name)),
        selected = sort(unique(rp_patient_characteristics$cdm_name)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = "rp_chars_cohort",
        label = "Cohort",
        choices = sort(unique(rp_patient_characteristics$group_level)),
        selected = sort(unique(rp_patient_characteristics$group_level)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
    )),
    tabsetPanel(
      type = "tabs",
      
  # rp_patient_characteristics
      
    tabPanel(
      "Cohort demographics",
      tags$hr(),
      gt_output("gt_rp_patient_characteristics") %>%
       withSpinner()
    ),
    tabPanel(
      "Cohort large scale characteristics",
      tags$hr(),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "rp_chars_lsc_domain",
          label = "Domain",
          choices = sort(unique(rp_large_scale_characteristics$table_name)),
          selected = sort(unique(rp_large_scale_characteristics$table_name)),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = TRUE
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "rp_chars_index_time_window",
          label = "Time window",
          choices = sort(unique(rp_large_scale_characteristics$variable_level)),
          selected = sort(unique(rp_large_scale_characteristics$variable_level)),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = TRUE
        )
      ),
      tags$hr(),
      DT::dataTableOutput("dt_rp_large_scale_characteristics") %>%
        withSpinner()

 )
    )
  ),
 
 # rt_incidence ----
 tabItem(
   tabName = "rt_incidence",
   h3("Incidence estimates"),
   p("Incidence estimates are shown below, please select configuration to filter them:"),
   p("Database and study outcome"),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_cdm_name",
       label = "CDM name",
       choices = unique(rt_incidence$cdm_name),
       selected = unique(rt_incidence$cdm_name),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_outcome_cohort_name",
       label = "Outcome name",
       choices = unique(rt_incidence$outcome_cohort_name),
       selected = unique(rt_incidence$outcome_cohort_name),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   p("Denominator population settings"),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_denominator_age_group",
       label = "Age group",
       choices = unique(rt_incidence$denominator_age_group),
       selected = unique(rt_incidence$denominator_age_group)[1],
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_denominator_sex",
       label = "Sex",
       choices = unique(rt_incidence$denominator_sex),
       selected = "Both",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_denominator_days_prior_observation",
       label = "Days prior observation",
       choices = unique(rt_incidence$denominator_days_prior_observation),
       selected = unique(rt_incidence$denominator_days_prior_observation)[1],
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   p("Analysis settings"),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_analysis_outcome_washout",
       label = "Outcome washout",
       choices = unique(rt_incidence$analysis_outcome_washout),
       selected = unique(rt_incidence$analysis_outcome_washout),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_analysis_repeated_events",
       label = "Repeated events",
       choices = unique(rt_incidence$analysis_repeated_events),
       selected = unique(rt_incidence$analysis_repeated_events),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_analysis_complete_database_intervals",
       label = "Complete period",
       choices = unique(rt_incidence$analysis_complete_database_intervals),
       selected = unique(rt_incidence$analysis_complete_database_intervals),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   p("Dates"),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_analysis_interval",
       label = "Interval",
       choices = unique(rt_incidence$analysis_interval),
       selected = "years",
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_incidence_estimates_incidence_start_date",
       label = "Incidence start date",
       choices = as.character(unique(rt_incidence$incidence_start_date)),
       selected = as.character(unique(rt_incidence$incidence_start_date)),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )
   ),
   tabsetPanel(
     type = "tabs",
     tabPanel(
       "Table of estimates",
       downloadButton("rt_incidence_estimates_download_table", "Download current estimates"),
       DTOutput("rt_incidence_estimates_table") %>% withSpinner()
     ),
     tabPanel(
       "Plot of estimates",
       p("Plotting options"),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_incidence_estimates_plot_x",
           label = "x axis",
           choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
           selected = "incidence_start_date",
           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = FALSE
         )
       ),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_incidence_estimates_plot_facet",
           label = "Facet by",
           choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
           selected = c("outcome_cohort_name"),
           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_incidence_estimates_plot_colour",
           label = "Colour by",
           choices = c("cdm_name", "outcome_cohort_name", "denominator_target_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
           selected = "cdm_name",
           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       plotlyOutput(
         "rt_incidence_estimates_plot",
         height = "800px"
       ) %>%
         withSpinner(),
       h4("Download figure"),
       div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
       div(
         style = "display: inline-block;",
         textInput("rt_incidence_estimates_download_height", "", 10, width = "50px")
       ),
       div("cm", style = "display: inline-block; margin-right: 25px;"),
       div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
       div(
         style = "display: inline-block;",
         textInput("rt_incidence_estimates_download_width", "", 20, width = "50px")
       ),
       div("cm", style = "display: inline-block; margin-right: 25px;"),
       div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
       div(
         style = "display: inline-block; margin-right:",
         textInput("rt_incidence_estimates_download_dpi", "", 300, width = "50px")
       ),
       downloadButton("rt_incidence_estimates_download_plot", "Download plot")
     )
   )
 ),
 
 # rt_chars -----
 tabItem(
   tabName = "rt_chars",
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_chars_cdm",
       label = "Database",
       choices = sort(unique(rt_patient_characteristics$cdm_name)),
       selected = sort(unique(rt_patient_characteristics$cdm_name)),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_chars_cohort",
       label = "Cohort",
       choices = sort(unique(rt_patient_characteristics$group_level)),
       selected = sort(unique(rt_patient_characteristics$group_level)),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )),
   tabsetPanel(
     type = "tabs",
     
     tabPanel(
       "Cohort demographics",
       tags$hr(),
       gt_output("gt_rt_patient_characteristics") %>%
         withSpinner()
     ),
     tabPanel(
       "Cohort large scale characteristics",
       tags$hr(),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_chars_lsc_domain",
           label = "Domain",
           choices = sort(unique(rt_large_scale_characteristics_index$table_name)),
           selected = sort(unique(rt_large_scale_characteristics_index$table_name)),
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_chars_index_time_window",
           label = "Time window",
           choices = sort(unique(rt_large_scale_characteristics_index$variable_level)),
           selected = sort(unique(rt_large_scale_characteristics_index$variable_level)),
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       tags$hr(),
       DT::dataTableOutput("dt_rt_large_scale_characteristics_index") %>%
         withSpinner()
       
     )
   )
 ), 
 
 
 
 # rt complications -----
 tabItem(
   tabName = "rt_complications",
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_surv_cdm",
       label = "Database",
       choices = sort(unique(rt_90_day$cdm_name)),
       selected = sort(unique(rt_90_day$cdm_name)),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )),
   div(
     style = "display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(
       inputId = "rt_surv_cohort",
       label = "Cohort",
       choices = sort(unique(rt_90_day$group_level)),
       selected = sort(unique(rt_90_day$group_level)),
       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
       multiple = TRUE
     )),
   tabsetPanel(
     type = "tabs",
     tabPanel(
       "Events during follow-up",
       tags$hr(),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_post_chars_lsc_domain",
           label = "Domain",
           choices = sort(unique(rt_large_scale_characteristics_post$table_name)),
           selected = sort(unique(rt_large_scale_characteristics_post$table_name)),
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       div(
         style = "display: inline-block;vertical-align:top; width: 150px;",
         pickerInput(
           inputId = "rt_post_chars_index_time_window",
           label = "Time window",
           choices = sort(unique(rt_large_scale_characteristics_post$variable_level)),
           selected = sort(unique(rt_large_scale_characteristics_post$variable_level)),
           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
           multiple = TRUE
         )
       ),
       tags$hr(),
       DT::dataTableOutput("dt_rt_large_scale_characteristics_post") %>%
         withSpinner()
     ),
     tabPanel(
       "90 day cumulative incidence",       
       tags$hr(),
       DT::dataTableOutput("dt_rt_90_day") %>%
         withSpinner()
     ),
     tabPanel(
       "Survival plot",
       tags$hr(),
       plotlyOutput(
         "rt_surv",
         height = "800px"
       ) %>%
         withSpinner()
     )
   )
 )
  # end -----
    )
  )
)


