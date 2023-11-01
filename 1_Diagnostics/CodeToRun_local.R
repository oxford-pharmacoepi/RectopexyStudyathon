# Dependency management -----
# install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()

# Load packages ------
library(DBI)
library(CDMConnector)
library(dbplyr)
library(dplyr)
library(CodelistGenerator)
library(PatientProfiles)
library(IncidencePrevalence)
library(here)
library(readr)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "pharmetrics"

# Database connection details -----
db <- dbConnect(RPostgres::Postgres(),
                dbname = "cdm_iqvia_pharmetrics_plus_202203" ,
                port = Sys.getenv("DB_PORT") ,
                host = "163.1.65.51",
                user = Sys.getenv("DB_USER"),
                password =  Sys.getenv("DB_PASSWORD"))
cdm_schema <- "public_100k"
write_schema <- "results"

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "rectopexy_"

# Run the study ------
source(here("RunStudy.R"))
