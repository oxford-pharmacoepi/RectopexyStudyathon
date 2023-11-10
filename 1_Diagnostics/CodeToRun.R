# Dependency management -----
# install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()

# Load packages ------
library(DBI)
library(CDMConnector)
library(dbplyr)
library(dplyr)
library(RPostgres)
library(odbc)
library(CodelistGenerator)
library(PatientProfiles)
library(DrugUtilisation)
library(IncidencePrevalence)
library(here)
library(readr)
library(zip)
library(stringr)
library(testthat)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "....."

# Database connection details -----
db <- dbConnect(".....")
cdm_schema <- "...."
write_schema <- "...."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "rectopexy_"

# Run the study ------
source(here("RunStudy.R"))
