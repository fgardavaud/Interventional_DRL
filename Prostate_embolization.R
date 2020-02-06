######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 02/05/2020

###################### set-up section ################################

# Set the projet path to the root level
root.dir = rprojroot::find_rstudio_root_file()

# # load readxl package to read easyly Excel file with an install conditon
# if(!require(readxl)){
#   install.packages("readxl")
#   library(readxl)
# }

# load lubridate package to determine patient age from birthdate with an install conditon
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

# load doMC package for parallel computing
if(!require(doMC)){
  install.packages("doMC")
  library(doMC)
}
# load foreach package for parallel computing on for loop
if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}
# load doparallel package to determine patient age from birthdate with an install conditon
# if(!require(doParallel)){
#   install.packages("doParallel")
#   library(doParallel)
# }
# load tictoc package to measure running time of R code
if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}
# load excel package to write results in Excel file
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}
# load tidyverse if necessary for data handling
# if(!require(tidyverse)){
#   install.packages("tidyverse")
#   library(tidyverse)
# }

############################### data import section ##################################
# read the database with data frame existing test
# my_all_data <- read_excel("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.xlsx")

## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunetely Excel importation yield to parse errors in date data for instance.
# SO TO AVIOD THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

tic("to import detailled data in Rstudio")
#my_all_data <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.csv", sep = ";")
if(exists("my_all_data")){
  print("raw data importation have already done")
}else{
my_all_data <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export.csv", sep = ";")
}
toc()

################################## data tayloring section ##################################
# Collums of interest selection
selection = c("Study.date..YYYY.MM.DD.", "Accession.number",
              "Patient.ID", "Patient.birthdate..YYYY.MM.DD.",
              "Patient.weight..kg.", "Patient.size..cm.",
              "BMI", "Standard.study.description", "Performing.physician.last.name",
              "Peak.Skin.Dose..mGy.",
              "Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.",
              "Total.Acquisition.DAP..mGy.cm..","Total.Fluoro.DAP..mGy.cm..",
              "Total.Air.Kerma..mGy.",
              "Total.Acquisition.Air.Kerma..mGy.", "Total.Fluoro.Air.Kerma..mGy.",
              "Total.Time.of.Fluoroscopy..s.", "Number.of.Acquisition.Series",
              "Irradiation.Event.Type","Proprietary.Type", "Dose.Preference",
              "Justification.Code","Raised.alerts.",
              "Positioner.Primary.Angle..deg.", "Positioner.Secondary.Angle..deg.",
              "Field.of.View..cm.")

# data filter to keep only interested collums for this study
Study_data <- my_all_data[,selection]

######################## age patient computation #################################

#  instance null vector with appropriate dimension to bind with Study_data
age_patient <- rep(0, nrow(Study_data))

# Loop with parallelization to calculate patient age in years 
# and add this information to Study_data dataframe
# also have a condition to test global environement objet for debugging
tic("for loop with parallelization")
if(exists("Study_data_age")){
  print("patient age computation have already done")
}else{
cores <- detectCores()
registerDoMC(cores - 1)
age_patient <- foreach(i = 1:nrow(Study_data)) %dopar% {
  naiss = ymd_hms(Study_data[i,4])
  evt = as.POSIXct(Study_data[i,1])
  age = as.period(interval(naiss, evt))@year
  age_patient <- age
}
}
toc()

age_patient <- as.character(age_patient)
Study_data_age <-cbind(Study_data,age_patient)

############### clinical indication selection and data control #################

Study_data_prostate <- subset(Study_data_age, Standard.study.description == "EMBOLISATION PROSTATIQUE")

# patient number control
patient_number <- length(unique(Study_data_prostate[,"Patient.ID"]))
# patient ID control with Mathias list to verify if selectde patients are identical than matthias' list
patient_list_matthias <- read.csv2("data/Liste_patient_matthias.csv", sep = ";")
patient_list_matthias_DW <- as.data.frame(patient_list_matthias[,2])
colnames(patient_list_matthias_DW) <- 'Patient.ID.Matthias'
patient_list_matthias <- as.array(patient_list_matthias[,2])
patient_list <- as.array(unique(Study_data_prostate[,"Patient.ID"]))

patient_number_verified <- length(intersect(patient_list, patient_list_matthias))

patient_verified <- intersect(patient_list, patient_list_matthias)
patient_comparison <- as.data.frame(patient_verified)

# loop to harmonize nrow dimension for each column between patient_comparison and patient_list_matthias
loop_end <- abs(nrow(patient_list_matthias) - nrow(patient_comparison))
loop_lenght <- c(1:loop_end)
for (i in loop_lenght) {
    patient_comparison <- rbind(patient_comparison, c(0))
}
# add patient_list_matthias values to patient_comparison
patient_comparison$patient_list_matthias <- patient_list_matthias

# loop to harmonize nrow dimension for each column between patient_comparison and patient_list
loop_end <- abs(nrow(patient_list) - nrow(patient_comparison))
loop_lenght <- c(1:loop_end)
for (i in loop_lenght) {
  patient_comparison <- rbind(patient_comparison, c(0))
}
# add patient_list values to patient_comparison
patient_comparison$patient_list <- patient_list
# write excel file to verify by other user the collected data
write.xlsx(patient_comparison, 'data/comparaison_liste_patient.xlsx', sheetName = "Comparaison_ID_patient", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

# patient_list_cat %>%
#   distinct(Patient.ID.Script, Patient.ID.Matthias, .keep_all = TRUE)
# duplicated(patient_list_cat[,1:2])


################ CBCT acquisition analysis #############################
# Summary of acquisition type repartition between all examinations
Acquisition_table <- table(Study_data_prostate$Patient.ID, Study_data_prostate$Irradiation.Event.Type)
CBCT_position <- which(Study_data_prostate$Irradiation.Event.Type == "ROTATIONAL_ACQUISITION")

# selection of sequences with CBCT
Study_data_prostate_CBCT <- subset(Study_data_prostate, Irradiation.Event.Type == "ROTATIONAL_ACQUISITION")
# count patient number with CBCT /!\ some patient could have multiple exams
Study_data_prostate_CBCT <- data.frame(Study_data_prostate_CBCT$Patient.ID, Study_data_prostate_CBCT$Accession.number, Study_data_prostate_CBCT$Irradiation.Event.Type)
Patient_number_CBCT <- length(unique(Study_data_prostate_CBCT[,"Study_data_prostate_CBCT.Patient.ID"]))

# select only Patient ID and Irradiation event to table CBCT number per examination
Exam_ID_list_CBCT <- table(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number, droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))
# PatientID_list_CBCT <- table(droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Patient.ID), droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))



################ TODO -LIST ########################

# A faire :
# statistique sur CBCT
# fréquence sur FOV
# fréquence sur angle d'incidence
