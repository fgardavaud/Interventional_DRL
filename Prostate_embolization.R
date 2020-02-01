######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 01/24/2020

###################### set-up section ################################

# Set the projet path to the root level
root.dir = rprojroot::find_rstudio_root_file()

# load readxl package to read easyly Excel file with an install conditon
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

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



################################## data taylor section ##################################
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

# instance the today date in appropriate format
#evt <- ymd_hms(now())

#Loop without parallelization to calculate patient age in years and add this information to Study_data dataframe

# tic("for loop without parallelization")
# loop_lenght <- c(1:nrow(Study_data))
# for (i in loop_lenght) {
# naiss <- ymd_hms(Study_data[i,5])
# age <- as.period(interval(naiss, evt))@year
# age_patient[i] <- age
# }
# Study_data_age <-cbind(Study_data,age_patient)
# toc()

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

#################### clinical indication selection #################

Study_data_prostate <- subset(Study_data_age, Standard.study.description == "EMBOLISATION PROSTATIQUE")
# patient number control
patient_number <- length(unique(Study_data_prostate[,"Patient.ID"]))
# patient ID control with Mathias list
patient_list <- as.data.frame(unique(Study_data_prostate[,"Patient.ID"]))
patient_list_matthias <- read.csv2("data/Liste_patient_matthias.csv", sep = ";")
patient_list_cat <- cbind.fill(patient_list,patient_list_matthias)
#duplicated(patient_list_cat[,1:3])
# write.csv(patient_list, "data/liste_de_patient.csv")



################ DRL establishement section ########################

# A faire :
# faire des subset en fonction de la description clinique d'intérêt
# faire un test pour chaque subset pour savoir si les valeurs de la colonne Irradiation.Event.Type comporte au moins une fois "ROTATIONAL_ACQUISITION" 
# colonne avec facteurs à 4 niveaux
# si oui => CBCT; sinon => pas de CBCT.
