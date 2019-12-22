######################################################################
##                                                                  ##                                  
##          DRL completion from DoseWatch database export           ##
##                                                                  ##                                 
######################################################################

# Workspace cleanning
rm(list = ls(all.names = TRUE))

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


# read the database
# my_all_data <- read_excel("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.xlsx")
my_all_data <- read_excel("data/CV-IR_Tenon_Radiologie_detailed_data_export.xlsx")

# Collums of interest selection
selection = c("Study date (YYYY-MM-DD)", "Accession number", "Patient ID", "Patient birthdate (YYYY-MM-DD)",
              "Patient weight (kg)", "Patient size (cm)",
              "BMI", "Standard study description", "Performing physician last name",
              "Image and Fluoroscopy Dose Area Product (mGy.cm2)", "Total Time of Fluoroscopy (s)",
              "Total Air Kerma (mGy)", "Irradiation Event Type", "Total Number of Radiographic Frames")
# data filter to keep only interested collums for DRL computation
DRL_data <- my_all_data[,selection]

#  instance null vector with appropriate dimension to bind with DRL_data
age_patient <- rep(0, nrow(DRL_data))
# age_tampon <- rep(0, 1000)

# instance the today date in appropriate format
evt <- ymd_hms(now())

#Loop to calculate patient age in years and add this information to DRL_data dataframe

# tic("for loop without parallelization")
# loop_lenght <- c(1:nrow(DRL_data))
# for (i in loop_lenght) {
# naiss <- ymd_hms(DRL_data[i,4])
# age <- as.period(interval(naiss, evt))@year
# age_patient[i] <- age
# }
# DRL_data <-cbind(DRL_data,age_patient)
# toc()

tic("for loop with parallelization 2nd try")
cores <- detectCores()
registerDoMC(cores - 2)
age_patient <- foreach(i = 1:nrow(DRL_data)) %dopar% {
  naiss = ymd_hms(DRL_data[i,4])
  age = as.period(interval(naiss, evt))@year
  age_patient <- age
}
toc()

age_patient <- as.character(age_patient)
DRL_data <-cbind(DRL_data,age_patient)






