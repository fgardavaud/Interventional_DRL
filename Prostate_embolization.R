######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 02/10/2020

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
# load prettyR package for better statistical analysis
if(!require(prettyR)){
  install.packages("prettyR")
  library(prettyR)
}
# # load sulmarytoolspackage for better table output
# if(!require(summarytools)){
#   install.packages("summarytools")
#   library(summarytools)
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


####### CBCT +/- and large display LD+/- labelization  ###########################

CBCT_label <- cut(Study_data$Irradiation.Event.Type,c("FLUOROSCOPY","","STATIONARY_ACQUISITION", "STEPPING_ACQUISITION", "ROTATIONAL_ACQUISITION"),c("CBCT-","CBCT-","CBCT-","CBCT-", "CBCT+"))

bmi_IP=cut(patient_IP$BMI,c(0,18.5,25,30,Inf),c("low","medium","high","obese"))

# on converti les facteurs en caractères.
bmi_IP=factor(as.character(bmi_IP),levels=c("medium","low","high","obese"))
# on rajoute à l'object patient une nouvelle colonne "bmi"
patient_IP$bmi=bmi_IP



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

# patient and exam number control
patient_number <- length(unique(Study_data_prostate[,"Patient.ID"]))
exam_number <- length(unique(Study_data_prostate[,"Accession.number"]))
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

############### global statistical analysis #################
# global stat for unique value by exam
Study_data_prostate_stat <- cbind(Study_data_prostate[,2:3],Study_data_prostate[,5:7], Study_data_prostate[,10:26], as.numeric(Study_data_prostate[,27])) # to select row of interest for statistical computing
Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat[,1:14], Study_data_prostate_stat[,23]) # to select column with unique value per exam
colnames(Study_data_prostate_stat_unique) <- c("Accession.Number","Patient.ID", "Patient_weight", "Patient.size", "BMI", "Peak.Skin.Dose",
                                                    "Image.and.Fluoroscopy.DAP", "Total.Acquisition.DAP", "Total.Fluoro.DAP",
                                                   "Total.Air.Kerma", "Total.Acquisition.Air.Kerma", "Total.Fluoro.Air.Kerma",
                                                   "Total.Time.of.Fluoroscopy", "Number.of.Acquisition.Series", "Patient.Age")
Study_data_prostate_stat_unique <- Study_data_prostate_stat_unique[!duplicated(Study_data_prostate_stat_unique$Accession.Number), ] # to keep only one row per exam to perform global stats
print("TNN12 is the codename in DW for non overexposure")
print("TNN13 is the codename in DW for overexposure")
temp_DAP_fluoro <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,8]))[Study_data_prostate_stat_unique[,8]]) # convert factor with level to a dataframe for statistics computation
colnames(temp_DAP_fluoro) <- c("Total.Acquisition.DAP") # add colnames for labellization
temp_DAP_graph <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,9]))[Study_data_prostate_stat_unique[,9]]) # convert factor with level to a dataframe for statistics computation
colnames(temp_DAP_graph) <- c("Total.Fluoro.DAP") # add colnames for labellization
Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat_unique[,1:7], temp_DAP_fluoro, temp_DAP_graph, Study_data_prostate_stat_unique[,10:15]) # bind alll columns for summary

Global_stat_unique_value <- summary(Study_data_prostate_stat_unique)

## global stat for non unique value by exam : FOV and incidence angles

Study_data_prostate_stat_multiple <- Study_data_prostate_stat[ ,20:22] # to select FOV and incidence angles columns
FOV_ok_list <- which(Study_data_prostate_stat_multiple$Field.of.View..cm. > 0) # to keep only FOV > 0 as FOV value = 0 it's an DoseWatch export error
Study_data_prostate_stat_multiple <- Study_data_prostate_stat_multiple[FOV_ok_list,] # to supress FOV bad value
Global_stat_multiple_value <- summary(Study_data_prostate_stat_multiple)


################ CBCT acquisition analysis #############################
# Summary of acquisition type repartition between all examinations
Acquisition_table <- table(Study_data_prostate$Patient.ID, Study_data_prostate$Irradiation.Event.Type)
CBCT_position <- which(Study_data_prostate$Irradiation.Event.Type == "ROTATIONAL_ACQUISITION")

# selection of sequences with CBCT
Study_data_prostate_CBCT <- subset(Study_data_prostate, Irradiation.Event.Type == "ROTATIONAL_ACQUISITION")
# count patient number with CBCT /!\ some patient could have multiple exams
Study_data_prostate_CBCT <- data.frame(Study_data_prostate_CBCT$Patient.ID, Study_data_prostate_CBCT$Accession.number, Study_data_prostate_CBCT$Irradiation.Event.Type, Study_data_prostate_CBCT$Proprietary.Type)
Patient_number_CBCT <- length(unique(Study_data_prostate_CBCT[,"Study_data_prostate_CBCT.Patient.ID"]))
Patient_number_wo_CBCT <- length(unique(Study_data_prostate[,"Patient.ID"])) - Patient_number_CBCT
# select only Patient ID and Irradiation event to table CBCT number per examination
Exam_ID_list_CBCT <- table(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number, droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))
# statistical analysis on CBCT acquisition

CBCT_stat <- describe(Exam_ID_list_CBCT, num.desc=c("mean","median","sd","min","max","valid.n"))

#CBCT_mean <- mean(Exam_ID_list_CBCT)
### graphic to illustrate CBCT acquisition repartition
# lbls <- Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number # create label = accession number
Study_data_prostate_CBCT_acquisition_num <- as.numeric(Exam_ID_list_CBCT,2) # convert factor in numeric value for pourcent
pct <- round(Study_data_prostate_CBCT_acquisition_num/sum(Study_data_prostate_CBCT_acquisition_num)*100,1) # generate pourcent of CBCT acquisition
lbls <- paste(pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Exam_ID_list_CBCT, labels = lbls, col=rainbow(length(lbls)), main = "CBCT acquisiton number for each patient")
dev.print(device = png, file = "output/CBCT_acquisition_number.png", width = 600, height = 400)

### CBCT protocol analysis and graph
CBCT_protocol_used <- table(droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Proprietary.Type))
pct <- round(as.numeric(CBCT_protocol_used)/sum(as.numeric(CBCT_protocol_used))*100,1) # generate pourcent of CBCT protocols
lbls <- names(CBCT_protocol_used) # create label = protocol name
lbls <- paste(lbls, pct, sep = ", \n ") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(CBCT_protocol_used, labels = lbls, cex = 0.8, main = "CBCT protocols used")
dev.print(device = png, file = "output/CBCT_protocols.png", width = 600, height = 400)


################ FOV size analysis #############################

# Summary of FOV distribution between all examinations
FOV_table <- table(droplevels(Study_data_prostate$Patient.ID), Study_data_prostate$Field.of.View..cm.)
FOV_table <- FOV_table[,2:5] # supress FOV = 0 cm column as it's an error of DoseWatch export
FOV_table <- round(prop.table(FOV_table, margin = 1)*100) # table in purcent for each examn


FOV_tab_total <- round(prop.table(addmargins(FOV_table,1),1)*100,1)  # FOV frequencies in purcent by patient plus total frequency
# FOV_tab_total <- cbind(addmargins(prop.table(addmargins(FOV_table,1),1),2), c(margin.table(FOV_table,1),sum(FOV_table)))
# colnames(FOV_tab_total)<-c(colnames(FOV_table),"TOTAL","EFFECTIF") 


# ### graphic to illustrate FOV distribution in all examinations
#
lbls <- colnames(FOV_tab_total) # create label = FOV size
lbls <- paste(lbls, "cm") # add cm
pct <- round(tail(FOV_tab_total,1)/sum(tail(FOV_tab_total,1))*100) # generate pourcent of total FOV size
lbls <- paste(lbls, pct, sep = ", \n") # add percents to labels
lbls <- paste(lbls," %",sep="") # ad % to labels 
pie(tail(FOV_tab_total,1), labels = lbls, cex =0.9, main = "FOV distribution in all examinations")
dev.print(device = png, file = "output/FOV_distribution.png", width = 600, height = 400)



################ TODO -LIST ########################

# A faire :
# labeliser les données entre large display +/-
# labeliser les données entre CBCT +/-
# FOV distribution CBCT+/- et LD +/-
# table de fréquence CBCT+/- ~ FOV; LD+/- ~ FOV
# summary CBCT +/- ; LD +/-
# t.student CBCT +/- ; LD +/-
# wilcoxon CBCT +/- ; LD +/-
# PCA CBCT +/- ~ dose pic ; LD +/- ~ dose pic
# TSNE CBCT +/- ~ dose pic ; LD +/- ~ dose pic
