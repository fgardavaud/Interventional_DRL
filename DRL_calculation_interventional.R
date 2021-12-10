######################################################################
##                                                                  ##                                  
##          DRL completion from DoseWatch database export           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 01/14/2020


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

# load tidyverse for data science such as data handling and visualization
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

# load grateful to list package used
if(!require(grateful)){
  install.packages("grateful")
  library(grateful)
}

# load prettyR to perform tailored statistical analysis
if(!require(prettyR)){
  install.packages("prettyR")
  library(prettyR)
}

# # load Rcommander GUI for basic stat analysis
# if(!require(Rcmdr)){
#   install.packages("Rcmdr")
#   library(Rcmdr)
# }

############################### data import section ##################################


## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunately Excel importation yield to parse errors in date data for instance.
# SO TO AVOID THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

# the study is based from patient database extracted between 02/01/2018 to 1/10/2021

tic("to import detailled data in Rstudio")
if(exists("DoseWatch_export")){
  print("raw data importation have already done")
}else{
  
  # operations to correctly read DoseWatch export file *NEW* format only for DoseWatch v3.2.3 and above.
  # if you have DoseWatch v3.1 or under comment the three following lines and uncomment the last previous command line.
  all_content = readLines("data/Interventional_Tenon_Radiologie_detailed_data_export.csv") # to read the whole file
  skip_content = all_content[-c(1,2,3)] # to suppress the first 3 rows with bad value yield to bad header with read.csv2 function
  DoseWatch_export <- read.csv2(textConnection(skip_content), sep = ";")
  # my_all_data <- read.csv2("data_v2/Interventional_Tenon_Radiologie_detailed_data_export.csv", sep = ";")
}
toc()




################################## data tailoring section ##################################
# data selection to keep only interested columns for this study
DoseWatch_Selected_data <- DoseWatch_export %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                       Patient.birthdate..YYYY.MM.DD.,
                                                       Patient.weight..kg., Patient.size..cm.,
                                                       BMI, Standard.study.description,
                                                       Peak.Skin.Dose..mGy.,
                                                       Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                       Total.Air.Kerma..mGy.,
                                                       Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                       Irradiation.Event.Type,Proprietary.Type, Dose.Preference,
                                                       ) # to select column of interest and keeping the column's name

# convert Series.time, Patient Birthdate and Study date  columns in right time format
DoseWatch_Selected_data <- DoseWatch_Selected_data %>%
  mutate(Series.Time = hm(Series.Time), Patient.birthdate..YYYY.MM.DD. = ymd_hms(Patient.birthdate..YYYY.MM.DD.),
         Study.date..YYYY.MM.DD. = as.POSIXct(Study.date..YYYY.MM.DD.))

# sort each line by Accession number and then by acquisition hour
DoseWatch_Selected_data <- arrange(DoseWatch_Selected_data, Accession.number, Series.Time)

######################## age patient computation #################################
#  instance null vector with appropriate dimension to bind with Study_data
Patient.Age <- rep(0, nrow(DoseWatch_Selected_data))

# Loop with parallelization to calculate patient age in years 
# and add this information to Study_data dataframe
# also have a condition to test global environment object for debugging
tic("for loop with parallelization")
if(exists("Study_data_selected_age")){
  print("patient age computation have already done")
}else{
  cores <- detectCores()
  registerDoMC(cores - 1)
  Patient.Age <- foreach(i = 1:nrow(DoseWatch_Selected_data)) %dopar% {
    #naiss = ymd_hms(DoseWatch_Selected_data[i,4]) # deprecated line as mutate function can convert easily "time" column
    #evt = as.POSIXct(DoseWatch_Selected_data[i,1]) # deprecated line as mutate function can convert easily "time" column
    # by suppressing those 2 previous lines and use mutate function instead => Computing acceleration by a factor 6 !!!!!!
    age = as.period(interval(DoseWatch_Selected_data[i,6], DoseWatch_Selected_data[i,2]))@year
    Patient.Age <- age
  }
}
toc()

Patient.Age <- as.character(Patient.Age)
Study_data_age <-cbind(DoseWatch_Selected_data,Patient.Age)

Study_data_selected_age <- Study_data_age %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                     Patient.Age,
                                                     Patient.birthdate..YYYY.MM.DD.,
                                                     Patient.weight..kg., Patient.size..cm.,
                                                     BMI, Standard.study.description,
                                                     Peak.Skin.Dose..mGy.,
                                                     Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                     Total.Air.Kerma..mGy.,
                                                     Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                     Irradiation.Event.Type,Proprietary.Type, Dose.Preference)

############### column format conversion #################
# convert patient years in numeric
Study_data_selected_age$Patient.Age <- as.numeric(Study_data_selected_age$Patient.Age)

############### Remove duplicated lines in order to have 1 line by exam ############
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# if you have several exam without accession number associated the following.
# command line will only keep the first exam without accession number 
#  SO YOU COULD LOST DATA INFORMATION
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
Study_data_without_duplicates <- Study_data_selected_age[!duplicated(Study_data_selected_age$Accession.number), ] # to keep only one row for each exam time.

# select only right BMI for DRL analysis between 18 and 35 in France
Study_data_without_duplicates <- Study_data_without_duplicates %>% filter(between(BMI, 18, 35))

# keep only Standard Study description with 10 exams or more (condition in French law)

dfc <- Study_data_without_duplicates %>% count(Standard.study.description) # add a counter for each standard study description
# add a new column with the previous counter
Study_data_without_duplicates$n_occurence <- with(dfc, n[match(Study_data_without_duplicates$Standard.study.description,Standard.study.description)])
# keep only Standard study with 10 exams or more
Exam_data_frequent_wo_duplicates <- Study_data_without_duplicates %>%
  group_by(Standard.study.description) %>%
  filter(n() > 9)
# keep only columns of interest for statistical analysis
Exam_data_frequent_wo_duplicates <- Exam_data_frequent_wo_duplicates %>% select(Patient.weight..kg., Patient.size..cm.,
                                            BMI, Standard.study.description, Peak.Skin.Dose..mGy.,
                                            Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                            Total.Air.Kerma..mGy.,
                                            Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                            Irradiation.Event.Type,Proprietary.Type, n_occurence)

# list Study Description with interest
list_study_description <- unique(Exam_data_frequent_wo_duplicates$Standard.study.description)

# print in terminal the Standard Study Description to consider 
print("In this study you have to consider the following description to perform DRL analysis (10 exams or more) :")
print(cat(unique(Exam_data_frequent_wo_duplicates$Standard.study.description)))
print(paste0("The Study description number you have to consider is : ", length(unique(Exam_data_frequent_wo_duplicates$Standard.study.description))))

# clean Global environment
rm(dfc, DoseWatch_Selected_data, Study_data_age, cores)


############### Stat analysis #################

# Plot histogram for each frequent exam description

ggplot(Exam_data_frequent_wo_duplicates) +
 aes(x = Standard.study.description) +
 geom_bar(fill = "#9B5D5D") +
 labs(x = "Description d'examen", y = "Nombre d'actes", title = "Nombre d'examens selon l'acte clinique pour des patients d'IMC standard",
 caption = "histo_exam") +
 theme_minimal()

# loop to create subset to contain only data asssociated for only one exam description
for (exam_description in list_study_description) { 
DRL_group_description <- paste("NRD_group_",exam_description, sep = "")
assign(DRL_group_description,Exam_data_frequent_wo_duplicates %>% filter(Standard.study.description == exam_description))
}

# perform main stats for each exam description

describe(dose.DOD_IP_NB, num.desc=c("mean","median","sd","min","max","valid.n"))















################ DRL establishement section ########################

# A faire :
# faire des subset en fonction de la description clinique d'intérêt
# TIPS, chimioembol hépatique, drainage biliaire, embolisation artères bronchiques, embolisation fibrome utérin, TIPS, vertébroplastie
# colonne avec facteurs à 49 niveaux
# attention à faire correpondre avec la labellisation locale
# faire un test pour chaque subset pour savoir si les valeurs de la colonne Irradiation.Event.Type comporte au moins une fois "ROTATIONAL_ACQUISITION" 
# colonne avec facteurs à 4 niveaux
# si oui => CBCT; sinon => pas de CBCT.
# prendre aléatoirement dans chaque subset 10 lignes consécutives et généré un fichier .csv avec le même canave que le fichier .csv de l'IRSN.
# mettre condition sur :
  # IMC : 18 < IMC_patient < 35.
  # PSD, Air Kerma total, DAP total ≠ NULL.

