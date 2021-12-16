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
rm(dfc, DoseWatch_Selected_data, Study_data_age)


############### Stat analysis #################

# Plot histogram for each frequent exam description
Exam_data_frequent_wo_duplicates %>%
  filter(n_occurence >= 20L) %>%
  ggplot() +
  aes(x = Standard.study.description) +
  geom_bar(fill = "#D96748") +
  labs(x = "Description d'examen", y = "Nombre d'actes", title = "Nombre d'examens (n > 19) selon l'acte clinique pour des patients d'IMC standard",
       caption = "histo_exam") +
  theme_gray()
ggsave(path = "output/", filename = "Frequent_Exam_Description_histogram.png", width = 12)

# Plot PSD boxplot for each frequent exam description
Exam_data_frequent_wo_duplicates %>%
  filter(n_occurence >= 20L) %>%
  ggplot() +
  aes(x = Standard.study.description, y = Peak.Skin.Dose..mGy.) +
  geom_boxplot(shape = "circle", fill = "#D96748") +
  labs(x = "Description d'examen", y = "Dose pic à la peau (mGy)", 
       title = "Distribution de la dose pic à la peau pour les type d'exams les plus fréquent (n >19)") +
  theme_gray()
ggsave(path = "output/", filename = "PSD_boxplot.png", width = 12)

# loop to create subset to contain only data associated for only one exam description
for (exam_description in list_study_description) {
  DRL_data <-  Exam_data_frequent_wo_duplicates %>% filter(Standard.study.description == exam_description) # to select only current exam description data
  exam_description <- gsub("[ ]", "_", exam_description, perl=TRUE) # replace " " by "_" from the date value
  DRL_name <- paste("DRL_data_",exam_description, sep = "") # to generate current exam description name for data
  assign(DRL_name, DRL_data) # to generate current exam description data with associated name
  path_name <- paste("output/", DRL_name, ".xlsx", sep ="") # create path name to save Excel file
  write.xlsx(DRL_data, path_name, sheetName = DRL_name,
             colNames = TRUE, rowNames = FALSE, append = TRUE, overwrite = TRUE) # create Excel file of main stat for current exam description
  # DRL_stat_name <- paste("DRL_stat_",exam_description, sep = "") # to generate current exam description name for associated stat
  # DRL_stat <- describe(DRL_data, num.desc=c("mean","median","sd","min","max","valid.n")) # to generate current exam description name for stat
  # assign(DRL_stat_name, DRL_stat) # to generate current exam description stat with associated name
  rm(DRL_data, DRL_name, path_name) # clean Global environment
}

# get statistics (mean, sd, max, min) round at unit for each frequent exam description
Local_DRL <- Exam_data_frequent_wo_duplicates%>%
  group_by(Standard.study.description) %>%
  summarise(
    # stats for peak skin dose data in mGy
    mean_PSD_mGy = round(mean(Peak.Skin.Dose..mGy., na.rm = TRUE),0),
    med_PSD_mGy = round(median(Peak.Skin.Dose..mGy., na.rm = TRUE),0),
    sd_PSD_mGy = round(sd(Peak.Skin.Dose..mGy., na.rm = TRUE),0),
    max_PSD_mGy = round(max(Peak.Skin.Dose..mGy., na.rm = TRUE),0),
    min_PSD_mGy = round(min(Peak.Skin.Dose..mGy., na.rm = TRUE),0),
    # stats for Total Dose Area Product data in mGy.cm^2
    mean_DAP_mGy.cm2 = round(mean(Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., na.rm = TRUE),0),
    med_DAP_mGy.cm2 = round(median(Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., na.rm = TRUE),0),
    sd_DAP_mGy.cm2 = round(sd(Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., na.rm = TRUE),0),
    max_DAP_mGy.cm2 = round(max(Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., na.rm = TRUE),0),
    min_DAP_mGy.cm2 = round(min(Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., na.rm = TRUE),0),
    # stats for total air kerma data in mGy
    mean_AK_mGy = round(mean(Total.Air.Kerma..mGy., na.rm = TRUE),0),
    med_AK_mGy = round(median(Total.Air.Kerma..mGy., na.rm = TRUE),0),
    sd_AK_mGy = round(sd(Total.Air.Kerma..mGy., na.rm = TRUE),0),
    max_AK_mGy = round(max(Total.Air.Kerma..mGy., na.rm = TRUE),0),
    min_AK_mGy = round(min(Total.Air.Kerma..mGy., na.rm = TRUE),0),
    # stats for fluoroscopy time data in seconds
    mean_Scop_Time_s = round(mean(Total.Time.of.Fluoroscopy..s., na.rm = TRUE),0),
    med_Scop_Time_s = round(median(Total.Time.of.Fluoroscopy..s., na.rm = TRUE),0),
    sd_Scop_Time_s = round(sd(Total.Time.of.Fluoroscopy..s., na.rm = TRUE),0),
    max_Scop_Time_s = round(max(Total.Time.of.Fluoroscopy..s., na.rm = TRUE),0),
    min_Scop_Time_s = round(min(Total.Time.of.Fluoroscopy..s., na.rm = TRUE),0),
    # stats for acquisition (graphy and CBCT/3D) number data
    mean_Acq_Num = round(mean(Number.of.Acquisition.Series, na.rm = TRUE),0),
    med_Acq_Num  = round(median(Number.of.Acquisition.Series, na.rm = TRUE),0),
    sd_Acq_Num = round(sd(Number.of.Acquisition.Series, na.rm = TRUE),0),
    max_Acq_Num = round(max(Number.of.Acquisition.Series, na.rm = TRUE),0),
    min_Acq_Num = round(min(Number.of.Acquisition.Series, na.rm = TRUE),0),
    # Exam number
    Exam_number = n(),
)
write.xlsx(Local_DRL, 'output/Local_DRL.xlsx', sheetName = "Local_DRL",
           col.names = TRUE, row.names = TRUE, append = FALSE, overwrite = TRUE)


# flights %>%
#   group_by(month) %>%
#   summarise(
#     max_delay = max(dep_delay, na.rm = TRUE),
#     min_delay = min(dep_delay, na.rm = TRUE),
#     mean_delay = mean(dep_delay, na.rm = TRUE)
#   )
# ExamAET.stat <- Patient_merge_data_all_source %>%
#   group_by(AE.source) %>%
#   summarise(
#     count = n(),
#   )






## ####################### Miscellaneous #####################################
# Create word document to list package citation
cite_packages(out.format = "docx", out.dir = file.path(getwd(), "output/"))








################ DRL establishement section ########################

# A faire :
# RECUPERER SI Irradiation.Event.Type comporte au moins une fois "ROTATIONAL_ACQUISITION"
# si oui => CBCT; sinon => pas de CBCT.
# prendre aléatoirement dans chaque subset 10 lignes consécutives et généré un fichier .csv avec le même canevas que le fichier .csv de l'IRSN.
# mettre condition sur :
# PSD, Air Kerma total, DAP total ≠ NULL.

