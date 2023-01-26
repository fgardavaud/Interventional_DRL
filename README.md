# Interventional_DRL

Script in R langage to extract data in DoseWatch Database to established DRL in interventional radiology in regards with French law. The script generate also DRL statistics and boxplot on the major dose parameters.

# Prerequisite

Several R packages are mandatory but the script analyze your system and installs, if necessary, missing packages.

In your project root path you have to create one folder named "data" (and put your data here) and another folder named "output" where the output files will be placed there.

**You have to export your database from DoseWatch for a unique interventionnal room and for a unique year.** Then you will get to files : the general and the detailled excel file

**You have to convert in .csv the detailled .xlsx file**

**Becareful the DRL files generation is only adapt for the french law format.**

# Adaptation to your workstation environment

You have to edit the following lines in order to run the script :

-   line 94 : edit the name of the .csv file to adapt with your own configuration,

-   from line 287 : edit the tittle (in French language) of the graphs if necessary.

# Test environment

This script have been tested in R 4.2.2 version in Rstudio v2022.12.0+353 in macOS Monterey version. But it should work on any OS system if your R program have the ability to work with additional R packages

# Additional R packages list available from CRAN

![](images/image-94367853.png)
