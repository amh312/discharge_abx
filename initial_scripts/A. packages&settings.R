#A. packages&settings.R

#This script loads the packages and sets the error handling options for the R script.
#It is run at the beginning of each session
#as a backstop to ensure packages are loaded.
#Packages are, however, also loaded at the beginning of each R script to illustrate where they are used.
#Package versions used in the original analysis are listed above each package load command.

options(error = NULL)

###v2.0.0
library(tidyverse)

###v3.0.1
library(AMR)

###v7.0.1
library(caret)

###v1.18.2.1
library(data.table)

###v1.0.6
library(MIMER)

###v0.95
library(corrplot)

###v1.8.0
library(glue)

###v1.1.7
library(rlang)

###v0.9.6
library(ggrepel)

###v1.19.0.1
library(pROC)

###v1.4
library(PRROC)

###v2.6.5
library(psych)

###v0.84.1
library(irr)

###v1.2
library(kappaSize)
