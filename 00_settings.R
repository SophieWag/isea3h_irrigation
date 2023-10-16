
rm(list=ls(all=TRUE))
Sys.setlocale('LC_ALL','C')
options(scipen = 999)

# ===== Package dependencies ======= #
library(ranger)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(dggridR)
library(pmr)
library(RColorBrewer)
library(ggforce)
library(vip)
library(caret)
library(scales)
library(ROSE)
library(DescTools)
library(openair)
library(pdp)
library(regclass)

# ===== Path definition ======= #
iFol = "..."
oFol = "..."
resultFol = "..."
codeFol = "..."


# ===== Base dataset ======= #
# created by the preparation function 
load(file=paste0(iFol,"final_data.RData"))






