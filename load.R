# Create vector of needed packages that can be found in CRAN
packages <- c(
  "vroom", "ggplot2", "readxl", "tidyverse", "ggfortify", "DT", "reshape2", "knitr", "magrittr",
  "rstatix", "cowplot", "ggpubr", "pwr", "dvmisc", "vegan", "ape", "BiocManager", "viridis",
  "Matrix", "sf", "devtools", "randomcoloR", "kableExtra", "stringr", "tibble", "lme4", "lmerTest", "doBy",
  "emmeans", "circlize", "seriation", "ranger", "h2o", "propr", "ggExtra", "agricolae",
  "FSA", "rcompanion", "doParallel", "ecodist", "GUniFrac", "ggrepel",
  "gridExtra", "ggh4x", "DescTools", "ggtext", "multcompView", "correlation"
)

# Create vector of needed packages that can be found using Bioconductor
bioc_list <- c("phyloseq", "biomformat")

# If any above packages need to be installed, you can uncomment and use the following commands to do so all at
# once, or you can install them all separately one by one, depending on how much oversight you'd like in the process.

# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages], repos = "http://cran.us.r-project.org")}
# # 
# installed_packages <- bioc_list %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   BiocManager::install(bioc_list)}

# Load packages using the following two commands
invisible(lapply(packages, library, character.only = TRUE))
invisible(lapply(bioc_list, library, character.only = TRUE))


#download LDM, microbiome, and GLMM..
# 
library(BiocManager)
BiocManager::install("microbiome")
# 
library(devtools)
install_github("hk1785/GLMM-MiRKAT", force=T)

#Aldex2
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ALDEx2")

#LDM
install.packages("LDM_5.0.tar.gz", repos=NULL) 

#Heatmap pkg
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ComplexHeatmap")

library(ALDEx2)
library(ComplexHeatmap)
library(LDM)
library(GLMMMiRKAT)
library(microbiome)
library(biomformat)

