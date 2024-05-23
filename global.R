# Rgraphviz package not available on CRAN so add package from BioConductor
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.16")
# BiocManager::install("Rgraphviz")

##########
# Before deploying to shinyapps.io run the following lines in the console to avoid
# failure to deploy due to BioConductor package
# library(BiocManager)
# options(repos = BiocManager::repositories())
##########

library(Rgraphviz)

# Load packages
library(abind)
library(colorRamps)
library(crosstalk)
library(DT)
library(ellipse)
library(foreach)
library(Hmisc)
library(jsonlite)
library(lme4)
library(mada)
library(magic)
library(Matrix)
library(msm)
library(mvmeta)
library(mvtnorm)
library(plotrix)
library(png)
library(rio)
library(rlang)
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(stats)
library(yaml)
