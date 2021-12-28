#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))
remotes::install_github("mattcowgill/ggannotate")

# load packages and install if not installed --------------------------------------------
pacman::p_load(tidyverse, readxl, lme4, modelsummary, hrbrthemes, ggannotate,
               equatiomatic, brms, haven, usethis, here, data.table,
               arm, insight, performance, merTools, DHARMa, lubridate, naniar, osmdata,
               mapsapi, geonames, tidygeocoder, sf,
               install = TRUE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())
