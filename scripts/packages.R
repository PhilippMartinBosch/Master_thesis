#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))
remotes::install_github("mattcowgill/ggannotate")
remotes::install_github("hrbrmstr/albersusa")
# load packages and install if not installed --------------------------------------------
pacman::p_load(tidyverse, readxl, lme4, modelsummary, hrbrthemes, ggannotate,
               equatiomatic, brms, haven, usethis, here, data.table,
               arm, insight, performance, merTools, DHARMa, lubridate, naniar, osmdata,
               mapsapi, geonames, tidygeocoder, sf, vctrs, rnaturalearth, rnaturalearthdata,
               maps, mapdata, totalcensus, tigris, usmap, broom.mixed, tidybayes, gt, 
               htmltools, fontawesome, formattable, kableExtra, extrafont, wesanderson,
               scico, paletteer, Polychrome, magick, performance, bayesplot, emmeans,
               rstanarm, ggokabeito, ggpubr,
               install = TRUE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())

