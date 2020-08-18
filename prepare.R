
#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

if(!dir.exists("output/BM")) {
    dir.create("output/BM", showWarnings = FALSE)
}

if(!dir.exists("output/BM/statistics")) {
    dir.create("output/BM/statistics", showWarnings = FALSE)
}

if(!dir.exists("output/BM/statistics/Epilularis")) {
    dir.create("output/BM/statistics/Epilularis", showWarnings = FALSE)
}

if(!dir.exists("output/BM/statistics/Epopulnea")) {
    dir.create("output/BM/statistics/Epopulnea", showWarnings = FALSE)
}


#if(!dir.exists("output/DT")) {
#    dir.create("output/DT", showWarnings = FALSE)
#}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               lubridate,
               ggplot2,
               viridis,
               sciplot,
               scales,
               data.table,
               plantecophys,
               stringr,        
               lme4,
               cowplot,
               nlme,
               gridExtra,
               ggthemes,
               lmerTest,
               multcomp,
               chron)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in sourcefiles)source(z1)

sourcefilesDT <- dir("scripts/DT", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z2 in sourcefilesDT)source(z2)
