#### This is the master script to process the WTC drought x CO2 dataset
####
#### author: Mingkai Jiang
####         m.jiang@westernsydney.edu.au
####

############################# set up #################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")


############################# plotting #################################
### leaf gas exchange and water fluxes
make_pilularis_physiological_plots()
make_populnea_physiological_plots()

### dry down charts of the two species
make_down_down_charts()

### whole plant hydraulic conductance for two species
make_whole_plant_hydraulic_conductance_plot()

### leaf area of two species
make_leaf_area_plot()



############################# end #################################


