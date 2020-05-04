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
### make glasshouse condition (T, RH and VPD during day and night times) plots
make_glasshouse_condition_plots()

### leaf area of two species
make_leaf_area_plot()

### read in leaf area and biomass data & make plots for the two species
make_leaf_area_and_biomass_plots()

### make soil water content and leaf transpiration plots for the two species
make_swc_and_transpiration_plots()

### transpiration as function of leaf area
make_transpiration_leaf_area_plot()

### pre-dawn and midday leaf water potentials
make_leaf_water_potential_plots()

### leaf gas exchange and water fluxes
make_pilularis_physiological_plots()
make_populnea_physiological_plots()

### dry down charts of the two species
make_dry_down_charts()

### whole plant hydraulic conductance for two species
make_whole_plant_hydraulic_conductance_plot()



############################# end #################################


