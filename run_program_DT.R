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

#### set output directory
outdir <- "output/DT/"

############################# plotting #################################
### make glasshouse condition (T, RH and VPD during day and night times) plots
make_glasshouse_condition_plots_DT()

### leaf area of two species
#make_leaf_area_plot_DT()

### read in leaf area and biomass data & make plots for the two species
make_leaf_area_and_biomass_plots_DT()

### make soil water content and leaf transpiration plots for the two species
make_swc_and_transpiration_plots_DT()

### transpiration as function of leaf area
make_transpiration_leaf_area_plot_DT()

### pre-dawn and midday leaf water potentials
make_leaf_water_potential_plots_DT()

### pre-dawn water potential with swc
make_predawn_water_potential_with_swc_plots_DT()

### whole plant hydraulic conductance for two species
make_whole_plant_hydraulic_conductance_plot_DT()

### leaf gas exchange fluxes
make_pilularis_physiological_plots_DT()
make_populnea_physiological_plots_DT()

### normalized Asat, i.e. Asat(time) / Asat(D1)
make_normalized_Asat_plots_DT()

### CO2 ratios of A and gs
make_CO2_ratios_of_A_and_gs_plots_DT()

make_log_CO2_ratios_of_A_and_gs_plots_DT()

############################# end #################################


