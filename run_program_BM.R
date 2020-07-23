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

outdir <- "output/BM/"

############################# plotting #################################
### make glasshouse condition (T, RH and VPD during day and night times) plots
### open the function to plot!
make_glasshouse_condition_plots()

### leaf area of two species
#make_leaf_area_plot()

### read in leaf area and biomass data & make plots for the two species
make_leaf_area_and_biomass_plots_by_CO2()
make_leaf_area_and_biomass_plots_by_H2O()

### make soil water content and leaf transpiration plots for the two species
make_swc_and_transpiration_plots()

### transpiration as function of leaf area
make_transpiration_leaf_area_plot()

### pre-dawn and midday leaf water potentials
make_leaf_water_potential_plots()

### pre-dawn water potential with swc
make_predawn_water_potential_with_swc_plots()

### whole plant hydraulic conductance for two species
make_whole_plant_hydraulic_conductance_plot()

### leaf gas exchange fluxes
make_pilularis_physiological_plots()
make_populnea_physiological_plots()

### normalized Asat, i.e. Asat(time) / Asat(D1)
make_normalized_Asat_plots()

### CO2 ratios of A and gs
make_CO2_ratios_of_A_and_gs_plots_with_se()

make_CO2_difference_of_A_and_gs_plots_with_se()

#make_log_CO2_ratios_of_A_and_gs_plots()


### Water ratios of A and gs
make_water_ratios_of_A_and_gs_plots_with_se()

############################# end #################################


