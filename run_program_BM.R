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

############################# data processing #################################
### Prepare data beyond day 6 and 16
re_calculate_means_and_sds()

############################# plotting #################################
### make glasshouse condition (T, RH and VPD during day and night times) plots
### open the function to plot!
make_glasshouse_condition_plots()

### This is based on the full dataset, not only just a subset
### In addition to print out met figure, it also checks the effect of VPD
make_glasshouse_condition_plots_full_data()


### leaf area of two species
#make_leaf_area_plot()

### read in leaf area and biomass data & make plots for the two species
make_statistics_leaf_area_and_biomass_table_revised()

make_leaf_area_and_biomass_plots_by_CO2()

make_leaf_area_and_biomass_plots_by_H2O()

### plot % allocation of biomass
make_biomass_allocation_plot()


### make statistical summary table for fluxes, at the start of the drydown
make_statistics_leaf_water_relations_gas_exchange_table_at_the_start_of_the_drydown()

### make satistical summary table for fluxes during the drydown - linear test
make_statistics_leaf_water_relations_gas_exchange_table_revised()


### make statistical summary table for fluxes, before the 
### pre-determined physiological threshold was reached
make_statistics_leaf_water_relations_gas_exchange_table_before_stress()

### make statistical summary table for fluxes,
### 1. average over days before stress was reached (i.e. on and before day 6 and 16)
### 2. on day 6 and 16 (i.e. the day before stress was reached)
make_average_and_fixed_date_rate_comparison_before_threshold()

### check number of days before stress is reached
#check_statistics_on_number_of_unstress_days()


### make statistical summary table for fluxes during the drydown - non-linear test
#make_statistics_leaf_water_relations_gas_exchange_table_nonlinear()


### make soil water content and leaf transpiration plots for the two species
make_swc_and_transpiration_plots()

#make_swc_and_transpiration_plots_based_on_raw_data() # the two plots are identical so OK

### transpiration as function of leaf area
make_transpiration_leaf_area_plot()

### pre-dawn and midday leaf water potentials
make_leaf_water_potential_plots()

#make_leaf_water_potential_plots_based_on_raw_data() # the two plots are identical so OK

### pre-dawn water potential with swc
make_predawn_water_potential_with_swc_plots()

### whole plant hydraulic conductance for two species
make_whole_plant_hydraulic_conductance_plot()

### leaf gas exchange fluxes
make_physiological_plots()

#make_physiological_plots_based_on_raw_data() # the two plots are identical so OK
 
#make_pilularis_physiological_plots()

#make_populnea_physiological_plots()

### normalized Asat, i.e. Asat(time) / Asat(D1)
### add error bars
### stopped here. 
### other issues to check - the explaination of the results lacked statistical power,
make_normalized_Asat_plots()

### CO2 ratios of A and gs
make_CO2_ratios_of_A_and_gs_plots_with_se()

#make_CO2_difference_of_A_and_gs_plots_with_se()

#make_log_CO2_ratios_of_A_and_gs_plots()


### Water ratios of A and gs
#make_water_ratios_of_A_and_gs_plots_with_se()

############################# end #################################


