### This is guide to using the code to reproduce the results in the paper
### so pleae read it first when trying to reproduce the results
### For most cases, both the R script and the corresponding results
### are saved here to quickly regenerate the results used in the report
### For example, all the simulation results are saved, so running the 
### simulation code (R scripts) will do nothing. 
### To completely redo all the jobs, simple remove the saved results 
### (only keep R scripts) and follow the instructions below. 
### However, I would not recommend to do so because running the entire simulation
### is very time consuming.


1. Regenerate Figure 1
use the R scripts in demo/nullcorr.
the result is saved in the same directory as equal.pdf


2. Regenerate Figure 2
use the R scripts in demo/overlap.
the result is saved in the same directory as color_plot.pdf


3. Regenerate simulation results and the corresponding tables
use the R scripts in main_code/demo/bench.
run the code with following order:

01_generate_data.R
02_fit_kmeans.R
03_select_nclusters.R
04_plot_results.R
05_create_tables.R

The results are saved in folder setting1/2/3/4/5,
For example, the plot in simulation setting 1 (Figure 3)
is in demo/bench/setting1/Facet.

To completely re-run the code, delete the folders above
and run the 5 R scripts above in order.

05_create_tables.R gives the tables in suppliment results.

4. Table 1 results in section Empirical validation
run the R script Empirical_validation.R in demo/data_examples


5. Reproduce Figure 8 and Table 3
run the R script confusion_table.R in demo/data_examples
