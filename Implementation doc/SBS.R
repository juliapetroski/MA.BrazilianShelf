#install.packages("StrathE2E2", repos="https://www.marineresourcemodelling.maths.strath.ac.uk/sran/")

library(StrathE2E2)
e2e_info_pk()

basemodel <- e2e_read("Brazilian_shelf", "2010-2019", models.path = "Implementation doc/Files", results.path="Implementation doc/Files/SBS_results", model.ident = "baseline")
baseresults <- e2e_run(basemodel, nyear = 50, csv.output = TRUE)

e2e_plot_ts(basemodel,baseresults,"ECO")
e2e_plot_ts(basemodel,baseresults,"CATCH")

e2e_calculate_hrscale(basemodel)
e2e_extract_hr(basemodel,baseresults,csv.output = TRUE)

e2e_plot_fdrivers(basemodel, "ACTIVITY")
e2e_plot_fdrivers(basemodel, "HARVESTR")
e2e_plot_fdrivers(basemodel, "DISCARDS")
e2e_plot_fdrivers(basemodel, "OFFAL")
e2e_plot_fdrivers(basemodel, "ABRASION")
e2e_plot_eco(basemodel, selection="NUT_PHYT", results=baseresults)
e2e_plot_eco(basemodel, selection="SEDIMENT", results=baseresults)
e2e_plot_eco(basemodel, selection="FISH", results=baseresults)
e2e_plot_eco(basemodel, selection="PREDATORS", results=baseresults)
e2e_plot_eco(basemodel, selection="CORP_DISC", results=baseresults)
e2e_plot_eco(basemodel, selection="BENTHOS", results=baseresults)
e2e_plot_eco(basemodel, selection="ZOOPLANKTON", results=baseresults)

e2e_plot_catch(basemodel, baseresults, selection="BY_GEAR")
e2e_plot_catch(basemodel, baseresults, selection="BY_GUILD")

e2e_compare_obs(selection="ANNUAL", basemodel, results = baseresults)
e2e_compare_obs(selection="MONTHLY", basemodel, results = baseresults)

e2e_plot_edrivers(basemodel)

e2e_plot_biomass(basemodel, results=baseresults)

e2e_plot_migration(basemodel, results=baseresults)

e2e_plot_trophic(basemodel, results=baseresults)

hr <- c(0,0.5,0.75,1.0,1.25,2.0,3.0)
pf_yield_data <- e2e_run_ycurve(basemodel,selection="PLANKTIV", nyears=3, HRvector=hr,
                                HRfixed=1,csv.output=FALSE)

names(pf_yield_data)
e2e_plot_ycurve(model, selection="PLANKTIV", results=pf_yield_data,
                title="Planktivorous yield with baseline demersal fishing")

demersal_yield <- e2e_run_ycurve(basemodel, selection = "DEMERSAL", nyears = 50, HRvector = c(0,0.5,1.0,1.5,2.0,2.5,3.0), HRfixed = 1, csv.output = FALSE)
planktiv_yield <- e2e_run_ycurve(basemodel, selection = "PLANKTIV", nyears = 50, HRvector = c(0, 2, 4, 6, 8, 10), HRfixed = 1, csv.output = FALSE)
e2e_plot_ycurve(basemodel, selection="DEMERSAL", results=demersal_yield,
                title="")
e2e_plot_ycurve(basemodel, selection="PLANKTIV", results=planktiv_yield,
                title="")

?e2e_run_ycurve()

# Fitting

str(basemodel$data$fleet.model,max.level=1)     
parm_list <- e2e_get_parmdoc(9)
?e2e_optimize_eco() 
e2e_optimize_act()

e2e_optimize_eco(
  basemodel,
  nyears = 40,
  n_iter = 500,
  start_temperature = 1,
  cooling = 0.975,
  toppredlock = TRUE,
  quiet = TRUE,
  csv.output = TRUE,
  runtime.plot = TRUE
)

e2e_optimize_hr(
  basemodel,
  nyears = 10,
  n_iter = 10,
  start_temperature = 0.0005,
  cooling = 0.975,
  quiet = TRUE,
  csv.output = TRUE,
  runtime.plot = TRUE
)

test_act_eco <- e2e_optimize_act(
  basemodel,
  selection = "ECO",
  n_iter = 50,
  start_temperature = 0.5,
  cooling = 0.975,
  csv.output = TRUE,
  runtime.plot = TRUE,
  nyears = 10,
  quiet = TRUE
)

?e2e_optimize_act

test_act_HR <- e2e_optimize_act(
  basemodel,
  selection = "HR",
  n_iter = 100,
  start_temperature = 0.5,
  cooling = 0.985,
  csv.output = TRUE,
  runtime.plot = TRUE,
  n_traj = 50,
  deltaHi = 0.2,
  attenuationstep = 500,
  deltaG = 0.25
)

plot_data <- e2e_plot_opt_diagnostics(
  basemodel,
  selection = "ACT",
  fitted.to = "HR",
  use.saved = FALSE,
  use.example = FALSE,
  results = test_act_HR
)

str(plot_data,max.level=1)

sens_results <- e2e_run_sens(basemodel, nyears=50, n_traj=16, postprocess=TRUE) 

?e2e_plot_opt_diagnostics
str(basemodel$data$fleet.model,max.level=1)     
scale_values <- e2e_calculate_hrscale(basemodel)
scale_values
parm_list <- e2e_get_parmdoc(9)
e2e_optimize_act()


