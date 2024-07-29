library(decisionSupport)
library(ggplot2)
library(pls)
# Load the data

input_estimates <- read.csv("input_estimates.csv")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))


# Define the decision function
decision_function <- function(x, varnames){
  #the baseline is just a normal income without bio gas
  annual_household_income <- income_per_month * 12 
  
  # calculate the cost 
  biogas_manure_cost_precalc <- installation_cost + biogas_cost_per_year + 	
    labour_cost + equipement_cost + manure_raw_material_cost
  biogas_manure_cost <- vv(biogas_manure_cost_precalc,var_CV, n_years)
  
  biogas_industry_cost_precalc <- installation_cost + biogas_cost_per_year + 	
    labour_cost + equipement_cost + industry_raw_material_cost
  biogas_industry_cost <- vv(biogas_industry_cost_precalc,var_CV, n_years)
  
  # Profit from biogas production system
  # how much bigas we can produced per year
  #we can assume that the machine will be running 15 per month
  biogas_product_per_year_precalc <- biogas_product * 15 * 12
  biogas_product_per_year <- vv(biogas_product_per_year_precalc,var_CV, n_years)
  
  # revenue of biogas
  # to get the net revenue subtract the annual operation cost from the price
   annual_revenue_biogas <- biogas_product_per_year - biogas_price
  
manure_biogas_result <- annual_revenue_biogas - biogas_manure_cost

NPV_biogas <-
  discount(manure_biogas_result, discount_rate, calculate_NPV = TRUE)

NPV_household <- discount(annual_household_income, discount_rate, calculate_NPV = TRUE)

 
return(list(biogas_NPV = NPV_biogas,
            NO_biogas_NPV = NPV_household,
            NPV_decision_do = NPV_biogas - NPV_household,
            Cashflow_decision_do = manure_biogas_result))
}

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("input_estimates.csv"),
  model_function = decision_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

#### Plot Net Present Value (NPV) distributions 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("biogas_NPV", "NO_biogas_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("biogas_NPV",
                                             "NO_biogas_NPV"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')
#### Cashflow analysis

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")

#### Projection to Latent Structures (PLS) analysis

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)

plot_pls(pls_result, threshold = 0)

#### Value of Information (VoI) analysis
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])
#
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "biogas_NPV") # first value on return result
plot_evpi(evpi, decision_vars = "NPV_decision_do")
# too high uncertain for the equipement.. so in order to get a PI you should
#spend not more than 50,000

compound_figure(mcSimulation_object = mcSimulation_results, input_table = input_estimates, 
                plsrResults = pls_result, EVPIresults = evpi, 
                decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", base_size = 7)


