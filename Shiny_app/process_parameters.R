##################
# file name: process_parameters.R
# Prepare the parameters for user display
# created by: Anne Meyer
# created on: 2023-11-07
##################

# read the dataset
DataParameters <- read.csv("data/ahle_params_senegal_samples.csv")
DataParameters <- DataParameters %>% 
  filter(!(Parameter %in% c("Beta_JM", # same as female
                            "Beta_SubAM", # same as female
                            "Beta_AM", "Beta_AF", # not used?
                            "Beta_Ox",  # not used
                            "Alpha_Ox", # not used
                            "castration_proportion", # not used
                            "CullOx", # not used
                            "DM_req_prpn_Ox", # not used
                            "draught_day_value", # not used
                            "draught_rate", # not used
                            "fvOx", # not used
                            "lwOx", # not used
                            "Gamma_Ox", # not used
                            "N_Ox_t0", # not used
                            "hides_rate", "hides_rate_mor", "hides_value", # not used
                            "DM_req_prpn_JF", # same for all classes
                            "DM_req_prpn_JM", # same for all classes
                            "DM_req_prpn_SubAM", # same for all classes
                            "DM_req_prpn_SubAF", # same for all classes
                            "DM_req_prpn_AF", # same for all classes
                            "Health_exp_treatment_JF", # not used (prev and trt in same var)
                            "Health_exp_treatment_JM", # not used
                            "Health_exp_treatment_SubAF", # not used
                            "Health_exp_treatment_SubAM", # not used
                            "Health_exp_treatment_AF", # not used
                            "Health_exp_treatment_AM", # not used
                            "Health_exp_treatment_Ox", # not used
                            "Health_exp_prev_JF", # same for all classes
                            "Health_exp_prev_JM", # same for all classes
                            "Health_exp_prev_SubAF", # same for all classes
                            "Health_exp_prev_SubAM", # same for all classes
                            "Health_exp_prev_AF", # same for all classes
                            "Health_exp_prev_Ox", # same for all classes
                            "Infrastructure_per_head_JF", # not used
                            "Infrastructure_per_head_JM", # not used
                            "Infrastructure_per_head_SubAF", # not used
                            "Infrastructure_per_head_SubAM", # not used
                            "Infrastructure_per_head_AF", # not used
                            "Infrastructure_per_head_AM", # not used
                            "Infrastructure_per_head_Ox", # not used
                            "Labour_cost_head_AF", # not used
                            "Labour_cost_head_JF", # not used
                            "Labour_cost_head_SubAF", # not used
                            "Labour_cost_head_JM", # not used
                            "Labour_cost_head_SubAM", # not used
                            "Labour_cost_head_dairy", # not used
                            "Labour_cost_head_Oxen" # not used
  )
  )
  )

ParamNames <- read.csv("data/ahle_params_english_names.csv", encoding="UTF-8")
DataParameters <- merge(DataParameters, ParamNames, by="Parameter", all.x=T)

## Parameters presented for both scenarios
DataParametersBoth <- DataParameters %>% subset(Present=="Both")

### special processing for labour cost bcs not intuitive for display
temp_cost <- DataParametersBoth %>% subset(Parameter=="Labour_cost_head_AM")
temp_prop <- DataParametersBoth %>% subset(Parameter=="lab_non_health")

for (i in 1:4) {
temp_cost$Value[temp_cost$Species==temp_prop$Species[i]&temp_cost$Scenario==temp_prop$Scenario[i]] <- 
  temp_cost$Value[temp_cost$Species==temp_prop$Species[i]&temp_cost$Scenario==temp_prop$Scenario[i]] * 
    temp_prop$Value[i]
}

DataParametersBoth <- rbind(DataParametersBoth %>% subset(!(Parameter %in% c("Labour_cost_head_AM","lab_non_health"))), temp_cost)

# create the frame for the input selectors
myParametersBoth <- DataParametersBoth %>% select(Parameter, LongName) %>% unique 
myParametersBoth <- setNames(myParametersBoth$Parameter,myParametersBoth$LongName)

## Parameters presented for one scenario only because no change
DataParametersSingle <- DataParameters %>% subset(Present=="Ideal"&Scenario=="Ideal")
# create the frame for the input selectors
myParametersSingle <- DataParametersSingle %>% select(Parameter, LongName) %>% unique 
myParametersSingle <- setNames(myParametersSingle$Parameter,myParametersSingle$LongName)


# read the PPR dataset
DataParametersPPR <- read.csv("data/ahle_params_senegal_samples_ppr.csv")
DataParametersPPR <- DataParametersPPR %>% 
  filter(!(Parameter %in% c(
    "Health_exp_prev_JF", # same for all classes
    "Health_exp_prev_JM", # same for all classes
    "Health_exp_prev_SubAF", # same for all classes
    "Health_exp_prev_SubAM", # same for all classes
    "Health_exp_prev_AF", # same for all classes
    "Health_exp_prev_Ox" # same for all classes
  )
  )
  )
DataParametersPPR <- merge(DataParametersPPR, ParamNames, by="Parameter", all.x=T)
DataParametersPPR$Present <- NULL 
