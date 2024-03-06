##################
# file name: process_parameters.R
# Prepare the parameters for user display
# created by: Anne Meyer
# created on: 2023-11-07
##################

# read the dataset
DataParameters <- read.csv("data/ahle_params_senegal.csv")
ParamNames <- read.csv("data/ahle_params_english_names.csv", encoding="UTF-8")
DataParameters <- merge(DataParameters, ParamNames, by="Parameter", all.x=T)

## Parameters presented for both scenarios
DataParametersBoth <- DataParameters %>% subset(Present=="Both")

### special processing for labour cost bcs not intuitive for display
temp_cost <- DataParametersBoth %>% subset(Parameter=="Lab_SR")
temp_prop <- DataParametersBoth %>% subset(Parameter=="lab_non_health")

for (i in 1:4) {
temp_cost$Value[temp_cost$Species==temp_prop$Species[i]&temp_cost$Scenario==temp_prop$Scenario[i]] <- 
  temp_cost$Value[temp_cost$Species==temp_prop$Species[i]&temp_cost$Scenario==temp_prop$Scenario[i]] * 
    temp_prop$Value[i]
}

DataParametersBoth <- rbind(DataParametersBoth %>% subset(!(Parameter %in% c("Lab_SR","lab_non_health"))), temp_cost)

# create the frame for the input selectors
myParametersBoth <- DataParametersBoth %>% select(Parameter, LongName) %>% unique 
myParametersBoth <- setNames(myParametersBoth$Parameter,myParametersBoth$LongName)

## Parameters presented for one scenario only because no change
DataParametersSingle <- DataParameters %>% subset(Present=="Ideal"&Scenario=="Ideal")
# create the frame for the input selectors
myParametersSingle <- DataParametersSingle %>% select(Parameter, LongName) %>% unique 
myParametersSingle <- setNames(myParametersSingle$Parameter,myParametersSingle$LongName)


# read the PPR dataset
DataParametersPPR <- read.csv("data/ahle_params_senegal_ppr.csv")
DataParametersPPR <- merge(DataParametersPPR, ParamNames, by="Parameter", all.x=T)
DataParametersPPR$Present <- NULL 
