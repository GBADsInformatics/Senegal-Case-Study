##################
# file name: process_ahle_data.R
# Prepare the AHLE data for dashboard display
# created by: Anne Meyer
# created on: 2023-11-12
##################

## parameters
# number of runs of simulation
n_sim <- 10000
# the t-critical value based on the confidence level and (n1+n2-2) degrees of freedom:
t <- 1.96

# read the dataset
DataFull <- read.csv("data/model_results.csv")

## Flag costs and values
Costs <- c('Health Cost', 'Labour Cost', 'Feed Cost', 'Capital Cost', 'Infrastructure Cost')
Values <-c('Gross Margin', 'Value of Offtake', 'Value of Herd Increase', 'Value of Milk', 'Value of Hides', 'Value of Manure')
DataFull$Sign <- ifelse(DataFull$Item=='Gross Margin', 0, ifelse(DataFull$Item %in% Costs, -1, +1))


# ---------------------------------------------------------------------------
### 0. Initial population data for reference
DataParameters <- read.csv("data/ahle_params_senegal.csv")
InitialPopData <- DataParameters %>% 
  filter(Parameter %like% "t0" & Scenario=="Current") %>% 
  mutate(Scenario="Initial",Pop=Value) %>% 
  mutate(Group=recode(Parameter,
                      'N_NM_t0'='Juvenile Male', 
                      "N_NF_t0"='Juvenile Female',
                      "N_JM_t0"='Subadult Male',
                      "N_JF_t0"='Subadult Female',
                      "N_AM_t0"='Adult Male',
                      "N_AF_t0"='Adult Female')) %>% 
  select(Scenario,Species,Group,Pop)

BothInitialPopData <- InitialPopData %>% 
  group_by(Scenario, Group) %>%
  summarize(Pop=sum(Pop)) %>%
  ungroup %>%
  mutate(Species="Both") %>% 
  select(Scenario,Species,Group,Pop)

InitialPopData <- rbind(InitialPopData,BothInitialPopData)

InitialPopDataGrouped <- InitialPopData %>% 
  mutate(Group=recode(Group,
                      "Adult Female"="Adult Combined",
                      "Adult Male"="Adult Combined",
                      "Juvenile Female"="Juvenile Combined",
                      "Juvenile Male"="Juvenile Combined",
                      "Subadult Female"="Subadult Combined",
                      "Subadult Male"="Subadult Combined")) %>%
  group_by(Scenario, Species, Group) %>% 
  summarize(Pop=sum(Pop))

InitialPopDataOverall <- InitialPopData %>% 
  mutate(Group="Overall") %>%
  group_by(Scenario, Species, Group) %>% 
  summarize(Pop=sum(Pop))

InitialPopData <- rbind(InitialPopData,InitialPopDataGrouped,InitialPopDataOverall)

# ---------------------------------------------------------------------------
### 1. Plot Gross Margin + its elements as a dodged bar chart for Current and Ideal (Wudu's figure SVEPM page 150)

# Subset data to keep all groups only
groupval <- "Overall"
DataSubset<-DataFull[which(DataFull$Group == groupval),]
# Subset data for items of interest
itemval <- c('Gross Margin', 'Value of Offtake', 'Value of Herd Increase', 'Value of Milk', 'Value of Hides', 'Value of Manure','Health Cost', 'Labour Cost', 'Feed Cost') # not considered here: 'Capital Cost', 'Infrastructure Cost'
DataSubset<-DataSubset[which(DataSubset$Item %in% itemval),]
# Subset data for scenarios of interest
DataSubset <- subset(DataSubset, Scenario %in% c('Current','Ideal'))

DataSubset$ItemFactor <- factor(DataSubset$Item, levels = itemval)
DataSubset$ScenarioFactor <- factor(DataSubset$Scenario, levels = c('Current','Ideal'))

# Dodged bar plot with cost in negative and with mean and error bars, and M and B for large numbers
DataSubset$MeanSign <- ifelse(DataSubset$Sign==0,1,DataSubset$Sign)*DataSubset$Mean
DataSubset$MeanSignLabel <- ifelse(abs(DataSubset$Mean)<(10^8),paste0(formatC(DataSubset$MeanSign*10^-6,format = "f", digits = 1), " M"), paste0(formatC(DataSubset$MeanSign*10^-9, format = "f", digits = 1), " B"))


### 2. Plot Waterfall graph AHLE + its elements (as in dashboard))

# Create a difference dataframe 
DataDifference <- subset(DataSubset, Scenario=='Current', select=c('Item','Sign','ItemFactor','Species'))
DataDifference$MeanDiff <- NA 
DataDifference$MeanDiffStDev <- NA 

for (i in 1:nrow(DataDifference)) {
  m1 <- DataSubset$Mean[DataSubset$Scenario=='Ideal'&DataSubset$Item==DataDifference$Item[i]&DataSubset$Species==DataDifference$Species[i]]
  s1 <- DataSubset$StDev[DataSubset$Scenario=='Ideal'&DataSubset$Item==DataDifference$Item[i]&DataSubset$Species==DataDifference$Species[i]]
  m2 <- DataSubset$Mean[DataSubset$Scenario=='Current'&DataSubset$Item==DataDifference$Item[i]&DataSubset$Species==DataDifference$Species[i]]
  s2 <- DataSubset$StDev[DataSubset$Scenario=='Current'&DataSubset$Item==DataDifference$Item[i]&DataSubset$Species==DataDifference$Species[i]]
  DataDifference$MeanDiff[i] <- m1 - m2
  DataDifference$MeanDiffStDev[i] <- sqrt((s1^2) + (s2^2))                       
}

DataDifference$Scenario <- "Difference"
DataDifference$ItemFactor <- factor(DataDifference$Item, levels = itemval)
DataDifference$Color <- ifelse(DataDifference$Item=='Gross Margin',0,DataDifference$Sign)
ColorLabelEN <- c('AHLE','Revenue','Cost')
DataDifference$ColorLabel <- factor(DataDifference$Color, levels = c(0,1,-1), labels = ColorLabelEN)
DataDifference$tMeanDiffStDev <- t*DataDifference$MeanDiffStDev

DataDifference$MeanDiffLabel <- ifelse(
  abs(DataDifference$MeanDiff)<(10^8),
  paste0(formatC(DataDifference$MeanDiff*10^-6,format = "f", digits = 1), " M"), 
  paste0(formatC(DataDifference$MeanDiff*10^-9, format = "f", digits = 1), " B")
  )


### 3. Table Gross Margin + its elements for Current and Ideal

# Add the difference to the scenario dataframe
DataDifferenceTable <- DataDifference %>% select("Species","Item","Sign","MeanDiff","MeanDiffStDev","Scenario")
names(DataDifferenceTable) <- c("Species","Item","Sign","Mean","StDev","Scenario")
DataAll <- rbind(DataDifferenceTable, subset(DataSubset, select=c("Species","Item", "Sign", "Mean", "StDev", "Scenario")))

# Format items
DataAll$ItemFactor <- factor(DataAll$Item, levels = itemval)
DataAll$MeanLabel <- ifelse(abs(DataAll$Mean)<(10^8),
 paste0(formatC(DataAll$Mean*10^-6,format = "f", digits = 1), " M"),
 paste0(formatC(DataAll$Mean*10^-9, format = "f", digits = 1), " B"))
DataAll$Lower <- DataAll$Mean-t*DataAll$StDev
DataAll$Upper <- DataAll$Mean+t*DataAll$StDev
DataAll$LowerLabel <- ifelse(abs(DataAll$Lower)<(10^8),
 paste0(formatC(DataAll$Lower*10^-6,format = "f", digits = 1), " M"),
 paste0(formatC(DataAll$Lower*10^-9, format = "f", digits = 1), " B"))
DataAll$UpperLabel <- ifelse(abs(DataAll$Upper)<(10^8),
 paste0(formatC(DataAll$Upper*10^-6,format = "f", digits = 1), " M"),
 paste0(formatC(DataAll$Upper*10^-9, format = "f", digits = 1), " B"))
DataAll$MeanAndInterval <- paste0(DataAll$MeanLabel, " [", ifelse(DataAll$Lower<DataAll$Upper, DataAll$LowerLabel, DataAll$UpperLabel), "; ", ifelse(DataAll$Lower<DataAll$Upper,DataAll$UpperLabel, DataAll$LowerLabel), "]")

DataAllTable <- DataAll %>% ungroup %>%
    select(Species, Scenario, ItemFactor, Sign, MeanAndInterval)  %>% 
    pivot_wider(names_from="Scenario", values_from=c("MeanAndInterval"))

DataAllTable <- DataAllTable %>% mutate(Contribution=recode(Sign, "0" = "-", "-1" = "Cost", "1" = "Revenue")) %>% arrange(Contribution)


### 4. Table AHLE by category animal

# Split AHLE in three:
# Animal Health Expenditure (AHE) = Health Cost under Current scenario
# AHLE due to mortality = GM(ZeroMMort, without health costs) - GM(Current)
# AHLE due to morbidity = GM(Ideal) - GM(ZeroMMort, without health costs)
groupval <- c('Juvenile Combined', 'Subadult Combined', 'Adult Combined','Overall')
AHLEData <- read.csv("data/ahle_components.csv")
AHLEData <- AHLEData %>% 
  subset(Group %in% groupval) %>% 
  mutate(AHLE=recode(AHLE,"Mortality"="mort","Production loss"="morb","Health expenditure"="AHE", "Total"="tot")) %>% 
  pivot_wider(names_from="AHLE", values_from=c("Mean","SD"))

PopAHLEData <- subset(InitialPopData, Group %in% groupval, select=c('Species','Group','Pop'))
AHLEData  <- merge(AHLEData,PopAHLEData, by=c('Species','Group'))

AHLEData <- AHLEData %>% 
  mutate(AHLE_tot_low=Mean_tot-t*SD_tot,
         AHLE_tot_up=Mean_tot+t*SD_tot,
         AHLE_morb_perc=ifelse(Mean_morb<0|Mean_mort<0, NA,Mean_morb/Mean_tot),
         AHLE_mort_perc=ifelse(Mean_morb<0|Mean_mort<0, NA,Mean_mort/Mean_tot),
         AHE_perc=ifelse(Mean_morb<0|Mean_mort<0,NA,Mean_AHE/Mean_tot),
         AHLE_per_head=paste0(round(Mean_tot/Pop,0), ' FCFA'),
         AHLE_per_head_usd=paste0(round(Mean_tot/(Pop*600),0), ' USD'),
         AHLE_per_head_low=paste0(round(AHLE_tot_low/Pop,0), ' FCFA'),
         AHLE_per_head_up=paste0(round(AHLE_tot_up/Pop,0), ' FCFA'),
         AHLE_per_head_interval=paste0("[",AHLE_per_head_low,"; ",AHLE_per_head_up,"]"),
         pop=paste0(formatC(Pop*10^-6,format = "f", digits = 2), " M"),
         AHLE_tot_simple=ifelse(abs(Mean_tot)<(10^8),
                                paste0(formatC(Mean_tot*10^-6,format = "f", digits = 1), " M"),
                                paste0(formatC(Mean_tot*10^-9, format = "f", digits = 1), " B")),
         AHLE_tot_low_simple=ifelse(abs(AHLE_tot_low)<(10^8),
                                paste0(formatC(AHLE_tot_low*10^-6,format = "f", digits = 1), " M"),
                                paste0(formatC(AHLE_tot_low*10^-9, format = "f", digits = 1), " B")),
         AHLE_tot_up_simple=ifelse(abs(AHLE_tot_up)<(10^8),
                                    paste0(formatC(AHLE_tot_up*10^-6,format = "f", digits = 1), " M"),
                                    paste0(formatC(AHLE_tot_up*10^-9, format = "f", digits = 1), " B")),
         AHLE_tot_interval=paste0("[",AHLE_tot_low_simple,"; ",AHLE_tot_up_simple,"]"),
         AHLE_mort_simple=ifelse(Mean_mort<(10^8),
                                 paste0(formatC(Mean_mort*10^-6,format = "f", digits = 1), " M"),
                                 paste0(formatC(Mean_mort*10^-9, format = "f", digits = 1), " B")),
         AHLE_morb_simple=ifelse(Mean_morb<(10^8),
                                 paste0(formatC(Mean_morb*10^-6,format = "f", digits = 1), " M"),
                                 paste0(formatC(Mean_morb*10^-9, format = "f", digits = 1), " B")),
         AHE_simple=ifelse(Mean_AHE<(10^8),
                           paste0(formatC(Mean_AHE*10^-6,format = "f", digits = 1), " M"),
                           paste0(formatC(Mean_AHE*10^-9, format = "f", digits = 1), " B"))
  )

# Adapt labels and factoring
GroupLabel <- c('Juveniles','Subadults','Adults','All groups')
AHLEData$GroupFactor <- factor(AHLEData$Group, levels = groupval, labels = GroupLabel)
AHLEData <- arrange(AHLEData, GroupFactor)


### ---------------------------------------------------------------------------------------------------------------------------------------------------
### 5. AHLE composition as a donut

DonutData <- subset(AHLEData, Group == 'Overall', select = c('Disease','Species','AHLE_mort_perc','AHLE_morb_perc','AHE_perc')) 
DonutData <- DonutData %>% pivot_longer(AHLE_mort_perc:AHE_perc, names_to='category', values_to='perc')

DonutData <- DonutData %>% mutate(categoryName = recode(category,
  AHLE_mort_perc = "Mortality losses",
  AHLE_morb_perc = "Production losses",
  AHE_perc = "Animal health expenditure"))


### ---------------------------------------------------------------------------------------------------------------------------------------------------
### 6.Population graph

groupval <- c('Juvenile Male', 'Juvenile Female','Subadult Male', 'Subadult Female','Adult Male','Adult Female')
PopData <- DataFull %>% 
  subset(Item=='Population' & Group %in% groupval & Scenario %in% c('Current', 'Ideal')) %>% 
  mutate(Pop=Mean) %>%
  select(Scenario,Species,Group,Pop)

BasePopData <- InitialPopData %>% filter(Group %in% groupval)

PopData <- rbind(PopData,BasePopData)

groupvalEN <- c('Juvenile males', 'Juvenile females','Subadult males', 'Subadult females','Adulte males','Aulte females')
PopData$Group <- factor(PopData$Group, levels=groupval, labels=groupvalEN)
PopData$Scenario <- factor(PopData$Scenario, levels=c("Initial","Current","Ideal"))

GrowthData <- PopData %>% 
  group_by(Scenario,Species) %>% 
  summarize(Total=sum(Pop)) %>% 
  group_by(Species) %>%
  mutate(Total_init=Total[Scenario=="Initial"]) %>% 
  filter(Scenario!="Initial") %>%
  mutate(Growth=paste0(round(100*(Total-Total_init)/Total_init,0),"%"))

