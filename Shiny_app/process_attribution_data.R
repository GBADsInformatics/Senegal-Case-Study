##################
# file name: process_attribution_data.R
# Prepare the attribution data for dashboard display
# created by: Anne Meyer
# created on: 2023-11-12
##################

## read the dataset
attribution_summary <- read.csv('data/attribution_summary_disease.csv')

#categories in English
AHLECategoriesEN <- c("Mortality","Production loss")
SpeciesCategoriesEN <- c("Goats","Sheep")
Production.systemCategoriesEN <- c("Agropastoral")
Age.classCategoriesEN <- c("Juvenile","Subadult","Adult female","Adult male")
CauseCategoriesEN <- c("External","Non-infectious","Infectious")
DiseaseCategoriesEN <- c("Other causes","PPR")

attribution_summary$AHLE <- factor(attribution_summary$AHLE, levels=AHLECategoriesEN, labels=AHLECategoriesEN)
attribution_summary$Species <- factor(attribution_summary$Species, levels=SpeciesCategoriesEN, labels=SpeciesCategoriesEN)
attribution_summary$Production.system <- factor(attribution_summary$Production.system, levels=Production.systemCategoriesEN, labels=Production.systemCategoriesEN)
attribution_summary$Age.class <- factor(attribution_summary$Age.class, levels=Age.classCategoriesEN, labels=Age.classCategoriesEN)
attribution_summary$Cause <- factor(attribution_summary$Cause, levels=CauseCategoriesEN, labels=CauseCategoriesEN)
attribution_summary$Disease <- factor(attribution_summary$Disease, levels=DiseaseCategoriesEN, labels=DiseaseCategoriesEN)

# Levels can be ordered differently by assigning them below (from most internal = 1). Note: no "Production system" for Senegal case study
attribution_summary$Level1<- attribution_summary$Disease
attribution_summary$Level2<- attribution_summary$Age.class
attribution_summary$Level3<- attribution_summary$Species
attribution_summary$Level4<- attribution_summary$AHLE
attribution_summary$Level5<- attribution_summary$Cause

attribution_summary_l1 <- attribution_summary %>% mutate(Label=paste0(Level5,Level4,Level3,Level2,Level1), Text=Level1, Parent=paste0(Level5,Level4,Level3,Level2), Value=mean)
attribution_summary_l2 <- attribution_summary %>% group_by(Level5,Level4,Level3,Level2) %>% summarize(mean=sum(mean)) %>% mutate(Value=mean, Label=paste0(Level5,Level4,Level3,Level2), Text=Level2, Parent=paste0(Level5,Level4,Level3))
attribution_summary_l3 <- attribution_summary %>% group_by(Level5,Level4,Level3) %>% summarize(mean=sum(mean)) %>% mutate(Value=mean, Label=paste0(Level5,Level4,Level3), Text=Level3, Parent=paste0(Level5,Level4))
attribution_summary_l4 <- attribution_summary %>% group_by(Level5,Level4) %>% summarize(mean=sum(mean)) %>% mutate(Value=mean, Label=paste0(Level5,Level4), Text=Level4, Parent=Level5)
attribution_summary_l5 <- attribution_summary %>% group_by(Level5) %>% summarize(mean=sum(mean)) %>% mutate(Value=mean, Label=Level5, Text=Level5, Parent="AHLE")

treemap_data <- rbind(attribution_summary_l5[,c("Label","Parent","Value","Text")],attribution_summary_l4[,c("Label","Parent","Value","Text")],attribution_summary_l3[,c("Label","Parent","Value","Text")],attribution_summary_l2[,c("Label","Parent","Value","Text")],attribution_summary_l1[,c("Label","Parent","Value","Text")])

treemap_data$formatted_values <- ifelse(treemap_data$Value<(10^8),
 paste0(formatC(treemap_data$Value*10^-6,format = "f", digits = 1), " M"),
 paste0(formatC(treemap_data$Value*10^-9, format = "f", digits = 1), " B"))

