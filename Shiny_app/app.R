##################
# file name: app.R
# created by: Anne Meyer
# created on: 2023-11-12
##################

## packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(plotly)
library(flextable)
library(freedom)
library(data.table)
library(ggrepel)
library(shinydashboard)

## helper functions
format_scientific <- function(x) {
  formatC(x, format = "e", digits = 1)
}

format_percent <- function(x) {
  ifelse(is.na(x),x,paste0(round(x*100,1), ' %'))
}

## data processing
source("process_ahle_data.R")
source("process_attribution_data.R")
source("process_parameters.R")

## Shiny app

# Define UI for the application
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "GBADs case study in Senegal: burden of disease in small ruminants in the mixed crop livestock sector", titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User guide", tabName = "tab1", icon = icon("file-lines")),
      menuItem("Gross margin", tabName = "tab2", icon = icon("bar-chart")),
      menuItem("Scenario differences", tabName = "tab3", icon = icon("bar-chart")),
      menuItem("AHLE", tabName = "tab4", icon = icon("bar-chart")),
      menuItem("Types of AHLE", tabName = "tab5", icon = icon("bar-chart")),
      menuItem("Attribution", tabName = "tab6", icon = icon("bar-chart")),
      menuItem("Herd size", tabName = "tab7", icon = icon("bar-chart")),
      menuItem("Input parameters (1)", tabName = "tab8", icon = icon("bar-chart")),
      menuItem("Input parameters (2)", tabName = "tab9", icon = icon("bar-chart")),
      menuItem("Input parameters (3)", tabName = "tab10", icon = icon("bar-chart")),
      menuItem("Back to GBADs dashboards", href = "https://gbadske.org/dashboards/", icon = icon("home"))
    ) 
  ),
  dashboardBody(
      tabItems(
        tabItem(tabName = "tab1",
              tags$h2("Context"),
              tags$body("This dashboard presents the results of a case study conducted in 2023 within the GBADs programme. This study aimed at utilising existing data related to livestock production and health in Senegal to estimate the burden of disease in small ruminants. We used a Dynamic Population Model (DPM) to estimate the Animal Health Loss Envelope (AHLE) for small ruminants in the mixed-crop livestock sector specifically.",tags$br()),
              tags$body(HTML("<a href='https://gbadske.org/dashboards/ahle-casestudy/'>More information on the analytical approach can be found here.</a>")),
              tags$h2("Stakeholders"),
              tags$body("The work presented here was funded by the International Development Research Centre in Canada. It was coordinated by the World Organization for Animal Health and implemented by the University of Liverpool, the Institut Sénégalais de Recherches Agricoles and the Direction des Services Vétérinaires du Ministère de l'Elevage et des Productions Animales, with the support of Murdoch University and the Ecole Inter-Etats des Sciences et Médecine Vétérinaires.",tags$br()),
              tags$image(src="logos.jpg", width="65%", height="auto"),
              tags$h2("Navigation"),
              tags$body("All results are presented for sheep and goats separately, as well as for both species combined. The species of interest can be selected in each tab via a drop-down list.",tags$br(),tags$br(),"All parameters and prices were aligned on year 2022.",tags$br(),tags$br(),"The results can be explored using the different tabs in the sidebar menu:"),
              tags$body(
                tags$ul(
                  tags$li(tags$span("Gross margin:", style = "text-decoration: underline;"), "this tab displays the annual gross margin estimated by the DPM for each scenario and species, as well as the different components of the partial budget analysis."),
                  tags$li(tags$span("Scenario differences:", style = "text-decoration: underline;"), "the differences between the ideal and the current scenarios are displayed here, for the gross margin and each of its components of revenue and costs."),
                  tags$li(tags$span("AHLE:", style = "text-decoration: underline;"), "this tab shows the estimated animal health loss envelope, for all causes combined and for Peste des Petits Ruminants specifically."),
                  tags$li(tags$span("Types of AHLE:", style = "text-decoration: underline;"), "this tabs shows the distribution of the AHLE between three types of losses: mortality, production losses and animal health expenditure."),
                  tags$li(tags$span("Attribution:", style = "text-decoration: underline;"), "this tab shows a breakdown of the AHLE into its mortality and production loss components and attributes these to high-level causes (infectious, non-infectious, or external), and to Peste des Petits Ruminants specifically."),
                  tags$li(tags$span("Herd size:", style = "text-decoration: underline;"), "this tab shows the livestock population used in the DPM and its predicted annual growth rate."),
                  tags$li(tags$span("Input parameters:", style = "text-decoration: underline;"), "these three tabs display the values of the input parameters used in the DPM, for the different species and scenarios.")
                )
              )
      ),
      tabItem(tabName = "tab2",
              selectInput("speciesP1", "Select a species", c("Sheep","Goats","Both")),
              h3("Annual gross margin by scenario for the selected species"),
              plotlyOutput("plot1")
      ),
      tabItem(tabName = "tab3",
              selectInput("speciesT1P2", "Select a species",
                          c("Sheep","Goats","Both")),
              h3("Difference between ideal and current scenarios for the selected species"),
              plotlyOutput("plot2"),
              fluidRow(uiOutput("table1"))
      ),
      tabItem(tabName = "tab4",
              selectInput("speciesT2", "Select a species",
                          c("Sheep","Goats","Both")),
              h3("Distribution of the AHLE by age groups: all causes"),
              uiOutput("table2a"),
              h3("Distribution of the AHLE by age groups: PPR only"),
              uiOutput("table2b")
      ),
      tabItem(tabName = "tab5",
              selectInput("speciesT5P3", "Select a species",c("Sheep","Goats","Both")),
              selectInput("DiseaseT5P3", "Cause(s) to consider",c("All causes" = "All","PPR only" = "PPR")),
              h3("Distribution of the AHLE by type of losses for the selected species"),
              plotOutput("plot3"),
              fluidRow(uiOutput("table5"))
      ),
      tabItem(tabName = "tab6",
              h3("Attribution of mortality and production losses (expert data)"),
              h4("Animal health expenditure is not accounted for in this graph."),
              plotlyOutput("plot6")
      ),
      tabItem(tabName = "tab7",
              selectInput("speciesP7", "Select a species", c("Sheep","Goats","Both")),
              h4("Population data used in the model: initial values and end-of-year values by scenario"), 
              plotlyOutput("plot7"),
              uiOutput("table4")
      ),
      tabItem(tabName = "tab8",
              selectInput("speciesP4", "Select a species",c("Sheep","Goats")),
              h3("Value of parameters which vary by species and by scenario"),
              plotOutput("plot4")
      ),
      tabItem(tabName = "tab9",
              h3("Value of parameters which do not vary by species and by scenario (point estimates)"),
              uiOutput("table3"),
              h3("Value of parameters which do not vary by scenario (distributions)"),
              plotOutput("plot5")
      ),
      tabItem(tabName = "tab10",
              h3("Value of parameters used to assess the PPR-specific AHLE"),
              h4("All parameters below refer to the incidence of PPR specifically."),
              plotOutput("plot8")
      )
      )
      ),
  tags$head(tags$style(HTML("
    .main-header .logo {
      background-color: orange;
      color: black;
    } ")))
  )


server <- function(input, output) {

  output$plot1 <- renderPlotly({
    plot1 <- plot_ly() %>% 
      add_trace(data = DataSubset %>% filter(Species==input$speciesP1&ScenarioFactor=="Ideal"), y=~ItemFactor, x=~MeanSign, type='bar', color=I('#997700'), name="Ideal", error_x=list(array=~t*StDev, color="#a7a5a5", thickness=1)) %>% 
      add_trace(data = DataSubset %>% filter(Species==input$speciesP1&ScenarioFactor=="Current"), y=~ItemFactor, x=~MeanSign, type='bar', color=I('#eecc66'), name="Current", error_x=list(array=~t*StDev, color="#a7a5a5", thickness=1)) %>% 
      layout(title=" ", 
             yaxis=list(title=""), 
             xaxis=list(title="FCFA"), 
             legend=list(title=list(text="Scenario")),
             yaxis = list(hoverformat = ".2f"))
})
 
output$plot2 <- renderPlotly({
   plot2 <- plot_ly() %>% 
     add_trace(data = DataDifference %>% filter(Species==input$speciesT1P2&Color==0), x=~ItemFactor, y=~MeanDiff, type='bar', color=I('#fc9272'), name=ColorLabelEN[1], error_y=list(array=~tMeanDiffStDev, color="#a7a5a5", thickness=1)) %>% 
     add_trace(data = DataDifference %>% filter(Species==input$speciesT1P2&Color==1), x=~ItemFactor, y=~MeanDiff, type='bar', color=I('#fee0d2'), name=ColorLabelEN[2], error_y=list(array=~tMeanDiffStDev, color="#a7a5a5", thickness=1)) %>% 
     add_trace(data = DataDifference %>% filter(Species==input$speciesT1P2&Color==-1), x=~ItemFactor, y=~MeanDiff, type='bar', color=I('#a6bddb'), name=ColorLabelEN[3], error_y=list(array=~tMeanDiffStDev, color="#a7a5a5", thickness=1)) %>%
     layout(title=" ", 
            xaxis=list(title=""), 
            yaxis=list(title="Difference ideal minus current (FCFA)"), 
            legend=list(title=list(text="Contribution to the AHLE")),
            yaxis = list(hoverformat = ".2f")) 
 })
 
 
 output$table1 <- renderUI({

   return(
     DataAllTable %>% filter(Species==input$speciesT1P2) %>% 
     flextable(col_keys= c("ItemFactor", "Contribution", "Current", "Ideal", "Difference")) %>% 
     theme_booktabs() %>%
     bg(i = ~Sign == 0, bg='#fc9272') %>% 
     bg(i = ~Sign == -1, bg='#a6bddb') %>%
     bg(i = ~Sign == 1, bg='#fee0d2')  %>%
     width(j = 1:5, 2.5) %>% 
     set_header_labels(ItemFactor = "Category",
                         Contribution = "Contribution to the AHLE", 
                         Current = "Current scenario",
                         Ideal = "Ideal scenario",
                         Difference = "Difference ideal minus current") %>%
      htmltools_value())
 })
 
 output$table2a <- renderUI({
   return(
       AHLEData %>% filter(Species==input$speciesT2&Disease=="All") %>% 
       flextable(col_keys= c("GroupFactor", "pop", "AHLE_tot_simple","AHLE_tot_low_simple","AHLE_tot_up_simple", "AHLE_per_head","AHLE_per_head_low","AHLE_per_head_up", "AHLE_per_head_usd")) %>% 
       theme_booktabs() %>% 
       bg(i = ~Group == "Overall", bg='#fc9272') %>% 
       set_header_labels(GroupFactor = "Category",
                         pop = "Population",
                         AHLE_tot_simple = "Mean", 
                         AHLE_tot_low_simple = "2.5%",
                         AHLE_tot_up_simple = "97.5%",
                         AHLE_per_head = "Mean", 
                         AHLE_per_head_low = "2.5%",
                         AHLE_per_head_up = "97.5%",
                         AHLE_per_head_usd = "USD equivalent") %>%
       add_header_row(top = TRUE, values = c("Category","Population","Total AHLE","Total AHLE","Total AHLE", "AHLE per head", "AHLE per head", "AHLE per head","AHLE per head"))  %>%
       merge_v(part = "header") %>% 
       merge_h(part = "header") %>% 
       htmltools_value()     
   ) 
 })

 output$table2b <- renderUI({
   return(
     AHLEData %>% filter(Species==input$speciesT2&Disease=="PPR") %>% 
       flextable(col_keys= c("GroupFactor", "pop", "AHLE_tot_simple","AHLE_tot_low_simple","AHLE_tot_up_simple", "AHLE_per_head","AHLE_per_head_low","AHLE_per_head_up", "AHLE_per_head_usd")) %>% 
       theme_booktabs() %>% 
       bg(i = ~Group == "Overall", bg='#fc9272') %>% 
       set_header_labels(GroupFactor = "Category",
                         pop = "Population",
                         AHLE_tot_simple = "Mean", 
                         AHLE_tot_low_simple = "2.5%",
                         AHLE_tot_up_simple = "97.5%",
                         AHLE_per_head = "Mean", 
                         AHLE_per_head_low = "2.5%",
                         AHLE_per_head_up = "97.5%",
                         AHLE_per_head_usd = "Equivalent USD") %>%
       add_header_row(top = TRUE, values = c("Category","Population","Total AHLE","Total AHLE","Total AHLE", "AHLE per head", "AHLE per head", "AHLE per head","AHLE per head"))  %>%
       merge_v(part = "header") %>% 
       merge_h(part = "header") %>% 
       htmltools_value()     
   ) 
 })
 
 output$table5 <- renderUI({
   return(
     AHLEData %>% filter(Species==input$speciesT5P3&Disease==input$DiseaseT5P3) %>% 
       flextable(col_keys= c("GroupFactor", "pop", "AHLE_tot_simple", "AHLE_mort_simple", "AHLE_mort_perc", "AHLE_morb_simple", "AHLE_morb_perc", "AHE_simple", "AHE_perc")) %>% 
       theme_booktabs() %>% 
       set_formatter(AHLE_mort_perc = format_percent,
                     AHLE_morb_perc = format_percent,
                     AHE_perc = format_percent
       ) %>%
       bg(i = ~Group == "Overall", bg='#fc9272') %>% 
       set_header_labels(GroupFactor = "Category",
                         pop = "Population",
                         AHLE_tot_simple = "Total AHLE", 
                         AHLE_mort_simple = "FCFA", 
                         AHLE_mort_perc = "%",
                         AHLE_morb_simple = "FCFA", 
                         AHLE_morb_perc = "%",
                         AHE_simple = "FCFA", 
                         AHE_perc = "%") %>%
       add_header_row(top = TRUE, values = c("Category","Population","Total AHLE","Mortality losses","Mortality losses","Production losses","Production losses", "Animal health expenditure", "Animal health expenditure"))  %>%
       merge_v(part = "header") %>% 
       merge_h(part = "header") %>% 
       htmltools_value()
   ) 
 })
 
 output$plot3 <- renderPlot({
   
   DonutDataPlot <- DonutData %>% filter(Species==input$speciesT5P3&Disease==input$DiseaseT5P3)
   DonutDataPlot$ymax <- cumsum(DonutDataPlot$perc)
   DonutDataPlot$ymin <- c(0, head(DonutDataPlot$ymax, n=-1))
   DonutDataPlot$labelPosition <- (DonutDataPlot$ymax + DonutDataPlot$ymin) / 2
   DonutDataPlot$label <- paste0(DonutDataPlot$categoryName, ": ", round(100*DonutDataPlot$perc,1), "%")
   
   ggplot(DonutDataPlot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
   geom_rect() +
   geom_label_repel( x=3.5, aes(y=labelPosition, label=str_wrap(label, width=16)), size=6) +
   scale_fill_brewer(palette=1) +
   coord_polar(theta="y") +
   xlim(c(2, 4)) +
   theme_void() +
   theme(legend.position = "none")
 })
 
 output$plot4 <- renderPlot({
   DataParametersBothPlot <- DataParametersBoth %>% filter(Species==input$speciesP4) %>% mutate(Scenario=factor(Scenario,levels=c("Ideal","Current")))
   ggplot(DataParametersBothPlot %>% uncount(weight=n), aes(x=Value, group=Scenario, fill=Scenario)) +
     geom_density(alpha=.7) +
     scale_fill_manual(values=c("#997700","#eecc66")) +
     facet_wrap(~LongName, ncol=3, scales="free", labeller = label_wrap_gen()) +
     labs(x="Parameter value", y="", fill="Scenario")+
     theme_bw() +
     theme(text = element_text(size = 20))}, height = 2000
   ) 
 
 output$plot5 <- renderPlot({
   DataParametersSinglePlot <- DataParametersSingle %>% filter(Type=="Distribution")
   ggplot(DataParametersSinglePlot %>% uncount(weight=n), aes(x=Value, group=Species, fill=Species)) +
     geom_density(alpha=.5) +
     scale_fill_manual(values=c("#ee99aa","#994455")) +
     facet_wrap(~LongName, ncol=3, scales="free", labeller = label_wrap_gen()) +
     labs(y="Parameter value", x="", fill="Species")+
     theme_bw() +
     theme(text = element_text(size = 20))}, height = 2000
   )  
 
  output$plot8 <- renderPlot({
   ggplot(DataParametersPPR %>% uncount(weight=n), aes(x=Value, group=Species, fill=Species)) +
     geom_density(alpha=.5) +
     scale_fill_manual(values=c("#ee99aa","#994455")) +
     facet_wrap(~LongName, ncol=3, scales="free", labeller = label_wrap_gen()) +
     labs(y="Parameter value", x="", fill="Species")+
     theme_bw() +
     theme(text = element_text(size = 20))}, height = 2000
 )
 
 output$table3 <- renderUI({
   return(
     DataParametersSingle %>% filter(Type=="Point"&Species=="Goats") %>% 
       flextable(col_keys= c("LongName", "Value")) %>% 
       theme_box() %>% 
       set_header_labels(LongName = "Parameter", 
                         Value = "Input value") %>%
       merge_v(j= "LongName") %>%
       autofit() %>%
       bg(i = ~ Parameter %in% c("Beta","CullM","hides_rate_mor","Man_J","prpn_feed_paid_for"), bg = "#a7a5a5", part = "body") %>%
       htmltools_value()
   ) 
 })

 output$plot6 <- renderPlotly({
   plot6 <- plot_ly(
    type='treemap',
    branchvalues="total",
    ids=treemap_data$Label,
    labels=treemap_data$Text,
    parents=treemap_data$Parent,
    values= treemap_data$Value,
    hovertext = treemap_data$formatted_values,
    hovertemplate = "Attribution= %{label}<br>Value= %{hovertext} FCFA") %>%
    layout(colorway=c("darkgreen","darkred","darkblue"))
 })

   output$plot7 <- renderPlotly({
     PopDataPlot <- PopData %>% filter(Species==input$speciesP7)
     plot7 <- plot_ly() %>% 
       add_trace(data = PopDataPlot, x=~Group, y=~Pop, type='bar', color=~Scenario, colors=c("#a7a5a5","#eecc66","#997700"), hoverinfo = 'text', text = ~round(Pop,0)) %>% 
       layout(bargap = 0.1,
              barmode = "dodge",
              title=" ", 
              xaxis=list(title=" "), 
              yaxis=list(title="Number of head"),
              legend=list(title="Scenario")) 
     
 })
   
   output$table4 <- renderUI({
     return(
       GrowthData %>% 
         flextable(col_keys= c("Scenario", "Species", "Total_init", "Total", "Growth")) %>% 
         theme_booktabs() %>%
         bg(i = ~Scenario == "Current", bg="#eecc66") %>% 
         bg(i = ~Scenario == "Ideal", bg="#997700") %>%
         set_header_labels(Scenario = "Scenario",
                           Species = "Species", 
                           Total_init = "Initial population",
                           Total = "Final population",
                           Growth = "Annual growth rate") %>%
         htmltools_value())
   })
}

shinyApp(ui, server)
