
library(leaflet)
library(plotly)
library(shinythemes)
# Panel 1 - World Map 
world_panel <- tabPanel(
    "World Map",
    titlePanel("World Map of Health Statistics"),
    p(),
    
    fluidPage(
        leafletOutput("worldMap"),
        h2(textOutput("titleWorld")),
        em(textOutput("sexTitle")),
        p(),
        selectInput("datasetMap", "Choose a health topic",
                    choices = c("Neonatal Mortality Rate","Maternal Mortality Ratio",
                                "Infant Mortality Rate","Birth Attended By Skilled Personnel",
                                "Adolescent Birth Rate", 
                                "Basic Sanitization Services", "Basic Drinking Water Services",
                                "Crude Suicide Rates",
                                "Incedence of Malaria", "Incedence of Tuberculosis",
                                "Life Expectancy at Birth", "Medical Doctors",
                                "New HIV Infections", "Nursing and Midwife",
                                "Pharmacists", "Health Expenditure > 10% income",
                                "Universal Health Care Coverage"),
                    selected = "Life Expectancy at Birth"),
        
        selectInput("worldYear",
                    label = "Choose a year",
                    choices = c("Choose" = ""))
    ))

#Panel 2 - Country/Region Map 
#select_values <- unique(suicide$Location)

sidebar_content <- 
    sidebarPanel(
    selectInput("dataset", "Choose a health topic",
                choices = c("Neonatal Mortality Rate","Maternal Mortality Ratio",
                            "Infant Mortality Rate","Birth Attended By Skilled Personnel",
                            "Adolescent Birth Rate", 
                            "Basic Sanitization Services", "Basic Drinking Water Services",
                            "Crude Suicide Rates", 
                            "Incedence of Malaria", "Incedence of Tuberculosis",
                            "Life Expectancy at Birth", "Medical Doctors",
                            "New HIV Infections", "Nursing and Midwife",
                            "Pharmacists", "Health Expenditure > 10% income",
                            "Universal Health Care Coverage"),
                selected = "Life Expectancy at Birth"),
    
    selectInput("varCountry",
                label = "Choose a country to display",
                choices = c("Choose" = ""))
    )


main_content <- mainPanel(
    plotlyOutput("plot")
)


country_panel <- tabPanel(
    "Country/Region Plot",
    titlePanel("World Health Statistics by Country"),
    fluidPage(
        h2(textOutput("titleCountry")),
    sidebarLayout(
        sidebar_content, main_content)
    )
)

# panel 3 -- continent plot 
continent_panel <- 
    tabPanel("Continent Plot", 
             titlePanel("World Health Statistics by Continent"),
             fluidPage(
                 h2(textOutput("titleContinent")),
             sidebarLayout(
                 sidebarPanel(selectInput("datasetContinent", "Choose a health topic",
                             choices = c("Neonatal Mortality Rate","Maternal Mortality Ratio",
                                         "Infant Mortality Rate","Birth Attended By Skilled Personnel",
                                         "Adolescent Birth Rate", 
                                         "Basic Sanitization Services", "Basic Drinking Water Services",
                                         "Crude Suicide Rates", 
                                         "Incedence of Malaria", "Incedence of Tuberculosis",
                                         "Life Expectancy at Birth", "Medical Doctors",
                                         "New HIV Infections", "Nursing and Midwife",
                                         "Pharmacists", "Health Expenditure > 10% income",
                                         "Universal Health Care Coverage"),
                             selected = "Life Expectancy at Birth")),
             mainPanel(plotlyOutput("continent_plot"))
             )))
# panel 4 -- data source 
data_panel <- tabPanel(
    "Data",
    titlePanel("Data Source of Health Statistics"),
    #p() for discription
    tags$h4("Data Source"), 
    p("The data used to create this Shiny app is published by World Health Organizations and organized as Kaggle World Health Statistics by Zeus.
      The country shapefile is obtained from GADM, which is free to use for academic work."), 
    tags$b("Location Shapefiles:"), tags$a(href = "https://gadm.org/", "GADM"), tags$br(),
    tags$b("WHO World Health Statistics:"),tags$a(href = "https://www.who.int/data/gho/data/themes/topics/topic-details/GHO/world-health-statistics", "WHO"), tags$br(),
    tags$b("Kaggle World Health Statistics:"), tags$a(href = "https://www.kaggle.com/utkarshxy/who-worldhealth-statistics-2020-complete?select=incedenceOfMalaria.csv", "Kaggle"),
    tags$br(), tags$br(),
    tags$h4("Map Footnote"), 
    p("1. Since there were inconsistencies on when data was collected for each country, certain countries may display NA values for several years in a row. This is solely due to 
      said countries having no data collected on a given year and has no bearing on whether they have data for other years or not."),
    p("2. South Sudan's data was omitted from the maps entirely due to constraints with outdated country divisions 
    in the geographic shapefile. This data is still available through the alternative visualizations in the dashboard."),
    p("3. Disputed territories (e.g.: Western Sahara) were not labeled by any claimants to our best knowledge because we cannot be sure if their data was included at all."),
    p("4. Most dependent or overseas territories (e.g.: Puerto Rico, Greenland) were not labeled by their sovereign states to our best knowledge because we cannot 
    be sure if the sovereign states’ data pulled from each dependency due to their varying levels of autonomy."),
    p("5. French Guiana, Guadeloupe, Martinique, Mayotte, and Reunion were labeled as France as the sole exception to the previous note. Since these overseas 
    departments/regions are considered to hold the same status as mainland France, it is likely that the data collected for France also pulled from each territory."),
    p("6. The former Yugoslav Republic of Macedonia was relabeled as North Macedonia in accordance with a naming dispute being resolved within recent years, even though 
      the WHO data has not reflected these changes."),
    p("see reference in final write-up")
    
)


about_panel <- tabPanel(
    "About",
    titlePanel("About the Project and Descriptions"),
    tags$h4("Background"),
    "With the increasing noted prevalence and importance of world health, it has become vital to have accurate and understandable information accessible 
    to everyone. A huge barrier for health/medical services stems from stigmatization as well as the dispersal of false information/misunderstandings. Our project hoped 
    to address these communication issues by creating an interactive visualization dashboard that allows users to explore provided health topics and regions at your discretion.",
    tags$br(), tags$br(),
    tags$h4("Reports"),
    tags$h5("Health Patterns"),
    strong("How do the health patterns change throughout time? Are nations that were not struggling with easily preventable diseases now struggling with and what countries are there and why?"),
p("The current global health suggested that, overall, fewer people are dying from communicable and non-communicable diseases, especially in low- and middle-income countries. More people are 
gaining access to clean water and basic sanitation services. Worldwide, there is also a decreasing trend in maternal and infant mortality rates, accompanied by an increase in skilled health 
professionals density. Although the trend on world health looks promising, there are still differences between high-income areas vs. middle- and low-income areas.  Continent-wise, Europe 
tends to outperform the rest of the continents, by having the lowest incidence of communicable disease, lowest mortality rate, and the highest rate of access to professional health personnel.  
On the other hand, despite the positive trend in world health, certain countries still face health challenges. For example, countries like Afghanistan and the Philippines are currently experiencing 
a surge in HIV infections since 2000."),
tags$h5("Biological Sex Gap"),
strong("Considering areas with less health personnel as well as biological sex (F/M) - Is there a biological sex gap with health issues around the world?"),
p("Overall, countries across the world have seen a decrease in infant mortality and HIV infections as well as an increase in life expectancy since birth. 
However, there still exists a gap between sexes regarding health outcomes. In particular, males tend to experience a higher infant mortality rate, lower life 
expectancy, and higher HIV infections in comparison to females. Notably, such sex disparity appears to be more salient in countries with less health personnel. 
For example, in Chad, where only around 20% of the births were attended by professional health personnel, there is a larger gap in infant mortality rate between 
males and females than there is in Argentina, where the rate of births attended by professionals is around 95%. On the other hand, we acknowledge that the current 
health system could disproportionally put women and non-binary people at disadvantage. That said, the datasets we have did not collect gender identity, so we could 
not assess these important health disparities in this report.
"),
tags$h5("Future Directions"),
strong("What may be important courses of action to consider for medical personnel and local governments in order to ensure good public health/safety?"),
p("One way to ensure good public health and safety would be to increase national and international investment and support in research aimed specifically at improving health services coverage within 
and between countries, especially for low- and middle-income countries that have lots of health burdens. It is critical to encourage close collaborations between policymakers and researchers to adapt 
scientific findings into public health programs. In addition, health organizations around the world should continue building global and national research networks to coordinate research efforts. Finally, 
local governments should identify regions of high risk and take special measures to protect these population groups, taking into consideration their specific circumstances."),
strong("Are there any potential solutions/ways to prevent more health issues? - What might the potential barriers be for some countries?"),
p("It requires a global effort to ensure affordable access to health care systems as well as health promotion and disease prevention programs. One area of focus could be to build infrastructures that 
  would ensure access to safe and readily available water and sanitation services for people within and across countries."),
p("For certain communicable diseases, making vaccines widely available around the world could help reduce the incidence of that disease and lessen the social and economic burden of the disease on the community. The government needs to place 
  an emphasis on leveraging resources and incentivizing the development and production of vaccines on a national level."),
p("Finally, the government could focus on developing educational programs to promote health literacy, such as destigmatizing mental illness and suicides, in all populations which would allow people to make appropriate health decisions."),
p("In the context of national efforts to develop and implement public health policies, it is vital to not only protect and promote the well-being of its citizens, but also address potential barriers that certain population groups face towards 
getting access to health service. Social determinants, such as high poverty rate, stigmatization surrounding health behaviors, and educational disparity that are associated with diminished resources, could be those barriers. Thus, it is essential 
to involve other sections, such as the education, labor, and welfare sections, in addition to promoting health service through the health section."),
    tags$br(),
    tags$h4("Code"),
    "Code and data used to generate this Shiny app are available on ",tags$a(href="https://github.com/Elaineyex/world_health_shiny", "Github."),
    tags$br(), tags$br(),
    tags$h4("Webapp"),
    "The Shiny app is published ",tags$a(href="https://casey-mae-perez.shinyapps.io/2020-World-Health-Statistics/", "online."),
    tags$br(), tags$br(),
    tags$h4("Authors"),
    "Adriana Beltran Andrade, Catherine Park, Casey Perez, Rachel Yan, Xian (Elaine) Ye"
)


ui <- navbarPage(
    theme = shinytheme("cerulean"), 
    "World Health Statistics",
    world_panel,
    country_panel,
    continent_panel,
    data_panel,
    about_panel
)


