
library(shiny)
library(tidyverse)
library(readr)
library(shinydashboard)
library(shinythemes)
library(gapminder)
library(here)
library(janitor)
library(leaflet)
library(rgdal)
library(plotly)
library(shinyWidgets)


#data processing
#RY 
neonatalMortalityRate <- read_csv(here("data","neonatalMortalityRate.csv"))%>%
    clean_names() %>%
    separate(., first_tooltip, c("first_tooltip","range"), sep =" ") %>%
    rename(value = first_tooltip) %>%
    mutate(value = as.numeric(value)) %>%
    rename(sex = dim1) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

maternalMortalityRatio <- read_csv(here("data","maternalMortalityRatio.csv"))%>%
    clean_names() %>%
    separate(., first_tooltip, c("first_tooltip","range"), sep =" ") %>%
    rename(value = first_tooltip) %>%
    mutate(value = as.numeric(value)) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

infantMortalityRate <- read_csv(here("data","infantMortalityRate.csv"))%>%
    clean_names() %>%
    separate(., first_tooltip, c("first_tooltip","range"), sep =" ") %>%
    rename(value = first_tooltip) %>%
    rename(sex = dim1) %>%
    mutate(value = as.numeric(value)) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

birthAttendedBySkilledPersonal <- read_csv(here("data","birthAttendedBySkilledPersonal.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

#CP
adolescentBirthRate <- read_csv(here("data","adolescentBirthRate.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

atLeastBasicSanitizationServices <- read_csv(here("data","atLeastBasicSanitizationServices.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  rename(area = dim1) %>%
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

basicDrinkingWaterServices <- read_csv(here("data","basicDrinkingWaterServices.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>%
    mutate(value = as.numeric(value)) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

crudeSuicideRates <- read_csv(here("data","crudeSuicideRates.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>%
    rename(sex = dim1) %>%
    filter(period != 2016) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

eliminateViolenceAgainstWomen <- read_csv(here("data","eliminateViolenceAgainstWomen.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

incedenceOfMalaria <- read_csv(here("data","incedenceOfMalaria.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

incedenceOfTuberculosis <- read_csv(here("data","incedenceOfTuberculosis.csv")) %>%
  clean_names() %>%
  separate(first_tooltip, c("first_tooltip", "range"), " ") %>% 
  rename(value = first_tooltip) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

lifeExpectancyAtBirth <- read_csv(here("data","lifeExpectancyAtBirth.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>%
    rename(sex = dim1) %>% 
  filter(period >= 2000) %>%
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

medicalDoctors <- read_csv(here("data","medicalDoctors.csv"))%>%
    clean_names()  %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

newHivInfections <- read_csv(here("data","newHivInfections.csv")) %>%
    clean_names() %>%
    separate(., first_tooltip, c("first_tooltip","range"), sep =" ") %>%
    rename(value = first_tooltip) %>%
    mutate(value = as.numeric(value)) %>%
    rename(sex = dim1) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

nursingAndMidwife <- read_csv(here("data","nursingAndMidwife.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

pharmacists <- read_csv(here("data","pharmacists.csv")) %>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

population10SDG3_8_2 <- read_csv(here("data","population10SDG3.8.2.csv")) %>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  rename(area = dim1) %>%
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

uhcCoverage <- read_csv(here("data","uhcCoverage.csv"))%>%
    clean_names() %>%
    rename(value = first_tooltip) %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))


#create a list and add the variable 'continent' to all datasets
datalist <- list(adolescentBirthRate,atLeastBasicSanitizationServices,basicDrinkingWaterServices,birthAttendedBySkilledPersonal,crudeSuicideRates,incedenceOfMalaria,incedenceOfTuberculosis,infantMortalityRate,lifeExpectancyAtBirth,eliminateViolenceAgainstWomen,maternalMortalityRatio,medicalDoctors,neonatalMortalityRate,newHivInfections,nursingAndMidwife,pharmacists,population10SDG3_8_2,uhcCoverage)

names <- c("adolescentBirthRate","atLeastBasicSanitizationServices","basicDrinkingWaterServices","birthAttendedBySkilledPersonal","crudeSuicideRates","incedenceOfMalaria","incedenceOfTuberculosis","infantMortalityRate","lifeExpectancyAtBirth","eliminateViolenceAgainstWomen","maternalMortalityRatio","medicalDoctors","neonatalMortalityRate","newHivInfections","nursingAndMidwife","pharmacists","population10SDG3_8_2","uhcCoverage")

# contry-continent conversion 
countryContinent <- read.csv(here("data","countryContinent.csv"))%>%
    select(country, continent)

convert_function <- function(dataset){
    dataset <- left_join(dataset, countryContinent, by = c("location" = "country"))
}

list_add<-map(datalist,convert_function)
#names(list_add) <- names
for (i in seq(list_add)) 
    assign(names[i], list_add[[i]])


#WORLD*****

world_spdf1 <- readOGR(dsn = path.expand(here("data","TM_WORLD_BORDERS_SIMPL-0.3")),
                       layer= "TM_WORLD_BORDERS_SIMPL-0.3", stringsAsFactors = FALSE)

world_spdf1$NAME[world_spdf1$NAME == "Burma"] <- "Myanmar"
world_spdf1$NAME[world_spdf1$NAME == "Cape Verde"] <- "Cabo Verde"
world_spdf1$NAME[world_spdf1$NAME == "Cote d'Ivoire"] <- "Côte d’Ivoire"
world_spdf1$NAME[world_spdf1$NAME == "Czech Republic"] <- "Czechia"
world_spdf1$NAME[world_spdf1$NAME == "Korea, Democratic People's Republic of"] <- "Democratic People's Republic of Korea"
world_spdf1$NAME[world_spdf1$NAME == "Swaziland"] <- "Eswatini"
world_spdf1$NAME[world_spdf1$NAME == "Libyan Arab Jamahiriya"] <- "Libya"
world_spdf1$NAME[world_spdf1$NAME == "Micronesia, Federated States of"] <- "Micronesia (Federated States of)"
world_spdf1$NAME[world_spdf1$NAME == "Korea, Republic of"] <- "Republic of Korea"
world_spdf1$NAME[world_spdf1$NAME == "United States"] <- "United States of America"
world_spdf1$NAME[world_spdf1$NAME == "French Guiana"] <- "France"
world_spdf1$NAME[world_spdf1$NAME == "Guadeloupe"] <- "France"
world_spdf1$NAME[world_spdf1$NAME == "Martinique"] <- "France"
world_spdf1$NAME[world_spdf1$NAME == "Mayotte"] <- "France"
world_spdf1$NAME[world_spdf1$NAME == "Reunion"] <- "France"
world_spdf1$NAME[world_spdf1$NAME == "Ã…land Islands"] <- "Åland Islands"
world_spdf1$NAME[world_spdf1$NAME == "The former Yugoslav Republic of Macedonia"] <- "North Macedonia"



world_spdf1COPY <- readOGR(dsn = path.expand(here("data","TM_WORLD_BORDERS_SIMPL-0.3")),
                       layer= "TM_WORLD_BORDERS_SIMPL-0.3", stringsAsFactors = FALSE)

world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Burma"] <- "Myanmar"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Cape Verde"] <- "Cabo Verde"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Cote d'Ivoire"] <- "Côte d’Ivoire"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Czech Republic"] <- "Czechia"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Korea, Democratic People's Republic of"] <- "Democratic People's Republic of Korea"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Swaziland"] <- "Eswatini"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Libyan Arab Jamahiriya"] <- "Libya"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Micronesia, Federated States of"] <- "Micronesia (Federated States of)"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Korea, Republic of"] <- "Republic of Korea"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "United States"] <- "United States of America"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "French Guiana"] <- "France"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Guadeloupe"] <- "France"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Martinique"] <- "France"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Mayotte"] <- "France"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Reunion"] <- "France"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "Ã…land Islands"] <- "Åland Islands"
world_spdf1COPY$NAME[world_spdf1COPY$NAME == "The former Yugoslav Republic of Macedonia"] <- "North Macedonia"



incedenceOfMalariaWORLD <- read_csv(here("data","incedenceOfMalaria.csv"))%>%
  clean_names() %>% 
  rename(value = first_tooltip) %>%
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))


incedenceOfMalariaWORLD$value <- incedenceOfMalariaWORLD$value %>% 
  as.numeric(as.character(incedenceOfMalariaWORLD$value))

incedenceOfTuberculosisWORLD <- read_csv(here("data","incedenceOfTuberculosis.csv"))%>%
  clean_names() %>% 
  separate(first_tooltip, c("value", "range"), " ") %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))


incedenceOfTuberculosisWORLD$value <- incedenceOfTuberculosisWORLD$value %>% 
  as.numeric(as.character(incedenceOfTuberculosisWORLD$value))

atLeastBasicSanitizationServicesWORLD <- read_csv(here("data","atLeastBasicSanitizationServices.csv"))%>%
  clean_names() %>%
  rename(value = first_tooltip) %>% 
  filter(dim1 == "Total") %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))

population10SDG3_8_2WORLD <- read_csv(here("data","population10SDG3.8.2.csv")) %>%
  clean_names() %>%
  rename(value = first_tooltip) %>% 
  filter(dim1 == "Total") %>% 
  mutate(location = replace(location, location == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(location = replace(location, location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>% 
  mutate(location = replace(location, location == "Russian Federation", "Russia")) %>%
  mutate(location = replace(location, location == "Bolivia (Plurinational State of)", "Bolivia"))%>% 
  mutate(location = replace(location, location == "The former Yugoslav Republic of Macedonia", "North Macedonia"))


#server setup 
#countryplot
shinyServer(function(input, output, session) {
  
    datasetInput <- reactive({
        switch(input$dataset, 
               "Neonatal Mortality Rate" = neonatalMortalityRate,
               "Maternal Mortality Ratio" = maternalMortalityRatio,
               "Infant Mortality Rate" = infantMortalityRate,
               "Birth Attended By Skilled Personnel" = birthAttendedBySkilledPersonal,
               "Adolescent Birth Rate" = adolescentBirthRate,
               "Basic Sanitization Services" = atLeastBasicSanitizationServices,
               "Basic Drinking Water Services" = basicDrinkingWaterServices,
               "Crude Suicide Rates" = crudeSuicideRates, 
              # "Violence Against Women" = eliminateViolenceAgainstWomen,
               "Incedence of Malaria" = incedenceOfMalaria,
               "Incedence of Tuberculosis" = incedenceOfTuberculosis,
               "Life Expectancy at Birth" = lifeExpectancyAtBirth,
               "Medical Doctors"= medicalDoctors,
               "New HIV Infections" = newHivInfections,
               "Nursing and Midwife" = nursingAndMidwife,
               "Pharmacists" = pharmacists,
               "Health Expenditure > 10% income" = population10SDG3_8_2,
               "Universal Health Care Coverage" = uhcCoverage
               )
    })

    #country dataset & descriiption
    output$titleCountry <- renderText({
      dataset <- datasetInput()
      paste("Description of Data: ", unique(dataset$indicator))
    })
    
    output$plot <- renderPlotly({
        dataset <- datasetInput()
      # check if the dataset has sex variable 
        if (is.null(dataset$sex) == FALSE) {
          g2 = dataset %>%
            filter(location == input$varCountry) %>%
            ggplot(aes(x=period, y = value)) +
            geom_line(aes(color = sex)) +
            geom_point(size = 1, alpha = 0.8, aes(color = sex)) +
            labs(y=dataset$indicator, x = "Year") +
            theme_minimal() 
          ggplotly(g2)
        }
        #check if the dataset has area variable
        else if (is.null(dataset$area) == FALSE) {
          g3 = dataset %>%
            filter(location == input$varCountry) %>%
            ggplot(aes(x=period, y = value)) +
            geom_line(aes(color = area)) +
            geom_point(size = 1, alpha = 0.8, aes(color = area)) +
            labs(y=dataset$indicator, x = "Year") +
            theme_minimal() 
          ggplotly(g3)
        }
        else {
          g1 = dataset %>%
            filter(location == input$varCountry) %>%
            ggplot(aes(x=period, y = value)) +
            geom_line(color = "#00BFC4") +
            geom_point(size = 1, alpha = 0.8, color ="#00BFC4") + 
            labs(y=dataset$indicator, x = "Year") + 
            theme_minimal()
          ggplotly(g1)
        }
    })

    #Takes in dataset choosen and adjusts for available countries 
    observeEvent(c(input$dataset),
                 {dataset <- datasetInput()
                 dataCountries <- unique(dataset$location)
                 
                 updateSelectInput(session,
                                   "varCountry",
                                   choices = dataCountries)
                 
                 })    
#Function to filter Country by Continent Chosen by user
#don't need this given the continent plot
    # observeEvent(c(input$varContinent),
    #              {
    #                  gapminderCountry <- gapminder %>% 
    #                      filter(continent == input$varContinent) %>% 
    #                      pull(country)
    #                  
    #                  updateSelectInput(session,
    #                                    "varCountry",
    #                                   choices = gapminderCountry)
    #                  
    #              })
  
  #continent dataset
    output$titleContinent <- renderText({
      datasetContinent <- datasetInputContinent()
      paste("Description of Data: ", unique(datasetContinent$indicator))
    })
    datasetInputContinent <- reactive({
      switch(input$datasetContinent, 
             "Neonatal Mortality Rate" = neonatalMortalityRate,
             "Maternal Mortality Ratio" = maternalMortalityRatio,
             "Infant Mortality Rate" = infantMortalityRate,
             "Birth Attended By Skilled Personnel" = birthAttendedBySkilledPersonal,
             "Adolescent Birth Rate" = adolescentBirthRate,
             "Basic Sanitization Services" = atLeastBasicSanitizationServices,
             "Basic Drinking Water Services" = basicDrinkingWaterServices,
             "Crude Suicide Rates" = crudeSuicideRates, 
             "Incedence of Malaria" = incedenceOfMalaria,
             "Incedence of Tuberculosis" = incedenceOfTuberculosis,
             "Life Expectancy at Birth" = lifeExpectancyAtBirth,
             "Medical Doctors"= medicalDoctors,
             "New HIV Infections" = newHivInfections,
             "Nursing and Midwife" = nursingAndMidwife,
             "Pharmacists" = pharmacists,
             "Health Expenditure > 10% income" = population10SDG3_8_2,
             "Universal Health Care Coverage" = uhcCoverage
      )
    })
    output$continent_plot <- renderPlotly({
        datasetContinent <- datasetInputContinent()
        if (is.null(datasetContinent$sex) == FALSE) {
          p2 <- datasetContinent %>%
            filter(sex == "Both sexes") %>%
            group_by(period, continent)%>%
            na.omit()%>%
            summarise(mean = mean(value))%>%
            ggplot(aes(x = period, y = mean, color = continent))+ 
            geom_line()+
            geom_point() +
            labs(x = "Year", 
                 y = datasetContinent$indicator) +
            theme_bw()
          ggplotly(p2)
        } 
        else if (is.null(datasetContinent$area) == FALSE) {
          p3 <- datasetContinent %>%
            filter(area == "Total") %>%
            group_by(period, continent)%>%
            na.omit()%>%
            summarise(mean = mean(value))%>%
            ggplot(aes(x = period, y = mean, color = continent))+ 
            geom_line()+
            geom_point() +
            labs(x = "Year", 
                 y = datasetContinent$indicator) +
            theme_bw()
          ggplotly(p3)
        }
        else {
          p1 <- datasetContinent %>%
            group_by(period, continent)%>%
            na.omit()%>%
            summarise(mean = mean(value))%>%
            ggplot(aes(x = period, y = mean, color = continent))+ 
            geom_line()+
            geom_point() +
            labs(x = "Year", 
                 y = datasetContinent$indicator) +
            theme_bw()
          ggplotly(p1)
        }
        }
        )
    
#Choosing datasets for Map
    datasetInputMap <- reactive({
      switch(input$datasetMap, 
             "Neonatal Mortality Rate" = neonatalMortalityRate,
             "Maternal Mortality Ratio" = maternalMortalityRatio,
             "Infant Mortality Rate" = infantMortalityRate,
             "Birth Attended By Skilled Personnel" = birthAttendedBySkilledPersonal,
             "Adolescent Birth Rate" = adolescentBirthRate,
             "Basic Sanitization Services" = atLeastBasicSanitizationServicesWORLD,
             "Basic Drinking Water Services" = basicDrinkingWaterServices,
             "Crude Suicide Rates" = crudeSuicideRates, 
             "Incedence of Malaria" = incedenceOfMalariaWORLD,
             "Incedence of Tuberculosis" = incedenceOfTuberculosisWORLD,
             "Life Expectancy at Birth" = lifeExpectancyAtBirth,
             "Medical Doctors"= medicalDoctors,
             "New HIV Infections" = newHivInfections,
             "Nursing and Midwife" = nursingAndMidwife,
             "Pharmacists" = pharmacists,
             "Health Expenditure > 10% income" = population10SDG3_8_2WORLD,
             "Universal Health Care Coverage" = uhcCoverage
      )
    })
    
    
    #Takes in dataset choosen and adjusts for year
    observeEvent(c(input$datasetMap),
                 {datasetMap <- datasetInputMap()
                   dataMapYears <- unique(datasetMap$period)
                   
                   updateSelectInput(session,
                                     "worldYear",
                                     choices = dataMapYears)
                   
                 })
    
    output$titleWorld <- renderText({
      datasetMap <- datasetInputMap()
      paste("Description of Data: ", unique(datasetMap$indicator))
    })

    #Prints statement if dataset uses information about both sexes
    output$sexTitle <- renderText({
      datasetMap <- datasetInputMap()
      if (is.null(datasetMap$sex) != TRUE) {
        paste("Information about Both Sexes")
      }
    })
    
    
    output$worldMap <- renderLeaflet({
      datasetMap <- datasetInputMap()
      #Check if dataset has sex variable or not
      if(is.null(datasetMap$sex) == TRUE) {
        placeHolder <- datasetMap %>% 
          mutate(NAME = location) %>% 
          select(-indicator, -location) %>% 
          filter(period == input$worldYear)

      }
      
      else{
        placeHolder <- datasetMap %>% 
          mutate(NAME = location) %>% 
          select(-indicator, -location) %>% 
          filter(sex == "Both sexes") %>% 
          filter(period == input$worldYear)
      }
      
      world_spdf1COPY@data <- world_spdf1@data %>% 
        full_join(placeHolder, by = "NAME")
      
      mypalette2 <- colorBin(palette="viridis", domain = world_spdf1COPY@data$value, na.color = "grey")
      labels <- sprintf("<strong>%s</strong><br/>%g", 
                        world_spdf1COPY@data$NAME, world_spdf1COPY@data$value) %>% 
        lapply(htmltools::HTML) # this makes it be interactive with the HTML
      
      worldPlot <- leaflet(world_spdf1COPY) %>% 
        addTiles()  %>% 
        setView( lat=20, lng=20 , zoom=1.5) %>%    # the coordinate/zoom when the map loads 
        addLegend(pal = mypalette2, values = ~value, opacity = 0.7,
                  position = "bottomright", title = "Legend") %>% 
        addPolygons(fillColor = ~mypalette2(value),
                    weight = 1,
                    opacity = 1,
                    color = "white", 
                    dashArray = "3", # makes dash lines around countries 
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "grey", # When hovering over data, then create grey outline
                      dashArray = "",
                      fillOpacity = 0.9,
                      bringToFront = TRUE),
                    label = labels,      # here we are assigning the label
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto") 
        ) 
      
      worldPlot
    })##RenderLeaflet
  
    
})
