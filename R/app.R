library(shiny)
library(tidyverse)

library(zoo)
library(ggthemes)
library(plotly)

library(lubridate)
library(shinyWidgets)

library(maps)

library(leaflet)
library(bslib)
#final

## load data

calls <- read_rds("../data/clean_oneM.rds")
census <- read_rds("../data/Censusdata.RDS")


boroname <- c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND")

#### category
calls |>
  distinct(category) -> categorytest
category <- categorytest$category
##### crime progress
calls |>
  distinct(crime_progress) -> progresslist
progress <- progresslist$crime_progress

## for time series, please don't delete it! ##
opt <- calls |>
  select(incident_month, incident_hour)

monthword <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

calls_map <- calls %>%
  mutate(incident_month = case_when(incident_month == "1" ~ "January",
                                    incident_month == "2" ~ "February",
                                    incident_month == "3" ~ "March",
                                    incident_month == "4" ~ "April",
                                    incident_month == "5" ~ "May",
                                    incident_month == "6" ~ "June",
                                    incident_month == "7" ~ "July",
                                    incident_month == "8" ~ "August",
                                    incident_month == "9" ~ "September",
                                    incident_month == "10" ~ "October",
                                    incident_month == "11" ~ "November",
                                    incident_month == "12" ~ "December"))
calls_map$latitude <- as.numeric(calls$latitude)
calls_map$longitude <- as.numeric(calls$longitude)

barselect <- c("boro_nm", "crime_progress","category", "incident_month")

calls |>
  select(boro_nm, crime_progress, category, incident_month, incident_hour, -incident_date, -incident_time, -latitude, -longitude) -> calls2

#View(calls)

#Get only years and months
calls1<- transform(calls, Date = as.yearmon(incident_date))

#Finding no. of calls

#View(calls1)
calls$incident_date %>%
  range() ->
  date_range

#date_range
#View(calls)




# Define UI for random distribution application


#get total no. of calls


#########
ui <-fluidPage(theme = bs_theme(version = 5, bootswatch = "lumen"),
               
               # Application title
               titlePanel(title=div(img(src="picture.png"))),
               tabsetPanel(
                 tabPanel("About the NYPPA App",tags$div(
                   tags$br(),tags$h3("Background"),
                   "Resource management is crucial in city management. The recent “Defund the Police” movements have not made it easier for the Police departments around the US.
      Police budgets are have come under scrutiny by public servants. This App will come in handy as it allows the users run data , statistical and time series
      analysis of 911 call data to see if boroughs have different experiences, need different resources and much more.
      The analysis will help you determine the key areas to focus on and use the available resources effectively. Additional users can use interactive map of New York City to have geographical
      understading and distribution of the analysis.",
                   
                   #tags$br(),tags$br(),
                   tags$br(),tags$h4("About Tabs"),
                   "The App has three main tabs:",tags$br(),
                   "Data Analysis",tags$br(),
                   "Time Series Analysis",tags$br(),
                   "Map Analysis",
                   tags$br(),tags$h4("Authors"),
                   "Chaytanya , MS Data Science, American University",tags$br(),
                   "Connie, MS Data Science, American University",tags$br(),
                   "Tyler, MS Applied Economics, American University",tags$br(),
                   "Yuka, MS Applied Economics, American University",tags$br(),
                   "liberty, MS Applied Economics, American University",tags$br(),
                   tags$img(src = "AU_logo.png", width = "75px", height = "75px"),tags$br(),tags$br()
                 )), # tab end
                 tabPanel("Data Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              # Input: Selector for choosing dataset ----
                              selectInput(inputId = "dataset",
                                          label = "Select a Borough(s)",
                                          choices = c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISTAND" ),
                                          selected = c("BRONX"),
                                          multiple = TRUE),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              
                              tags$br(),
                              "Select Borough, plotting start date and plotting end date to update plots."
                            ), ## sidebar layout
                            mainPanel(
                              
                              plotOutput("plot")
                            ) #main end
                          )), # tab end
                 #### data analysis 2 so we dont mess up what is above
                 tabPanel("Variable Summary",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Select Variable",
                                       sidebarLayout(
                                         sidebarPanel(
                                           tags$br(),
                                           
                                           varSelectInput("variablebb", "Select Variable",
                                                          data = calls2,
                                                          multiple = FALSE
                                           ),
                                           tags$br(),
                                           pickerInput("categorybb","911 Calls Report Types", choices = category,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = category[1:57]),
                                           pickerInput("progressbb","Crime Progress", choices = progress,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = progress[1:4]),
                                           tags$br(),
                                           pickerInput("arrivebb","Select Months",
                                                       choices = c("12 am" = 0,"1 am" = 1, "2 am" = 2, "3 am" = 3, "4 am" = 4, "5 am" = 5,
                                                                   "6 am" = 6, "7 am" = 7, "8 am" = 8, "9 am" = 9, "10 am" = 10, "11 am" = 11,
                                                                   "12 pm" = 12, "1 pm" = 13, "2 pm" = 14, "3 pm" = 15, "4 pm" = 16,"5 pm" = 17,
                                                                   "6 pm" = 18, "7 pm" = 19, "8 pm" = 20, "9pm" = 21, "10 pm" = 22, "11 pm" = 23),,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = 1:12)),
                                         mainPanel(
                                           tags$br(),
                                           tableOutput("borosummary")))) ## tab panel end
                              
                              
                            ))), ## data analysis 2tab panel end
                 
                 #######time series########
                 
                 tabPanel("Time Series",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Time Series Plot",
                                       sidebarLayout(
                                         sidebarPanel(
                                           varSelectInput("typevar","Plot Based on",
                                                          data = opt),
                                           pickerInput("category","911 Calls Report Types", choices = category,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = category[1:57]),
                                           tags$br(),
                                           pickerInput("progressm","Crime Progress", choices = progress,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "Non",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = progress[1:4]),
                                           tags$br(),
                                           pickerInput("borotime","Select Boroughs", choices = boroname,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "Non",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = boroname[1:5]),
                                           tags$br(),
                                           pickerInput("months","Select Months", 
                                                       choices = c("January" = 1, "Frebuary" = 2, "March" = 3, "April" = 4,
                                                                   "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                                                   "September" = 9, "October"= 10, "November" = 11, "December" = 12),
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = 1:12),
                                           pickerInput("hoursmm", "Select Hours",
                                                       choices = c("12 am" = 0,"1 am" = 1, "2 am" = 2, "3 am" = 3, "4 am" = 4, "5 am" = 5,
                                                                   "6 am" = 6, "7 am" = 7, "8 am" = 8, "9 am" = 9, "10 am" = 10, "11 am" = 11,
                                                                   "12 pm" = 12, "1 pm" = 13, "2 pm" = 14, "3 pm" = 15, "4 pm" = 16,"5 pm" = 17,
                                                                   "6 pm" = 18, "7 pm" = 19, "8 pm" = 20, "9pm" = 21, "10 pm" = 22, "11 pm" = 23),
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = 1:24)),
                                         mainPanel(
                                           plotOutput("timeseriesmonth")))
                              ), ## tab panel end
                              
                              ######## arrive time summary
                              tabPanel("Gap Time Summary",
                                       tags$br(),
                                       tags$h4("Arrive Time After Report Called Time"),tags$br(),
                                       "Average Time Police Officers Take to Arrive Reported Case Location",tags$br(),
                                       sidebarLayout(
                                         sidebarPanel(
                                           tags$br(),
                                           pickerInput("categoryarrive","911 Calls Report Types", choices = category,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = category[1:57]),
                                           pickerInput("progressarrive","Crime Progress", choices = progress,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "None",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = progress[1:4]),
                                           tags$br(),
                                           pickerInput("boroarrive","Select Boroughs", choices = boroname,
                                                       options = list(`actions-box` = TRUE,
                                                                      `deselect-all-text` = "Non",
                                                                      `select-all-text` = "All!",
                                                                      `none-selected-text` = "zero"),
                                                       multiple = T,
                                                       selected = boroname[1:5]),
                                           tags$br(),
                                           pickerInput("arrivemonth","Select Months", choices = c(
                                             "January" = 1, "Frebuary" = 2, "March" = 3, "April" = 4,
                                             "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                             "September" = 9, "October"= 10, "November" = 11, "December" = 12),
                                             options = list(`actions-box` = TRUE,
                                                            `deselect-all-text` = "None",
                                                            `select-all-text` = "All!",
                                                            `none-selected-text` = "zero"),
                                             multiple = T,
                                             selected = 1:12),
                                            tableOutput("StatTable")),
                                       mainPanel(
                                         tags$br(),
                                         "Minutes as Units",tags$br(),
                                         tableOutput("arrivetable")))) ## tab panel end
                            
                          ))), ## tabspanel for time series end
               
               tabPanel("Map Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            pickerInput("selectboro","Choose Borough to display",
                                        choices = boroname,
                                        selected = boroname[1:5],
                                        options = list(`actions-box` = TRUE,
                                                       `deselect-all-text` = "None",
                                                       `select-all-text` = "All!",
                                                       `none-selected-text` = "zero"),
                                        multiple = T),
                            pickerInput("selectmonth","Choose Month to display",
                                        choices = monthword,
                                        selected = monthword[1:12],
                                        options = list(`actions-box` = TRUE,
                                                       `deselect-all-text` = "None",
                                                       `select-all-text` = "All!",
                                                       `none-selected-text` = "zero"),
                                        multiple = T),
                            pickerInput("selectprogress","Crime Progress", choices = progress,
                                        options = list(`actions-box` = TRUE,
                                                       `deselect-all-text` = "None",
                                                       `select-all-text` = "All!",
                                                       `none-selected-text` = "zero"),
                                        multiple = T,
                                        selected = progress[1:4]),
                            pickerInput("selectcategory","911 Calls Report Types", choices = category,
                                        options = list(`actions-box` = TRUE,
                                                       `deselect-all-text` = "None",
                                                       `select-all-text` = "All!",
                                                       `none-selected-text` = "zero"),
                                        multiple = T,
                                        selected = category[1:57])),
                          
                          # Show a plot of the generated distribution
                          mainPanel(leafletOutput("map",width="100%",height="800px"))
                        )
               ) ,
               
               
               tabPanel("Spread Sheet",
                        dataTableOutput("spreadsheet")) ## spreadsheet end
) ## tabset
) ## fluid page end

server <- function(input, output, session ) {
  
  
  
  level_selectInput <- reactive({
    switch(input$level_select,
           "QUEENS" = "QUEENS",
           "BRONX" = "BRONX",
           "BROOKLYN" = "BROOKLYN",
           "MANHATTAN" = "MANHATTAN",
           "STATEN ISlAND"="STATEN ISLAND")
    
  })
  
  
  ######### data analysis 2 ########
  
  borosum <- reactive ({
    calls2 |>
      filter(incident_month %in% input$arrivebb,
             category %in% input$categorybb,
             crime_progress %in% input$progressbb) |>
      group_by(!!input$variablebb) |>
      summarise(count = n()) |>
      mutate(freq = round(count / sum(count), 2)) |>
      arrange(desc(freq))
  })
  
  output$borosummary <- renderTable({
    print(borosum())
  }) ##render table end
  
  ##### data analysis 2 end #######
  
  #####Histogram########
  
  histc <- reactive ({
    calls %>%
      filter(boro_nm %in% input$dataset)%>%
      select(boro_nm,incident_month)%>%
      group_by(boro_nm)%>%
      
      arrange(boro_nm)%>%
      drop_na()})
  
  output$plot <- renderPlot({ggplot(data=histc(),mapping=aes(x=boro_nm))+
      geom_bar(position = "stack")
  })
  
  
  
  ########time series ##############
  
  ##month
  
  callm <- reactive ({
    calls |>
      drop_na() |>
      filter(incident_month %in% input$months,
             category %in% input$category,
             boro_nm %in% input$borotime,
             crime_progress %in% input$progressm,
             incident_hour %in% input$hoursmm) |>
      group_by(!!input$typevar) |>
      mutate(n = n()) |>
      ungroup()
  })
  
  output$timeseriesmonth <- renderPlot({
    
    timeplot <- ggplot(data = callm(), mapping = aes(x = !!(input$typevar), y = n, group = boro_nm, color = boro_nm)) +
      stat_summary(fun = sum, geom = "line") +
      labs(title = paste("Total Numbers of Call Reports Based on" , input$typevar),
           y = "Numbers of Incidents",
           color = "Selected Boroughs") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    if (input$typevar == "incident_month") {
      timeplot <- timeplot +
        scale_x_continuous(
          breaks = seq_along(month.name),
          labels = month.name) +
        labs(x = "Month(s) in 2021")
    } else {
      timeplot <- timeplot+
        labs(caption = "0 means midnight (24 hours)",
             x = "Hours")
    }
    timeplot
  })
  
  
  ##### arrive time summarization table
  arrive <- reactive ({
    calls |>
      filter(incident_month %in% input$arrivemonth,
             category %in% input$categoryarrive,
             boro_nm %in% input$boroarrive,
             crime_progress %in% input$progressarrive,
             gap > 0.1) |>
      drop_na() |>
      group_by(boro_nm) |>
      rename(Borough = boro_nm) |>
      summarise(Average = mean(gap), Maximum = max(gap), Median= median(gap),Minimum = min(gap), StandardDev = sd(gap), Count = n())  }) ## reactive end
  
  output$arrivetable <- renderTable({
    print(arrive())
  }) ##render table end
  
  output$StatTable <- renderTable({
    calls |>
      filter(incident_month %in% input$arrivemonth,
             category %in% input$categoryarrive,
             boro_nm %in% input$boroarrive,
             crime_progress %in% input$progressarrive,
             gap > -0.1) |>
      drop_na() |>
      group_by(boro_nm) |>
      rename(Borough = boro_nm) |>
      summarise(Average = mean(gap), StandardDev = sd(gap), Count = n()) |>
      mutate(Freq = round(Count / sum(Count), 2),
             Exp = c(.17, .31, .19, .27, .06),
             ChiSq = ((Freq - Exp)^2)/Exp)
    #add up the ChiSq to get test statistic
    # compare to Chi Sq distribution with n - 5 degrees of freedom
    # as this gets very large, it tends to normal distribution
  })
  
  ##################timeseries end ##########################
  
  
  ##################map analysis ##########################
  
  mapy <- reactive({
    calls_map |>
      filter(boro_nm %in% input$selectboro,
             incident_month %in% input$selectmonth,
             crime_progress %in% input$selectprogress,
             category %in% input$selectcategory) |>
      group_by(boro_nm) |>
      group_by(boro_nm) |>
      summarise(count = n(), log = mean(longitude), lab =mean(latitude))})
  
  colorcode <- reactive({
    colorFactor(topo.colors(7), input$selectboro)
  })
  
  
  output$map <- renderLeaflet({
    
    m <- leaflet() %>% setView(lng= -73.89, lat=40.86, zoom = 10)
    m %>% addTiles() |>  addProviderTiles(providers$CartoDB.Positron) |>
      addCircles(data = mapy(), lat = ~ lab, lng = ~ log,
                 radius = ~sqrt(count)*10, popup = ~boro_nm) })
  
  
  ##################map analysis end ##########################
  
  
  
  
  ###### spread sheet #######
  output$spreadsheet <- renderDataTable(
    calls
  )
  
  
  
  
} ## server end!


#####

# Run the app ----
shinyApp(ui = ui, server = server)
