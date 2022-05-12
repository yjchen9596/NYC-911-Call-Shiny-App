##Data 613 American University

## shiny project by Yuka, Chaytanya, Tyler, Connie, Liberty

library(shiny)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(maps)
library(leaflet)
library(bslib)
library(gridExtra)
library(zoo)


### do we need these?? ###

#library(ggthemes)
#library(plotly)

## Load data and setup

set.seed(1)

calls <- read_rds("../data/clean_oneM.rds")
Censusdata <- read_rds("../data/Censusdata.RDS")

Censusdata |>
  select(boro_nm =  County, everything()) -> census

boroname <- c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND")
monthword <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

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

calls_map <-calls %>% mutate(
  incident_month = case_when(incident_month == "1" ~ "January",
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
                             incident_month == "12" ~ "December"),
  latitude = as.numeric(latitude),
  longitude = as.numeric(longitude))


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

# Define UI for random distribution application

#########
ui <-fluidPage(theme = bs_theme(version = 5, bootswatch = "lumen"),


 # Application title
  titlePanel(title=div(img(src="picture.png"))),
  tabsetPanel(
    tabPanel("About the NYPPA App",tags$div(
      tags$br(),tags$h3("Background"),tags$p(
        "Resource management is crucial in city management. The recent “Defund the Police” movements have made it harder for the Police departments around the US.
      Police budgets are have come under scrutiny by public servants. This App will come in handy as it allows the users to run city data and perform statistical and time series
      analysis of 911 call data to see if boroughs have different experiences, need different resources, and much more.
      The analysis will help you determine the key areas to focus on and use the available resources effectively. Additionally, users can utilize an interactive map of New York City to have a geographical
      understading of the distribution of  911 calls across NYC. It will also allow users to solve complex problems and better understand what types of calls occur
        in NYC and make data driven decisions."),

                   #tags$br(),tags$br(),
                   tags$h4("Tabs"),
                   "This App has five main tabs:",tags$br(),
                   "Data Analysis",tags$br(),
                   "Variable Sumary",tags$br(),
                   "Time Series Analysis",tags$br(),
                   "Map Analysis",tags$br(),
                   "Spreadsheet",tags$br(),

      tags$br(),tags$h4("Authors"),
      "Chaytanya , MSDS Business Analytics, American University",tags$br(),
      "Connie, MSDS Public Affairs, American University",tags$br(),
      "Tyler, MSDS Business Analytics, American University",tags$br(),
      "Yuka, MSDS Public Affairs, American University",tags$br(),
      "Liberty, MSDS Business Analytics, American University",tags$br(),
      tags$img(src = "AU_logo.png", width = "75px", height = "75px"),tags$br(),tags$br()
    )), # tab end

# ***************************************************************************************************
    tabPanel("Data Analysis",
             mainPanel(
               tabsetPanel(
                 tabPanel("Bar plot Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              # Input: Selector for choosing dataset ----
                              selectInput(inputId = "dataset",
                                          label = "Select a Borough(s)",
                                          choices = c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND" ),
                                          selected = c("BRONX"),multiple = TRUE),
                              checkboxGroupInput("month1", "Which Report month?",
                                                 choices = monthword,selected = monthword[1:12]),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
               tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                            ), ## sidebar panel layout
                            mainPanel(
                              plotOutput("plot")
                            )#main end
                          )#sidebarlayout
                 ),
                 ######box plot analysis
                 tabPanel("Box plot Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "dataset1",
                                          label = "Select a Borough(s)",
                                          choices = c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND" ),
                                          selected = c("BRONX","BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND"),multiple = TRUE),
                            ),
                            mainPanel(
                              plotOutput("plotbox", width = "100%")
                            ))
                 ),
               tabPanel("Density Plot Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "dataset3",
                                        label = "Select a Borough(s)",
                                        choices = c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND" ),
                                        selected = c("BRONX","BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND"),multiple = TRUE),
                            verbatimTextOutput("AOV")
                          ),
                          mainPanel(
                            plotOutput("DT"),
                            plotOutput("pie")
                          ))
               ),
               tabPanel("Census and Survey Data",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "dataset4",
                                        label = "Select a Borough(s)",
                                        choices = c("BRONX", "BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND" ),
                                        selected = c("BRONX","BROOKLYN", "QUEENS","MANHATTAN","STATEN ISLAND"),multiple = TRUE)
                          ),
                          mainPanel(
                            plotOutput("ANV")
                          ))
               )
               )#tabset panel
             )#main panel
    ),

                 #### Variable Summary
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
                                                       choices = c("January" = 1, "Frebuary" = 2, "March" = 3, "April" = 4,
                                                                   "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                                                   "September" = 9, "October"= 10, "November" = 11, "December" = 12),
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
                                             selected = 1:12)),
                                         mainPanel(
                                           tags$br(),
                                           "Minutes as Units",tags$br(),
                                           tableOutput("arrivetable")))), ## gap time tab panel end

                            # ))), ## tabspanel for time series end

                 tabPanel("Distribution by Borough",
                          tags$br(),
                          tags$h4("Chi-Square Goodness of Fit - Using a Sample Size of 1000"),tags$br(),
                          "Null Hypothesis: Distribution of calls follows that of residential distribution by borough",tags$br(),
                          "Alternative Hypothesis: Distribution of calls does not follow the residential distribution by borough", tags$br(),
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("categoryarrivechi","911 Calls Report Types", choices = category,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "None",
                                                         `select-all-text` = "All!",
                                                         `none-selected-text` = "zero"),
                                          multiple = T,
                                          selected = category[1:57]),
                              pickerInput("progressarrivechi","Crime Progress", choices = progress,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Non",
                                                         `select-all-text` = "All!",
                                                         `none-selected-text` = "zero"),
                                          multiple = T,
                                          selected = progress[1:4]),
                              tags$br(),
                              pickerInput("arrivemonthchi","Select Months", choices = c(
                                "January" = 1, "Frebuary" = 2, "March" = 3, "April" = 4,
                                "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                "September" = 9, "October"= 10, "November" = 11, "December" = 12),
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "None",
                                                         `select-all-text` = "All!",
                                                         `none-selected-text` = "zero"),
                                          multiple = T,
                                          selected = 1:12)),
                            mainPanel(
                              tableOutput("StatTable"),
                              textOutput("ChiSqResult")))) ## tab distribution panel end
                 ))), ## tabspanel for time series end

    tabPanel("Map Analysis",
             leafletOutput("map", width="100%", height="700px"),
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = "auto", left = "auto" , right = 20, bottom = 50,
                           width = 330, height = 370,  style = "background-color: #FFFFFF", fillOpacity = 0.8,
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
                                       selected = category[1:57]))),

    tabPanel("Spreadsheet",
             dataTableOutput("spreadsheet")) ## spreadsheet end
    ) ## tabset
  ) ## fluid page end


##########SERVER##################
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
  #####################Data Analysis#####################
  #####Barplot########

  histc <- reactive ({
    calls_map%>%
      filter(boro_nm %in% input$dataset,incident_month %in% input$month1)%>%
      select(boro_nm,incident_month)%>%
      group_by(boro_nm)%>%
      arrange(boro_nm)%>%
      drop_na()})


  output$plot <- renderPlot({ggplot(data=histc(),mapping=aes(x=boro_nm,fill=boro_nm))+
      geom_bar(position = "stack")+
      xlab("Borough(s)") +
      ylab("No of Incident Calls - Grouped by Month ") +
      ggtitle("Borough(s) vs Number of Calls - Grouped by Month")
  })
  dce <-reactive({
    census %>%
      filter(boro_nm %in% input$dataset4)%>%
      select(everything())})

  density<-calls[, c("boro_nm","gap")]
  density$gap<-substr(density$gap,1,4)
  density$gap<-as.double(density$gap)

  density |>
    filter(gap>=0) |>
    drop_na()->lg

  lg|>
    mutate(Ct = gap-gap+1)|>
    group_by(boro_nm)|>
    summarize(summ = sum(Ct))->crime_rate

  dce2 <-reactive({
    lg %>%
      filter(boro_nm %in% input$dataset3)%>%
      select(everything())})

  dce3 <-reactive({
    crime_rate %>%
      filter(boro_nm %in% input$dataset3)%>%
      select(everything())->crime_rate

    crime_rate$fraction = crime_rate$summ/sum(crime_rate$summ)
    crime_rate$ymax = cumsum(crime_rate$fraction)
    crime_rate$ymin = c(0, head(crime_rate$ymax, n=-1))
    crime_rate

    })
 #***************************************
  dce4 <-reactive({
    calls %>%
      filter(boro_nm %in% input$dataset3)%>%
      select(everything()) -> calls2

    calls2 |>
      select(incident_hour, incident_month)->lk

    lk |>
      group_by(incident_hour)|>
      summarize(cnt = sum(incident_month-incident_month+1))
    })




  output$ANV <- renderPlot({
    options(scipen=999)

p1 <- ggplot(data = dce(), mapping = aes(x = population, y = reorder(boro_nm, population)))+
        geom_point(color = "red", size = 3) +
        labs(title = "Population by Borough in New York City",
             subtitle = "2020 American Census",
             y = "Borough",
             x = "Population")

p2 <- ggplot(data = dce(), mapping = aes(x = medianIncome, y = reorder(boro_nm, medianIncome)))+
  geom_errorbar(aes(xmin=medianIncome-Income_moe,xmax=medianIncome+Income_moe))+
  geom_point(color = "red", size = 3) +
  labs(title = "Median Income by Borough in New York City",
       subtitle = "2020 American Community Survey",
       y = "Borough",
       x = "Median Income")

grid.arrange(p1,p2, nrow=1)

  })

  output$DT <- renderPlot({

    # dt1 <- ggplot(dce2(), aes(x=gap))+
    #   geom_density(alpha=0.8,size=1, fill="green")+
    #   labs(title = "Police response time New York",
    #        y = "gensity",
    #        x = "Response time")


    dt2 <- ggplot(dce2(), aes(x=gap, fill = boro_nm))+
      geom_density(alpha=0.5,size=1)+
      scale_x_log10()+
      labs(title = "Police Response Time in New York City per Borough",
           y = "Density",
           x = "Log(Response time)")

    #grid.arrange(dt1,dt2, nrow=1)
    dt2
})


  output$pie <- renderPlot({


    x <-ggplot(dce3(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=boro_nm)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4))+
      labs(title = "Proportion of Calls")



    # y <- ggplot(dce4())+
    #      geom_bar(aes(x=incident_hour, y = cnt,fill = cnt),stat="identity")+
    #      scale_fill_gradient(low = "green",high = "red")

   # grid.arrange(y,x, nrow=1)
    x

  })


  output$AOV <- renderPrint({
      one.way<-aov(gap~boro_nm, data = dce2())
      summary(one.way)
})


  ########time series ##############

  #####boxplot######

  boxp<-reactive({ calls %>%
      filter(boro_nm %in% input$dataset1)%>%
      select(boro_nm,incident_month)%>%
      group_by(boro_nm,incident_month)%>%
      summarize(number= n())%>%
      drop_na() })


  output$plotbox <- renderPlot({


    ggplot(data = boxp()) +
      geom_boxplot(mapping = aes(x = boro_nm,y=number, fill=boro_nm))+
      xlab("Borough(s)") +
      ylab("Total Number of Incident Calls ") +
      ggtitle("Borough(s) vs Total Number of Calls")

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
             gap >= 0) |>
      drop_na() |>
      group_by(boro_nm) |>
      rename(Borough = boro_nm) |>
      summarise(Average = mean(gap), Maximum = max(gap), Median= median(gap),Minimum = min(gap), StandardDev = sd(gap), Count = n())  }) ## reactive end

  output$arrivetable <- renderTable({
    print(arrive())
  }) ##render table end

   ChiSqTable <- reactive({
     calls |>
       filter(incident_month %in% input$arrivemonthchi,
              category %in% input$categoryarrivechi,
              crime_progress %in% input$progressarrivechi,
              gap >= 0) |>
       drop_na() |>
       slice_sample(n=1000) |>
       group_by(boro_nm) |>
       rename(Borough = boro_nm) |>
       summarise(Average = mean(gap), Count = n()) |>
       mutate(Freq = round(Count / sum(Count), 2),
              ExpFreq = c(.17, .31, .19, .27, .06),
              Exp = ExpFreq * sum(Count),
              ChiSq = ((Count - Exp)^2)/Exp) |>
       select(-ExpFreq, -Freq)

   })
   ChiSqPVal <- reactive({
     pchisq(q = sum(ChiSqTable()$ChiSq), df = 5-1, lower.tail = FALSE)
   })

   output$StatTable <- renderTable({
     print(ChiSqTable())
     #add up the ChiSq to get test statistic
     # compare to Chi Sq distribution with n - 5 degrees of freedom
     # as this gets very large, it tends to normal distribution
   })
   output$ChiSqResult <- renderText({
     #print("Chi square test statistic is", sum(ChiSqTable()$ChiSq))
     if (ChiSqPVal() < 0.05){
       print("There is sufficient evidence to say that the true distribution of 911 calls
       do not match the distribution of NYC residents by borough")
     }
     else {
       print("We do not have sufficient evidence to say that true distribution of 911 calls
       do not match the population distribution of NYC residents")
     }
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
      summarise(count = n(), log = mean(longitude), lab =mean(latitude))|>
       ungroup()})

###colorcoding
   pal<-colorFactor(palette = c('red', 'blue', 'green','purple', 'orange'),
                    domain = calls_map$boro_nm)

   color <- colorFactor(palette = topo.colors(7), domain = calls$crime_progress)


   output$map <- renderLeaflet({

     m <- leaflet() |>
       setView(lng = mean(calls_map$longitude), lat = mean(calls_map$latitude), zoom = 10)
     m %>% addTiles() |>
       addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) |>
       addCircles(data = mapy(), lat = ~ lab, lng = ~ log,weight = 10,
                  radius = ~sqrt(count)*10, popup = ~boro_nm, color= ~pal(boro_nm))|>
       addLegend(data = mapy(), pal = pal, values = ~boro_nm, title = "NYC Boroughs",
                 "topleft")
     })

  ##################map analysis end ##########################


  ###### spread sheet #######
  output$spreadsheet <- renderDataTable(
    calls
  )
} ## server end!


#####

# Run the app ----
shinyApp(ui = ui, server = server)
