library(shiny)
library(ggplot2)
library(tidyr)
library(shinythemes)
library(ggthemes)
library(plotly)


adult <- read.csv("data/nutritionADULT.csv")
# Rename DataValue column
names(adult)[names(adult) == "AVG.DataValue."] <- "DataValue"
# Delete unnecessary column
adult <-  adult[, colnames(adult) != "COUNT.LocationDesc."]

high_school <- read.csv("data/nutritionHS.csv")


# Rename the variables for the adult data table and reorder them so the X and Y variables don't start as the same variables in the graph
adult_question_list <- list("% of Adults considered Obese" ="Obesity among adults aged >= 18 years", "% Meeting Aerobic Guidelines"="Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years","Median Daily Fruit Consumption"="Median daily frequency of fruit consumption among adults aged >= 18 years", "Median Daily Vegetable Consumption"= "Median daily frequency of vegetable consumption among adults aged >= 18 years", "% of Adults at a Healthy Weight"="Healthy weight among adults aged >= 18 years","% of Adults Not Undertaking Physical Activity in their Leisure Time" = "No leisure-time physical activity among adults aged >= 18 years")

adult_question_list2 <- list("% of Adults Not Undertaking Physical Activity in their Leisure Time" = "No leisure-time physical activity among adults aged >= 18 years", "% of Adults considered Obese" ="Obesity among adults aged >= 18 years", "% Meeting Aerobic Guidelines"="Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years","Median Daily Fruit Consumption"="Median daily frequency of fruit consumption among adults aged >= 18 years", "Median Daily Vegetable Consumption"= "Median daily frequency of vegetable consumption among adults aged >= 18 years", "% of Adults at a Healthy Weight"="Healthy weight among adults aged >= 18 years")


# Rename the variables for the high school data table and reorder them so the X and Y variables don't start as the same variables in the graph
hs_question_list <- list("Average Daily Computer Use"="Computer use among high school students", "Average Daily Soda Consumption"="Soda consumption among high school students", "Average Daily TV Watched"="Television viewing among high school students","% of Students at a Healthy Weight"="Healthy weight among high school students", "% of Students Meeting Physical Activity Guidelines"="Meeting aerobic physical activity guidelines among high school students", "% of Students Considered Obese"="Obesity among high school students", "% of Students who Participate in daily school physical education classes" = "Participation in daily school physical education classes among high school students", "Median daily frequency of fruit consumption"="Median daily frequency of fruit consumption among high school students", "Median daily frequency of vegetable consumption"="Median daily frequency of vegetable consumption among high school students")

hs_question_list2 <- list("% of Students Considered Obese"="Obesity among high school students", "Average Daily Computer Use"="Computer use among high school students", "Average Daily Soda Consumption"="Soda consumption among high school students", "Average Daily TV Watched"="Television viewing among high school students","% of Students at a Healthy Weight"="Healthy weight among high school students", "% of Students Meeting Physical Activity Guidelines"="Meeting aerobic physical activity guidelines among high school students", "% of Students who Participate in daily school physical education classes" = "Participation in daily school physical education classes among high school students", "Median daily frequency of fruit consumption"="Median daily frequency of fruit consumption among high school students", "Median daily frequency of vegetable consumption"="Median daily frequency of vegetable consumption among high school students")

adult_strat_list <- list("African American" = "Black, non-Hispanic", "Caucasian" = "White, non-Hispanic", "Multiracial" = "Multiracial, non-Hispanic", "Other" = "Other, non-Hispanic", "Hispanic" = "Hispanic")

# Not all states had complete data, so they were removed only for the 9 State Analysis graphs on the High School Students tab
state_list <- list("Alabama", "Alaska","Arizona","Arkansas","Connecticut","Florida","Georgia","Guam","Idaho","Illinois","Iowa","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","United States","Utah","Virginia","Washington","West Virginia","Wisconsin","Wyoming","Vermont")




ui <- fluidPage(theme = shinytheme("slate"),
  tabsetPanel(
    tabPanel("Adults",
             headerPanel("United States Nutrition Data For Adults Aged 18+"),
       sidebarLayout(
          sidebarPanel(
            h3("Select Desired Data Below"),
                radioButtons
            (
                  inputId = "Overall_Year",
                  label = "Choose Year",
                  choices = unique(adult$YearEnd)
                ),
                selectInput(
                  inputId = "Overall_xLabel",
                  label = "Choose X Variable",
                  choices = adult_question_list
                ),
                selectInput(
                  inputId = "Overall_yLabel",
                  label = "Choose Y Variable",
                  choices = adult_question_list2
                )
          ),
            mainPanel(
              fluidRow(
                column( 
                  width = 6,
                  h3("Adult Nutrition Data By Gender"),
                  plotlyOutput(outputId = "gender_graph")
                ),
                column( 
                  width = 6,
                  h3("Adult Nutrition Data By Ethnicity"),
                  plotlyOutput(outputId = "ethnicity_graph")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h3("Overall Adult Nutrition Data"),
                  plotlyOutput(outputId = "overall_graph", height = "320px")
                )
              )
            )
          )
        ),
    tabPanel("High School Students",
             headerPanel("United States Nutrition Data For High Schoolers"),
       sidebarLayout(
         sidebarPanel(
            h3("Choose Variables for the Countrywide Graph"), 
            radioButtons(
               inputId = "hs_Year",
               label = "Choose Year",
               choices = unique(high_school$YearEnd)
             ),
             selectInput(
               inputId = "hs_xLabel",
               label = "Choose X Variable",
               choices = hs_question_list
             ),
             selectInput(
               inputId = "hs_yLabel",
               label = "Choose Y Variable",
               choices = hs_question_list2
           ),
           "** All variables except for average daily fruit and vegetable consumption are reported as the % prevalence. The variables for average daily computer use, soda consumption, and television viewing can be interpretted as the % of High School students who take part in that activity daily.",
           h3("Choose Variable for The State by State Comparison"),
           selectInput(
             inputId = "hs_dot_var",
             label = " ",
             choices = hs_question_list2
           ),
           h3("Choose State to Analyze Change from 2013-2015"),
           selectInput(
             inputId = "hs_state",
             label = " ",
             choices = state_list
           )
       ),
       mainPanel(
         column(
           width = 8,
           fluidRow(
               h3("Countrywide Comparison"),
               plotlyOutput(outputId = "hs_overall_graph")
           ),
           fluidRow(
             tags$head(
               tags$style(HTML(" 
                               #State_Label {
                               font-family: 'Lobster', cursive;
                               font-weight: 500;
                               font-size: 20px;
                               }
                               ")), # Adjusting the font for the h3 tags and the 'State_Label' text output
               tags$style(HTML("
                               h3 {
                               font-family: 'Lobster', cursive;
                               font-weight: 50;
                               font-size: 20px;
                               
                               }
                               "))
               ),
             h3("2013-2015 Change for:"),
             textOutput(outputId = "State_Label")
           ),
           fluidRow(
             column(
               width = 4,
               plotOutput(outputId = "hs_computer", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_soda", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_tv", height = "200px")
             )
           ),
           fluidRow(style = "height:20px"),
           fluidRow(
             cellArgs = list(style = "padding: 6px"),
             column(
               "\n",
               width = 4,
               plotOutput(outputId = "hs_obesity", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_aerobics", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_phys_ed", height = "200px")
             )
           ),
           fluidRow(style = "height:20px"),
           fluidRow(
             column(
               width = 4,
               plotOutput(outputId = "hs_healthy", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_fruit", height = "200px")
             ),
             column(
               width = 4,
               plotOutput(outputId = "hs_veggie", height = "200px")
             )
           )),
         column(
           width = 4,
           h3("State by State Comparison"),
           plotlyOutput(outputId = "hs_dot_plot", height = "800px")
         ))
       )
     )
  )
)

             
             

server <- function(input, output) {
  
  

  
# ----------------------------------------------------------------------- #
# ------------------------- Adult Nutrition Tab ------------------------- #
# ----------------------------------------------------------------------- #
  
    
  # Reactive datasets for the "Overall" graph
  # Since we are taking subsets of our dataframe based on individual row values rather than column names, it was easiest to create a new reactive dataset for the x & y variables separately
      # the only problem with this method is that if you delete the null/0 values, your datasets will be different lengths and you'll be unable to plot them against each other
            # I solved this problem in the graphs on the high school nutrition tab (code below) but couldn't get the same method to work for these datasets
  # the creation of the reactive datasets for each graph follows this pattern 
  # This first data set takes a subset based on the year chosen, since that applies to both the x & y variables
  adult_overall <- reactive({
    c <- subset(adult, adult$YearEnd == input$Overall_Year)
    c <- subset(c, c$Stratification1 == "Overall")
    return(c)
  })
  
  # this dataset represents the subset for the x variable chosen
  adult_overall_sub_x <- reactive({
    a <- subset(adult_overall(), adult_overall()$Question == input$Overall_xLabel)
    return(a)
  })
  
  # this dataset represents the subset for the y variable chosen
  adult_overall_sub_y <- reactive({
    b <- subset(adult_overall(), adult_overall()$Question == input$Overall_yLabel)
    return(b)
  })
  
  
  # Overall Graph
  # in order to pull data from the two separate reactive datasets we created, I had to reference them under geom_point() instead of within the ggplot() 
  output$overall_graph <- ({
    plt <- renderPlotly({
      ggplot() + 
        geom_point(aes(x=adult_overall_sub_x()$DataValue, y=adult_overall_sub_y()$DataValue, col=adult_overall_sub_x()$LocationDesc)) + 
        labs(y = "Y Variable", x = "X Variable") + 
        theme_economist() +
        theme(legend.position = "none")
    })
  })

  

# --------------------- Gender --------------------- #
  
  
  # Reactive datasets for the "Gender" graph
  adult_gender <- reactive({
    c <- subset(adult, adult$YearEnd == input$Overall_Year)
    c <- subset(c, c$StratificationCategory1 == "Gender")
    return(c)
  })
  
  adult_gender_sub_x <- reactive({
    a <- subset(adult_gender(), adult_gender()$Question == input$Overall_xLabel)
    return(a)
  })
  
  adult_gender_sub_y <- reactive({
    b <- subset(adult_gender(), adult_gender()$Question == input$Overall_yLabel)
    return(b)
  })
  
  # Gender Graph
  output$gender_graph <- ({
    plt <- renderPlotly({
      ggplot() + 
        geom_point(aes(x=adult_gender_sub_x()$DataValue, y=adult_gender_sub_y()$DataValue, col=adult_gender_sub_x()$Stratification1)) + 
        labs(y = "Y Variable", x = "X Variable", color = "Gender") +
        theme_economist() + 
        theme(legend.position = "right", legend.title = element_text(face = "bold", size = 14))
    })
  })
  
  
# --------------------- ETHNICITY --------------------- #
  
  
  # Reactive datasets for the "Ethnicity" graph
  adult_ethnicity <- reactive({
    c <- subset(adult, adult$YearEnd == input$Overall_Year)
    c <- subset(c, c$StratificationCategory1 == "Race/Ethnicity")
    return(c)
  })
  
  adult_ethnicity_sub_x <- reactive({
    a <- subset(adult_ethnicity(), adult_ethnicity()$Question == input$Overall_xLabel)
    return(a)
  })
  
  adult_ethnicity_sub_y <- reactive({
    b <- subset(adult_ethnicity(), adult_ethnicity()$Question == input$Overall_yLabel)
    b  <- subset(b, b$LocationDesc %in% unique(adult_ethnicity_sub_x()$LocationDesc))
    return(b)
  })
  
  adult_ethnicity_sub_x_2 <- reactive({
    a <- subset(adult_ethnicity_sub_x(), adult_ethnicity_sub_x()$LocationDesc %in% unique(adult_ethnicity_sub_y()$LocationDesc))
    return(a)
  })
  
  # Ethnicity Graph
  output$ethnicity_graph <- ({
    plt <- renderPlotly({
      ggplot() + 
        geom_point(aes(x=adult_ethnicity_sub_x()$DataValue, y=adult_ethnicity_sub_y()$DataValue, col=adult_ethnicity_sub_x()$Stratification1)) + 
        labs(y = "Y Variable", x = "X Variable", color = "Race/Ethnicity") +
        theme_economist() + 
        theme(legend.position = "right", legend.title = element_text(face = "bold", size = 14))
    })
  })

  
  
  
# ----------------------------------------------------------------------------- #
# ------------------------- High School Nutrition Tab ------------------------- #
# ----------------------------------------------------------------------------- #

  # Reactive datasets for the "Overall" graph
  hs_overall <- reactive({
    d <- subset(high_school, high_school$YearEnd == input$hs_Year)
    d <- subset(d, d$DataValue != "0") # Removing the "0" values from the dataset so we are able to plot the slope. 
    return(d)
  })
  
  hs_overall_sub_x <- reactive({
    a <- subset(hs_overall(), hs_overall()$Question == input$hs_xLabel)
    return(a)
  })
  
  # Removing the null/0 values removes states that didn't have data, so our new x and y datasets would be different lengths
  # Here, I take a second subset of the y dataset I created, setting the LocationDesc = to all the locations in our new x dataset
  hs_overall_sub_y <- reactive({
    b <- subset(hs_overall(), hs_overall()$Question == input$hs_yLabel)
    b <- subset(b, b$LocationDesc %in% hs_overall_sub_x()$LocationDesc)
    return(b)
  })
  
  # Here we do the same thing as above, now setting the LocationDesc = to all the locations in our new y dataset. This creates equal length datasets that we are able to graph against each other
  hs_overall_sub_x_2 <- reactive({
    a <- subset(hs_overall_sub_x(), hs_overall_sub_x()$LocationDesc %in% unique(hs_overall_sub_y()$LocationDesc))
    return(a)
  })
  

  
  # Overall Graph
  output$hs_overall_graph <- ({
    plt <- renderPlotly({
      ggplot() + 
        geom_point(aes(x=hs_overall_sub_x_2()$DataValue, y=hs_overall_sub_y()$DataValue, col=hs_overall_sub_y()$LocationDesc)) + 
        labs(y = "Y Variable", x = "X Variable") + 
        theme_economist() +
        theme(legend.position = "none") +
        #scale_x_continuous(limits = c(0,max(x=hs_overall_sub_x_2()$DataValue))) +
        geom_smooth(method = "lm", aes(x=hs_overall_sub_x_2()$DataValue, y=hs_overall_sub_y()$DataValue)) +
        coord_cartesian(xlim = c(0,max(hs_overall_sub_x_2()$DataValue)))
    })
  })
  

  
  

  # Reactive Dataset for the Dot Plot (State by State Graph)
  hs_dot <- reactive({
    b <- subset(hs_overall(), hs_overall()$Question == input$hs_dot_var )
    b <- b[order(-b$LocationDesc),]
    return(b)
  })
  
  # Dot Plot Graph
  output$hs_dot_plot <- ({
    plt <- renderPlotly({
      ggplot(hs_dot(), aes(x=hs_dot()$DataValue, y=hs_dot()$LocationDesc)) +
        labs(y = "State", x="X Variable") +
        geom_point(aes(x=hs_dot()$DataValue, y=hs_dot()$LocationDesc)) +
        theme_economist() +
        theme(axis.title.y = element_blank()) +
        coord_cartesian(xlim = c(0,max(hs_dot()$DataValue))) +
        scale_y_discrete(limits = rev(levels(hs_dot()$LocationDesc)))
    })
  })

  
  
  # Reactive dataset choosing the state to analyze
  hs_state <- reactive({
    d <- subset(high_school, high_school$LocationDesc == input$hs_state)
    d <- subset(d, d$DataValue != "0")
    return(d)
  })
  
# Text Output for the State
  output$State_Label <- renderText({
    paste("2013-2015 Change Data For: ")
    input$hs_state
  })

# Daily Computer Use Reactive  Dataset and graph  
  hs_computer_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Computer use among high school students")
    return(a)
  })
  
  output$hs_computer <- ({
    plt <- renderPlot({
      ggplot(hs_computer_y(), aes(x=c("2013","2015"), y=hs_computer_y()$DataValue, fill=hs_computer_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        theme_economist() +
        labs(title = "Daily Computer Use") +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_computer_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
    
  })

# Daily Soda Consumption Dataset and graph  
  hs_soda_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Soda consumption among high school students")
    return(a)
  })
  
  output$hs_soda <- ({
    plt <- renderPlot({
      ggplot(hs_soda_y(), aes(x=c("2013","2015"), y=hs_soda_y()$DataValue, fill=hs_soda_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        theme_economist() +
        labs(title = "Daily Soda Consumption") +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_soda_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
# Daily TV Viewing Dataset and Graph
  hs_tv_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Television viewing among high school students")
    return(a)
  })
  
  output$hs_tv <- ({
    plt <- renderPlot({
      ggplot(hs_tv_y(), aes(x=c("2013","2015"), y=hs_tv_y()$DataValue, fill=hs_tv_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "Daily TV Viewing") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_tv_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
  
# Obesity dataset and graph
  hs_obesity_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Obesity among high school students")
    return(a)
  })
  
  output$hs_obesity <- ({
    plt <- renderPlot({
      ggplot(hs_obesity_y(), aes(x=c("2013","2015"), y=hs_obesity_y()$DataValue, fill=hs_obesity_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "% Considered Obese") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_obesity_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
  # Phys Ed dataset and graph
  hs_phys_ed_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Participation in daily school physical education classes among high school students")
    return(a)
  })
  
  output$hs_phys_ed <- ({
    plt <- renderPlot({
      ggplot(hs_phys_ed_y(), aes(x=c("2013","2015"), y=hs_phys_ed_y()$DataValue, fill=hs_phys_ed_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "% Who Participate in Daily Gym Classes") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=11)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_phys_ed_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
  
  
  # Aerobics/Exercise dataset and graph
  hs_aerobics_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Meeting aerobic physical activity guidelines among high school students")
    return(a)
  })
  
  output$hs_aerobics <- ({
    plt <- renderPlot({
      ggplot(hs_aerobics_y(), aes(x=c("2013","2015"), y=hs_aerobics_y()$DataValue, fill=hs_aerobics_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "% Who Meet Exercise Requirements") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,50)) +
        geom_text(aes(label = hs_aerobics_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
  # % healthy weight dataset and graph
  hs_healthy_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Healthy weight among high school students")
    return(a)
  })
  
  output$hs_healthy <- ({
    plt <- renderPlot({
      ggplot(hs_healthy_y(), aes(x=c("2013","2015"), y=hs_healthy_y()$DataValue, fill=hs_healthy_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "% at a Healthy Weight") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12)) +
        coord_cartesian(ylim = c(0,100)) +
        geom_text(aes(label = hs_healthy_y()$DataValue), size = 5, hjust = 0.5, vjust = -1, col = "black", position = "stack")
    })
  })
  
  # Daily fruit consumption dataset and graph
  hs_fruit_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Median daily frequency of fruit consumption among high school students")
    return(a)
  })
  
  output$hs_fruit <- ({
    plt <- renderPlot({
      ggplot(hs_fruit_y(), aes(x=c("2013","2015"), y=hs_fruit_y()$DataValue, fill=hs_fruit_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "Daily Fruit Consumption") +
        theme_economist() + 
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12))
    })
  })
  
  # Daily Vegetable consumption dataset and graph
  hs_veggie_y <- reactive({
    a <- subset(hs_state(), hs_state()$Question == "Median daily frequency of vegetable consumption among high school students")
    return(a)
  })
  
  output$hs_veggie <- ({
    plt <- renderPlot({
      ggplot(hs_veggie_y(), aes(x=c("2013","2015"), y=hs_veggie_y()$DataValue, fill=hs_veggie_y()$YearEnd)) +
        geom_bar(stat = "identity") +
        labs(title = "Daily Vegetable Consumption") +
        theme_economist() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12))
    })
  })

}

shinyApp(ui=ui, server=server)

