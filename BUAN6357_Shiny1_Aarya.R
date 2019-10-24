
library(shiny)
library(readxl)

my_data_girl <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 1)
data_girl = my_data_girl[-c(1:3),]
x_girl = data_girl[[1,3]]
y_girl = data_girl[4:103,3:4]
names(y_girl) = c(paste("names",x_girl,sep=""),paste("count",x_girl,sep=""))

for(i in seq(from=5, to=194, by=3)){
  curr_year = data_girl[[1,i]]
  temp_name_count = data_girl[4:103,(i+1):(i+2)]
  names(temp_name_count) = c(paste("names",curr_year,sep=""),paste("count",curr_year,sep=""))
  y_girl = cbind(y_girl,temp_name_count)
}

my_data_boys <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 2)
data_boys = my_data_boys[-c(1:3),]
x_boys = data_boys[[1,3]]
y_boys = data_boys[4:103,3:4]
names(y_boys) = c(paste("names",x_boys,sep=""),paste("count",x_boys,sep=""))

for(i in seq(from=5, to=194, by=3)){
  curr_year_boys = data_boys[[1,i]]
  temp_name_count_boys = data_boys[4:103,(i+1):(i+2)]
  names(temp_name_count_boys) = c(paste("names",curr_year_boys,sep=""),paste("count",curr_year_boys,sep=""))
  y_boys = cbind(y_boys,temp_name_count_boys)
}

my_data_g <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 1)
data_g = my_data_g[-c(1:3),]
x_g = data_g[[1,3]]
y_g = data_g[4:103,3:4]
names(y_g) = c(paste("names",x_g,sep="_"),paste("count",x_g,sep="_"))

for(i in seq(from=5, to=194, by=3)){
  if(i==95){
    curr_year_g = data_g[[1,(i+1)]]
    temp_name_count_g = data_g[4:103,(i+1):(i+2)]
    names(temp_name_count_g) = c(paste("names",curr_year_g,sep="_"),paste("count",curr_year_g,sep="_"))
    y_g = cbind(y_g,temp_name_count_g)
  } else{
    curr_year_g = data_g[[1,i]]
    temp_name_count_g = data_g[4:103,(i+1):(i+2)]
    names(temp_name_count_g) = c(paste("names",curr_year_g,sep="_"),paste("count",curr_year_g,sep="_"))
    y_g = cbind(y_g,temp_name_count_g)}
}
nms = y_g[,1]
for (i in seq(from=3, to=129, by=2)){
  nms = union(nms,y_g[,i])
}

my_data_b <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 2)
data_b = my_data_b[-c(1:3),]
x_b = data_b[[1,3]]
y_b = data_b[4:103,3:4]
names(y_b) = c(paste("names",x_b,sep="_"),paste("count",x_b,sep="_"))

for(i in seq(from=5, to=194, by=3)){
  if(i==56){
    curr_year_b = data_b[[1,(i+1)]]
    temp_name_count_b = data_b[4:103,(i+1):(i+2)]
    names(temp_name_count_b) = c(paste("names",curr_year_b,sep="_"),paste("count",curr_year_b,sep="_"))
    y_b = cbind(y_b,temp_name_count_b)  
  } else{
    curr_year_b = data_b[[1,i]]
    temp_name_count_b = data_b[4:103,(i+1):(i+2)]
    names(temp_name_count_b) = c(paste("names",curr_year_b,sep="_"),paste("count",curr_year_b,sep="_"))
    y_b = cbind(y_b,temp_name_count_b)
  }
  
}
nms_b = y_b[,1]
for (i in seq(from=3, to=129, by=2)){
  nms_b = union(nms_b,y_b[,i])
}

library(shiny)
library(tidyverse)
library(DT)

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Assignment 1"),
  dashboardSidebar(),
  dashboardBody(
    (navbarPage("Most Popular Baby Names Revealed !!",
                tabPanel("Popular Girl Names Per Year",
                         sidebarPanel(HTML("Enter year"),
                                      
                                      # Numeric input for sample size
                                      numericInput(inputId = "n",
                                                   label = "Year",
                                                   value = 1954,
                                                   min = 1954, max = 2018,
                                                   step = 1)),
                         mainPanel(DT::dataTableOutput(outputId = "rankTable"))
                ),
                tabPanel("Popular Boy Names Per Year",
                         sidebarPanel(HTML("Enter year"),
                                      
                                      # Numeric input for sample size
                                      numericInput(inputId = "nb",
                                                   label = "Year",
                                                   value = 1954,
                                                   min = 1954, max = 2018,
                                                   step = 1)),
                         mainPanel(DT::dataTableOutput(outputId = "rankTable1"))
                ),
                tabPanel("Popularity of girl Names over Years",
                         sidebarPanel(HTML("Enter year"),
                                      
                                      # Numeric input for sample size
                                      selectInput(inputId = "name","Please select Girl's name",
                                                  choices = unique(nms),
                                                  selected = "Christine")),
                         mainPanel(plotOutput(outputId = "scatterplot"))
                ),
                tabPanel("Popularity of boy Names over Years",
                         sidebarPanel(HTML("Enter year"),
                                      
                                      # Numeric input for sample size
                                      selectInput(inputId = "nameb","Please select Boys's name",
                                                  choices = unique(nms_b),
                                                  selected = "John")),
                         mainPanel(plotOutput(outputId = "scatterplot1"))
                )
                
    )
    )
  )
)
server <- function(input, output) {
  # Create data table
  output$rankTable <- DT::renderDataTable({
    req(input$n)
    dt_girl = y_girl[1:10,c(paste("names",toString(input$n),sep=""),paste("count",toString(input$n),sep=""))]
    DT::datatable(data = dt_girl, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$rankTable1 <- DT::renderDataTable({
    req(input$nb)
    dt_boys = y_boys[1:10,c(paste("names",toString(input$nb),sep=""),paste("count",toString(input$nb),sep=""))]
    DT::datatable(data = dt_boys, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$scatterplot <- renderPlot({
    allPossibleYears = c(1954:2018)
    allCounts = numeric()
    for (i in 1954:2018){
      nameYear = paste("names",i,sep="_")
      countYear = paste("count",i,sep="_")
      temp = select(y_g,nameYear,countYear)[y_g[,nameYear] == input$name,]
      count = 0
      if(nrow(temp) == 0){
        count = 0
      } else{
        count = temp[,countYear]
      }
      allCounts = c(allCounts,count)
    }
    plot(allPossibleYears,allCounts)
  })
  
  output$scatterplot1 <- renderPlot({
    allPossibleYears = c(1954:2018)
    allCounts = numeric()
    for (i in 1954:2018){
      nameYear = paste("names",i,sep="_")
      countYear = paste("count",i,sep="_")
      temp = select(y_b,nameYear,countYear)[y_b[,nameYear] == input$nameb,]
      count = 0
      if(nrow(temp) == 0){
        count = 0
      } else{
        count = temp[1,countYear]
      }
      allCounts = c(allCounts,count)
    }
    plot(allPossibleYears,allCounts)
  })
  
}

shinyApp(ui, server)

