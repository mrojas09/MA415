#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# 
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(shinythemes)

#import data from CSVs
data2010 <- read.csv('BP Apprehensions 2010.csv')
data2017 <- read.csv('PB Apprehensions 2017.csv')
ts1 <- read.csv('PB monthly summaries.csv',row.names=1) #Reads in Csv
ts2 <- as.vector(t(ts1))


# Define UI for application

ui <- fluidPage(theme = shinytheme("slate"),
                
                #title
                titlePanel("US-Mexico Boarder Patrol Apprehensions"),
                
                #sidebar where you can select the sector
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    selectInput('comp', 'Choose comparison', 
                                choices = c('Monthly Summaries'= '10', 'Big Bend' = '1', 'Del Rio' = '2', 'El Centro' = '3', 'El Paso' = '4', 'Laredo' = '5', 'Rio Grande Valley' = '6', 'San Diego' = '7', 'Tuscon' = '8', 'Yuma' = '9'),
                                selected = 'Big Bend'),
                    br(),
                    
                    # Cnn Report Video that was provided to us via Blackboard 
                    h3("Trumps Border Wall Pledge"),
                    HTML('<iframe width="390" height="270" src="https://www.youtube.com/embed/lk99uQQGEok" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                    br(),
                    
                    # Written Summary of the CNN report provided to us via Blackboard 
                    h4("US-Mexico boarder apprehensions have been steadily declining since 2000, and in April of 2017 they reached historic lows."), 
                    h4("These downward trends and record lows can be accredited to President Trump’s election. "),
                    h4("The data shows that President Trump’s aggression towards immigration laws is having a deterring effect, and migrants are less enticed to enter the country. "),
                    h4("DHS credits the administration policy as the lowest apprehensions prior to this past April were in December of 2011. This new low is 7,000 apprehensions less than the previous. In a year since April 2016, apprehensions were down 62%, though there are multiple different reasons for such a change."),
                    h4("Here you can find the monthly summaries of apprehensions by sector in a time series from 2000 to 2017 depicting the changes throughout time."),
                    br(),
            
                    # Trump Cartoon Video
                    h5("Actual Trump Quotes Read by Cartoon Trump"),
                    HTML('<iframe width="390" height="270" src="https://www.youtube.com/embed/DREO6cwbGeQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                    br(),
                    
                    #Logos
                    h5("Powered by: "),
                    img(src= "RStudio-Ball.png",height = 50, width = 50, hspace = "10"), 
                    tags$img(src= "CNN-Logo.png",height = 60, width = 70, hspace = "10"),
                    tags$img(src= "git-desktop-logo.png",height = 60, width = 60, hspace = "20"),
                    tags$img(src= "Youtube-logo-black.png",height = 60, width = 70)
                   
                  ),
                  
                  #Location of barplot
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph", plotOutput("appPlot", height = 500)),
                      tabPanel("Table", tableOutput("table"))
                      
                    ),
                    
                    # Top 3 Apprehensions per years 2010 & 2017
                    #max_apps_2010 <- tail(sort(mat["2010",]),3),
                    #max_apps_2017 <- tail(sort(mat["2017",]),3),
                    
                    br(),
                    h5("Top 3 highest Apprehensions for the year 2000 are:    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Top 3 highest Apprehensions for the year 2000 are:"),
                    h6("61361 for the month of March   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _   46184 for the month of October"),
                    h6("55237 for the month of April   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  47211 for the month of November"),
                    h6("47045 for the month of May   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _   43251 for the month of December"),
                    br(),
                    
                    #Video of Trump Singing "Do you want to build a wall" to Frozen background music
                    h3("Do You Want To Build a Wall? - Donald Trump (Frozen Parody)"),
                    HTML('<iframe width="890" height="540" src="https://www.youtube.com/embed/GVN17U3Vg34" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                    br(),
                    
                    #Video Caption
                    h4('Whats the best way for United States president-elect Donald Trump to convince people that a wall between America and Mexico is a good idea?'),
                    h4('Why, a song in the style of Frozens "Do You Want To Build a Snowman?" of course.'),
                    h3('Enjoy!'),
                    br()
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Reactive which processes data based on selected Sector
  selectedData <- reactive({
    if (as.numeric(input$comp) != 10){
      selected = as.numeric(input$comp)
      
      #take the selected row from each dataset 
      d1 = as.data.frame(data2010[c(selected),])
      d2 = as.data.frame(data2017[c(selected),])
      
      #Bind the data from each dataset
      mydata <- rbind(d1, d2)
      
      #Take out the row which lists the sector
      mydata$Sector <- NULL
      #Specify which row is which year
      rownames(mydata) <- c("2010", "2017")
      
      return(mydata)
    }
    else{
      return(ts1) # else returns summary data
    }
    
  })
  
  output$appPlot <- renderPlot({
    
    #Take reactive data
    mydata <- selectedData()
    
    # If not monthly Summaries then do this
    if (as.numeric(input$comp) != 10){
      #Plot the data
      
      months <- c("October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August", "September")
      par(bg = "black") # Plot Background Color
      
      barplot(as.matrix(mydata),main = "2010/2017 US. Apprehensions", beside = TRUE, xlab = "Months", ylab = "Apprehensions",
              names.arg = months, 
              col = c("limegreen", "maroon"), col.main = "white", font.main = 2,family = "mono", bty="n",axes = FALSE)
      
      xTicks <- c(2,5,8,11,14,17,20,23,26,29,32,35) # x Axis tick mark positions
      
      #Custom x and y axis
      axis(1, at = xTicks, labels = months, col = 'maroon', col.axis = 'white', col.ticks = 'limegreen', cex.axis = 0.9, font = 2, family = 'mono',las=2)
      axis(2, col = 'maroon', col.axis = 'white', col.ticks = 'limegreen', cex.axis = 0.9, font =3, family = 'mono',las=2)
      
      legend("topright",legend=c(":  2010", ":  2017"), fill = c("limegreen","maroon"), xjust = 0, title.adj = 0.5,title = "Legend",title.col = "grey30",
             text.col = "grey45",box.lty=2, box.lwd=2, box.col="limegreen")
      grid(nx = NA, ny = NULL, col = "grey26", lty = 2)
    }
    else {
      
      # if monthly summaries is chosen then do this
      ts3 <- ts(rev(ts2), start = c(2000, 1), frequency=12)
      
      mat <- as.matrix(ts1)
      
      avg_lst <- list() # List used to append the averages of each year 
      for (i in 1:18){
        element <- mean(mat[i,])
        avg_lst <- c(avg_lst,element)
      }

      avgVec <- as.vector(rev(avg_lst)) 
      years <- c(2000:2017) #  will be used as a paramter for the points
      yearStr <- as.character(years) # Conversion of proir vector to Strings to be used as labels
      
      par(bg = "black")
      ts.plot(ts3, gpars=list(main = "US Apprehensions Yearly Summary", col.main = "white", font.main = 2,family = "mono",xlab="Year", ylab="Apprehensions",xlim = c(2000,2017), ylim = c(0,250000),pch = 19,axes = FALSE,col= 'maroon', lwd=2,lty=c(1:3)))
      yticks <- seq(50000, 200000, 20000) # y Axis Bounds incrementing by 20k
      
      #Custom x and y axis
      axis(1, col = 'maroon', col.axis = 'white', col.ticks = 'limegreen', cex.axis = 1.5, font = 2, family = 'mono')
      axis(2, at = yticks, col = 'maroon', col.axis = 'white', col.ticks = 'limegreen', cex.axis = 0.9, font =3, family = 'mono',las=2)
      
      abline(h = yticks, lty = 2, col = "grey26") # Horizontal line Grid
      
      # Mean apprehensions per year
      points(years, avgVec, pch = 13, col = "limegreen",lwd = 1)
      text(years,avgVec,labels=yearStr, cex=0.6, pos = 3,col= "white") # Point Labels
      
      legend(2011,250000,pch=c(13),col = "limegreen",title.adj = 0.5,title = "Legend",title.col = "grey30",text.col = "grey45",legend=c(":  Avg. Apprehension"),box.lty=2, box.lwd=2, box.col="limegreen")
      
    }
  })
  
  output$table <- renderTable(selectedData())
}

# Run the application 
shinyApp(ui = ui, server = server)
  
