#install.packages("DT")
#install.packages("tidyr")
#install.packages("shiny")
#install.packages('rsconnect')
#install.packages("shinythemes")
library(rsconnect)
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(dplyr)

##### data cleaning/assembly 
# note: SRM chosen to measure color these measurements vary based on source
# note: IBU can also vary based on source (since these measurements were constructed
# from multiple websites and user comments)
#
#         -   data description   -   
#
# Beer Name, Calories, ABV, COLOR-SRM, IBU

bluemoon<-c("Blue Moon Belgian White", 164, .0536, 13, 4.5)
budlight<-c("Bud Light", 110, .042, 3, 18)
coorslight <- c("Coors Light", 102, .042, 2, 15)
corona<- c("Corona Extra", 148, .046, 2.6, 12)
guinness<-c("Guinness Draught", 125, .0427, 50, 42)
negromodelo<-c("Negra Modelo", 170, .054, 19, 28)
heineken <-c("Heineken", 150, .05, 3.55, 23)
fattire<- c("New Belgium Fat Tire", 160, .052, 16, 22)
newcastle <-c("Newcastle Brown Ale", 150, .047, 23, 20)
pacifico<- c("Pacifico", 145, .044, 3, 14)
redstripe<- c("Red Stripe", 153, .049, 4, 10)
samadamsoct<- c("Sam Adams Octoberfest", 180, .054, 20, 16)
shocktop<- c("Shock Top", 168, .052, 4, 10)
sierranevada<-c("Sierra Nevada Pale Ale", 175, .056, 7, 38)
stella<-c("Stella Artois",154, .052, 3.6, 30)

beerdf<-rbind(bluemoon,budlight,coorslight,corona,guinness,
              negromodelo,heineken,fattire,newcastle,pacifico,
              redstripe,samadamsoct,shocktop,sierranevada,stella)
beerdf<-as.data.frame(beerdf)
str(beerdf)
colnames(beerdf)<-c("name","Calories","ABV","Color_SRM","IBU")
row.names(beerdf)<-NULL
beerdf$name<-as.character(beerdf$name)
beerdf$Calories<-as.numeric(levels(beerdf$Calories))
beerdf$ABV<-as.numeric(levels(beerdf$ABV))
beerdf$Color_SRM<-as.numeric(levels(beerdf$Color_SRM))
beerdf$IBU<-as.numeric(levels(beerdf$IBU))

beertbl<- tbl_df(beerdf)
beerdf
####################################################################
# to host on shinyapps.io


# shiny app

server = function(input, output, session) {
  library(ggplot2)
  library(DT)
  library(shiny)
  
  
  output$plot<- renderPlot({
    ggplot(beerdf, aes(Calories, ABV)) + geom_point()
  })
  
  beer<- reactive({
    user_click<- input$user_click
    sel<- nearPoints(beerdf,user_click,threshold = 10,maxpoints = 5)
    return(sel)
  })
  output$table<- DT::renderDataTable(DT::datatable(beer()))
  output$table<- DT::renderDataTable(beertbl[1:15,],
                                        options = list(paging=F),
                                        rownames=F,
                                        filter="top")
}

ui <-   fluidPage(
  theme=shinytheme("cosmo"),
  titlePanel(strong("Visualizing common beers")),
  h1("Brice Randolph"),
  plotOutput("plot", click="user_click"),
  DT::dataTableOutput("table"),
  
  sidebarLayout(
    
    sidebarPanel(
      withTags(
        div(
          b("bold"),
          br(),
          hr(),
          code("plot(lynx)")
        ))),
    
    mainPanel(
      tabsetPanel(
        
      )
    ))
)
shinyApp(ui = ui, server = server)



######################################################################
# try using openGL based package to 3d plot data
install.packages("rgl")
library(rgl)
file.show(system.file("NEWS", package = "rgl"))
example(surface3d)
example(plot3d)
open3d()
plot3d(beerdf$ABV,beerdf$Color_SRM,beerdf$IBU,col = rainbow(15))
# result: hard to see what's going on. cannot interact with points
