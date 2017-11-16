library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(dplyr)
library(rgl)
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
beerdf<-data.frame(beerdf,row.names=NULL, stringsAsFactors = FALSE)
colnames(beerdf)<-c("name","Calories","ABV","Color_SRM","IBU")
beerdf$Calories<-as.numeric(beerdf$Calories)
beerdf$ABV<-as.numeric(beerdf$ABV)
beerdf$Color_SRM<-as.numeric(beerdf$Color_SRM)
beerdf$IBU<-as.numeric(beerdf$IBU)
beertbl<- tbl_df(beerdf)
beerdf

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
    sel<- nearPoints(beerdf,user_click,threshold = 100,maxpoints = 1)
    return(sel)
  })
  
  output$tableDT<- DT::renderDataTable(DT::datatable(beer()))
  output$tableDTog<- DT::renderDataTable(beerdf,options = list(paging=F), rownames=F, filter = "top")
  
  output$mydownload <- downloadHandler(
    filename = "beers.csv",
    content = function(file){
      write.csv(beerdf,file)})
}

ui <-   fluidPage(
  theme=shinytheme("cosmo"),
  titlePanel(strong("Visualizing common beers")),
  h1("Brice Randolph"),
  p("The goal of this visualization is to see if there are any combinations of 
    bitterness, alcohol content, and color that are not commonly observed.  In other words, can we 
    use this information to find new types of beer?"),
  h3("Click on a point to select a specific beer"),
  plotOutput("plot", click="user_click"),
  DT::dataTableOutput("tableDT"),
  h1("The entire data set can be seen below:"),
  DT::dataTableOutput("tableDTog"),
  downloadButton(outputId = "mydownload", label = "Download Table")
)

shinyApp(ui = ui, server = server)

