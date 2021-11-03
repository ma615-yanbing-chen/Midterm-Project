#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sp)
library(sf)
library(tidyverse)
library(magrittr)
library(maps)
library(plotly)
library(rsconnect)
rsconnect::setAccountInfo(name='jacobchen',
                          token='728B069FB9E9A968BFE1DDF08DB18568',
                          secret='/SRxqQN2P3/yHYGCIZusUZa4gqfwWWFGdn07FRnJ')

data(state.fips)
mapstate=st_as_sf(map('state',plot = F,fill = T))
pop <- paste("State:",toupper(mapstate$ID))
Sys.setlocale(category = "LC_CTYPE", locale = "C")

#### Load data

strawb<-read.csv("Strawberries.csv")

pest<-read.csv("Pesticides.csv")


#### Drop the no-info columns

drop_no_info_cols <- function(df){
    cnames = colnames(strawb)
    T = NULL
    for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
    drop_cols <- cnames[which(T == 1)]
    return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

#Separate Data.Item into 2 columns
strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

#### explore
distinct(strawb, Strawberries)

distinct(strawb, items)

distinct(strawb, discription)

distinct(strawb, units)

#Separate Domain into 2 columns(?)
strawb %<>% separate(col=Domain,
                     into = c("dname","type"),
                     sep = ",",
                     fill = "right")
distinct(strawb,dname)
distinct(strawb,type)

# make a copy of Domain.Category 
strawb %<>%
    mutate(Chemicals=Domain.Category) %>%
    relocate(Chemicals, .after=Domain.Category)

## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals
bb <- strawb$Chemicals %>% str_detect("CHEM")

## index 
ind_C <- (!bb)*(1:dim(strawb)[1])
## 
r1 <- ind_C[ind_C > 0]
## set entries in Chemicals column to " " if they don't start with CHEM
strawb$Chemicals[r1] <- " "


## now we need a list of chemicals

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))
distinct(strawb, details)
distinct(strawb, type)

strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                     (type=="HERBICIDE")|
                                     (type=="INSECTICIDE")|
                                     (type=="OTHER"))
distinct(strawb,type)

# separate details into 2 columns
strawb_chem %<>% separate(col = details,
                          into = c("Pesticide", "Pesticides_code"),
                          sep = "=",
                          fill = "right")
strawb_chem$Pesticide<-tolower(strawb_chem$Pesticide)
strawb_chem$Pesticide=str_trim(strawb_chem$Pesticide)

distinct(strawb_chem,Pesticide)

##### pest
pest_no<-pest[!apply(pest =="", 1, all),]

colnames(pest_no)[1]="Pesticide"
pest_no$Pesticide<-tolower(pest_no$Pesticide)
a<-pest_no %>% `$`(Pesticide) %>% unique() 
pest_both<-filter(strawb_chem,Pesticide %in% a)


######wrangle
strawb_pest<-dplyr::left_join(pest_both,pest_no,by='Pesticide')

levels(as.factor(strawb_pest$Strawberries))

strawb_pest_cut <- strawb_pest[,-c(1,5,6,10,12,13,17)]
strawb_pest_cut_data <- strawb_pest_cut[strawb_pest_cut$Value != " (D)",]
f_data <- strawb_pest_cut_data[strawb_pest_cut_data$Bee.Toxins != "",]

### Shinny application start

State <- c("california", "florida", "new York", "washington")
Percentage <- c(0.9, 0.08, 0.01, 0.02)

data1 <- data.frame(State, Percentage)
mapStates = map("state", fill = TRUE, plot = FALSE)
pal <- colorNumeric(
    palette = "RdBu",
    domain = Percentage*100
) 

# UI
ui <- navbarPage("MA 615 Midterm",id = "MT",
    tabPanel("Interactive map",
             setBackgroundImage(src = "13.jpeg"),
    sidebarLayout(      
        sidebarPanel(
            helpText("The proportion of production of Strawberry of US in 2017",color = "red"),
        ),
        mainPanel(
            tabsetPanel(
             tabPanel("map",leafletOutput("map"))
            )
        )
    )
    ),
    tabPanel("EDA",
    sidebarLayout(
        sidebarPanel(
            selectInput("year1", "Year:", 
                        choices=c("2016","2017","2018","2019")),
            selectInput("state1","State:",
                        choices=c("CALIFORNIA","FLORIDA","WASHINGTON"))
        ),
        mainPanel(
            tabsetPanel(
             tabPanel("Plot1",plotOutput("plot1")),
             tabPanel("Point Plot",plotOutput("plot2")),
             tabPanel("Percentage of Pesticide Usage",plotlyOutput("plot3"))
            )
        )
    ))
)

#Serevr

    server <- function(input, output) {
        output$plot1<-renderPlot(
            ggplot(data = f_data[f_data$Year == input$year1,])+
                geom_bar(mapping = aes(x = Bee.Toxins),fill = "lightblue"))
        output$plot2<-renderPlot(
            ggplot(data = filter(f_data,Year==input$year1,State==input$state1) %>% 
                       group_by(Pesticide,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE)),
                   aes(x=sum_value,y=Bee.Toxins,group=Pesticide,color=Pesticide))+
                geom_point(stat="identity",size=10)+
                labs(x='Dose of each Pesticide',y='To Bee.Toxin',title='Pesticide Usage/Bee Toxins in 2019 California')+
                theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(color='black',size=23),
                                                                    axis.text.y = element_text(color='black',size=25))+
              theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
              theme(text=element_text(family="Times New Roman",size=28))+
              theme(text=element_text(size=28, family="Times New Roman"))+
              theme(plot.title = element_text(hjust = 0.5)))
        output$plot3<-renderPlotly(plot_ly(
                                     data = f_data[f_data$Year == input$year1& f_data$State == input$state1,],
                                     type = "pie",
                                     values = ~Value,
                                     labels = ~Pesticide,
                                     text = ~Bee.Toxins,
                                     hovertemplate = "%{label}; <br>Percentage: %{percent}; </br>%{values} "))
        output$map<-renderLeaflet(
            leaflet(mapstate)%>%
                addTiles()%>%
                addPolygons(data=mapstate,color='white',stroke = F,fillOpacity = 0) %>% 
                addPolygons(data = mapstate %>% filter(ID %in% c("california","florida","new york","washington")),
                            stroke = T,label = paste0(data1$Percentage*100,"%"),fillOpacity = 0.7,weight = 0.5,
                            color = ~pal(data1$Percentage*100),) %>% 
                addLegend(pal = pal, values = ~Percentage*100, labFormat = labelFormat(suffix='%'),opacity = 0.7,title='Percentage') %>% 
                addProviderTiles(providers$Stamen.Terrain)
        )
}

       
# Run the application 
shinyApp(ui = ui, server = server)