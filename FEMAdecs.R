library(ggplot2)
library(googleVis)
require(shiny)
library(zipcode)

data1<-na.omit(read.csv("FEMAdecs.csv"))
data2<-na.omit(read.csv("FEMAdecs.csv"))
data2<-data2[2:32305,c(6,9,10,11,12,14,15),]
names(data2)[1:7]<- c("State","IncidentType","Title","IncidentBeginDate","IncidentEndDate","Zip","County")

data2<-data2[2:32305,]

trial<-data.frame(data2,gsub('(County)', '', data2$County))
data3<-data.frame(trial, gsub("\\s*\\([^\\)]+\\)","",as.character(trial$County)))
data3$Location <- do.call(paste, c(data3[c(1, 9)], sep = ",")) 

data3<-data3[,c(1:6,10)]


data(zipcode)
zdat<-transform(zipcode, Location=interaction(state,city,sep=','))

#Merge Zipcode Data (zdat) and FEMA Disaster Data (data3) for geolocation lookup prep
geodat<-merge(data3,zdat,by="Location")
geodat$latlong<-paste(geodat$latitude, geodat$longitude, sep = ':')
geodat$IncidentBeginDate<-as.Date(geodat$IncidentBeginDate, format="%m/%d/%Y")
geodat$IncidentEndDate<-as.Date(geodat$IncidentEndDate, format="%m/%d/%Y")

##Server
shinyServer(function(input, output){
  dataInput<-reactive({
    
    switch(input$incidents,
           choices=levels(geodat$IncidentType))
  }
  )
  
  
  output$gvis<-renderGvis({
    subDat<-subset(geodat, geodat$IncidentType %in% input$incidents)
    gvisGeoChart(subDat,
                 locationvar='latlong',
                 colorvar='IncidentType',
                 options=list(region='US', 
                              displayMode='markers',
                              resolution='provinces',
                              width=800,
                              height=500
                 ))
  }
  )
}
)


#UI
require(shiny)
shinyUI(pageWithSidebar(
  headerPanel(textOutput("FEMA Disaster Reporting")),
  sidebarPanel(
    selectInput("incidents", "Choose Incident:", choices = levels(geodat$IncidentType))),
  
  mainPanel(
    htmlOutput("gvis")
    
  ))
)



##Reg Plot
plot(data3)
