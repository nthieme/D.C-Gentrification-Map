library(shiny)
library(shinydashboard)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)
library(maptools)
library(rgeos)
library(mapproj)
library(raster)
library(rasterVis)
library(htmlwidgets)
library(stringi)

### A working version of this app is at https://furriertransform.shinyapps.io/GentDC/


##get shape file
tract <- readOGR(dsn=".", layer = "cb_2015_11_tract_500k")
tract@data$GEOID<-as.character(tract@data$GEOID)

##get tract data
D= read.csv("tract_diff1.csv")
gen.trac=c(1901, 2101, 2102, 9508, 9509,2201,2202, 2400,2301,2502, 2801, 2900, 3100,3200,2802,3500,3400,3301,9203,9201,9301, 9302, 5301, 5201, 4801, 4600, 8701, 8702, 9000, 4701, 4702, 10800, 10600, 8410, 8402, 8802, 8804, 8903, 9601, 8001, 7901, 7903, 7808, 9903, 9905, 8002, 10200, 6801, 6802, 11000, 6400, 7200, 7100, 10400)
dng = c(702, 9505, 9507, 9501, 2302, 9504, 9400, 9204, 9102, 11100, 8803, 8904, 9602, 7806, 7809, 7807, 7803, 7804, 9603, 9604, 9904, 9906, 9907, 7709, 7707, 9902, 7603, 7601, 7605, 7603, 7401, 7503, 7504, 7502 , 7407, 7406, 7408, 7403, 7409, 7304, 9700, 9804, 9803, 9801, 9802, 9807, 9810, 9811, 19000)
D = D[which(D[,1]%in%c(gen.trac, dng)),]
gen.stat = ifelse(D[,1]%in%gen.trac,"gen", "dng")
D = cbind(D,gen.stat)

D[,3]= as.character(D[,3])
D[,4]= as.character(D[,4])

##join tract shape with tract data
for(i in 1:nrow(D)){
  if(D[i,3]%in%c("non","none")){
    D[i,3]=0
  }else{
    if(D[i,3]=="new"){
      D[i,3]=.04
    }
  }
}

for(i in 1:nrow(D)){
  if(D[i,4]%in%c("non","none")){
    D[i,4]=0
  }else{
    if(D[i,4]=="new"){
      D[i,4]=.04
    }
  }
}
D[,3]=as.numeric(D[,3])*100
D[,4]=as.numeric(D[,4])*100

D[which(nchar(D[,1])==3),1] = paste("11001000",D[which(nchar(D[,1])==3),1], sep="")
D[which(nchar(D[,1])==4),1] = paste("1100100",D[which(nchar(D[,1])==4),1], sep="")
D[which(nchar(D[,1])==5),1] = paste("110010",D[which(nchar(D[,1])==5),1], sep="")
names(D)[1]="id"

id = c(11001001901,11001002101, 11001002102,11001009505,11001009507, 11001002201,11001002202,11001009508,
       11001009501,11001009509,11001002400, 11001002301,11001002302,11001009504,11001009400,11001002502,11001002801,
       11001002900,11001003100,11001003200,11001009201,11001009301,11001002802,11001003500,11001003400,11001003301,
       11001009203,11001009204, 11001009302, 11001000702, 11001005301, 11001005201, 11001004801,1001004600,
       11001008701,11001008702,11001009102, 11001011100,11001009000,11001004701,11001004702, 11001010600,11001008803,
       11001008804,11001009601,11001008410, 11001008402,11001008802, 11001008903,11001008904,11001009602,11001007806,11001007809,
       11001008001,11001007901,11001007903,11001009604,11001009603,11001007803,11001007804, 11001007808,11001007807,
       11001010800,11001008002,11001006801,11001006802,11001009906,11001009907, 11001007707,11001009904,11001009905,
       11001009902,11001010200,11001011000,11001006400,11001007200,11001007100,11001007601,11001007709,11001007603,
       11001007502, 11001009301,11001007403,11001007409, 11001007503, 11001007504,11001007407,11001007406,11001007401,
       11001010400,11001007304, 11001009804,11001009700,11001009801,11001009802, 11001009811,11001009803,11001009810,
       11001009807)
name = c("Brightwood", "Brightwood Park", "South Manor Park", "Hampshire Knolls", "Queen Chapel", "North Petworth",
         "East Petworth", "East Ft Totten", "Pleasant Hill", "North Michigan Park", "South Petworth", "SE Petworth",
         "AF Retirement", "S University Heights", "E Brookland", "Between 16th and Columbia", "NE Columbia Heights",
         "NW Columbia Heights", "East Park View", "West Park View", "North Edgewood", "East Brookland", "East Columbia Heights",
         "West Columbia Heights","Howard University", "East of Howard", "SE Edgewood","SW Edgewood","SE Brookland",
         "N Glover Park", "Dupont Circle", "E Dupont Circle", "Shaw", "Truxton Circle", "East Eckington", "Eckington",
         "Brentwood", "Gateway/SCentral/Langdon/Arboretum", "Ft. Lincoln", "N NoMA/Mt Vernon", "S NoMA/MT Vernon",
         "NOMA", "Ivy City", "N Trinidad",
         
         "S Trinidad", "Eastland Gardens", "N Trindad/Stanton", "S Trinidad/Stanton",
         "Carver", "Langston", "Mayfair", "N Deanwood", "S Deanwood", "Stanton/Kingman", "NW Kingman Park", "NE Kingman Park",
         "River Terrace", "Benning", "Central Northeast", "Lincoln Heights", "Northeast Boundary ", "Burrville",
         "GWU", "S Kingman Park", "Hill East", "South Hill East", "N Benning Ridge", "Benning Ridge", "S Benning Ridge",
         "West Marshall Heights", "East Marshall Heights", "Ft Davis Park", "Southwest Waterfront", "Ft Mcnair",
         "Buzzard Point", "Navy Yard", "E Navy Yard", "Fairlawn", "N Fairlawn", "Hillcrest", "Skyland/Buena Vista",
         "Garfield Heights/Buena Vista", "Shipley/Douglas", "Shipley", "Historic Anacostia", "S Historic Anacostia",
         "S Barry Farms", "Barry Farms", "N Barry Farms", "St Elizabeth/Congress Heights", "Douglass", "Congress Heights",
         "E Washington Highlands", "N Washington Highlands", "Washington Highlands", "S Washington Highlands", 
         "S Congress Heights", "Bellevue/Washinton Highlands", "Bellevue")


R = cbind(id, name)

# create a new version
df.polygon2<-tract #tract is the 

twelve = read.csv("dec_2012.csv")
dc = which(twelve$tract_fips%in%df.polygon2$GEOID)
twelve = twelve[dc,]
twelve_keep = twelve[which(twelve[,1]%in%D$id),]
twelve_200 = twelve_keep[,c(1,8)]

fifteen = read.csv("jun_2015.csv")
dc = which(fifteen$tractcode%in%df.polygon2$GEOID)
fifteen = fifteen[dc,]
fifteen_keep = fifteen[which(fifteen[,1]%in%D$id),]
fifteen_200 = fifteen_keep[,c(1,2)]

diff_wifi = fifteen_200-twelve_200
D = cbind(D, diff_wifi)

ggtract<-fortify(tract, region = "GEOID") 
ggtract<-left_join(ggtract, D, by=c("id")) 
ggtract = ggtract[(1:nrow(ggtract))[-which(is.na(ggtract$CaBi_start))],]

#~centers for distinguishing gentrified
near_cent=ggtract%>%group_by(id)%>%summarise(maxlat = max(lat), minlat=min(lat), maxlong = max(long), minlong=min(long))%>%
  transform(centerlat= (maxlat+minlat)/2,centerlong= (maxlong+minlong)/2) 

near_cent_all = near_cent[,c(1,6,7)]
gen_cent=data.frame(near_cent_all[which(near_cent_all[,1]%in%D[which(D[,5]=="gen"),1]),c(2,3)])
names(gen_cent)=c("lat", "long")



# create a rec-field to make sure that we have the order correct
# this probably is unnecessary but it helps to be careful
df.polygon2@data$rec<-1:nrow(df.polygon2@data)
tmp <- left_join(df.polygon2@data, D, by=c("GEOID"="id")) %>% arrange(rec)

# replace the original data with the new merged data. Also need to remove the tracts we don't include because they didn't
###qualify
rem=(1:nrow(tract))[-which(tract$GEOID%in%D$id)]
df.polygon2@data<-na.omit(tmp)
df.polygon2@polygons=df.polygon2@polygons[-rem]

myLabelFormat = function(..., reverse_order = FALSE, end= FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
      if(end){
        cuts=paste(cuts, "%", sep="")
      }
      ct = rep(0, length(cuts))
      
      for(i in 1:(length(ct)-1)){
        ct[i] = paste(as.character(cuts[i+1]),as.character(cuts[i]), sep=" to ")
      }
      ct[length(ct)] = paste(cuts[length(cuts)], "+", sep="")
      
      cuts = ct
    } 
  }else{
    labelFormat(...)
  }
}


name.vec = rep(0, nrow(df.polygon2@data))

for(i in 1:length(name.vec)){
  this.name=which(R[,1]==df.polygon2@data$GEOID[i])
  
  if(length(this.name)>0){
    name.vec[i]=R[this.name, 2]
  }else{
    name.vec[i]=df.polygon2@data$GEOID[i]
  }
}

df.polygon2@data = cbind(df.polygon2@data, name.vec)



###building map of the permits
popup <- paste0("Neighborhood: ", df.polygon2@data$name.vec, "<br>", "Change in # of Permits: ", round(df.polygon2$build_diff,2))
pal <- colorBin(
  palette = "RdBu",
  domain = df.polygon2$build_diff,5
)



attr(pal, "colorArgs")$bins = rev(attr(pal, "colorArgs")$bins)


map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df.polygon2, 
              fillColor = ~pal(build_diff), 
              color = "#000", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 2, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon2$build_diff ,
            position = "bottomright", 
            title = "Change in # of Permits", labFormat = myLabelFormat(reverse_order = TRUE, end= FALSE)) %>% addCircles(gen_cent[,2], gen_cent[,1])


###building map of the capi
popup <- paste0("Neighborhood: ", df.polygon2@data$name.vec, "<br>", "Change in Ridership: ", round(df.polygon2$CaBi_start,2))
pal <- colorBin(
  palette = "RdBu",
  domain = df.polygon2$CaBi_start,5
)



attr(pal, "colorArgs")$bins = rev(attr(pal, "colorArgs")$bins)


map_cabi<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df.polygon2, 
              fillColor = ~pal(CaBi_start), 
              color = "#000", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 2, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon2$CaBi_start ,
            position = "bottomright", 
            title = "Change in Ridership", labFormat = myLabelFormat(reverse_order = TRUE, end= TRUE)) %>% addCircles(gen_cent[,2], gen_cent[,1])


###building the map of wifi

popup <- paste0("Neighborhood: ", df.polygon2@data$name.vec, "<br>", "Change in Wifi Coverage >200kpbs: ", round(df.polygon2$pcat_all,2))
pal <- colorBin(
  palette = "RdBu",
  domain = df.polygon2$pcat_all,4
)



attr(pal, "colorArgs")$bins = rev(attr(pal, "colorArgs")$bins)


map_wifi<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df.polygon2, 
              fillColor = ~pal(pcat_all), 
              color = "#000", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 2, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon2$pcat_all ,
            position = "bottomright", 
            title = "Change in Wifi Coverage", labFormat = myLabelFormat(reverse_order = TRUE, end= FALSE)) %>% addCircles(gen_cent[,2], gen_cent[,1])




app <- shinyApp(
  ui <- fluidPage(titlePanel("Demo D.C Gentrification Maps"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      p("Each panel will let you see a difference between 2012 and 2016."),p("The leftmost panel shows 
                                                                                             the difference in the number of building permits filed for each tract. This is the traditional idea
                                                                                             of gentrifcation. New houses. New stores."),
                      p("The middle panel shows the percentage increase in Capital Bikeshare riders leaving from that tract.
                        Capital Bikeshare stations are quick and cheap to build. This is a less traditional marker of gentrification."),
                      p("The rightmost panel shows the difference in the percentage of households with high-speed internet.
                        A jump of 1 means a difference of 20 percent. This is a new way of looking at gentrification. 
                        WiFi speeds are chosen by the people who live in the neighborhood. They don't need a planning
                        committee or Board of Directors to approve them." )
                      
                      ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Building Permits", leafletOutput('Permit'), height = "100%"),
                        tabPanel("Capital Bikeshare", leafletOutput('Cabi'), height = "100%"),
                        tabPanel("Wifi Coverage", leafletOutput('Wifi'), height = "100%")
                      )
                    )
                      )
                    )
  
  ,
  server <- function(input, output) {
    
    output$Cabi <- renderLeaflet(map_cabi)
    output$Permit <- renderLeaflet(map2)
    output$Wifi <- renderLeaflet(map_wifi)
  }
  )

