#Script takes in x,y location of aid projects
#Stack of arbitrary rasters
#Aggregates to any arbitrary polygon for analysis
#Outputs a Decision Tree MLA for various outcomes

library(sp)
library(maptools)
library(plyr)
library(raster)
library(rgdal)
library(rCharts)
library(leafletR)
library(rpart)
library(GISTools)
library(rattle)  				# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)
#library(rMaps)

wd = "/users/bcarancibia/CUNY_IS_608/final_project/honduras/"
setwd(wd)


#Choose spatial aggregation at which analysis will be performed
GADM_Aggregation = "NP_GRID_0_25_arcdeg.shp"

if(file.exists("Attribute_GIS.csv") == FALSE)
{

#------------------------
#Aid Data Manipulation - eventually this will just be another raster.
#Right now it's read in manually.

#Read in aid project data
INPUT_PRJ="NPL_AMP_projects.csv"

#Read in aid GIS data
INPUT_GIS="NPL_geocoded_projectLocations.csv"



#Define sectors for which cost surface will be produced
sector = vector()
sector[1] = "agriculture"


GIS_sector_data = list()
input_PRJ_data = read.csv(INPUT_PRJ, header=TRUE)
input_GIS_data = read.csv(INPUT_GIS, header=TRUE)

input_PRJ_GIS = merge(input_PRJ_data, input_GIS_data, by="project_ID")

#Divide distributions and commitments by the # of project rows.  Assumes equal splits.
#Uncertainty raster layers will fix this eventually, so doing a quick/dirty job of this
#for only columns I intend to use.
t1 <- table(input_PRJ_GIS$project_ID)
t1 <- as.data.frame(t1)
t1 <- rename(t1, c("Var1"="project_ID"))
t2 <- (merge(t1,input_PRJ_GIS,by='project_ID'))

t2$c_2000 = t2$c_2000 / t2$Freq
t2$c_2001 = t2$c_2001 / t2$Freq
t2$c_2002 = t2$c_2002 / t2$Freq
t2$c_2003 = t2$c_2003 / t2$Freq
t2$c_2004 = t2$c_2004 / t2$Freq
t2$c_2005 = t2$c_2005 / t2$Freq
t2$c_2006 = t2$c_2006 / t2$Freq
t2$c_2007 = t2$c_2007 / t2$Freq
t2$c_2008 = t2$c_2008 / t2$Freq
t2$c_2009 = t2$c_2009 / t2$Freq
t2$c_2010 = t2$c_2010 / t2$Freq
t2$c_2011 = t2$c_2011 / t2$Freq
t2$c_2012 = t2$c_2012 / t2$Freq

input_data<-t2

for (i in 1:length(sector))
  {
  #Define the column that holds the sector codes here:
  sector_data <- input_data[grep(sector[i], input_data$amp_sector_name, ignore.case=TRUE),]
  
  #Define lat/long fields here:  
  geocoded_sector_data <- sector_data[rowSums(is.na(sector_data[,c("latitude","longitude")]))==0,]
  GIS_sector_data[[i]] <- SpatialPointsDataFrame(list(geocoded_sector_data$longitude,geocoded_sector_data$latitude), sector_data)
  proj4string(GIS_sector_data[[i]] ) <- CRS("+init=epsg:4326")  
  }


GIS_GADM = readShapePoly(GADM_Aggregation)
proj4string(GIS_GADM ) <- CRS("+init=epsg:4326")
Aid_overlay = list()
Aid_raster = list()

#Define the ADM column you want to aggregate on
for (i in 1:length(sector))
  {
  Aid_raster[[i]] = rasterize(GIS_sector_data[[i]],Rlayer,field="total_c_to_2012",fun="sum")
  
  }

#Raster Aggregations

#Population Density (roughly per sq. km)
Rlayer <- raster("lspop2011.tif")
Attribute_GIS <- extract(Rlayer,GIS_GADM, fun='mean', na.rm=TRUE, sp=TRUE)
Attribute_GIS <- rename(Attribute_GIS, c("lspop2011" = "POP_DENSITY"))

#Droughts in the past? 1980-2001 (binary)
Rlayer <- raster("dr_events.tif")
Attribute_GIS <- extract(Rlayer,Attribute_GIS, fun='max', na.rm=FALSE, sp=TRUE)

#Distance to roads
Rlayer <- raster("dist_road.tif")
Attribute_GIS <- extract(Rlayer,Attribute_GIS, fun='mean', na.rm=TRUE, sp=TRUE)

#Distance to cities
Rlayer <- raster("dist_city.tif")
Attribute_GIS <- extract(Rlayer,Attribute_GIS, fun='mean', na.rm=TRUE, sp=TRUE)

#Rice yield potential
Rlayer <- raster("res02_crav6190l_l021_000_yld.tif")
Attribute_GIS <- extract(Rlayer,Attribute_GIS, fun='mean', na.rm=TRUE, sp=TRUE)

#All Crops: Ratio between output and maximum output
Rlayer <- raster("gap2000_gap_2000_hi_cl.tif")
Attribute_GIS <- extract(Rlayer,Attribute_GIS, fun='mean', na.rm=TRUE, sp=TRUE)

#Currently broken - needs to be iterated through over all.
Attribute_GIS <- extract(Aid_raster[[i]],Attribute_GIS, fun='sum',na.rm=TRUE,sp=TRUE)  

proj4string(Attribute_GIS) <- CRS("+init=epsg:4326")

#Threats to human water security (higher is worse - http://www.viewsoftheworld.net/?p=896)
Rlayer <- raster("WaterSecurity.tif")

Attribute_GIS@data$WS = NA

for (i in 1:nrow(Attribute_GIS))
  {
  Attribute_GIS@data$WS[i] = extract(Rlayer,Attribute_GIS[i,], na.rm=TRUE, small=TRUE, method="bilinear", buffer=.10, weights=TRUE, fun="MEAN")
  }



#Join in the AidData aggregations - again, eventually this will be just another raster in the stack.
AODF <- as.data.frame(Aid_overlay)
Attribute_GIS@data = merge(Attribute_GIS@data, AODF, by="row.names")


#---------------
write.csv(Attribute_GIS@data, file="Attribute_GIS.csv")

} else {
  Attribute_GIS = read.csv("Attribute_GIS.csv", header=TRUE)
  #Need to make this a GIS layer here.
}

#Prepare for the Predictive Model

#Replace all NAs with 0s.  Eventually this should be done more elegantly.
AT_GIS <- Attribute_GIS
AT_GIS@data[is.na(AT_GIS@data)] <- 0

#Determine timeframe over which aid is summed:
year = 1999
yrs = vector()
for (i in 1:10)
{
  yrs[i] = year + i
}


#Do renames fro better presentation
AT_GIS = rename(AT_GIS, c("WS"="WATER_SECURITY"))

eval_str = "AT_GIS$TOT_AID = "
for (i in 1:(length(yrs)))
  {
  #Create aggregated temporal aid measures for each cell

  eval_str = paste(eval_str,"AT_GIS$c_", yrs[i], " + ", sep="")
  
  }

eval_str = substr(eval_str,1,nchar(eval_str)-2)
eval(parse(text=eval_str))

AT_GIS$TOT_AID = AT_GIS$layer.1
AT_GIS$QBIN_AID = 0
AT_GIS$QBIN_AID[which(AT_GIS$TOT_AID > 0)] <- 1
AT_GIS$BIN_AID = "Likely No Project"
AT_GIS$BIN_AID[which(AT_GIS$TOT_AID > 0)] <- "Likely Yes Project"


#Model the conditions under which aid is currently allocated.
#ASSUMPTION: Current conditions are "cheapest" in terms of efforts to continue
#providing money to.

f = formula("BIN_AID ~ POP_DENSITY + dr_events + dist_road + dist_city + WATER_SECURITY")
  
fit <- rpart(f, data=AT_GIS, control=rpart.control(cp=.045, surrogatestyle=1))
 



#windows()
title_tr = "Cost Surface Model"
#par(mfrow = c(1,1), xpd = NA)
plotcp(fit)
fancyRpartPlot(fit)

#Make our prediction
AT_GIS$PRED_PROJ <- predict(fit, newdata = AT_GIS)

#Make the B/C surface
AT_GIS$BEN_COST <- (AT_GIS$gap2000_gap_2000_hi_cl/AT_GIS$PRED_PROJ[,1])

#Load in the Nepal shape file
ADM_GIS = "NP_ADM0.shp"
ADM_path = normalizePath(ADM_GIS)
ADM_shp = readShapePoly(ADM_path)

#Map out ancilliary data
#Agricultural Aid
AT_GIS$AidCol = "red"
AT_GIS$AidCol[AT_GIS$TOT_AID > 0] = "green"
GIS_sector_data[[i]]$col = "red"
GIS_sector_data[[i]]$col[GIS_sector_data[[i]]$total_c_to_2012 > 0] = "green"

shades = auto.shading(AT_GIS$TOT_AID,cutter=rangeCuts, cols=brewer.pal(5,'Greens'),n=5)
choropleth(AT_GIS,AT_GIS$TOT_AID,shades)
PRED_Title = "Agr. Aid (USD)"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
#plot(GIS_sector_data[[i]], add=TRUE, col="black", pch=16)
choro.legend(80.5,27.2, shades, title=PRED_Title)

#Ag potential
shades = auto.shading(AT_GIS$res02_crav6190l_l021_000_yld,cutter=rangeCuts, cols=brewer.pal(5,'RdYlGn'),n=5)
choropleth(AT_GIS,AT_GIS$res02_crav6190l_l021_000_yld,shades)
PRED_Title = "Agricultural Potential (kg/ha)"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

#Distance to Roads
shades = auto.shading(AT_GIS$dist_road,cutter=rangeCuts, cols=brewer.pal(5,'Reds'),n=5)
choropleth(AT_GIS,AT_GIS$dist_road,shades)
PRED_Title = "Distance to Roads (Relative)"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

#POP DENS
shades = auto.shading(AT_GIS$POP_DENSITY,cutter=rangeCuts, cols=brewer.pal(5,'Reds'),n=5)
choropleth(AT_GIS,AT_GIS$POP_DENSITY,shades)
PRED_Title = "Population Density (per sq km)"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

#Map it out
shades = auto.shading(AT_GIS$PRED_PROJ[,1],cutter=quantileCuts, cols=brewer.pal(7,'YlOrRd'),n=7)
choropleth(AT_GIS,AT_GIS$PRED_PROJ[,1],shades)
PRED_Title = "Estimated Cost "
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

AT_GIS$BEN_STD = AT_GIS$gap2000_gap_2000_hi_cl / max(AT_GIS$gap2000_gap_2000_hi_cl)

#Map out the benefits layer
shades = auto.shading(AT_GIS$BEN_STD,cutter=rangeCuts, cols=brewer.pal(5,'RdYlGn'),n=5)
choropleth(AT_GIS,AT_GIS$BEN_STD,shades)
PRED_Title = "Estimated Benefit"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

AT_GIS$BEN_COST <- (AT_GIS$BEN_STD/AT_GIS$PRED_PROJ[,1])
#Map out the benefits/costs layer
shades = auto.shading(AT_GIS$BEN_COST,cutter=rangeCuts, cols=brewer.pal(5,'RdYlGn'),n=5)
choropleth(AT_GIS,AT_GIS$BEN_COST,shades)
PRED_Title = "Estimated Benefit/Costs"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

AT_GIS$CVAL = abs(AT_GIS$QBIN_AID - AT_GIS$PRED_PROJ[,2])
#Map out uncertainty in our cost layer
#Based on a cross-validation.
#VERY elementary, and doesn't include true uncertainty
#in the data, yet.
shades = auto.shading(AT_GIS$CVAL,cutter=rangeCuts, cols=brewer.pal(5,'Reds'),n=5)
choropleth(AT_GIS,AT_GIS$CVAL,shades)
PRED_Title = "Uncertainty in Cost Surface"
add.masking(poly.outer(AT_GIS,ADM_shp))
plot(ADM_shp, add=TRUE)
choro.legend(80.5,27.2, shades, title=PRED_Title)

  # Observed Next Time Step
  column_id = paste("PRED_AT_GIS$X",yrs[f_yr],"_con",sep="")
  #bin_obs_data = eval(parse(text="AT_GIS$X2001_conB"))
  bin_obs_data = eval(parse(text=column_id))
  shades = auto.shading(bin_obs_data,cutter=rangeCuts, cols=brewer.pal(4,'Set3'),n=4)
  choropleth(PRED_AT_GIS,bin_obs_data,shades)
  OBS_Title = paste(yrs[f_yr], "observed")
  choro.legend(29.3,-1.481531, shades, title=OBS_Title)

#   
#   #For Continious Estimate:
#   shades = auto.shading(AT_GIS$con_est_2000,cols=brewer.pal(10,'YlOrRd'),n=10)
#   choropleth(AT_GIS,AT_GIS$con_est_2000,shades)
#   
#   #For Continious Observed:
#   shades = auto.shading(AT_GIS$X2000_con,cols=brewer.pal(10,'YlOrRd'),n=10)
#   choropleth(AT_GIS,AT_GIS$X2000_con,shades)
  plotFile = paste(yrs[f_yr], "_projection")
  
  savePlot(filename = plotFile, type=c("png"), device=dev.cur())

  }

#AtrGeo <- toGeoJSON(data=Attribute_GIS, dest=tempdir())

 
 