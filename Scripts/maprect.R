#map rect data
#library
library(ggplot2)
library(sf)
library(dplyr)


setwd("C:/Users/cdestrem/Documents/STAGE_M2_BYC_MANCHE/DatagGen")



#load rect and ices data from R file
div<-readRDS(file="div.rds")
rect<-readRDS("rect.rds")
subrect<-readRDS("subrect.rds")
#these objects have geometry (class sf) and can be used as support for mapping 

#generate some data
pipo<-rect%>%filter(F_DIVISION%in%c("27.7.d","27.7.e","27.7.h","27.7.f"))%>%sample_frac(.1)%>%
	rowwise()%>%
	transmute(rect=ICESNAME,val=rnorm(1,mean=100,sd=20))%>%
	st_drop_geometry()%>%
	ungroup()
#first step : join with the right geometry support (here the rect)
rectsimple<-rect%>%transmute(rect=ICESNAME)
pipo<-left_join(pipo,rectsimple)%>%st_as_sf()
#get the limx and limy for coord_sf
rangex<-st_bbox(pipo)[c(1,3)]
rangey<-st_bbox(pipo)[c(2,4)]


#to add the ICES division border 
divsimple<-div%>%transmute(div=F_DIVISION)%>%st_as_sf()

#pipo has now a geometry column usable with gggplot
map<-ggplot()+theme_bw()+
	geom_sf(data=pipo,aes(fill=val))+
	geom_sf(data=divsimple,fill=NA,colour="light grey")+
	borders("world",fill="light grey",colour="light grey")+
	scale_fill_distiller(palette='Spectral',name="Value (unit)")+
	coord_sf(rangex,rangey)+
	xlab("Longitude")+ylab("Latitude")+
  scale_x_continuous(breaks = seq(min(rangex), max(rangex), by = 1)) +
	ggtitle("a gentle title")
map


