# cmp_code
if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(sf,leaflet,tidyverse,htmlwidgets)
###############################################################################Roads
setwd("H:/My Drive/CMP/cmp2050/1data")

###read df
df<-read_sf("tmc.shp")

##1 regional congestion==change tti to PTI for PTI analysis
df %>% st_drop_geometry() %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))
df %>% st_drop_geometry() %>% filter(Road=="Arterial") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))
df %>% st_drop_geometry() %>% filter(Road=="Freeway") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))

#details-art-free-am-pm
df %>% st_drop_geometry() %>% filter(Road=="Arterial" & Peak=="AM") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))
df %>% st_drop_geometry() %>% filter(Road=="Arterial" & Peak=="PM") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))

df %>% st_drop_geometry() %>% filter(Road=="Freeway" & Peak=="AM") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))
df %>% st_drop_geometry() %>% filter(Road=="Freeway" & Peak=="PM") %>% select(TTI) %>% summarize(Mean = mean(TTI, na.rm=TRUE))

##tti df
artAMt<- tmc %>% filter(Road=="Arterial" & Peak=="AM") %>% select(Peak, Road, tti3) %>% add_column(con="Arterial TTI AM Peak")
artPMt<- tmc %>% filter(Road=="Arterial" & Peak=="PM") %>% select(Peak, Road, tti3) %>% add_column(con="Arterial TTI PM Peak")
freeAMt<- tmc %>% filter(Road=="Freeway" & Peak=="AM") %>% select(Peak, Road, tti3) %>% add_column(con="Freeway TTI AM Peak")
freePMt<- tmc %>% filter(Road=="Freeway" & Peak=="PM") %>% select(Peak, Road, tti3) %>% add_column(con="Freeway TTI PM Peak")
##pti df
artAMp<- tmc %>% filter(Road=="Arterial" & Peak=="AM") %>% select(Peak, Road, pti3) %>% add_column(con="Arterial PTI AM Peak")
artPMp<- tmc %>% filter(Road=="Arterial" & Peak=="PM") %>% select(Peak, Road, pti3) %>% add_column(con="Arterial PTI PM Peak")
freeAMp<- tmc %>% filter(Road=="Freeway" & Peak=="AM") %>% select(Peak, Road, pti3) %>% add_column(con="Freeway PTI AM Peak")
freePMp<- tmc %>% filter(Road=="Freeway" & Peak=="PM") %>% select(Peak, Road, pti3) %>% add_column(con="Freeway PTI PM Peak")

#plot
pal <- colorFactor(palette = c("red","yellow","orange"), domain = artAMt$tti3)
pal1 <- colorFactor(palette = c("red","yellow","orange"), domain = artPMt$tti3)
pal2 <- colorFactor(palette = c("red","yellow","orange"), domain = artAMp$pti3)
pal3 <- colorFactor(palette = c("red","yellow","orange"), domain = artPMp$pti3)

pal4 <- colorFactor(palette = c("red","yellow","orange"), domain = freeAMt$tti3)
pal5 <- colorFactor(palette = c("red","yellow","orange"), domain = freePMt$tti3)
pal6 <- colorFactor(palette = c("red","yellow","orange"), domain = freeAMp$pti3)
pal7 <- colorFactor(palette = c("red","yellow","orange"), domain = freePMp$pti3)

test<-leaflet() %>% addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = artAMt,color = pal(artAMt$tti3),opacity = 0.7, fillOpacity = 0.1,weight = 2,group="Arterial (AM Peak)") %>%
  addPolylines(data = artPMt,color = pal1(artPMt$tti3),opacity = 0.7, fillOpacity = 0.1,weight = 2,group="Arterial (PM Peak)") %>%
  addPolylines(data = freeAMt,color = pal4(freeAMt$tti3),opacity = 0.7, fillOpacity = 0.1,weight = 2,group="Freeway (AM Peak)") %>%
  addPolylines(data = freePMt,color = pal5(freePMt$tti3),opacity = 0.7, fillOpacity = 0.1,weight = 2,group="Freeway (PM Peak)") %>%

  addLegend("bottomleft", pal = pal,values=artAMt$tti3,title = "Congestion (TTI)") %>%
  addLayersControl(overlayGroups =c("Arterial (AM Peak)","Arterial (PM Peak)","Freeway (AM Peak)","Freeway (PM Peak)"),
                   options = layersControlOptions(collapsed=FALSE),position = "bottomleft") %>%
  hideGroup(c("Arterial (PM Peak)","Freeway (AM Peak)","Freeway (PM Peak)")) %>%
  htmlwidgets::onRender("
        function() {$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Performance Measures</label>');}")
saveWidget(test,file = "H:/My Drive/CMP/cmp2050/3output/fig6_congestion-tti.html")
