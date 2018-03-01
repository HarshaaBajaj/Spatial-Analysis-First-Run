#'---
#'title: "Bosbiz Spatial Analysis"
#'output:
#'  html_document:
#'    self_contained: false
#'    number_sections: true
#'    fig_width: 10
#'    fig_height: 10
#'    fig_caption : true
#'    
#'---

#'Answer two questions:  
#'1. Are the points randomly distributed or is there a pattern?  
#'2. If they're not random, identify clusters of points.   
#'   
#' ## EDA  
#' Before point pattern analysis let us see what the data is telling us and understand what we are dealing with.  
#' This will help us make better decisions and hypothesis about the point patterns

#+ message=F,results='hide', warning=F
libs = c("readr", "Hmisc", "dplyr", "magrittr", "ggplot2","data.table","OneR","ggmap","googleVis","spatstat","dbscan")
lapply(libs, require, character.only = T)
op <- options(gvis.plot.tag='chart') # for plots to be generated in this script and not the default (browser)

# read the data set
#setwd('path to data')
bosbiz <- read_csv("bosbiz.csv")
#' ### First glance  
head(bosbiz)
summary(bosbiz) # as mentioned, lat,lon suggest close area and not wide spread, i.e location : Boston only
sapply(bosbiz, class)
describe(bosbiz) # 12149 * 5 # SIC with dups and NAs ; name, gh7, lat and lon with dups but no NAs

bosbiz <- distinct(bosbiz,.keep_all = T) # let's remove duplicate rows (if any)
describe(bosbiz) # down to 12004 * 5 # there were indeed 12149-12004= 145 dup rows

hist(bosbiz[,'lon'])
hist(bosbiz[,'lat'])
#' Clear uneven distribution  
#'    
#' ### Map view
#' Let's first just plot all the businesses onto the map

#+ results='asis'
plot(bosbiz %>% mutate(latlon = paste(lat, lon, sep = ':')) %>%
       gvisMap(
         locationvar = "latlon",
         tipvar = "name",
         options = list(
           showTip = T,
           showLine = F,
           mapType = 'normal',
           enableScrollWheel = TRUE,
           useMapTypeControl = T,
           width = 1400,
           height = 800)))
#'   
#' Northeast seems to be popular location!  
#'   
#' #### Let's see which are the most popular businesses by name in our data  

bosbiz <- as.data.frame(group_by(bosbiz,name) %>%  mutate(name_group_count=n()))
filter(bosbiz,name_group_count > 5) %>% ggplot(aes(reorder(name,name_group_count)))+
  geom_bar( fill ='red') + xlab('Name') + ggtitle('Most frequent by name') + coord_flip()

#' We can notice that Dunkin" Donuts appears twice because of different writing styles  
#' Since we are concerned with location and not names, as of now we can ignore this  
#+ message=F, warning=F

window<-make_bbox(bosbiz$lon,bosbiz$lat)
map <- ggmap(get_map(window,zoom=12,color='bw'))
less_15 <- map + geom_point(aes(lon,lat),col='black',size=.4,
                            data = filter(bosbiz,name_group_count<15))
less_15 + geom_point(aes(lon,lat,shape=bin(name_group_count),color=name),
                     data = filter(bosbiz,name_group_count>15),size=5)+
          ggtitle('Grouped by name of the business')
#'   
#' Just as before, popular bussiness seem to be towards northeast  
#'   
#' ### Let's now check which are the most popular businesses by SIC in our data  

bosbiz <- as.data.frame(group_by(bosbiz,sic) %>% mutate(sic_group_count=n()))
filter(bosbiz[!is.na(bosbiz$sic),],sic_group_count > 50) %>%
  ggplot(aes(reorder(sic,sic_group_count)))+
  geom_bar( fill ='red') + xlab('SIC') +ggtitle('Most frequent by SIC') + coord_flip()

head(bosbiz[which(bosbiz$sic==80119904),'name']) # Boston has a lot of doctors!

#' Map view
#+ message=F, warning=F
sic_less_100 <- map + geom_point(aes(lon,lat),col='black',size=.4,
                                 data = filter(bosbiz[!is.na(bosbiz$sic),],sic_group_count<250))
sic_less_100 + geom_point(aes(lon,lat,shape=bin(sic_group_count),color=factor(sic)),
                          data = filter(bosbiz[!is.na(bosbiz$sic),],sic_group_count>250),size=5)+
               ggtitle('Grouped by SIC') # yet again, northeast seems to be popular location!

#' #### So before we jump into answering our questions let's just see how the density and count plots look

#+ message=F
qmplot(lon,lat,data=bosbiz,geom = c('point','density2d'),size=I(1.3))+
  ggtitle('Density Map') 

qmplot(lon,lat,data=bosbiz,geom ='point')+
  geom_count(aes(color=..n..,size=..n..))+
  guides(color='legend')+
  ggtitle('Count Map')  

#' ### 1. Are the points randomly distributed or is there a pattern?
bosbiz.ppp <- ppp(bosbiz$lon,bosbiz$lat,window=as.owin(window[c(1,3,2,4)]))
#' Warning because two or more business are at the same location
temp<-bosbiz %>% mutate(latlon = paste(lat, lon, sep = ':')) %$% bosbiz[duplicated(latlon),] %>% arrange(lat)
head(temp)
#' This is why we should group them by name/location and then look for patterns, or else we'd loose data   
#' But for time being let us continue with the lon-lat pairs   
summary(bosbiz.ppp)
plot(density(bosbiz.ppp),main='Bosbiz Density') #just to make sure that we still see the prev ptrn

#' By now we seem to have a good sense of the point distribution pattern in our data   
#' I believe the data is clustered at different locations   
#'    
#' ### Let us confirm this using Quadrat and Ripley's K tests   
#'    
#' Null hypothesis : data follows Complete Spatial Randomness  
#' Alternate hypothesis : data is clustered   
#'    
#' ### Quadrat testing
qc<-quadratcount(bosbiz.ppp)
plot(bosbiz.ppp,main="Bosbiz")
plot(qc,add=T,col="red",cex=1.5)
title(xlab = 'Lon',ylab = 'Lat')

#' Doesn't seem CSR  
#'    
#' #### Chisq   
#'    
qt_csq<-quadrat.test(qc,method = 'Chisq',alternative = 'clustered')
qt_csq
plot(residuals(qt_csq),main = 'Residual plot for Chisq quadrat.test',ylab = 'Residuals')
abline(h = 0,col='red')

#' The p value and residual plot help us reject the null hypothesis   
#'    

plot(bosbiz.ppp,pch='+',cols='green',lwd=2,main = 'Chisq quadtest results')
plot(qt_csq,add=T,col='red',cex=1.4,lty=2,lwd=3)
pval <- eval(substitute(expression(p[chi^2]==z),
                          list(z=signif(qt_csq$p.value,3))))
title(sub=pval, cex.sub=2)

#' There are three numbers in each quadrat,   
#' * Top-left is the observed number of points in that quadrat  
#' * Top-right is the Poisson expected number of points in that quadrat  
#' * Bottom value is the Pearson residual value calculated as (Observed - Expected) / Sqrt(Expected)
#'   
#' #### Monte Carlo method with same alternate hypothesis i.e data is clustered  
#'   
qt_mc<-quadrat.test(qc,method = 'MonteCarlo',alternative = 'clustered',conditional = T,nsim = 5000)
qt_mc
plot(residuals(qt_mc),main = 'Residual plot for MC quadrat.test',ylab = 'Residuals')
abline(h = 0,col='red')

#' The p value and residual plot help us reject the null hypothesis again

plot(bosbiz.ppp,pch='+',cols='green',lwd=2,main = 'MC quadtest results')
plot(qt_mc,add=T,col='red',cex=1.4,lty=2,lwd=3)
pval <- eval(substitute(expression(p[chi^2]==z),
                        list(z=signif(qt_mc$p.value,3))))
title(sub=pval, cex.sub=2)

#' ### Although quadrat counts help as measure of dispersion, our p-values give us a strong basis that data
#' is indeed clustered
#'   

#' ### Ripley's K test   
#' Let's use the Kest which is an estimate based on event-to-event distance
#' to make our final conclusion about the pattern 

plot(Kest(bosbiz.ppp,nlarge = nrow(bosbiz)+1,var.approx = T),main='Ripley\'s K test')

#' Blue line: theoretical Poisson, if the data was following CSR point pattern  
#' Rest : Different estimation methods
#'    
#' Since all of the estimates are above the blue it is evident that our data is clustered, so now we can
#' move to answer the second question
#'   
#' ### 2. If they're not random, identify clusters of points.  
#'   
#' The most commonly used metric to identify clusters is distance.  
#' #### Stienen diagram  
#'    
#' It if formed by drawing a circle around each point of the pattern, with diameter equal to 
#' the nearest-neighbour distance for that point   
#'    
stienen(bosbiz.ppp)
#' Atleast 7 different clusters can be identified   
#'    
#' #### Nearest Neighbour Clutter Removal
nnc<-nnclean(bosbiz.ppp,k=20)
plot(split(nnc),main = 'Noise and Feature separation based on distance function',xlab='Lon',ylab='Lat') # 7-10 clusters are evident
marks(nnc) <- marks(nnc)$prob
plot(cut(nnc, breaks=3), chars=c(".", "+", "*"), cols=1:3,cex=1.5,main = 'Probablity break down',xlab='Lon',ylab='Lat')

#' #### Let's try the Density-Based Spatial Clustering of Applications with Noise approach  
#'         
#' eps = radius = kest shows clusters until r=.010, so should keep eps below that   
#' minPts= min pts to consider to form cluster, from countmap 30 seems a good number to start with  
#'    
#' cluster1 colored is the points that are not part of any other clusters  
#+ message=F
db<-dbscan(bosbiz[,3:4],eps=.002,minPts = 30) #eps the smaller the closer pairs
qmplot(lon,lat,data=bosbiz,geom ='point',legend = 'none',color=factor(db$cluster+1),size=I(2))
#'   
#' 17 widespread clusters because mp=30, if we increase it the bigger clusters would break down and more clusters would arise until decay   

#' Let's try increasing mp
#+ message=F
db1<-dbscan(bosbiz[,3:4],eps=.002,minPts = 100)
qmplot(lon,lat,data=bosbiz,geom ='point',legend = 'none',color=factor(db1$cluster+1),size=I(2))
#' 14 Clusters with closer bounds than before
#' This is very close to our previous probablity based feature extraction (nnc)

#+ message=F
db2<-dbscan(bosbiz[,3:4],eps=.002,minPts = 150)
qmplot(lon,lat,data=bosbiz,geom ='point',legend = 'none',color=factor(db2$cluster+1),size=I(2))
#' 12 Clusters with closer bounds
#'   
#' It is a hard trade-off choosing the right eps and mp values, so let's try the common method of kmeans

#+ message=F
km <- kmeans(bosbiz[,3:4],centers = 7)
qmplot(lon,lat,data=bosbiz,geom ='point',legend = 'none',color=factor(km$cluster),size=I(2))

# todo: investigate grouped by name/sic location patterns
    # din't do it yet because too many groups, even to pass as marks in ppp
    # will have to pick on certain criteria first
# todo: choose closer window in NE section to find out the kind of businesses trainling there
# todo: add additional features and test the contrast availablity of them in diff regions
# todo: ofcourse read and learn more about spatial analysis!
# ref: https://training.fws.gov/courses/references/tutorials/geospatial/CSP7304/documents/PointPatterTutorial.pdf


