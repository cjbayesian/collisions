#############################################################
## Map Visualization of Montreal Cycling Collisions 2006-2010
## Corey Chivers, corey.chivers@mail.mcgill.ca
## Made at HackTaVille in Montreal. September 2012
##
#############################################################
library(lubridate)
library(maptools)
library(Hmisc)

d<-read.csv('data/Bike Accidents.csv',sep='|')
mtl<-readShapePoly('data/montreal_borough_borders.shp')


d$date<-as.character(d$date)
d$date<-gsub(' ','',d$date)
d$date<-ymd(d$date,tz='EST')

startday <-(ymd("2006-01-01",tz='EST'))
all_days <- startday + c(0:(365*5)) * days(1)

n_points<-length(d[,1])
time_since<-array(dim=c(length(all_days),n_points))

##Pre-process timing matrix
trail_length<-30 #points persist for about a month
incident_rate<-numeric(length(all_days))
for(da in 1:length(all_days))
{
   print(all_days[da])

   time_since[da,]<-sapply(1:n_points,function(i){
      as.numeric(all_days[da]-d$date[i])})
   
   time_since[da,time_since[da,]>trail_length]<-trail_length
   time_since[da,time_since[da,]<0]<-trail_length
   incident_rate[da]<-sum(time_since[da,]!=trail_length)
}
   



## Plots
for(da in 1:length(all_days))
{
   png(paste('plots/',all_days[da],'.png',sep=''),
      height=800,
      width=1200,
      bg='grey')

   print(all_days[da])
   plot(mtl,col='darkgrey')
   text(-73.8, 45.7,paste("Cycling Collisions in Montreal\n",all_days[da]),cex=2)

   points(d$long,d$lat,
      pch=20,
      col=rgb(trail_length,0,0,trail_length-time_since[da,],maxColorValue=trail_length),
      cex=2)


   par(lwd=2.5,cex=1.4)
   subplot(plot(all_days[1:da],incident_rate[1:da],lwd=1.5,
               type='l',
               ylab='Incident Rate\n(# per month)',
               xlab='',
               col='red',
               ylim=c(0,max(c(1,max(incident_rate[1:da])))) ),  
            size=c(5.5,2),
            -73.95, 
            45.58,
            vadj=0,hadj=0)

   dev.off()
}

system('mencoder mf://plots/*.png -mf fps=15:type=png -ovc copy -oac copy -o collisions.avi')




