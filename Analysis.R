## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$SCC<-as.factor(NEI$SCC)
NEI$year<-as.factor(NEI$year)
##get the annual average 
anual_total<-tapply(NEI$Emission, NEI$year, sum)
all_annual_mean<-data.frame(year=c(1999,2002,2005,2008),sum=annual_median)

baltimore<-filter(NEI,fips=="24510")
baltimore_anual_total<-tapply(baltimore$Emission, baltimore$year, sum)
annual_median_bat<-data.frame(year=c(1999,2002,2005,2008),median=annual_bat)

library(ggplot2)
type_year<-aggregate(NEI$Emissions,list(NEI$type,NEI$year),mean)
gplot<-ggplot(type_year,aes(y=x,x=Group.2))+geom_point(aes(colour=factor(Group.1)))+geom_line(aes(group=Group.1,colour=Group.1))


Coal_index<-grep("*Coal*|*coal*",SCC$Short.Name)
coal_rows<-SCC[Coal_index,]
src_coal<-NEI[NEI$SCC %in% coal_rows$SCC, ]
coalsrc_anual_mean<-tapply(src_coal$Emissions, src_coal$year, mean)

car_index<-grep("*Vehicle*|*vehicle*",SCC$Short.Name)
car_rows<-SCC[car_index,]
src_vehicle_baltimore<-NEI[NEI$SCC %in% car_rows$SCC,]%>%filter(fips=="24510")
baltimore_vehiclesrc_anual_mean<-tapply(src_vehicle_baltimore$Emissions, src_vehicle_baltimore$year, mean)
gplot<-ggplot(baltimore_vehiclesrc_anual_mean, aes(x=., y=, group =1)) + geom_point(aes(colour = factor(Group.1)), size = 4)+geom_line()

##Gett all obs with vehicle as source
car_index<-grep("*Vehicle*|*vehicle*",SCC$Short.Name)
car_rows<-SCC[car_index,]
#Get baltimore and LA
src_vehicle_baltimore<-NEI[NEI$SCC %in% car_rows$SCC,]%>%filter(fips=="24510")
src_vehicle_LA<-NEI[NEI$SCC %in% car_rows$SCC,]%>%filter(fips=="06037")
# Calculate mean fore each year and label the City as Baltimore
baltimore_vehiclesrc_anual_mean<-tapply(src_vehicle_baltimore$Emissions, src_vehicle_baltimore$year, mean)%>%data.frame
baltimore_vehiclesrc_anual_mean$City<-rep("Baltimore",4)
baltimore_vehiclesrc_anual_mean$Year<-row.names(baltimore_vehiclesrc_anual_mean)
# Calculate mean fore each year and label the City as LA
LA_vehiclesrc_anual_mean<-tapply(src_vehicle_LA$Emissions, src_vehicle_LA$year, mean)%>%data.frame
LA_vehiclesrc_anual_mean$City<-rep("LA",4)
LA_vehiclesrc_anual_mean$Year<-row.names(LA_vehiclesrc_anual_mean)
Baltimore_LA_vehicle<-rbind(baltimore_vehiclesrc_anual_mean,LA_vehiclesrc_anual_mean)
cnames<-c("Mean","City","Year")
names(Baltimore_LA_vehicle)<-make.names(cnames)

gplot<-ggplot(Baltimore_LA_vehicle,aes(x=Year,y=Mean, group =City)) + geom_point(aes(colour = City), size = 4)+geom_line(aes(colour=City))



NEIBaltimore<- subset(NEI,NEI$fips=="24510" & NEI$type =="ON-ROAD")
