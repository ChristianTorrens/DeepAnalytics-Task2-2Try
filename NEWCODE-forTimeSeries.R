#### Installing and Calling libraries####

install.packages("chron")

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
devtools::install_github("hrbrmstr/taucharts")
source("calendarHeat.R")
install.packages("chron")
library("chron")
install.packages("tidyr")
install.packages("lubridate")
install.packages("hydroTSM")
install.packages("ggplot2")
install.packages("bdvis")
install.packages("magrittr")
install.packages("RColorBrewer")
install.packages("grid")
install.packages("zoo")
install.packages("padr")
install.packages("ggalt")
install.packages("taucharts")
install.packages("doBy")
install.packages("highcharter")
install.packages("reshape2")
install.packages("forecast")
install.packages("tslm") #says it is not available for this version
install.packages("datetime")
install.packages("anytime")


library(anytime)
library(datetime)
library(forecast)
library(reshape2)
library(highcharter)
library(doBy)
library(taucharts)
library(ggalt)
library(padr)
library(zoo)
library(caret)
library(tidyr)
library(dplyr)
library(timeDate)
library(lubridate)
library(readr)
library(hydroTSM)
library(ggplot2)
library(caret)
library(magrittr)
library(RColorBrewer)
library(chron)
library(bdvis)
library(grid)

#### Importing DataSet####
setwd("~/Dropbox/Ubiqum Master/Deep Analytics and Visualization/Task1_DefineDataScienceProject")
household <- read.csv("~/Dropbox/Ubiqum Master/Deep Analytics and Visualization/Task1_DefineDataScienceProject/household_power_consumption.txt", 
                      header=TRUE,sep=";",na.strings = c("?"))
options(digits=5)

View(household)
str(household)

household[1,]$Date #16/12/2006
household[2075259,]$Date #26/11/2010




####NEW household, turning minutes into hours from the beggining####
NEWhousehold <- household

####Dplyr DateTime####
NEWhousehold$Date<- as.character(NEWhousehold$Date)
NEWhousehold$Time<- as.character(NEWhousehold$Time)
NEWhousehold$DateTime<- paste(NEWhousehold$Date, household$Time)
NEWhousehold$DateTime <- dmy_hms(NEWhousehold$DateTime)
NEWhousehold$DateTime <- with_tz(NEWhousehold$DateTime, "Europe/Paris")
#household$DateTime <- strptime(household$DateTime, "%d/%m/%Y %H:%M:%S", tz= "Europe/Paris")
#household$Date<-as.POSIXct(household$DateTime,tz= "Europe/Paris" )

str(NEWhousehold)
View(NEWhousehold)


####Daylight saving  ####
startdate=as_datetime('2007-03-25 02:00:00')
enddate=as_datetime('2007-10-29 01:59:00')
NEWhousehold<-mutate(NEWhousehold, DateTime_2=
                    ifelse(DateTime >= as_datetime('2007-03-25 02:00:00') &
                             DateTime <= as_datetime('2007-10-28 01:59:00'),DateTime+ hours(1),
                           ifelse(DateTime >= as_datetime('2008-03-30 02:00:00') &
                                    DateTime <= as_datetime('2008-10-26 01:59:00'),DateTime+ hours(1),
                                  ifelse (DateTime >= as_datetime('2009-03-29 02:00:00') &
                                            DateTime <= as_datetime('2009-10-25 01:59:00'),DateTime+ hours(1),
                                          ifelse(DateTime >= as_datetime('2010-03-28 02:00:00') &
                                                   DateTime <= as_datetime('2010-10-31 01:59:00'),DateTime+ hours(1),
                                                 DateTime )))))

NEWhousehold$DateTime_2 <- as.datetime(NEWhousehold$DateTime_2,'%m/%d/%Y %H:%M:%S')
NEWhousehold$DateTime_2<-anytime(NEWhousehold$DateTime_2) 
View(NEWhousehold)

CHECKINGDATASET<- NEWhousehold%>%filter(DateTime>="2007-03-25 02:00:00" & DateTime<="2007-10-28 01:59:00")
write.csv(CHECKINGDATASET, "FirstDaylightChange.csv")
CHECKINGDATASET2<- NEWhousehold%>%filter(DateTime>="2008-03-25 02:00:00" & DateTime<="2008-10-28 01:59:00")
write.csv(CHECKINGDATASET2, "SecondDaylightChange.csv")

  
str(NEWhousehold)


####Finding NAs####
MatrixOfNAs<- filter(NEWhousehold, is.na(Global_active_power))
RowsOfNAs<- which(is.na(NEWhousehold$Global_active_power))
write.csv(RowsOfNAs, "typeofna.csv")

sum(is.na(NEWhousehold))
str(NEWhousehold)
View(NEWhousehold)

####Replacing NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
NEWhousehold$Global_active_power<- na.locf(NEWhousehold$Global_active_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
sum(is.na(NEWhousehold$Global_active_power))#to see how many NA
NEWhousehold$Global_reactive_power<- na.locf(NEWhousehold$Global_reactive_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
NEWhousehold$Voltage<- na.locf(NEWhousehold$Voltage, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
NEWhousehold$Global_intensity<- na.locf(NEWhousehold$Global_intensity, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
NEWhousehold$Sub_metering_1<- na.locf(NEWhousehold$Sub_metering_1, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
NEWhousehold$Sub_metering_2<- na.locf(NEWhousehold$Sub_metering_2, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
NEWhousehold$Sub_metering_3<- na.locf(NEWhousehold$Sub_metering_3, na.rm = FALSE, fromLast = FALSE, maxgap = 180)



####Replacing rest of NA´s.DAYS OUT####
##to know which Days they are out##
sum(is.na(NEWhousehold$Global_active_power))

DaysOut<- which(is.na(NEWhousehold$Global_active_power))

write.csv(DaysOut, "DaysOut.csv")
NEWhousehold[190498,]$DateTime_2 #28-04-2007
NEWhousehold[194220,]$DateTime_2 #until 30-04-2007

NEWhousehold[1309387,]$DateTime_2 #13-06-2009
NEWhousehold[1312691,]$DateTime_2 #until 15-06-2009


NEWhousehold[1397497,]$DateTime_2 #13-08-2009 5:00Am
NEWhousehold[1398387,]$DateTime_2 #until 13-08-2009 19:50 PM

NEWhousehold[1616970,]$DateTime_2 #12-01-2010 14:53
NEWhousehold[1620098,]$DateTime_2 #until 14-01-2010 19:01 PM

NEWhousehold[1712789,]$DateTime_2 #20-08-2010 03:52
NEWhousehold[1714815,]$DateTime_2 #until 21-08-2010 13:38 PM

NEWhousehold[1929819,]$DateTime_2 #17-08-2010 21:52
NEWhousehold[1937044,]$DateTime_2 #until 22-08-2010 19:12 PM

NEWhousehold[1984953,]$DateTime_2 # 25-09-2010 03:56 PM
NEWhousehold[1990189,]$DateTime_2 #until 28-09-2010 19:12 PM

#To change those NAs for zero value
NEWhousehold$Global_active_power[is.na(NEWhousehold$Global_active_power)]<-0
sum(is.na(household$Global_active_power))
NEWhousehold$Global_reactive_power[is.na(NEWhousehold$Global_reactive_power)]<-0
NEWhousehold$Global_intensity[is.na(NEWhousehold$Global_intensity)]<-0
NEWhousehold$Voltage[is.na(NEWhousehold$Voltage)]<-0
NEWhousehold$Sub_metering_1[is.na(NEWhousehold$Sub_metering_1)]<-0
NEWhousehold$Sub_metering_2[is.na(NEWhousehold$Sub_metering_2)]<-0
NEWhousehold$Sub_metering_3[is.na(NEWhousehold$Sub_metering_3)]<-0

sum(is.na(NEWhousehold))



####Columns with SAME ENERGY MEASURING METRICS####
NEWhousehold<-NEWhousehold %>% mutate(Global_ConsumptionKWh=((NEWhousehold$Global_active_power)/60))
NEWhousehold<-NEWhousehold %>% mutate(Global_Consumption_reactiveKWh=((NEWhousehold$Global_reactive_power)/60))
NEWhousehold<-NEWhousehold %>% mutate(Submetter1_kwh=(NEWhousehold$Sub_metering_1/1000))
NEWhousehold<-NEWhousehold %>% mutate(Submetter2_kwh=(NEWhousehold$Sub_metering_2/1000))
NEWhousehold<-NEWhousehold %>% mutate(Submetter3_kwh=(NEWhousehold$Sub_metering_3/1000))









####Create Month,Week, Day, WeekDay,Season column####
NEWhousehold$Hora <- hour(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Hora))

NEWhousehold$Mes <- month(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Mes))

NEWhousehold$Semana <- week(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Semana))

NEWhousehold$Dia <- day(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Dia))

NEWhousehold$DiaSemana <- wday(NEWhousehold$DateTime, label = TRUE, abbr = FALSE)
sum(is.na(NEWhousehold$DiaSemana))

NEWhousehold$Season<- quarter(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Season))

NEWhousehold$Any<- year(NEWhousehold$DateTime)
sum(is.na(NEWhousehold$Any))




####TURNING THE WHOLE DATASET INTO HOURS####
NEWhouseholdHOURS<- NEWhousehold
View(NEWhouseholdHOURS)
NEWhouseholdHOURS<- NEWhouseholdHOURS%>% group_by(Any,Mes,Dia,Hora)%>% mutate(SumaHoraKW = sum(Global_active_power))

NEWhouseholdHOURS<- NEWhouseholdHOURS%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHoraReKW = sum(Global_reactive_power))

NEWhouseholdHOURS<- NEWhouseholdHOURS%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHoraKitchenKW = sum(Sub_metering_1))

NEWhouseholdHOURS<- NEWhouseholdHOURS%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHoraLaundryKW = sum(Sub_metering_2))

NEWhouseholdHOURS<- NEWhouseholdHOURS%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHoraHeaterKW = sum(Sub_metering_3))

View(NEWhouseholdHOURS)

NEWhouseholdHOURS<- select(NEWhouseholdHOURS, DateTime_2, Any, Hora, Mes, Dia,DiaSemana,SumaHoraKW, SumaHoraReKW,SumaHoraKitchenKW,SumaHoraLaundryKW,SumaHoraHeaterKW)

View(NEWhouseholdHOURS)
NEWhouseholdHOURS$DateTime_2<-anytime(NEWhouseholdHOURS$DateTime_2) 
NEWhouseholdHOURS$DateTime_2<- format(ymd_hms(NEWhouseholdHOURS$DateTime_2),format='%m/%d/%Y %H')

NEWhouseholdHOURS<- distinct(NEWhouseholdHOURS)


NEWhouseholdHOURS$DateTime_2<-anytime(NEWhouseholdHOURS$DateTime_2) 

NEWhouseholdHOURS<- filter(NEWhouseholdHOURS, Any !=2006)


View(NEWhouseholdHOURS)
str(NEWhouseholdHOURS)



####CREATING A HOUSEHOLD 2 DATASET####
#1s Merge the hours to reduce input#
NEWhouseholdSUBSETTING<- NEWhousehold 
View(NEWhousehold)

####HOUSEHOLD HISTOGRAM PER SEASON #### 
#Not useful for TIME SERIES as its functions already check Seasonality
#NEWhouseholdSeason<- NEWhouseholdSUBSETTING

#NEWhouseholdSeason<- NEWhouseholdSeason%>% group_by(Any, Season)%>% mutate(SumaSeasonKW = sum(Global_active_power))

#NEWhouseholdSeason<- NEWhouseholdSeason%>% group_by(Any, Season)%>%mutate(SumaSeasonReactiveKW = sum(Global_reactive_power))

#NEWhouseholdSeason<- NEWhouseholdSeason%>% group_by(Any, Season)%>%mutate(SumaSeasonKitchenKW = sum(Sub_metering_1))

#NEWhouseholdSeason<- NEWhouseholdSeason%>% group_by(Any, Season)%>%mutate(SumaSeasonLaundryKW = sum(Sub_metering_2))

#NEWhouseholdSeason<- NEWhouseholdSeason%>% group_by(Any, Season)%>%mutate(SumaSeasonHeaterKW = sum(Sub_metering_3))

#NEWhouseholdSeasonGood<- select(NEWhouseholdSeason, DateTime_2, Any, Season,SumaSeasonKW,SumaSeasonReactiveKW,SumaSeasonKitchenKW, SumaSeasonLaundryKW,SumaSeasonHeaterKW )

#NEWhouseholdSeasonGood$DateTime_2<-anytime(NEWhouseholdSeasonGood$DateTime_2) 
#NEWhouseholdSeasonGood$DateTime_2<- format(ymd_hms(NEWhouseholdSeasonGood$DateTime_2),format='%Y')
#NEWhouseholdSeasonGood<- distinct(NEWhouseholdSeasonGood)

#NEWhouseholdSeasonGood$DateTime_2<-anytime(NEWhouseholdSeasonGood$DateTime_2) 

#NEWhouseholdSeasonGood<- filter(NEWhouseholdSeasonGood, Any !=2006 & !2010)

#View(NEWhouseholdSeasonGood)
#str(NEWhouseholdSeasonGood)



####HOUSEHOLD HISTOGRAM PER MONTH ####

NEWhouseholdMonth<- NEWhouseholdSUBSETTING

NEWhouseholdMonth<- NEWhouseholdMonth%>% group_by(Any,Mes)%>% mutate(SumaMonthKW = sum(Global_active_power))

NEWhouseholdMonth<- NEWhouseholdMonth%>% group_by(Any,Mes)%>%mutate(SumaMonthReactiveKW = sum(Global_reactive_power))

NEWhouseholdMonth<- NEWhouseholdMonth%>% group_by(Any,Mes)%>%mutate(SumaMonthKitchenKW = sum(Sub_metering_1))

NEWhouseholdMonth<- NEWhouseholdMonth%>% group_by(Any,Mes)%>%mutate(SumaMonthLaundryKW = sum(Sub_metering_2))

NEWhouseholdMonth<- NEWhouseholdMonth%>% group_by(Any, Mes)%>%mutate(SumaMonthHeaterKW = sum(Sub_metering_3))

NEWhouseholdMonthGood<- NEWhouseholdMonth


NEWhouseholdMonthGood<- select(NEWhouseholdMonthGood, DateTime_2, Any, Mes, SumaMonthKW,SumaMonthReactiveKW,SumaMonthLaundryKW,SumaMonthHeaterKW )


NEWhouseholdMonthGood$DateTime_2<-anytime(NEWhouseholdMonthGood$DateTime_2) 


NEWhouseholdMonthGood$DateTime_2<- format(ymd_hms(NEWhouseholdMonthGood$DateTime_2),format='%Y/%m')


NEWhouseholdMonthGood<- distinct(NEWhouseholdMonthGood)

NEWhouseholdMonthGood$DateTime_2<- as.ordered(NEWhouseholdMonthGood$DateTime_2)


View(NEWhouseholdMonthGood)

NEWhouseholdMonthGood<- filter(NEWhouseholdMonthGood, Any !=2006)

View(NEWhouseholdMonthGood)
str(NEWhouseholdMonthGood)



####HOUSEHOLD HISTOGRAM PER WEEK ####
NEWhouseholdWeek<- NEWhouseholdSUBSETTING

NEWhouseholdWeek<- NEWhouseholdWeek%>% group_by(Any,Semana)%>% mutate(SumaWeekKW = sum(Global_active_power))

NEWhouseholdWeek<- NEWhouseholdWeek%>% group_by(Any,Semana)%>%mutate(SumaWeekReKW = sum(Global_reactive_power))

NEWhouseholdWeek<- NEWhouseholdWeek%>% group_by(Any, Semana)%>%mutate(SumaWeekKitchenKW = sum(Sub_metering_1))

NEWhouseholdWeek<- NEWhouseholdWeek%>% group_by(Any, Semana)%>%mutate(SumaWeekLaundryKW = sum(Sub_metering_2))

NEWhouseholdWeek<- NEWhouseholdWeek%>% group_by(Any, Semana)%>%mutate(SumaWeekHeaterKW = sum(Sub_metering_3))

NEWhouseholdWeekGood<- NEWhouseholdWeek

NEWhouseholdWeekGood<- select(NEWhouseholdWeekGood, Any, Semana, SumaWeekKW,SumaWeekReKW,SumaWeekKitchenKW, SumaWeekLaundryKW,SumaWeekHeaterKW )

NEWhouseholdWeekGood<- distinct(NEWhouseholdWeekGood)


NEWhouseholdWeekGood<- filter(NEWhouseholdWeekGood, Any !=2006)

View(NEWhouseholdWeekGood)

unique(NEWhouseholdWeekGood$Semana)

str(NEWhouseholdWeekGood)


####HOUSEHOLD HISTOGRAM WEEKDAY/PER MONTH #### 

NEWhouseholdWDayMonth<- NEWhouseholdSUBSETTING

NEWhouseholdWDayMonth<- NEWhouseholdWDayMonth%>% group_by(Any, Semana, DiaSemana)%>% mutate(SumaWeekDayKW = sum(Global_active_power))

NEWhouseholdWDayMonth<- NEWhouseholdWDayMonth%>% group_by(Any, Semana, DiaSemana)%>%mutate(SumaWeekDayReKW = sum(Global_reactive_power))

NEWhouseholdWDayMonth<- NEWhouseholdWDayMonth%>% group_by(Any, Semana, DiaSemana)%>%mutate(SumaWeekDayKitchenKW = sum(Sub_metering_1))

NEWhouseholdWDayMonth<- NEWhouseholdWDayMonth%>% group_by(Any, Semana, DiaSemana)%>%mutate(SumaWeekDayLaundryKW = sum(Sub_metering_2))

NEWhouseholdWDayMonth<- NEWhouseholdWDayMonth%>% group_by(Any, Semana, DiaSemana)%>%mutate(SumaWeekDayHeaterKW = sum(Sub_metering_3))

NEWhouseholdWDayMonthGood<- NEWhouseholdWDayMonth

NEWhouseholdWDayMonthGood<- select(NEWhouseholdWDayMonthGood, Any, DiaSemana, Semana, SumaWeekDayKW,SumaWeekDayReKW,SumaWeekDayKitchenKW,SumaWeekDayLaundryKW,SumaWeekDayHeaterKW )

NEWhouseholdWDayMonthGood<- distinct(NEWhouseholdWDayMonthGood)

NEWhouseholdWDayMonthGood$DateTime_2<-anytime(NEWhouseholdWDayMonthGood$DateTime_2) 

NEWhouseholdWDayMonthGood<- distinct(NEWhouseholdWDayMonthGood)


View(NEWhouseholdWDayMonthGood)

unique(NEWhouseholdWDayMonthGood$DiaSemana)

NWEhouseholdWDayMonthGood<- filter(NEWhouseholdWDayMonth, Any !=2006)



####CHECKING PER HOUR WITHIN DAY OF WEEK####
#DIAMESHORA<- householdHOUR
#DIAMESHORA<- select(DIAMESHORA,DateTime_2, Any, Hora, Mes, Dia,DiaSemana,SumaHoraKW,SumaHoraReKW,SumaKitchenKW,SumaLaundryKW,SumaHeaterKW)
#DIAMESHORA<- distinct(DIAMESHORA) 

#DIAMESHORAGOOD<-DIAMESHORA

#DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaKW = sum(SumaHoraKW))
#DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaReKW = sum(SumaHoraReKW))
#DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaKitchen = sum(SumaKitchenKW))
#DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaLaundry = sum(SumaLaundryKW))
#DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaHeater = sum(SumaHeaterKW))


####GRAPHS CONSUMPTION PER HOUR ON EACH DAY OF THE WEEK####

#DIASEMANACTIVE<-household2%>% group_by(DiaSemana) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
#DIASEMANACTIVE

#DIASEMANAHORACTIVE<-household2%>% group_by(DiaSemana, Hora) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
#DIASEMANAHORACTIVE

#HORAACTIVE<-household2%>% group_by(Hora) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
#HORAACTIVE

#household2$HourKWSum<-household2%>% group_by(Hora) %>% mutate(ActivePerHour= sum(Global_ConsumptionKWh))
#HORAACTIVE




####365 Days per Year####
#DIAMES<- householdHOUR
#DIAMES$DiaDelAny<-""

#DIAMES$DiaDelAny<- strftime(DIAMES$DateTime_2, format = "%j", tz = "Europe/Paris")####to see which Day of the year it is
#DIAMES$DiaDelAny<-as.numeric(DIAMES$DiaDelAny)

#DIAMES<- DIAMES%>% group_by(DiaDelAny)%>% mutate(SumaDayKW = sum(SumaHoraKW))

#DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayReKW = sum(SumaHoraReKW))

#DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayKitchenKW = sum(SumaKitchenKW))

#DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayLaundryKW = sum(SumaLaundryKW))

#DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayHeaterKW = sum(SumaHeaterKW))


#View(DIAMES)

#unique(DIAMES$SumaDayKW)
#unique(DIAMES$SumaDayReKW)
#unique(DIAMES$SumaDayKitchenKW)
#unique(DIAMES$SumaDayLaundryKW)
#unique(DIAMES$SumaDayHeaterKW)
#unique(DIAMES$DiaDelAny)


#DIAMES$SumaHoraKW<- NULL
#DIAMES$SumaHoraReKW<- NULL
#DIAMES$SumaKitchenKW<- NULL
#DIAMES$SumaLaundryKW<- NULL
#DIAMES$SumaHeaterKW<- NULL
#DIAMES$Hora<- NULL
#DIAMES$Time<- NULL
#DIAMES$DateTime<- NULL
#DIAMES$Global_ConsumptionKWh<- NULL
#DIAMES$Global_Consumption_reactiveKWh<- NULL
#DIAMES$Submetter1_kwh<- NULL
#DIAMES$Submetter2_kwh<- NULL
#DIAMES$Submetter3_kwh<- NULL

#DIAMES<- distinct(DIAMES) 

#str(DIAMES)
#View(DIAMES)




#ggplot(data=DIAMES, aes(x=DiaDelAny , y=SumaDayKW, group=Any, colour=Any)) +
  #geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Any ~ .)#, margins = FALSE)

#p1 <- ggplot(DIAMES, aes(x=DiaDelAny, y=SumaDayKW, colour=Any, group=Any)) +
 # geom_line() 
#p1




####SET RULES OF OUTLIERS PER DAY####
boxplot(DIAMES$SumaDayKW) #Consumption Outlier Rule
boxplot(DIAMES$SumaDayReKW) #Reactive Outlier Rule
boxplot(DIAMES$SumaDayKitchenKW) #Kitchen Outlier Rule
boxplot(DIAMES$SumaDayLaundryKW) #Laundry Outlier Rule
boxplot(DIAMES$SumaDayHeaterKW) #Heater Outlier Rule

####DETECTING WHICH ARE THE ROWS OF THE OUTLIERS###
#For Rective Power
unique(DIAMES$SumaDayReKW)

OutliersRE <- filter(DIAMES, SumaDayReKW >= 1000)
OutliersRE
View(OutliersRE)

OutliersKITCHEN<-filter(DIAMES, SumaDayKitchenKW >= 900)
OutliersKITCHEN
View(OutliersKITCHEN)

OutliersLAUNDRY<-filter(DIAMES, SumaDayLaundryKW >= 1050)
OutliersLAUNDRY
View(OutliersLAUNDRY)

OutliersHEATER<-filter(DIAMES, SumaDayHeaterKW >= 3750)
OutliersHEATER
View(OutliersHEATER)

OutliersIn2<-filter(DIAMES,SumaDayLaundryKW >= 1050 & SumaDayHeaterKW >= 3750  )
OutliersIn2
View(OutliersIn2)


#WANT TO CREATE A SUBSET WITH ALL THE INFO THOSE..



####TIME SERIES VECTORIZING AND PLOTS####
#HOURS
tsHour<-ts(NEWhouseholdHOURS$SumaHoraKW,frequency=8760,start=c(2007,1), end=c(2010, 7896))
plot(tsHour)

plot(decompose(tsHour))

df_tslm <- data.frame(SumaHoraKW = tsHour, as.numeric(time(tsHour)))
names(df_tslm) <- c("SumaHoraKW", "time")

tslm <- tslm(SumaHoraKW~season + trend,df_tslm)
fc_tslm <- forecast(tslm, h=36)
autoplot(fc_tslm)
summary(fc_tslm)

#MONTH
ts_month <- ts(NEWhouseholdMonthGood$SumaMonthKW, frequency=12, start = c(2007, 1),
               end = c(2010, 10))
plot(ts_month)

plot(decompose(ts_month))

df_tslm <- data.frame(NEWhouseholdMonthGoodSumaMonthKW = ts_month, as.numeric(time(ts_month)))
names(df_tslm) <- c("SumaMonthKW", "time")

tslm <- tslm(SumaMonthKW~season + trend,df_tslm)
fc_tslm <- forecast(tslm, h=36)
autoplot(fc_tslm)
summary(fc_tslm)

#WEEK
ts_week <- ts(NEWhouseholdWeekGood$SumaWeekKW, frequency=53, start = c(2007, 1),
               end = c(2010, 47))
plot(ts_week)

plot(decompose(ts_week))

df_tslm <- data.frame(SumaWeekKW = ts_week, as.numeric(time(ts_week)))
names(df_tslm) <- c("SumaWeekKW", "time")

tslm <- tslm(SumaWeekKW~season + trend,df_tslm)
fc_tslm <- forecast(tslm, h=36)
autoplot(fc_tslm)
summary(fc_tslm)



#WEEKDAY
ts_weekDay <- ts(NEWhouseholdWDayMonthGood$SumaWeekDayKW, frequency=365, start = c(2007, 1),
              end = c(2010, 329))
plot(ts_weekDay)

plot(decompose(ts_weekDay))

df_tslm <- data.frame(SumaWeekDayKW = ts_weekDay, as.numeric(time(ts_weekDay)))
names(df_tslm) <- c("SumaWeekDayKW", "time")

tslm <- tslm(SumaWeekDayKW~season + trend,df_tslm)
fc_tslm <- forecast(tslm, h=36)
autoplot(fc_tslm)
summary(fc_tslm)


#HOUR WITHIN DAY





