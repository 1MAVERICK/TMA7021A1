#DATA MINING & ANALYTICS - TMA7021- ASSIGNMENT 1
# AHMED ABDULSALAM ALI - 1142600011
#START OF THE CODE
#Reading Location Hotspot 2014.csv file for (Sealngor State) form week 36 to week 53 only.
LHS14 <- function() {
x <- read.csv("LocationHotspot2014.csv")
x$State <- toupper(x$State)
x <- x[x$State == "SELANGOR" & x$Week %in% 36:53, ]
x
}
LHS14 <- LHS14()
dim(LHS14)
View(LHS14)
#
#Reading Location Hotspot 2015.csv file for (Sealngor State) form week 1 to week 18 only.
LHS15 <- function() {
y <- read.csv("LocationHotspot2015.csv")
y$State <- toupper(y$State)
y <- y[y$State == "SELANGOR" & y$Week %in% 1:18, ]
y
}
LHS15 <- LHS15()
dim(LHS15)
View(LHS15)
#
#Reading RainWind.csv file and filter
RW <- function() {
z <- read.csv("RainWind.csv")
z <- z[z$Year==2014|z$Year==2015,]
z$Date <- as.Date(paste(z$Year, z$Month, z$Day), format = "%Y %m %d")
z$Week <- as.numeric(format(z$Date,"%W"))+1
z
}
RW <- RW()
# Removing variables (Year, Month and Day) from RainWind dataset due to it's already converted to Date format
#Removing variable Stnno from RainWind (RW) dataset to prepare it for merging with (ALHS) datasets in one dataset called (dengue_data_2014_2015)
# Filter RainWind file within the period from "Sep-1-2014" to "Apr-30-2015" only.
NRW <- function() {
RW <- subset(RW, select= -Stnno)
RW <- subset(RW, select= -Month)
RW <- subset(RW, select= -Day)
RW[RW$Date >= as.Date("2014-09-01") & RW$Date <= as.Date("2015-04-30"),]
}
NRW <- NRW()
dim(NRW)
View(NRW)
# Removing RW
RW <- NULL
NRW$Rainfall_mm <- (function(){
trim <- as.numeric(as.character.factor(NRW$Rainfall_mm))
ifelse(test = (is.na(trim) | trim < 0), yes = 0, no = trim)
})()
NRW$Wind_mean_24Hr[NRW$Wind_mean_24Hr<0] <- 0
NRW$Humidity <- (function(){
trim <- as.numeric(as.character.factor(NRW$Humidity))
ifelse(test = (is.na(trim)), yes = 0, no = trim)
})()
NRW$Solar_Radiation_Mjm2 <- (function(){
trim <- as.numeric(as.character.factor(NRW$Solar_Radiation_Mjm2))
ifelse(test = (is.na(trim) | trim < 0), yes = 0, no = trim)
})()
# Rename variables in NRW data frame before aggregate them in weekly form
names(NRW)[2]<-paste("Weekly_Rainfall")
names(NRW)[3]<-paste("Weekly_Wind")
names(NRW)[4]<-paste("Weekly_Humidity")
names(NRW)[5]<-paste("Weekly_Solar_Radiation_Mjm2")
View(NRW)
ListWeek <- list(Year=NRW$Year, Week=NRW$Week)
Weekly_Wind = aggregate(NRW$Weekly_Wind, ListWeek, mean, na.rm=T, simplify = T)
Weekly_Rainfall =  aggregate(NRW$Weekly_Rainfall, ListWeek, mean, na.rm=T)
Weekly_Humidity = aggregate(NRW$Weekly_Humidity, ListWeek, mean, na.rm=T)
Weekly_Solar_Radiation_Mjm2 =  aggregate(NRW$Weekly_Solar_Radiation_Mjm2, ListWeek, mean, na.rm=T)
#(i). Display the dimension of dengue outbreak dataset & weather dataset
dim(LHS14)
dim(LHS14)
dim(NRW)
#(ii). Binding LHS14, LHS15 in one dataset called (ALHS)
ALHS <- rbind(LHS14,LHS15)
dim(ALHS)
View(ALHS)
#Merging ALHS & NRW
dengue_data_2014_2015 <- Reduce(
function(a,b) {
merge(a,b, by = c("Year","Week"))
},
list(ALHS, Weekly_Wind, Weekly_Rainfall, Weekly_Humidity, Weekly_Solar_Radiation_Mjm2)
)
View(dengue_data_2014_2015)
# Rename variables in (dengue_data_2014_2015)  data frame after aggregation
names(dengue_data_2014_2015)[8]<-paste("Weekly_Rainfall")
names(dengue_data_2014_2015)[9]<-paste("Weekly_Wind")
names(dengue_data_2014_2015)[10]<-paste("Weekly_Humidity")
names(dengue_data_2014_2015)[11]<-paste("Weekly_Solar_Radiation_Mjm2")
View(dengue_data_2014_2015)
#Exporting data set in to CSV file
write.csv(dengue_data_2014_2015, "C:/Users/Maverick/Google Drive/TMA7021A1/dengue_data_2014_2015.csv")
##check if there is a blank space column " Area " in (dengue_data_2014_2015) dataset
grepl(" ",dengue_data_2014_2015$Area)
#(iii). Removing blank spaces from "Aerea" column in (dengue_data_2014_2015) dataset
dengue_data_2014_2015$Area <- gsub(" ","_",dengue_data_2014_2015$Area)
##check if there is a blank space in column " Location " in (dengue_data_2014_2015) dataset
grepl(" ",dengue_data_2014_2015$Location)
#i. Replace blank space with underscore from Location
dengue_data_2014_2015$Location <- gsub(" ","_",dengue_data_2014_2015$Location)
##check if there is a brackets in column " Location " in (dengue_data_2014_2015) dataset
grepl("[()]",dengue_data_2014_2015$Location) # TRUE
grepl("[[]]",dengue_data_2014_2015$Location) # FALSE
grepl("[{}]",dengue_data_2014_2015$Location) # FALSE
#ii. Remove All brackets
dengue_data_2014_2015$Location <- gsub("[()]","",dengue_data_2014_2015$Location)
##check if there is any other punctuation characters in column " Location " in (dengue_data_2014_2015) dataset
grepl("[[:punct:]]",dengue_data_2014_2015$Location)
#iii. Replace ("-", "/", "&", and ",") with underscore
dengue_data_2014_2015$Location <- gsub("[[:punct:]]","_",dengue_data_2014_2015$Location)
View(dengue_data_2014_2015)
#Reading Mukim_Selangor.csv file
Mukim_Selangor <- (function(){
M<-read.csv("Mukim_Selangor.csv", header = F)
names(M) <- c("Area", "Population")
M <- M[M$Population!="",]
Mukim <- grepl("[a-z]", M$Area, ignore.case = F)
install.packages("stringr")
library(stringr)
M$Area <- str_trim(M$Area)
M <- M[!Mukim,]
M <- M[M$Area!="JUMLAH",]
M <- M[M$Area!="TOTAL",]
M <- M[M$Area!="(Batang Berjuntai)",]
M$Population <-apply(M, MARGIN = 1, function(n){
as.numeric(gsub(n["Population"], pattern = "[ ,]", replacement = ""))
})
M$Area <- sapply(M$Area, function(n){
n<-gsub("ULU ","HULU ",n)
n<-gsub(" ", "_", str_to_title(n))
})
M
})()
View(Mukim_Selangor)
# Aggregate dengue_2014_2015 data frame with Mukim_Selangor data frame by Area
DDP14_15 <- Reduce(
function(a,b) {
merge(a,b, by = "Area")
},
(list(dengue_data_2014_2015, Mukim_Selangor))
)
View(DDP14_15)
# Set Factor As numeric & rename it
DDP14_15$Total_Kes <- as.numeric(DDP14_15$Total_Kes)
Weekly_Area_Outbreak <- function(){
x <- aggregate(DDP14_15$Total_Kes,
by=list(Year=DDP14_15$Year, Week=DDP14_15$Week, Area = DDP14_15$Area), FUN="sum", na.rm=TRUE)
names(x)[6] <- "Total_Area_Cases"
x
}()
View(DDP14_15)
DDP14_15$Duration_of_Outbreak_in_day <- as.numeric(DDP14_15$Duration_of_Outbreak_in_day)
#Exporting and Writing the last DATA Frame
write.csv(DDP14_15, "C:/Users/Maverick/Google Drive/TMA7021A1/DDP14_15.csv")
# Creating the plot chart to compare the total outbreak cases for all areas in Selangor from September 2014 to April 2015 only
install.packages("ggplot2")
library(ggplot2)
qplot(Area, data=DDP14_15, geom="Density", fill=Area, alpha=I(.7),
main="Total_Outbreak", xlab="Area",
ylab="Outbreak")
install.packages("rmarkdown")
library(rmarkdown)
#END OF THE CODE
