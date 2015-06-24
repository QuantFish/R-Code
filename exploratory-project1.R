#Exploratory analysis assignment 1, Corsera
# loading data
hpc <- read.table("household_power_consumption.txt",sep=";",header=T,na.strings=c('?'))
hpcsub1 <- subset(hpc, hpc$Date == '1/2/2007')
hpcsub2 <- subset(hpc, hpc$Date == '2/2/2007')
hpcsub3 <- rbind(hpcsub1,hpcsub2)

# plot 1
png(filename="plot1.png",width=480,height=480,units="px",pointsize=12)
hist(as.numeric(hpcsub3$Global_active_power),main="Global Active Power", xlab="Global Active Power(kilowatts)", col="red")
dev.off()

#plot 2
png(filename="plot2.png",width=480,height=480,units="px",pointsize=12)
plotTo <- hpcsub3$Global_active_power
dateTime <- paste(hpcsub3$Date,hpcsub3$Time)
cdateTime <- strptime(dateTime,format="%d/%m/%Y %H:%M:%S")
plot(cdateTime,plotTo,type="l",xlab="",ylab="Global Active Power (kikowatts)")
dev.off()

#plot 3
png(filename="plot3.png",width=650,height=650,units="px",pointsize=12)
plot(cdateTime,hpcsub3$Sub_metering_1,type="l",col="black",ylab="Energy sub metering", xlab="")
lines(cdateTime,hpcsub3$Sub_metering_2,type="l",col="red")
lines(cdateTime,hpcsub3$Sub_metering_3,type="l",col="blue")
legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off()

#plot4
png(filename="plot4.png",width=550,height=550,units="px",pointsize=12)
par(mfrow=c(2,2))

# 1st graph
plot(cdateTime,hpcsub3$Global_active_power,type="l",xlab="",ylab="Global Active Power")

# 2nd graph
plot(cdateTime,hpcsub3$Voltage, xlab="datetime",ylab="Voltage", type="l")

# 3rd graph
plot(cdateTime, hpcsub3$Sub_metering_1,type="l",col="black",ylab="Energy sub metering", xlab="")
lines(cdateTime, hpcsub3$Sub_metering_2, type="l", col="red")
lines(cdateTime, hpcsub3$Sub_metering_3,type="l",col="blue")
legend("topright",bty="n",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# 4th graph
plot(cdateTime,hpcsub3$Global_reactive_power, type="l",ylab="Global_reactive_power", xlab="datetime")
dev.off()

#Exersices ISLR chapter 2 exercise 7a
#Function for calculating Euclidian distance
euc.dist <- function(x1,x2) {
      sqrt(sum((x1-x2)^2))
}
#Euclidian distance with three qualifiers
euc.dist <- function(x1,x2,x3) {
    sqrt(sum((x1-x2-x3)^2))
}

hist(dates,"month")
dates <- hpc$Date
dates <- as.Date(dates,format="%d/%m/%Y")

#Exersices ISLR chapter 2 exercise 8c
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college=data.frame(college,Elite)

#If you wish to know which university has the highest rate of PhD's
summary(college$PhD)
nrow(subset1 <- college[college$PhD == 103,])
row.names(subset1)

#Diffrent plots from the Auto data set.
pairs(Auto[1:8]) #Not very helpful, too much info for my taste
str(Auto$name) # Factor with 304 diffrent levels. Too many for grouping by name

# Cylinders
str(Auto$cylinders) # Should be a factor model, so we convert.
Auto$cylinders <- as.factor(Auto$cylinders)
plot(Auto$cylinders,Auto$mpg,xlab="Cylinders",ylab="Mileage",varwidth=T,col=C(2:6))
# Conclusion: It seems like 4 cyl gives most mileage.

#Weight, displacement, horsepower
plot(Auto$horsepower,Auto$mpg,xlab="Horsepower",ylab="Mileage",col="black",pch=19)
plot(Auto$weight,Auto$mpg,xlab="Weight",ylab="Mileage",col="blue",pch=19)
plot(Auto$displacement,Auto$mpg,xlab="Displacement",ylab="Mileage",col="red",pch=19)
#Weight, displacement and horsepower seem to have an inverse effect with mpg. The plots
#look like they are almost the same shape, so I am going to test if the two are correlated.
plot(Auto$weight, Auto$horsepower,xlab="Weight",ylab="Horsepower",col="blue",pch=19)
plot(Auto$displacement,Auto$horsepower,xlab="Displacement",ylab="Horsepower",col="blue",pch=19)
plot(Auto$displacement,Auto$weight,xlab="Displacement",ylab="Weight",col="blue",pch=19)

#cor to find a correlation
cor(Auto$weight,Auto$horsepower) #Highly correlated, may only use one variable when predicting mpg
cor(Auto$displacement,Auto$horsepower) 
# Even more correlated do not need both variables in prediction.
cor(Auto$displacement,Auto$weight) 
# Highly correlated. Do not need to use both variables in prediction.
# Because these 3 variables are highly correlated with each other it would be unnecessary to use them all for prediction.

# Year
str(Auto$year) # check to see if year is factor.
Auto$year <- as.factor(Auto$year) # convert to factor in order to create boxplot
plot(Auto$year,Auto$mpg,varwidth=T,xlab="Year",ylab="Mileage",col="blue",pch=19)
# Overall increase in mpg over the years. Almost doubled in 1 decade.

# Origin
# ?Auto shows that the origin variables refers to 1 = usa; 2 = europe; 3 = japan
Auto$origin <- as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg,varwidth=T, xaxt="n",xlab="",ylab="Mileage",col="blue",pch=19)
labels <- paste(c("USA", "Europe","Japan"))
text(x=seq_along(labels),y=par("usr")[3]-a,srt=0,labels=labels,xpd=T)
#Conclusion: Japanese cars have higher mpg than US or European cars.

# Accelaration
plot(Auto$acceleration,Auto$mpg,xlab="Acceleration",ylab="Mileage",col="black",pch=19)
cor(Auto$acceleration,Auto$mpg) # not strong correlation. I would leave this variable out when predicting mpg because of weak pattern.

# Exercise 2.4.10.
library(MASS)
Boston
str(Boston) # 506 rows and 14 columns, rows = observations, columns = variables.
names(Boston)
data(Boston)
dev.copy(pdf,'BostonPlot.pdf')
pairs(Boston)d
dev.off()
?pairs

# Div pairwise scatterplots of the predictors with comments
Boston$chas <- as.factor(Boston$chas) #for the boxplots
Boston$crim <- Boston$crim/100
plot(~zn + crim, Boston, col = "#00000022",pch=19)
plot(~indus + crim,Boston,col="#00000022",pch=19)
plot(~chas + crim,Boston,col="#00000022",pch=19)
plot(~nox + crim,Boston,col="#00000022",pch=19)
plot(~rm + crim,Boston,col="#00000022",pch=19)
plot(~age + crim,Boston,col="#00000022",pch=19)
plot(~dis + crim,Boston,col="#00000022",pch=19)
plot(~rad + crim,Boston, col="#00000022",pch=19)
plot(~tax + crim,Boston,col="#00000022",pch=19)
plot(~ptratio + crim,Boston,col="#00000022",pch=19)
plot(~black + crim,Boston,col="#00000022",pch=19)
plot(~lstat + crim,Boston,col="#00000022",pch=19)
plot(~medv + crim,Boston,col="#00000022",pch=19)
plot(~zn + crim, Boston[Boston$crim > 50],col="#00000022", pch=19)
plot(~indus + crim,Boston[Boston$crim > 50],col="#00000022", pch=19)
plot(~chas + crim, Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~nox + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~rm + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~age + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~dis + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~rad + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~tax + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~ptratio + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~black + crim,Boston[Boston$crim > 50],col="#00000022",pch=19)
plot(~lstat + crim,Boston[Boston$crim > 50],col="#00000022", pch=19)
plot(~medv + crim,Boston[Boston$crim > 50],col="#00000022", pch=19)

# hard to predict anything about the crime rate from the scatterplot matrice from individual graphs.

attach(Boston)
hist(crim,breaks = 50)
pairs(Boston[crim < 10,])
nrow(Boston[crim < 6,])/nrow(Boston)
# 80% of neighborhoods have crime rate per capita of less than 6%

pairs(Boston[crim<20,])

nrow(Boston[crim > 20,])
nrow(Boston[crim > 20,])/nrow(Boston)
#3.5% of Boston's suburbs have crime rate of more than 20% ???

# Use the range function to find the range of numerical variables
# For the 18 suburbs with crime rate > 20, the tax rate is 666, and the
# ptratio is 20.2. This is not representative of anything particular.
range(Boston[crim>20,]$crim)

median(ptratio)

#10 g

row.names(Boston[min(medv),])
# row.names function will return the return row number of whatever criteria
# is in its parameter.
# range for tax in the entire data set.
range(tax)
#value of tax when medv is minimum
Boston[min(medv),]$tax

#run the same code for the other predictors
Boston[min(medv),]$age
Boston[min(medv),]$rad
Boston[min(medv),]$tz
Boston[min(medv),]$black
Boston[min(medv),]$chas
Boston[min(medv),]$indus

nrow(Boston[rm>7,])
nrow(Boston[rm>8,])
nrow(Boston[rm>8,]$crim)
#low crime rate
detach(Boston)

# Examples of created HDF5 directories

library(rhdf5)

created=h5createFile("example.h5")
created

created = h5createGroup("example.h5","foo")
created = h5createGroup("example.h5","baa")
created = h5createGroup("example.h5","foo/foobaa")
h5ls("example.h5")

# Writing to groups
A = matrix(1:10,nr=5,nc=2)
h5write(A,"example.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B,"scale") <- "liter"
h5Write(B,"example.h5","foo/foobaa/B")
h5ls("example.h5")

# Write a data set
df = data.frame(1L:5L,seq(0,1,length.out=5),c("ab","cde","fghi","a","s"),
                stringsAsFactors=F)
h5write(df,"example.h5","df")
h5ls("example.h5")

# Reading data
readA = h5read("example.h5","foo/A")
readB = h5read("example.h5","foo/foobaa/B")
readdf = h5read("example.h5","df")
readA

#writing and reading chunks
h5write(c(12,13,14),"example.h5","foo/A",index=list(1:3,1))
h5read("example.h5","foo/A")

# Getting data off webpages - readLines()
con <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode <- readLines(con)
close(con)
htmlCode

# Parsing with XML
library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url,useInternalNodes=T)

xpathSApply(html,"//title",xmlValue)
xpathSApply(html,"//td[@id='col-citedby']",xmlValue)

#GET from httr package
library(httr);html2 = GET(url)
content2 = content(html2,as="text")
parsedHtml = htmlParse(content2,asText=T)
xpathSApply(parsedHtml,"//title",xmlValue)





