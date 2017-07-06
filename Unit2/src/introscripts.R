sd(c(5,8,12))
which.min(c(4,1,6))
8*6
2^16

sqrt(2)
abs(-12)

Country=c("Brazil","China","India","Switzerland","USA")
ls()
LifeExpectancy=c(74,76,65,83,79)
seq(0,100,2)

CountryData = data.frame(Country,LifeExpectancy)
CountryData$Population=c(199000,1390000,1240000,7997, 318000)
Country=c("Australia", "Greece")
LifeExpectancy=c(82,81)
Population=c(23050,11125)
NewCountryData=data.frame(Country,LifeExpectancy,Population)
AllCountryData=rbind(CountryData,NewCountryData)


getwd()
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe=subset(WHO,Region=="Europe")
write.csv(WHO_Europe, "WHO_Europe.csv")
rm WHO_Europe

mean(WHO$Under15)
sd(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[which.min(WHO$Under15)]
WHO$Country[which.max(WHO$Under15)]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]
mean(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]

hist(WHO$CellularSubscribers)
summary(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region,xlab="",ylab="Life Expectancy",main="Life Expectancy of Countries by Region")

table(WHO$Region)
tapply(WHO$Over60,WHO$Region,mean)
tapply(WHO$LiteracyRate,WHO$Region,min)
tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=TRUE)
tapply(WHO$ChildMortality,WHO$Region,min,na.rm=TRUE)


USDA = read.csv("USDA.csv")
USDA$Description[which.max(USDA$Sodium)]
names(USDA)
HighSodium = subset(USDA,Sodium > 10000)
nrow(HighSodium)
USDA$Description[match("CAVIAR",USDA$Description)]
USDA$Sodium[match("CAVIAR",USDA$Description)]
sd(USDA$Sodium)
sd(USDA$Sodium,na.rm=TRUE)
hist(USDA$VitaminC)
hist(USDA$VitaminC,xlim=c(1,100))
hist(USDA$VitaminC,xlim=c(1,100),breaks=2000)
subset(USDA$VitaminC,USDA$VitaminC > 100)
tst=subset(USDA,USDA$VitaminC > 300
tst[c("Description","VitaminC")]
USDA$HighSodium=as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighFat=as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs=as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
USDA$HighProtein=as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, sd, na.rm=TRUE)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month=months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
arrests=subset(mvt,Arrest==TRUE)
nrow(arrests)
nrow(mvt)
which.max(table(arrests$Month))

jpeg('arrestdatehist.jpg')
hist(mvt$Date, breaks=100)
dev.off()

jpeg('arrestdateboxplot.jpg')
boxplot(mvt$Date ~ mvt$Arrest)
dev.off()

gta2001=subset(mvt,Year==2001)
gta2007=subset(mvt,Year==2007)
gta2012=subset(mvt,Year==2012)

sort(table(mvt$LocationDescription))

Top5=subset(mvt,(LocationDescription=="STREET"
|LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"
|LocationDescription=="ALLEY"|LocationDescription=="GAS STATION"
|LocationDescription=="DRIVEWAY - RESIDENTIAL"))

Top5$LocationDescription = factor(Top5$LocationDescription)
st

street=subset(Top5,LocationDescription=="STREET")
parking=subset(Top5,LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)")
alley=subset(Top5,LocationDescription=="ALLEY")
gas=subset(Top5,LocationDescription=="GAS STATION")
driveway=subset(Top5,LocationDescription=="DRIVEWAY - RESIDENTIAL")

table(street$Arrest)
table(parking$Arrest)
table(alley$Arrest)
table(gas$Arrest)
table(driveway$Arrest)

11595/(144969+11595) 
1603/(13249+1603)
249/(2059+249)
439/(1672+439)
132/(1543+132)

#this does a table of all 5 values
tst=table(Top5$LocationDescription,Top5$Arrest)

#Download and read the following files into R, using the read.csv function: IBMStock.csv, GEStock.csv, ProcterGambleStock.csv, CocaColaStock.csv, and BoeingStock.csv. (Do not open these files in any spreadsheet software before completing this problem because it might change the format of the Date field.)
#
#Call the data frames "IBM", "GE", "ProcterGamble", "CocaCola", and "Boeing", respectively. Each data frame has two variables, described as follows:
IBM=read.csv("IBMStock.csv")
GE=read.csv("GEStock.csv")
ProcterGamble=read.csv("ProcterGambleStock.csv")
CocaCola=read.csv("CocaColaStock.csv")
Boeing=read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

nrow(IBM)
nrow(GE)
nrow(ProcterGamble)
nrow(CocaCola)
nrow(Boeing)

plot(IBM$Date,IBM$StockPrice,"l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue", lty=2)
lines(ProcterGamble$Date, ProcterGamble$StockPrice, lty=2)

abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="purple")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="green")

abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

tapply(IBM$StockPrice,months(IBM$Date), mean)

tapply(GE$StockPrice,months(GE$Date), mean)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice,months(Boeing$Date), mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date), mean)

CPS=read.csv("CPSData.csv")
CC=read.csv("CountryCodes.csv")

sort(table(CPS$Region))

hisp=subset(CPS,CPS$Hispanic!=0)

marrna=subset(CPS,is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))
table(CPS$Region, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

MetroAreaMap=read.csv('MetroAreaCodes.csv')
nrow(MetroAreaMap)
CountryMap=read.csv('CountryCodes.csv')
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
table(is.na(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea, mean))
sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

CPS = merge(CPS, MetroAreaMap, by.x="Country", by.y="Code", all.x=TRUE)

NY=subset(CPS,CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",na.rm=TRUE)

sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=="Somolia", CPS$MetroArea, sum, na.rm=TRUE))

AP=read.csv("AnonymityPoll.csv")

table(poll$Sex, poll$Region)

table(poll$State,poll$Region=="Midwest")
sort(tapply(poll$Region=="Midwest",poll$State,sum))

table(poll$Smartphone,poll$Internet.Use)
table(is.na(poll$Smartphone),is.na(poll$Internet.Use))

table(poll$Info.On.Internet==0)

table(poll$Worry.About.Info)
table(is.na(poll$Worry.About.Info))

table(poll$Anonymity.Possible)
table(is.na(poll$Anonymity.Possible))

table(limited$Worry.About.Info)
table(is.na(limited$Worry.About.Info))

table(limited$Anonymity.Possible)
table(is.na(limited$Anonymity.Possible))

table(poll$Tried.Masking.Identity)
table(is.na(poll$Tried.Masking.Identity))

table(limited$Privacy.Laws.Effective)
table(is.na(limited$Privacy.Laws.Effective))

hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)

#these are wrong, they didn't want age to match info.on.internet
table(limited$Age==limited$Info.On.Internet)
table(limited$Age,limited$Info.On.Internet)

tapply(limited$Info.On.Internet,limited$Smartphone==1,mean)
tapply(limited$Tried.Masking.Identity,limited$Smartphone==1,mean,na.rm=TRUE)
