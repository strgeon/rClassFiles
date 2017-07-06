# TODO: Add comment
# 
# Author: Scott
###############################################################################

getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src"

baseball=read.csv("baseball.csv")
str(baseball)
summary(baseball)

nrow(baseball)
table(baseball$Year)
# 
# 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1973 1974 1975 1976 1977 1978 
#   20   20   20   20   20   20   20   24   24   24   24   24   24   24   26   26 
# 1979 1980 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1996 1997 
#   26   26   26   26   26   26   26   26   26   26   26   26   26   28   28   28 
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
#   30   30   30   30   30   30   30   30   30   30   30   30   30   30   30 

nrow(table(baseball$Year))
length(table(baseball$Year))
# [1] 47
subset(baseball,Playoffs>0)
baseball=subset(baseball,Playoffs>0)
length(table(baseball$Year))
# [1] 47
str(baseball)
nrow(baseball)
# [1] 244
#Through the years, different numbers of teams have been invited to the playoffs. Which of the following has been the number of teams making the playoffs in some season? Select all that apply.
tapply(baseball$Playoffs,baseball$Year,sum)
table(baseball$Year)
# 
# 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1973 1974 1975 1976 1977 1978 
#    2    2    2    2    2    2    2    4    4    4    4    4    4    4    4    4 
# 1979 1980 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1996 1997 
#    4    4    4    4    4    4    4    4    4    4    4    4    4    4    8    8 
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
#    8    8    8    8    8    8    8    8    8    8    8    8    8    8   10 
table(table(baseball$Year))
# 
#  2  4  8 10 
#  7 23 16  1 

PlayoffTable = table(baseball$Year)
names(PlayoffTable)
nrow(table(baseball$Year))
hist(baseball$Year,nrow(table(baseball$Year)))
subset(baseball,Year==2001)
PlayoffTable[c("1990", "2001")]
# 
# 1990 2001 
#    4    8 

PlayoffTable[as.character(baseball$Year)]
as.character(baseball$Year)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

nrow(subset(baseball,NumCompetitors==8))
# [1] 128
table(baseball$NumCompetitors)
# 
#   2   4   8  10 
#  14  92 128  10 

as.numeric(baseball$RankPlayoffs == 1)
#   [1] 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0
#  [38] 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0
#  [75] 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
# [112] 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 1
# [149] 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0 1 0 0 1 0 0 0 0 0 0
# [186] 1 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 1 0
# [223] 1 0 0 0 0 0 0 1 1 0 0 1 1 0 1 0 0 1 1 0 1 0

baseball$RankPlayoffs == 1
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
# 
#   0   1 
# 197  47 

mod01=glm(WorldSeries ~ Year,data=baseball,family="binomial")
names(baseball)
#  [1] "Team"           "League"         "Year"           "RS"            
#  [5] "RA"             "W"              "OBP"            "SLG"           
#  [9] "BA"             "Playoffs"       "RankSeason"     "RankPlayoffs"  
# [13] "G"              "OOBP"           "OSLG"           "NumCompetitors"
# [17] "WorldSeries"   
summary(mod01)
mod1=glm(WorldSeries ~ Year,data=baseball,family="binomial")
mod2=glm(WorldSeries ~ RS,data=baseball,family="binomial")
mod3=glm(WorldSeries ~ RA,data=baseball,family="binomial")
mod4=glm(WorldSeries ~ W,data=baseball,family="binomial")
mod5=glm(WorldSeries ~ OBP,data=baseball,family="binomial")
mod6=glm(WorldSeries ~ SLG,data=baseball,family="binomial")
mod7=glm(WorldSeries ~ BA,data=baseball,family="binomial")
mod8=glm(WorldSeries ~ RankSeason,data=baseball,family="binomial")
mod9=glm(WorldSeries ~ OOBP,data=baseball,family="binomial")
mod10=glm(WorldSeries ~ OSLG,data=baseball,family="binomial")
mod11=glm(WorldSeries ~ NumCompetitors,data=baseball,family="binomial")
mod12=glm(WorldSeries ~ League,data=baseball,family="binomial")
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)
summary(mod8)
summary(mod9)
summary(mod10)
summary(mod11)
summary(mod12)
model=glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors,data=baseball,family="binomial")
summary(model)

cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])
#                     Year        RA RankSeason NumCompetitors
# Year           1.0000000 0.4762422  0.3852191      0.9139548
# RA             0.4762422 1.0000000  0.3991413      0.5136769
# RankSeason     0.3852191 0.3991413  1.0000000      0.4247393
# NumCompetitors 0.9139548 0.5136769  0.4247393      1.0000000

cols=names(baseball)
cols

mod20=glm(WorldSeries ~ Year + RA,data=baseball,family="binomial")
mod21=glm(WorldSeries ~ Year + RankSeason,data=baseball,family="binomial")
mod22=glm(WorldSeries ~ Year + NumCompetitors,data=baseball,family="binomial")
mod23=glm(WorldSeries ~ RankSeason + RA,data=baseball,family="binomial")
mod24=glm(WorldSeries ~ NumCompetitors + RA,data=baseball,family="binomial")
mod25=glm(WorldSeries ~ NumCompetitors + RankSeason,data=baseball,family="binomial")
#Year/RA
#Year/RankSeason
#Year/NumCompetitors
#RA/RankSeason
#RA/NumCompetitors
#RankSeason/NumCompetitors

#1,3,8,11
summary(mod20)
summary(mod21)
summary(mod22)
summary(mod23)
summary(mod24)
summary(mod25)
summary(mod1)
summary(mod3)
summary(mod8)
summary(mod11)