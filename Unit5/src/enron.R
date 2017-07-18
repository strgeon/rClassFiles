# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data"

emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)

strwrap(emails$email[1])
#  [1] "North America's integrated electricity market requires cooperation on"                      
#  ... more deleted
emails$responsive[1]
# [1] 0

table(emails$responsive)
# 
#   0   1 
# 716 139 

library(tm)
corpus = VCorpus(VectorSource(emails$email))
corpus[[1]]$content
# [1] "North America's integrated electricity market requires cooperation on environmental policies Commission for Environmental Cooperation releases working paper on North America's electricity market Montreal, 27 November 2001 -- The North American Commission for Environmental Cooperation (CEC) is releasing a working paper highlighting the trend towards increasing trade, competition and cross-border investment in electricity between Canada, Mexico and the United States. It is hoped that the working paper, Environmental Challenges and Opportunities in the Evolving North American Electricity Market, will stimulate public discussion around a CEC symposium of the same title about the need to coordinate environmental policies trinationally as a North America-wide electricity market develops. The CEC symposium will take place in San Diego on 29-30 November, and will bring together leading experts from industry, academia, NGOs and the governments of Canada, Mexico and the United States to consider the impact of the evolving continental electricity market on human health and the environment. \"Our goal [with the working paper and the symposium] is to highlight key environmental issues that must be addressed as the electricity markets in North America become more and more integrated,\" said Janine Ferretti, executive director of the CEC. \"We want to stimulate discussion around the important policy questions being raised so that countries can cooperate in their approach to energy and the environment.\" The CEC, an international organization created under an environmental side agreement to NAFTA known as the North American Agreement on Environmental Cooperation, was established to address regional environmental concerns, help prevent potential trade and environmental conflicts, and promote the effective enforcement of environmental law. The CEC Secretariat believes that greater North American cooperation on environmental policies regarding the continental electricity market is necessary to: *  protect air quality and mitigate climate change, *  minimize the possibility of environment-based trade disputes, *  ensure a dependable supply of reasonably priced electricity across North America *  avoid creation of pollution havens, and *  ensure local and national environmental measures remain effective. The Changing Market The working paper profiles the rapid changing North American electricity market. For example, in 2001, the US is projected to export 13.1 thousand gigawatt-hours (GWh) of electricity to Canada and Mexico. By 2007, this number is projected to grow to 16.9 thousand GWh of electricity. \"Over the past few decades, the North American electricity market has developed into a complex array of cross-border transactions and relationships,\" said Phil Sharp, former US congressman and chairman of the CEC's Electricity Advisory Board. \"We need to achieve this new level of cooperation in our environmental approaches as well.\" The Environmental Profile of the Electricity Sector The electricity sector is the single largest source of nationally reported toxins in the United States and Canada and a large source in Mexico. In the US, the electricity sector emits approximately 25 percent of all NOx emissions, roughly 35 percent of all CO2 emissions, 25 percent of all mercury emissions and almost 70 percent of SO2 emissions. These emissions have a large impact on airsheds, watersheds and migratory species corridors that are often shared between the three North American countries. \"We want to discuss the possible outcomes from greater efforts to coordinate federal, state or provincial environmental laws and policies that relate to the electricity sector,\" said Ferretti. \"How can we develop more compatible environmental approaches to help make domestic environmental policies more effective?\" The Effects of an Integrated Electricity Market One key issue raised in the paper is the effect of market integration on the competitiveness of particular fuels such as coal, natural gas or renewables. Fuel choice largely determines environmental impacts from a specific facility, along with pollution control technologies, performance standards and regulations. The paper highlights other impacts of a highly competitive market as well. For example, concerns about so called \"pollution havens\" arise when significant differences in environmental laws or enforcement practices induce power companies to locate their operations in jurisdictions with lower standards. \"The CEC Secretariat is exploring what additional environmental policies will work in this restructured market and how these policies can be adapted to ensure that they enhance competitiveness and benefit the entire region,\" said Sharp. Because trade rules and policy measures directly influence the variables that drive a successfully integrated North American electricity market, the working paper also addresses fuel choice, technology, pollution control strategies and subsidies. The CEC will use the information gathered during the discussion period to develop a final report that will be submitted to the Council in early 2002. For more information or to view the live video webcast of the symposium, please go to: http://www.cec.org/electricity. You may download the working paper and other supporting documents from: http://www.cec.org/programs_projects/other_initiatives/electricity/docs.cfm?varlan=english. Commission for Environmental Cooperation 393, rue St-Jacques Ouest, Bureau 200 Montréal (Québec) Canada H2Y 1N9 Tel: (514) 350-4300; Fax: (514) 350-4314 E-mail: info@ccemtl.org ***********"

# Pre-process data
corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]$content

# Create matrix

dtm = DocumentTermMatrix(corpus)
dtm
# <<DocumentTermMatrix (documents: 855, terms: 22164)>>
# Non-/sparse entries: 102863/18847357
# Sparsity           : 99%
# Maximal term length: 156
# Weighting          : term frequency (tf)

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm

# <<DocumentTermMatrix (documents: 855, terms: 788)>>
# Non-/sparse entries: 51612/622128
# Sparsity           : 92%
# Maximal term length: 19
# Weighting          : term frequency (tf)

labeledTerms = as.data.frame(as.matrix(dtm))

# Add in the outcome variable
labeledTerms$responsive = emails$responsive

str(labeledTerms)


# Split the data

library(caTools)

set.seed(144)

spl = sample.split(labeledTerms$responsive, 0.7)

train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive~., data=train, method="class")

prp(emailCART)

# Make predictions on the test set

pred = predict(emailCART, newdata=test)
pred[1:10,]
#first column is non-responsive, second column is responsive
#            0          1
# 2  0.2156863 0.78431373
# 5  0.9557522 0.04424779
# 11 0.9557522 0.04424779
# 13 0.8125000 0.18750000
# 28 0.4000000 0.60000000
# 37 0.9557522 0.04424779
# 47 0.9557522 0.04424779
# 58 0.9557522 0.04424779
# 61 0.1250000 0.87500000
# 62 0.1250000 0.87500000

pred.prob = pred[,2]

# Compute accuracy

table(test$responsive, pred.prob >= 0.5)
#    
#     FALSE TRUE
#   0   195   20
#   1    17   25


(195+25)/(195+25+17+20)
# [1] 0.8560311

# Baseline model accuracy

table(test$responsive)
215/(215+42)
# 
#   0   1 
# 215  42 
# [1] 0.8365759

# ROC curve

library(ROCR)

predROCR = prediction(pred.prob, test$responsive)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values
# [[1]]
# [1] 0.7936323

