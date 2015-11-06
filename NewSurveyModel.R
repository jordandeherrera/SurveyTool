#Load maturity model values and Qlist
load(file="maturity.model.values.Rdata")
Qlist <- read.csv("survey/Qlist.csv")
load("~/survey/survey.results.Rdata")

#Load maturity model values and Qlist
load(file="maturity.model.values.Rdata")

#Get category names
QCategory <- Qlist[1:nrow(Qlist),1:2]

#Get responses
QResponse <- unique(Qlist[,1])

#Create a copy of presults
pcategories <- presults[,1:ncol(presults)]

#Rename columns after categories instead of questions
colnames(pcategories) <- QCategory[,2]

#Create loop to replace all values
#For loop for columns is letter i
for (i in 4:ncol(pcategories)){
  #For loop for rows is letter j
  for (j in 1:nrow(pcategories)){
    
    pcategories[j,i] <- MaturityModelValues[MaturityModelValues$Category==pcategories[j,i],2]
  }}

#Create unique sector, market cap, and category lists for loop
unique.sectors <- unique(pcategories[,2])

unique.marketcap <- unique(pcategories[,3])

unique.categories <- unique(colnames(pcategories[,4:ncol(pcategories)]))

## Market Cap Calculation
#Create a score data frame
pcategories.score.marketcap <- data.frame(MarketCap=unique.marketcap, Category=sort(rep(unique.categories, length(unique.marketcap))), Score=0)

#Replace pcategories score with means from survey results
for (i in 1:length(unique.marketcap)){
  
  for (j in 1:length(unique.categories)){
    
    tmp.vector <- as.vector(as.numeric(pcategories[pcategories[,3]==unique.marketcap[i],colnames(pcategories)==unique.categories[j]]))
    
    pcategories.score.marketcap[pcategories.score.marketcap$MarketCap==unique.marketcap[i] & pcategories.score.marketcap$Category==unique.categories[j],3] <- round(mean(tmp.vector[!is.na(tmp.vector)]),2)
  }}

pcategories.score.marketcap <- pcategories.score.marketcap[complete.cases(pcategories.score.marketcap),]	