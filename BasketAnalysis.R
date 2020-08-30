#Basket Analysis
#Inspired by: https://www.datacamp.com/community/tutorials/market-basket-analysis-r
Packages <- c("arules", "arulesViz", "tidyverse", "readr", "knitr", 
              "ggplot2", "lubridate", "plyr", "dplyr")
#install.packages(Packages)
lapply(Packages, library, character.only=TRUE)

#InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country
retail <- read.csv('C:/Users/rasmu/OneDrive/Skrivebord/Github/Data/Online_retail')
retail <- retail[complete.cases(retail), ]
retail = retail %>% mutate(Description = as.factor(Description), Country = as.factor(Country), 
                 Date =  as.Date(retail$InvoiceDate), TransTime = format(retail$InvoiceDate,"%H:%M:%S"),
                 InvoiceNo = as.numeric(as.character(retail$InvoiceNo)))

transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(retail)paste(retail$Description,
                                            collapse = ","))
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
write.csv(transactionData,"C:/Users/rasmu/OneDrive/Skrivebord/Github/Data/OnlineRetailtr.csv", 
          quote = FALSE, row.names = FALSE)

tr <- read.transactions('C:/Users/rasmu/OneDrive/Skrivebord/Github/Data/OnlineRetailtr.csv', 
                        format = 'basket', sep=',')

summary(tr)

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
                         
windows()
par(mfrow=c(2, 1))                 
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
inspect(association.rules[1:10])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules]
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
inspect(head(metal.association.rules))

subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
windows()
par(mfrow=c(2, 1))  
plot(subRules, jitter=0)
plot(subRules,method="two-key plot", jitter=0)

windows()
par(mfrow=c(2, 1))  
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")
subRules2<-head(subRules, n=20, by="lift")
windows()
plot(subRules2, method="paracoord")
                         
