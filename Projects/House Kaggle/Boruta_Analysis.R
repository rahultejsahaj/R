#install.packages('pROC')
library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)

ROOT.DIR <- ".."

ID.VAR <- "Id"
TARGET.VAR <- "SalePrice"
sample.df <- read.csv(file="train.csv",stringsAsFactors = FALSE)

# extract only candidate feture names
candidate.features <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.type <- sapply(candidate.features,function(x){class(sample.df[[x]])})
table(data.type)
print(data.type)

# deterimine data types
explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[[x]])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.data.types) <- unique.classes

# pull out the response variable
response <- sample.df$SalePrice

# remove identifier and response variables
sample.df <- sample.df[candidate.features]

# for numeric set missing values to -1 for purposes of the random forest run
for (x in attr.data.types$integer){
  sample.df[[x]][is.na(sample.df[[x]])] <- -1
}

for (x in attr.data.types$character){
  sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"}
  
  
  set.seed(13)
  bor.results <- Boruta(sample.df,response,
                        maxRuns=101,
                        doTrace=0)
  
  print(bor.results)
  
  
  plot(bor.results, xlab = "", xaxt = "n")
   lz<-lapply(1:ncol(bor.results$ImpHistory),function(i)
     bor.results$ImpHistory[is.finite(bor.results$ImpHistory[,i]),i])
   names(lz) <- colnames(bor.results$ImpHistory)
   Labels <- sort(sapply(lz,median))
   axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(bor.results$ImpHistory), cex.axis = 0.7)
  
   # final.boruta <- TentativeRoughFix(boruta.train)
  #  print(final.boruta)
   
   getSelectedAttributes(bor.results, withTentative = F)
   
   boruta.df <- attStats(bor.results)
    class(boruta.df)
    print(boruta.df)
   
   #d$colm<-ifelse(is.na(d$colm), NO, d$colm)
  #names(traindata) <- gsub("_", "", names(traindata))
    
