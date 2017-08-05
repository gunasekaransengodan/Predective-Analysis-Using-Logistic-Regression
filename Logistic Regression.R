#WorkingDirectry Path Setting

getwd()
setwd("<File Path>") #OR Select Files Using Browse code
getwd()

#Read given csv file for analysys

goodForU <- read.csv("<File Path:snacks manufacturer survey Data>", header=T, stringsAsFactors = T)

#Quick View of data using some functions in R to know some basic details

str(goodForU)
dim(goodForU)
head(goodForU)
tail(goodForU)
names(goodForU)
summary(goodForU) #(data frame of 61 variable and all are int)

#Install some commonly required package and load the librarys 

library(dplyr) #Work with Data Frame and carry out some common manipulation
#linear Model or simple regression model function useage
library(gains) # Gains Chart, Gains tables and lift chart for prediction algorithms
library(irr) #Kappa matrix; Model Accuracy algorithm 
library(ROCR) #ROCR Curve; Cutoff Parameterized Performance Curve design
library(caret) #Confusion Matrix: Classification and Regression Training 

#1. Data preparation and exploration steps
#Quick Check Data to identify the column name and purpose description
View(goodForU)

#Prepare for Brand A

goodForUBrandA <- select(goodForU,X2,X9,X16,X23,X30)

#Quick Check Brand A

View(goodForUBrandA)
summary(goodForUBrandA)

#Prepare for meaningful Columns

colnames(goodForUBrandA) <- c("Farm_grown_ingredients_Chips","zero_grams_transfat_Chips","Natural_oil_Used_Chips","10Good_1Bad_Rate_Chips","10Minimally_1Heavily_processed_Chips")


#Meaningful Binary valued column and rate(0,1) Creation
#X23 Brand A chips : Rate the following 10=good for you, 1=bad for you

#Version1 : Create a Column to show Good bad target Identification with Rate >5(6 to 10) from goodForUBrandA
goodForUBrandA_Cleaned <- goodForUBrandA%>%mutate('10Good_1Bad_Rate_Chips_TargetRate'=ifelse(goodForUBrandA$'10Good_1Bad_Rate_Chips'>5,1,0))


#X30 Brand A chips : 10=minimallyProcessed / 1=Heavily processed on a 10 point scale
#Version2: Add similary one more Column to show Minimally or Heavily processed identification with Rate <= (5 to 1) to goodForUBrandA_Cleaned

goodForUBrandA_Cleaned <- goodForUBrandA_Cleaned%>%mutate('10Minimally_1Heavily_processed_Chips_Rate'= ifelse(goodForUBrandA$'10Minimally_1Heavily_processed_Chips'<=5,"Heavily Processed","Minimally Processed"))

#Quick New Columns values Check

View(goodForUBrandA_Cleaned)
names(goodForUBrandA_Cleaned)

#Check Anamolys and Fix If any using desired Method

colSums(is.na(goodForUBrandA_Cleaned))

#Make sure Original Data backed Up
#Unused Parent Data Removed Because we created meaningful Derived Columns for Further Process
goodForUBrandA_Final <- goodForUBrandA_Cleaned[, -c(4,5)]

#Have a Look Final Data after Cleaning

View(goodForUBrandA_Final)
names(goodForUBrandA_Final)
summary(goodForUBrandA_Final)

#Split the entire data into training and validation sample Data Set

set.seed(200)
index <- sample(nrow(goodForUBrandA_Final),0.70*nrow(goodForUBrandA_Final), replace = F)
training <- goodForUBrandA_Final[index,]
testing <- goodForUBrandA_Final[-index,]

#Check the Good Bad rate of the Splitted Data Set for Traning and Test using New Column '10Good_1Bad_Rate_Chips_TargetRate'

table(goodForUBrandA_Final$`10Good_1Bad_Rate_Chips_TargetRate`) / nrow(goodForUBrandA_Final)
table(training$`10Good_1Bad_Rate_Chips_TargetRate`) / nrow(training)
table(testing$`10Good_1Bad_Rate_Chips_TargetRate`) / nrow(testing)

#All 3 Looks Same almost
#                       0(Bad)      1(Good)    

#Sample                 0.7460811   0.2539189
#Tranining              0.7472599   0.2527401
#validation/Testing     0.743331    0.256669

#View Dataset

View(training)
View(testing)

#Build Model; Target as Dependent and Others are Independent variable
#Build a Logistic Regression Model
#log(p/1-p) = -1.083 + (-0.370) * X2 + (-0.418) * X9 + (-0.418) * X16 + (-0.686) * X30

logRegModel <- glm(data = training,training$`10Good_1Bad_Rate_Chips_TargetRate`~.,family = "binomial")
summary(logRegModel)

#The Summary shows that all variables are highly significant. 
#So we can Finalize this model as Our Target Model for Testing/Validate to do analysys

#Validation of Model
#Predicted values

predictedvalues <- predict(logRegModel, type="response",newdata = testing)
head(predictedvalues)
tail(predictedvalues)

#Check the rate of 1, According to that the cut-off probability value will be set.

table(training$`10Good_1Bad_Rate_Chips_TargetRate` / nrow(training))

#The predicted cut-off value is 0.25473315
#So, We can assume anything with probability of 1 greated than 0.25473315 is 1 and other is 0
#1 is Good 0 is Bad
#Apply cutoff Value to predicted value to label

predictedvaluesCutoff <- ifelse(predictedvalues >= 0.25473315,1,0)

#Now we can run Kappa & confusion Matrix

kappa2(data.frame(testing$`10Good_1Bad_Rate_Chips_TargetRate`,predictedvaluesCutoff))
#Kappa = 0.391 (Classification Performance)

confusionMatrix(predictedvaluesCutoff,testing$`10Good_1Bad_Rate_Chips_TargetRate`,positive="1")
# Accuracy : 0.761 - 76%

# Prediction               Correct            InCorrect
# 0    1
# 0 4432  783 - Non Events 4432               783 
# 1  946 1074 - Events     1074               946

#The Model can be optimized further using differnt cut-off, for maximum accuracy 
#we should select the cut-off with maximum kappa matrix

sequenceValue <- seq(0.25,1,0.01)
n<-1
accuracy <- as.vector(length(sequenceValue))

for(i in sequenceValue)
{
  print(i)
  predictedvaluesCutoff <- ifelse(predictedvalues > i,1,0)
  accuracy[n] <- confusionMatrix(predictedvaluesCutoff,testing$`10Good_1Bad_Rate_Chips_TargetRate`,positive = "1")$overall[2]
  print(n)
  n=n+1
}

#accuracy has differnt kappa matrix for differnt cut-off
#Extract cut-off with maximum kappa using 'which function"

index <- which(accuracy==max(accuracy))

#cutoff for maximum kappa

accuracy[index]   #is [1] 0.3841868 0.3912095

#We can proceed Further with the model Cutoff value 0.3912095; Because we can getting Max accuracy with this value
#maximum Accuracy calculation

maximumAccuracyPrediction <- ifelse(predictedvalues > 0.3841868,1,0) #As > First in Max is 0.3912095
confusionMatrix(maximumAccuracyPrediction,testing$`10Good_1Bad_Rate_Chips_TargetRate`,positive = "1")

#Maximum Accuracy is (Kappa used for optimized Accuracy is  0.3912095)

#0.7657  - 76%   

#Confidence Interval
#95% CI : (0.7558, 0.7754)

#Check Confidence Interval with Method

confint.default(logRegModel) #getAnywhere(confint.default) (Code USed)

#95% CI obtained from confint.default() is based on Asymptotic narmality

#ROCR Curve 

performanceCurvePrediction <- prediction(predictedvalues,testing$`10Good_1Bad_Rate_Chips_TargetRate`)
performanceCurve <- performance(performanceCurvePrediction, "tpr","fpr") #True False +ve rate
plot(performanceCurve,col="red")

abline(0,1,lty=8,col="grey")
rocrAccuracy <- performance(performanceCurvePrediction,"auc")
rocrAccuracy <- unlist(slot(rocrAccuracy,"y.values"))
rocrAccuracy

#The Accuracy is 0.7527641; More than 0.50, So It tell that MODEL is Very Good.

#Prepare the Gain Chart

testing$`10Good_1Bad_Rate_Chips_TargetRate` <- as.numeric(testing$`10Good_1Bad_Rate_Chips_TargetRate`)
head(testing$`10Good_1Bad_Rate_Chips_TargetRate`)

#Top 30% PROBABILITY of customers who thinks BRAND A is Good Lies here 
#(70 to 100%)
#0.25473315 0.42184542 0.54699576 0.64218205 

testing$PredictionProbability <- predict(logRegModel,type = "response",newdat=testing)
quantile(testing$PredictionProbability,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

gains(testing$`10Good_1Bad_Rate_Chips_TargetRate`, predict(logRegModel,type="response",newdata=testing), groups=5)

#Depth                           Cume   Cume Pct                    Mean
#of           Cume     Mean      Mean   of Total   Lift    Cume     Model
#File     N      N     Resp      Resp      Resp    Index   Lift     Score
#-------------------------------------------------------------------------
#25  1808   1808      0.55      0.55      53.1%     212    212      0.54
#40  1088   2896      0.29      0.45      70.1%     113    175      0.27
#61  1509   4405      0.17      0.35      83.9%      66    138      0.18
#86  1809   6214      0.11      0.28      94.9%      44    111      0.12
#100 1021   7235      0.09      0.26     100.0%      36    100      0.09

##Gain Chart tells that top 40% of the Probabilities contacin 70% of customers 
##Likes and believes that "Brand A" Chips are Good

#Factor Loop
#nameList <- names(goodForUBrandA_Final)
#nameList <- nameList[-4]
#goodForUBrandA_Final[,nameList] <- lapply(goodForUBrandA_Final[,nameList],factor)
#summary(goodForUBrandA_Final)
#str(goodForUBrandA_Final)

#glm(logRegModel) #0.33596
#Standardized COefficients shows 3 times impact in BRNAD A with data in `10Minimally_1Heavily_processed_Chips_Rate`Minimally Processed

##
## Determine overall brand perception with data what's there is Customers MIND
##

## Prediction 1: Are my brands made with farm grown ingredients like potato, corn or wheat?
Q1 <- goodForUBrandA_Final%>%group_by(Farm_grown_ingredients_Chips)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Final))%>%ungroup()%>%data.frame()
str(Q1)

#Answer : Close to 80% Customers believe BRAND A Chips are made with farm grown ingredients like potato, corn or wheat.

#Answerd 1
goodForUBrandA_Farmgrowningredients1 <- goodForUBrandA%>%mutate('Farm_grown_ingredients_Chips_TargetRate'=ifelse(goodForUBrandA$'Farm_grown_ingredients_Chips'==1,1,0))
goodForUBrandA_Farmgrowningredients1 <- goodForUBrandA_Farmgrowningredients1%>%group_by(`Farm_grown_ingredients_Chips_TargetRate`)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Farmgrowningredients1))%>%ungroup()%>%data.frame()
str(goodForUBrandA_Farmgrowningredients1)


## Prediction 2: Do my brands have zero grams trans-fat?

Q2 <- goodForUBrandA_Final%>%group_by(zero_grams_transfat_Chips)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Final))%>%ungroup()%>%data.frame()
str(Q2)

#Answer : Close to 32% Customers believe BRAND A Chips have zero grams trans-fat.

## Prediction 3: Are my brands made with natural oils?

Q3 <- goodForUBrandA_Final%>%group_by(Natural_oil_Used_Chips)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Final))%>%ungroup()%>%data.frame()
str(Q3)

#Answer : Close to 44% Customers believe BRAND A Chips are made with natural oils.

#Answerd 1
goodForUBrandA_NaturaloilUsed1 <- goodForUBrandA%>%mutate('Natural_oil_Used_Chips_TargetRate'=ifelse(goodForUBrandA$'Natural_oil_Used_Chips'==1,1,0))
goodForUBrandA_NaturaloilUsed1 <- goodForUBrandA_NaturaloilUsed1%>%group_by(`Natural_oil_Used_Chips_TargetRate`)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_NaturaloilUsed1))%>%ungroup()%>%data.frame()
str(goodForUBrandA_NaturaloilUsed1)


## Prediction 4: Is there an impact due to Processing Level?

#4 or Less Check

goodForUBrandA_4OrLessOverall <- goodForUBrandA%>%mutate('10Good_1Bad_Rate_Chips_TargetRate'=ifelse(goodForUBrandA$'10Good_1Bad_Rate_Chips'<5,1,0))
targetRate_4OrLessOverall <- goodForUBrandA_4OrLessOverall%>%group_by(`10Good_1Bad_Rate_Chips_TargetRate`)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_4OrLessOverall))%>%ungroup()%>%data.frame()
str(targetRate_4OrLessOverall)

#50% Overall perception is 4 Or Less

#50% (5 and Above)and above Perception finding

Q41TargetRate <- goodForUBrandA_Final%>%group_by(`10Good_1Bad_Rate_Chips_TargetRate`)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Final))%>%ungroup()%>%data.frame()
Q42processedRate <- goodForUBrandA_Final%>%group_by(`10Minimally_1Heavily_processed_Chips_Rate`)%>%summarise(Count=n(),Percent_Count = n()/count(goodForUBrandA_Final))%>%ungroup()%>%data.frame()

observationGoodBadprocessed <- with(goodForUBrandA_Final,table(`10Good_1Bad_Rate_Chips_TargetRate`,`10Minimally_1Heavily_processed_Chips_Rate`) / nrow(goodForUBrandA_Final))
observationGoodBadprocessed
View(observationGoodBadprocessed)
str(Q41TargetRate)
str(Q42processedRate)
#10Minimally_1Heavily_processed_Chips_Rate
#10Good_1Bad_Rate_Chips_TargetRate      Heavily Processed     Minimally Processed
# 0            0.6172348 (62%)          0.1288463(12%)
# 1            0.1117193 (11%)          0.1421996(14%)

#Answer : 

#100% in different Persective
## 62% Customers believe Brand A Chips are Heavily Processed(0) and is Bad(0) for Them
## 11% Customers believe Brand A Chips are Minimally Processed(1) and is Good(1) for Them 
## 13% Customers believe Brand A Chips are Minimally Processed(1) Even they think this is bad(0) for Them   
## 14% Customers believe Brand A Chips are Heavily Processed(0) Even they think this is good(1) for Them  

#Answerd 1 Count of Customers Believe that Brand A is using farm Grown Ingredients With zero grams transfat

farmGrownIngredientsWithzero_grams_transfat <- goodForUBrandA%>%mutate('farmGrownIngredientsWithzero_grams_transfat_TargetRate'=ifelse(goodForUBrandA$'Farm_grown_ingredients_Chips'==1 & goodForUBrandA$'zero_grams_transfat_Chips'==1,1,0))
farmGrownIngredientsWithzero_grams_transfat <- farmGrownIngredientsWithzero_grams_transfat%>%group_by(`farmGrownIngredientsWithzero_grams_transfat_TargetRate`)%>%summarise(Count=n(),Percent_Count = n()/count(farmGrownIngredientsWithzero_grams_transfat))%>%ungroup()%>%data.frame()
str(farmGrownIngredientsWithzero_grams_transfat)
View(farmGrownIngredientsWithzero_grams_transfat)

#Sample Question & Prediction Result   
#S1. Do you Believe Brand A chips are made with farm grown ingredients like potato, corn or wheat?

#Refer Prediction 1 Above

#Answer 'YES' Because in the total 80% Believes. Belief Accuracy us More than significant level. 

#S2. Do you believe that Brand A chips are good for you? Please rate on a 10 point scale. with 10 being good and 1 being bad

#Rating: 3

#Refer Prediction 2, Prediction 3 Above

#Customer of brand A believes only 32% of that have zero grams trans-fat(It will increase LDL(bad Cholesterol level) and 
#Decrase HDL(Good Cholesterol level) also this is Higher Risk of Developing Type 2 Diabetes) and
#44% only Believes Brand A Chips made by natural oils, 62% Customers believe 
#Brand A Chips are Heavily Processed(0) and is Bad(0) for Them 

#With above data observation I dont believe strongly say that Brand A chips are good for Me.

#S3. Do you believe manufacture X(Which makes BRAND A potato chips) is environmentally responsible? Please rate on a 10 point scale. with 10 being good and 1 being bad

brandAEnvironmentResponse <- select(goodForU,X38)
colnames(brandAEnvironmentResponse) <- c("EnvironmentResponse_factor_Rate")
brandAEnvironmentResponse_Summary <- brandAEnvironmentResponse%>%mutate('EnvironmentResponse_factor_TargetRate'=ifelse(brandAEnvironmentResponse$'EnvironmentResponse_factor_Rate'>5,1,0))

S3 <- brandAEnvironmentResponse_Summary%>%group_by(EnvironmentResponse_factor_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(brandAEnvironmentResponse_Summary))%>%ungroup()%>%data.frame()
str(S3)

#Rating: 4 #Not Environmentally Responsible (< 50%)
# Reason  : 45 % of customer only who think that Environmentally Responsible in their purchase and product used exprience with Environmental knowledge. 

#I dont think manufacture X(Which makes BRAND A potato chips) is environmentally responsible.
#Reason : 62% Customers believe #Brand A Chips are Heavily Processed(0)! 
#How does Heavily Processed food will affect environment!!
#Food Miles specifically affect environment by how they are related to emissions of Greenhouse gases. Each step in the food industry
#Food Production, Processing, transportation, processing, storage, distribution,marketing has some impact on environment. 
#Heavily processed have more impact practically!!

#S4. Is price an important factor in your purchase decision?  Please rate on a 10 point scale. with 10 being good and 1 being bad

brandAPurchaseDecisionPrice <- select(goodForU,X48)
colnames(brandAPurchaseDecisionPrice) <- c("PurchaseDecision_factor_Price")
brandAPurchaseDecisionPrice_Summary <- brandAPurchaseDecisionPrice%>%mutate('PurchaseDecision_factor_Price_TargetRate'=ifelse(brandAPurchaseDecisionPrice$'PurchaseDecision_factor_Price'>=5,1,0))

S4 <- brandAPurchaseDecisionPrice_Summary%>%group_by(PurchaseDecision_factor_Price_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(brandAPurchaseDecisionPrice_Summary))%>%ungroup()%>%data.frame()
str(S4)

#Rating:  3, Not Important (<50%)

# Reason  : 32 % of customer only who think that price an important factor slightly(=5) in their purchase decision.

#Insights and recommendations observed from the model output:

#Most of the  Customers believe Brand A Chips are Heavily Processed, not having zero grams trans-fat, not made with natural oils
#Apart from this product manufacturing process not environmentally responsible. All these factors are making negative impact of giving goodbye to sue the snacks.
#But customers are saying money is not finalizing their purchase decision. If manufacturer giving negative impacted product because of manufacturing cost reason

#Recommends: Manufacturer can happily increase cost of product after fullfilling above bad reason components to good reason component by adding right steps in process.

#Just a thought to Share(Relevance): In Tirupathi one laddu preparing cost is 35Rs But still their Selling cost is 25Rs, 
#People will buy even it cost 50Rs or above, But More than 100Rs is bad Idea to loose sales Revenue
#(other than Die hard fans/foodies(Considering best tasty food) may not buy more counts like today), 
#Even it is unique of its kind and famous Or Prasadam(Prople cant affort)



