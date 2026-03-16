## COSC 6520
## Project 1 - Milwaukee County EMS Data

################################################################################
## Part 1 - Data managment
## Part 1a - Dataset merging ###################################################
## Clean working environment
rm(list=ls())

## Import the 4 data sets
library(readr)
library(lubridate)
library(ggplot2)
library(sf)
library(tigris)
library(dplyr)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(ranger)
library(doParallel)
library(tidyr)

## using multiple cores
cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)

## EMS Calls for Service
## col_types used to make zip codes into a text value not a numeric
EMSCall <- read_csv("2023MKEEMSCallsForService.csv",
                    col_types = cols(ZIP_Code = col_character()))

## Fire Incident Detail
FireIncident <- read_csv("2023MKEFireIncidentDetail.csv",
                            col_types = cols(ZIP_Code = col_character()))

## Fixing timezone before anything else
FireIncident$Date_Received <- as.POSIXct(
  format(FireIncident$Date_Received, tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)
FireIncident$Date_Routed <- as.POSIXct(
  format(FireIncident$Date_Routed, tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)
FireIncident$Date_Dispatched <- as.POSIXct(
  format(as.POSIXct(FireIncident$Date_Dispatched, 
                    format = "%Y-%m-%d %H:%M:%OS", 
                    tz = "UTC"), 
         tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)
FireIncident$Date_Enroute <- as.POSIXct(
  format(as.POSIXct(FireIncident$Date_Enroute, 
                    format = "%Y-%m-%d %H:%M:%OS", 
                    tz = "UTC"), 
         tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)
FireIncident$Date_Arrived <- as.POSIXct(
  format(as.POSIXct(FireIncident$Date_Arrived, 
                    format = "%Y-%m-%d %H:%M:%OS", 
                    tz = "UTC"), 
         tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)
FireIncident$Date_Cleared <- as.POSIXct(
  format(FireIncident$Date_Cleared, tz = "UTC", usetz = FALSE),
  tz = "America/Chicago"
)

## Fire Unit Status
FireUnitPt1 <- read_csv("2023MKEFireUnitStatusPt1.csv")
FireUnitPt2 <- read_csv("2023MKEFireUnitStatusPt2.csv")

## Stacking each part into one since they share the exact same columns
FireUnit <- rbind(FireUnitPt1, FireUnitPt2)

## Merging FireUnit and FireIncident by case numbers
FireData <- merge(FireIncident, FireUnit, by = "Case_Number", all.x = TRUE)

## Checking that date columns are in the same format
class(FireData$Date_Received)
class(EMSCall$Incident_Date)

## Extracting date from FireData
FireData$Incident_Date <- as.Date(FireData$Date_Received)

## Confirm
class(FireData$Incident_Date)

## Final merge
FullData <- merge(FireData, EMSCall, by = c("Incident_Date", "ZIP_Code"), all.x = TRUE)

## Too large (rows = 16460329)

## Need to summarize data but also make categories for severity in types of calls
EMSCall$Severity <- ifelse(
  grepl("[0-9][A-D][0-9]", EMSCall$Original_Call_for_Service_Type),
  gsub(".*[0-9]([A-D])[0-9].*", "\\1", EMSCall$Original_Call_for_Service_Type),
  NA
)
unique(EMSCall$Severity)

## All unique call types are categorized into the 4 different letter codes
## in accordance with the medical priority dispatch system
table(EMSCall$Severity, useNA = "always")

## Labeling NA calls as "other" since they are still different types of calls
## just not ones that can be labeled in the traditional sense
EMSCall$Severity[is.na(EMSCall$Severity)] <- "Other"

## Summary of the EMS Call dataset after the modifications
EMSCallSummary <- aggregate(
  cbind(EMS_Total_Calls = rep(1, nrow(EMSCall)),
        EMS_High_Severity = EMSCall$Severity %in% c("C", "D"),
        EMS_Escalated = EMSCall$Original_Call_for_Service_Type != EMSCall$Final_Call_for_Service_Type),
  by = list(Incident_Date = EMSCall$Incident_Date,
            ZIP_Code = EMSCall$ZIP_Code),
  FUN = sum
)

## Now FullData can actually be created
FullData <- merge(FireData, EMSCallSummary, 
                  by = c("Incident_Date", "ZIP_Code"), 
                  all.x = TRUE)

## Part 1b - Data Cleaning #####################################################

## checking which columns have missing data
colSums(is.na(FullData))
## looking at the spread of calls throughout Milwaukee County
table(FullData$Municipality)
## distribution of the variables
summary(FullData)

## Because the Apt column has 664093 it will be dropped since it is not useful
FullData$Apt <- NULL

## Removing the 32 rows where case number is missing
FullData <- FullData[!is.na(FullData$Case_Number),]

## Keeping rows that only show Milwaukee municipality since other municipalities 
## that are in the data set are circumstances in which MFD responds to external calls
## we only want to focus on Milwaukee 
FullData <- FullData[FullData$Municipality == "MILWAUKEE",]

## Counting number of rows latitude and longitude columns are reading as 0 which
## is impossible for Milwaukee
sum(FullData$Latitude == 0 | FullData$Longitude == 0)

## Removing the 43 rows
FullData <- FullData[FullData$Latitude != 0 & FullData$Longitude != 0, ]

## Replacing EMS NA values with 0 since these missing values just mean no calls
## occured that day within the specific zip code
FullData$EMS_Total_Calls[is.na(FullData$EMS_Total_Calls)] <- 0
FullData$EMS_High_Severity[is.na(FullData$EMS_High_Severity)] <- 0
FullData$EMS_Escalated[is.na(FullData$EMS_Escalated)] <- 0

## Removing rows with no unit record
FullData <- FullData[!is.na(FullData$Unit_ID), ]

## Removing rows with missing arrival or enroute timestamps since they are likely
## cancelled trips
FullData <- FullData[!is.na(FullData$Date_Arrived) & !is.na(FullData$Date_Enroute), ]

## Calculating time variables
FullData$Response_Time <- as.numeric(difftime(FullData$Date_Arrived, 
                                              FullData$Date_Received, 
                                              units = "secs"))
FullData$Travel_Time <- as.numeric(difftime(FullData$Date_Arrived, 
                                            FullData$Date_Enroute, 
                                            units = "secs"))
FullData$Dispatch_Time <- as.numeric(difftime(FullData$Date_Dispatched, 
                                              FullData$Date_Received, 
                                              units = "secs"))
FullData$Turnout_Time <- as.numeric(difftime(FullData$Date_Enroute, 
                                             FullData$Date_Dispatched, 
                                             units = "secs"))

## Binary outcome variable - 1 = missed standard, 0 = met standard
FullData$Missed_Standard <- ifelse(
  (FullData$Turnout_Time + FullData$Travel_Time) > 320, 1, 0)

summary(FullData$Travel_Time)
summary(FullData$Response_Time)
summary(FullData$Dispatch_Time)
summary(FullData$Turnout_Time)

## Negative time values are showing
sum(FullData$Travel_Time < 0)
sum(FullData$Response_Time < 0)
sum(FullData$Dispatch_Time < 0)

## Removing since its impossible to arrive before you are dispatched
FullData <- FullData[FullData$Travel_Time >= 0 & 
                       FullData$Response_Time >= 0 & 
                       FullData$Dispatch_Time >= 0, ]

## Since there are also high travel times of about a week these will be filtered
## out
sum(FullData$Travel_Time > 7200)
sum(FullData$Response_Time > 7200)

FullData <- FullData[FullData$Response_Time <= 7200, ]
FullData <- FullData[FullData$Turnout_Time >= 0 & 
                       FullData$Turnout_Time <= 600, ]

## Final check
summary(FullData$Travel_Time)
summary(FullData$Response_Time)
summary(FullData$Dispatch_Time)
summary(FullData$Turnout_Time)
table(FullData$Missed_Standard)

## Storing time features in different colummns for easier use in analysis
FullData$Hour <- hour(FullData$Date_Received)
FullData$Day_of_Week <- wday(FullData$Date_Received, label = TRUE)
FullData$Month <- month(FullData$Date_Received, label = TRUE)

################################################################################
## Part 2
## Part 2a - Exploratory Data Analysis #########################################

## Call volume by zip codes
CallsByZip <- aggregate(Case_Number ~ ZIP_Code, 
                        data = FullData, 
                        FUN = length)
names(CallsByZip)[2] <- "Total_Calls"
CallsByZip <- CallsByZip[order(-CallsByZip$Total_Calls), ]
print(CallsByZip)
ggplot(CallsByZip, aes(x = reorder(ZIP_Code, -Total_Calls), y = Total_Calls)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "MFD Call Volume by ZIP Code",
       x = "ZIP Code",
       y = "Total Calls") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Call volume by hour
CallsByHour <- aggregate(Case_Number ~ Hour, 
                         data = FullData, 
                         FUN = length)
names(CallsByHour)[2] <- "Total_Calls"
print(CallsByHour)
ggplot(CallsByHour, aes(x = Hour, y = Total_Calls)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "MFD Call Volume by Hour of Day",
       x = "Hour of Day",
       y = "Total Calls") +
  theme_minimal()

## Call volume by month
CallsByMonth <- aggregate(Case_Number ~ Month, 
                          data = FullData, 
                          FUN = length)
names(CallsByMonth)[2] <- "Total_Calls"
print(CallsByMonth)
ggplot(CallsByMonth, aes(x = Month, y = Total_Calls)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "MFD Call Volume by Month",
       x = "Month",
       y = "Total Calls") +
  theme_minimal()

## Heatmap of Milwaukee by zipcode by call volume
WIZips <- zctas(state = "WI", year = 2010)
MKEZips <- filter(WIZips, ZCTA5CE10 %in% FullData$ZIP_Code)
MKEZips <- left_join(MKEZips, CallsByZip,
                     by = c("ZCTA5CE10" = "ZIP_Code"))

ggplot(MKEZips) +
  geom_sf(aes(fill = Total_Calls)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      name = "Total Calls") +
  labs(title = "MFD Call Volume by ZIP Code - Milwaukee 2023") +
  theme_minimal()

## Missed response standard rate 
ComplianceByZip <- aggregate(Missed_Standard ~ ZIP_Code,
                             data = FullData,
                             FUN = mean)
ComplianceByZip$Pct_Missed <- round(ComplianceByZip$Missed_Standard * 100, 1)

MKEZipsCompliance <- left_join(MKEZips, ComplianceByZip,
                               by = c("ZCTA5CE10" = "ZIP_Code"))

ggplot(MKEZipsCompliance) +
  geom_sf(aes(fill = Pct_Missed)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      name = "% Missed") +
  labs(title = "NFPA 1710 Standard Miss Rate by ZIP Code") +
  theme_minimal()

## Overall compliance rate
OverallCompliance <- mean(FullData$Missed_Standard == 0) * 100
cat("Percentage meeting NFPA 1710 standard:", round(OverallCompliance, 1), "%\n")
cat("Percentage missing NFPA 1710 standard:", round(100 - OverallCompliance, 1), "%\n")

## Average response time componenets
cat("Median Dispatch Time (seconds):", median(FullData$Dispatch_Time), "\n")
cat("Median Travel Time (seconds):", median(FullData$Travel_Time), "\n")
cat("Median Total Response Time (seconds):", median(FullData$Response_Time), "\n")

## Compliance by hour
ComplianceByHour <- aggregate(Missed_Standard ~ Hour,
                              data = FullData,
                              FUN = mean)
ComplianceByHour$Pct_Missed <- round(ComplianceByHour$Missed_Standard * 100, 1)
print(ComplianceByHour)

ggplot(ComplianceByHour, aes(x = Hour, y = Pct_Missed)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "black") +
  annotate("text", x = 1, y = 11.5, label = "NFPA 1710 Target (10%)", size = 3) +
  labs(title = "NFPA 1710 Miss Rate by Hour of Day - Milwaukee 2023",
       x = "Hour of Day",
       y = "% Incidents Missing Standard") +
  theme_minimal()

## Complicance by day of the week
ComplianceByDay <- aggregate(Missed_Standard ~ Day_of_Week,
                             data = FullData,
                             FUN = mean)
ComplianceByDay$Pct_Missed <- round(ComplianceByDay$Missed_Standard * 100, 1)

ggplot(ComplianceByDay, aes(x = Day_of_Week, y = Pct_Missed)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "black") +
  annotate("text", x = 1.5, y = 11.5, label = "NFPA 1710 Target (10%)", size = 3) +
  labs(title = "NFPA 1710 Miss Rate by Day of Week - Milwaukee 2023",
       x = "Day of Week",
       y = "% Incidents Missing Standard") +
  theme_minimal()

## Response time distribution
ggplot(FullData, aes(x = Travel_Time / 60)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkred", size = 1) +
  annotate("text", x = 5.5, y = 80000, label = "NFPA 1710\n4 Min Standard", 
           color = "darkred", size = 3) +
  labs(title = "Distribution of Travel Times - Milwaukee 2023",
       x = "Travel Time (Minutes)",
       y = "Number of Incidents") +
  xlim(0, 20) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

################################################################################
## Part 3 - Analysis 
## Part 3a - Performance Evaluation ############################################

## Data partitioning 
set.seed(1)
TrainIndex <- createDataPartition(FullData$Missed_Standard, p = 0.6, list = FALSE)
TrainData <- FullData[TrainIndex, ]
ValidData <- FullData[-TrainIndex, ]

## Part 3b - PCA ###############################################################

## PCA subset created 
SubPCA <- FullData[, c("Hour", "Dispatch_Time", "Turnout_Time",
                       "EMS_Total_Calls", "EMS_High_Severity",
                       "EMS_Escalated")]

cor(SubPCA)
## scaling
PCAScaled <- scale(SubPCA)

## running the PCA
PCA <- prcomp(PCAScaled, center = TRUE, scale. = TRUE)
summary(PCA)
PCA
PCA$sdev^2

## Scree plot
ScreeData <- data.frame(
  Component = 1:6,
  Eigenvalue = PCA$sdev^2
)

ggplot(ScreeData, aes(x = Component, y = Eigenvalue)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
  annotate("text", x = 5, y = 1.15, label = "Kaiser Criterion (1.0)",
           color = "darkred", size = 3) +
  labs(title = "Scree Plot - Incident Level PCA",
       x = "Principal Component",
       y = "Eigenvalue") +
  scale_x_continuous(breaks = 1:6) +
  theme_minimal()

## Part 3c - Logistic Regression ###############################################

## Leaving out EMS_High_Severity since it was the only model that showed high 
## correlation with EMS_Total_Calls
TrainData$Missed_Standard <- as.factor(TrainData$Missed_Standard)
ValidData$Missed_Standard <- as.factor(ValidData$Missed_Standard)

LogModel <- glm(Missed_Standard ~ Hour + Day_of_Week + Month +
                    ZIP_Code + Dispatch_Time + Turnout_Time +
                    EMS_Total_Calls + EMS_Escalated,
                  data = TrainData,
                  family = binomial)
summary(LogModel)

## Part 3d - Decision Trees ####################################################

## Converting to factors
TrainData$Missed_Standard <- as.factor(TrainData$Missed_Standard)
ValidData$Missed_Standard <- as.factor(ValidData$Missed_Standard)

# Full tree
set.seed(1)
FullTree <- rpart(Missed_Standard ~ Hour + Day_of_Week + Month + 
                    ZIP_Code + EMS_Total_Calls + 
                    EMS_High_Severity + EMS_Escalated,
                  data = TrainData,
                  method = "class",
                  cp = 0,
                  minsplit = 2,
                  minbucket = 1)
printcp(FullTree) |> as.data.frame() |> tail(20)

## since xerror starts falling off between 180-190 will use cp of 0.00003
## Pruned tree
set.seed(1)
PrunedTree <- rpart(Missed_Standard ~ Hour + Day_of_Week + Month + 
                      ZIP_Code + EMS_Total_Calls + 
                      EMS_High_Severity + EMS_Escalated,
                    data = TrainData,
                    method = "class",
                    cp = 0.001)
prp(PrunedTree,
    type = 1,
    extra = 1,
    under = TRUE)

## Checking for overfitting
printcp(PrunedTree)

## Confusion matrix
PredictedClass <- predict(PrunedTree, ValidData, type = "class")
confusionMatrix(PredictedClass, ValidData$Missed_Standard, positive = "1")

## Adjusting cutoff since only 23.7% of incidents are missing the standard
PredictedProb <- predict(PrunedTree, ValidData, type = "prob")
confusionMatrix(as.factor(ifelse(PredictedProb[,2] > 0.237, "1", "0")),
                ValidData$Missed_Standard, positive = "1")
## Sacrifice in accuracy but significant increase in sensitivity
## Model seems weak for this usecase since we need a lot higher sensitivity

## Gains table
ValidData$Missed_Standard <- as.numeric(as.character(ValidData$Missed_Standard))
GainsTable <- gains(ValidData$Missed_Standard, PredictedProb[,2])
GainsTable

## cumulative lift chart
plot(c(0, GainsTable$cume.pct.of.total * sum(ValidData$Missed_Standard)) ~ 
       c(0, GainsTable$cume.obs),
     xlab = "Number of Cases",
     ylab = "Cumulative Missed Standards Identified",
     main = "Cumulative Lift Chart - NFPA 1710 Miss Prediction",
     type = "l")
lines(c(0, sum(ValidData$Missed_Standard)) ~ c(0, dim(ValidData)[1]),
      col = "red",
      lty = 2)
legend("topleft", 
       legend = c("Model", "Random Selection"),
       col = c("black", "red"),
       lty = c(1, 2))

## DW Lift chart
barplot(GainsTable$mean.resp / mean(ValidData$Missed_Standard),
        names.arg = GainsTable$depth,
        xlab = "Percentile",
        ylab = "Lift",
        main = "Decile-Wise Lift Chart - NFPA 1710 Miss Prediction",
        col = "steelblue")
abline(h = 1, lty = 2, col = "red")

## ROC
RocObject <- roc(ValidData$Missed_Standard, PredictedProb[,2])
plot.roc(RocObject,
         main = "ROC Curve - NFPA 1710 Miss Prediction",
         col = "steelblue",
         print.auc = TRUE)

## Part 3d - Random Forest #####################################################

set.seed(1)
## using ranger instead of randomforest to take advantage of the multiple cores 
RandForest <- ranger(Missed_Standard ~ Hour + Day_of_Week + Month + 
                       ZIP_Code + EMS_Total_Calls + 
                       EMS_High_Severity + EMS_Escalated,
                     data = TrainData,
                     num.trees = 100,
                     mtry = 3,
                     importance = "permutation",
                     probability = TRUE,
                     num.threads = parallel::detectCores())
RandForest

## Importance plot
ImportanceDF <- data.frame(
  Variable = names(importance(RandForest)),
  Importance = importance(RandForest)
)
ggplot(ImportanceDF, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance - Random Forest",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()

## Confusion matrix
ValidData$Missed_Standard <- as.factor(ValidData$Missed_Standard)
RandForestProb <- predict(RandForest, ValidData)$predictions
RandForestClass <- as.factor(ifelse(RandForestProb[,2] > 0.266, "1", "0"))
confusionMatrix(RandForestClass, ValidData$Missed_Standard, positive = "1")

## Gains table
ValidData$Missed_Standard <- as.numeric(as.character(ValidData$Missed_Standard))
RFGainsTable <- gains(ValidData$Missed_Standard, RandForestProb[,2])
RFGainsTable

## Lift chart
plot(c(0, RFGainsTable$cume.pct.of.total * sum(ValidData$Missed_Standard)) ~ 
       c(0, RFGainsTable$cume.obs),
     xlab = "Number of Cases",
     ylab = "Cumulative Missed Standards Identified",
     main = "Cumulative Lift Chart - Random Forest",
     type = "l")
lines(c(0, sum(ValidData$Missed_Standard)) ~ c(0, dim(ValidData)[1]),
      col = "red",
      lty = 2)
legend("topleft",
       legend = c("Model", "Random Selection"),
       col = c("black", "red"),
       lty = c(1, 2))

## DW lift chart
barplot(RFGainsTable$mean.resp / mean(ValidData$Missed_Standard),
        names.arg = RFGainsTable$depth,
        xlab = "Percentile",
        ylab = "Lift",
        main = "Decile-Wise Lift Chart - Random Forest",
        col = "steelblue")
abline(h = 1, lty = 2, col = "red")

## ROC curve
ValidData$Missed_Standard <- as.factor(ValidData$Missed_Standard)
RFRocObject <- roc(as.numeric(as.character(ValidData$Missed_Standard)), 
                   RandForestProb[,2])
plot.roc(RFRocObject,
         main = "ROC Curve - Random Forest",
         col = "steelblue",
         print.auc = TRUE,
         ylim = c(0, 1))







