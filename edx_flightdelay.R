#### 1 LOAD LIBRARIES AND FUNCTIONS ####

# Load libraries
library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(anytime)
library(lubridate)
library(chron)
library(rvest)
library(kableExtra)
library(formatR)
library(rpart.plot)

# Define function to calculate ProbSpec or spread between true positive and false negative
# The function takes two arguments: the predicted values and the reference (true) values
calc_probspec <- function(pred, ref){
  
  # Create confusion matrix
  cm <- table(pred, ref)
  
  # Check if is a 5x5 matrix or 2x2
  if (dim(cm)[1] > 2){
    
    # If it is a 5x5 matrix, add categories b to e into one category
    cm_1 <- cm[1]
    cm_2 <- sum(cm[2:5])
    cm_3 <- sum(cm[6], cm[11], cm[16], cm[21])
    cm_4 <- sum(cm[6:25]) - cm_3
    
  } else  {
    
    # If it is a 2x2 matrix, then just use the four cells
    cm_1 <- cm[1]
    cm_2 <- cm[2]
    cm_3 <- cm[3]
    cm_4 <- cm[4]
  }
  
  # Calculate positive and negative prediction rates
  p_p <- (cm_2+cm_4)  # Total cases predicted 1
  p_n <- (cm_1+cm_3)  # Total cases predicted 0
  p_t <- p_p+p_n      # Total cases predicted overall
    
  # Calculate false negative probability
  p_fn <- cm_3/p_n
  
  # Calculate true positive probability
  p_tp <- cm_4/p_p
  
  # Calculate spread
  probsec <- p_tp - p_fn
  
  # Return spread, probabilities and distribution over classes
  return(c(probsec, p_fn, p_tp, p_n/p_t, p_p/p_t))
}

#### 2 IMPORT AND CLEAN RAW DATA ####

# Download data from
# https://www.transtats.bts.gov/DL_SelectFields.asp?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr

# Compile list of file paths
list_csv <- list.files(path = "rawdata/", pattern = "921995911_T_ONTIME_REPORTING_[0-9]{1,2}.csv", full.names = TRUE)

# Loop through files
for (i in seq_along(list_csv)){
  
  # Echo current file number and path
  print(str_c("Importing file ",i,": ",list_csv[i]))
  
  # Read current CSV file into temp df
  tmp_contents <- read.delim(list_csv[i], header = TRUE, sep = ",", quote = "")
  
  # Convert df to tibble
  tmp_contents <- tibble(tmp_contents)
  
  # If first file, create output tibble
  if (i == 1) {
    
    # Create output tibble
    flights <- tmp_contents
    
    # If second or later file
  } else {
    
    # Append rows to output tibble
    flights <- rbind(flights, tmp_contents)
  }
  
}

# Remove extra quotes
flights <- as.data.frame(sapply(flights, function(x) gsub("\"", "", x)))

# Remove empty column
flights <- flights %>% select(-X)

# Set column names
colnames(flights) <- c("DepYear", "DepMonth", "DepDay", "DepDate", "UniqueCarrier", "CarrierNo", "FlightNo", "Origin", "Dest", "PlanDepTime", "DepTime", "PlanArrTime", "ArrTime", "ArrDelay", "Cancelled", "Distance")

# Set column data types and transform values
flights$DepYear <- as.numeric(flights$DepYear)
flights$DepMonth <- as.numeric(flights$DepMonth)
flights$DepDay <- as.numeric(flights$DepDay)
flights$DepDate <- as.Date.character(flights$DepDate)
flights$PlanDepTime <- strftime(strptime(flights$PlanDepTime, format = "%H%M"), format = "%H:%M")
flights$DepTime <- strftime(strptime(flights$DepTime, format = "%H%M"), format = "%H:%M")
flights$PlanArrTime <- strftime(strptime(flights$PlanArrTime, format = "%H%M"), format = "%H:%M")
flights$ArrTime <- strftime(strptime(flights$ArrTime, format = "%H%M"), format = "%H:%M")
flights$ArrDelay <- as.numeric(flights$ArrDelay)
flights$Distance <- as.numeric(flights$Distance)

# Show NA values and consequently filter out rows where arrival delay is NA
colSums(is.na(flights))
flights <- flights %>% filter(!is.na(ArrDelay))
colSums(is.na(flights))

# Save data as clean csv
write_csv(flights, "rdata/flights.csv")

# Save table header for later use in report
flights_header <- head(flights)

# Clean up environment
rm(i)
rm(list_csv)
rm(tmp_contents)



#### 3 ENRICH DATA ####

# Add departure week number
flights <- flights %>% mutate(DepWeek = as.numeric(format(DepDate, "%V")))

# Translate month into season (1 = spring, 2 = summer, 3 = autumn, 4 = winter)
flights <- flights %>% mutate(DepSeason = as.numeric(ifelse(between(DepMonth,3,5), 1, ifelse(between(DepMonth,6,8), 2, ifelse(between(DepMonth,9,11), 3, ifelse(between(DepMonth,12,12), 4, ifelse(between(DepMonth,1,2), 4,5)))))))

# Add column with delayed flag: set delayed = 1 if arrival more than 15 minutes behind schedule
flights <- flights %>% mutate(Delayed = as.factor(ifelse(ArrDelay > 15, 1, 0)))

# Add column with delay category
flights <- flights %>% mutate(DelayCategory = as.factor(ifelse(between(ArrDelay,0,15), "a", ifelse(between(ArrDelay,16,30), "b", ifelse(between(ArrDelay, 31, 60), "c", ifelse(between(ArrDelay, 61, 120), "d", "e"))))))

# Change flight number to unique number by adding carrier code
flights <- flights %>% mutate(FlightNo = str_c(UniqueCarrier, FlightNo))

# Create column for route
flights <- flights %>% mutate(Route = str_c(Origin, Dest, sep = "-"))

# Format planned departure time into 1 hour time slots
flights$DepTimeslot <- as.character(format(round_date(strptime(flights$PlanDepTime, format = "%H:%M"), "60 minutes"), "%H:%M"))

# Create base list with 1 hour timeslots for use in EDA
  # Create sequence of numbers for each hour and padd to 4 digits
  timeslots <- sprintf("%04d", seq(0, 2400, 100))
  # Set in correct time format
  timeslots <- strftime(strptime(timeslots, format = "%H%M"), format = "%H:%M")
  # Change to tibble and set column name
  timeslots <- tibble(timeslots)
  colnames(timeslots) <- "DepTimeslot"

# Save data as clean and enriched csv
write_csv(flights, "rdata/flights_enriched.csv")



#### 4 SPLIT INTO DATASETS ####

# Filter only origin airpirt SFO
flights_sfo <- flights %>% filter(Origin == "SFO")
rm(flights)

# Save filtered dataset as CSV
write_csv(flights_sfo, "rdata/flights_sfo.csv")

# Create random 50% subset of data for further analysis
set.seed(1, sample.kind="Rounding")
use_flights_index <- createDataPartition(flights_sfo$Delayed, times = 1, p = 0.5, list = FALSE)
use_flights <- flights_sfo[use_flights_index,]

# Remove redundant variables
rm(use_flights_index)

# Split dataset randomly into (imbalanced) training and test datasets with 70/30 ratio
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(use_flights$Delayed, times = 1, p = 0.3, list = FALSE)
train <- use_flights[-test_index,]
test <- use_flights[test_index,]

# Remove redundant variables
rm(test_index)
rm(use_flights)

# Create balanced training set
  # Calculate how many rows we need to have of both delayed and not-delayed flights
  nrow_parts <- ceiling(nrow(train)/2)
  
  # Separate delayed and not-delayed flights in the SFO set
  flights_sfo_delayed <- flights_sfo %>% filter(Delayed == 1)
  flights_sfo_notdelayed <- flights_sfo %>% filter(Delayed == 0)
  
  # Calculate sampling probability for not-delayed flights
  p_forbalance <- nrow_parts/nrow(flights_sfo_notdelayed)
  
  # Take random sample of not-delayed flights equal to 50% of the target train set size
  set.seed(1, sample.kind="Rounding")
  use_flights_index <- createDataPartition(flights_sfo_notdelayed$Delayed, times = 1, p = p_forbalance, list = FALSE)
  use_flights_notdelayed <- flights_sfo_notdelayed[use_flights_index,]
  
  # Calculate sampling probability for delayed flights
  p_forbalance <- nrow_parts/nrow(flights_sfo_delayed)
  
  # Take random sample of not-delayed flights equal to 50% of the target train set size
  set.seed(1, sample.kind="Rounding")
  use_flights_index <- createDataPartition(flights_sfo_delayed$Delayed, times = 1, p = p_forbalance, list = FALSE)
  use_flights_delayed <- flights_sfo_delayed[use_flights_index,]
 
  # Combine equal parts delayed and non-delayed flights into one balanced set again 
  train_bal <- rbind(use_flights_notdelayed, use_flights_delayed)
  
  # Remove redundant variables
  rm(use_flights_index)
  rm(use_flights_delayed)
  rm(use_flights_notdelayed)
  rm(p_forbalance)
  rm(nrow_parts)
  rm(flights_sfo_delayed)
  rm(flights_sfo_notdelayed)

# Save filtered and split data for use in Rmarkdown file
save(flights_sfo, train, train_bal, test, flights_header, timeslots, file = "rdata/flights_split.RData")

# Load prepared data to skip previous code chunk
#load("rdata/flights_split.RData")

#### 5 DATA EXPLORATION ####

# EDA for number of flights

# Number of flights by week
flights_sfo %>% group_by(DepWeek) %>% summarize(n = n()) %>% ggplot(aes(DepWeek, n)) + geom_bar(stat = "identity") + scale_x_continuous(breaks = 1:52) + xlab("Week") + ylab("Number of flights")

# Number of flights by timeslot
flights_by_timeslot <- flights_sfo %>% group_by(DepTimeslot) %>% summarize(n = n())
left_join(timeslots, flights_by_timeslot) %>% ggplot(aes(DepTimeslot, n)) + geom_bar(stat = "identity") + xlab("Timeslot") + ylab("Number of flights")

# Number of flights by carrier
flights_sfo %>% group_by(UniqueCarrier) %>% summarize(n = n()) %>% ggplot(aes(reorder(UniqueCarrier, -n), n)) + geom_bar(stat = "identity") + xlab("Carrier") + ylab("Number of flights")

# Number of flights by destination
flights_sfo %>% group_by(Dest) %>% summarize(n = n()) %>% ggplot(aes(reorder(Dest, -n), n)) + geom_bar(stat = "identity") + scale_x_discrete(labels = NULL, breaks = NULL) + xlab("Destination airport") + ylab("Number of flights")

# Top 10 destinations (~44% of total flights)
flights_sfo %>% group_by(Dest) %>% summarize(n = n()) %>% filter(n > 5000) %>% mutate(pct = round(n/nrow(flights_sfo)*100,1)) %>% arrange(desc(n))

# Distribution of flight distance
flights_sfo %>% ggplot(aes(x = Distance)) + geom_histogram() + xlab("Flight distance (miles)") + ylab("Number of flights")

# 51% and 80% quantiles for flight distance
quantile(flights_sfo$Distance, c(0.51, 0.8))

# EDA for delay

# Delay percentage
flights_sfo %>% group_by(Delayed) %>% summarize(n = n()/nrow(flights_sfo)) %>% ggplot(aes(Delayed, n)) + geom_bar(stat = "identity") + xlab("Delayed") + ylab("Share of flights")

# Overall pdelayed
mean(as.numeric(flights_sfo$Delayed)-1)

# Overall mean and median delay duration
flights_sfo %>% filter(Delayed == 1) %>% select(ArrDelay) %>% summarize(mean = mean(ArrDelay), median = median(ArrDelay))

# Delay time histogram (only delayed flights in 5 min bins, maximized at 4 hours delay for readability)
  # Calculate number of delayed flights for use in cumulative curve and divide by y-axis limit
  num_delayed <- nrow(flights_sfo %>% filter(ArrDelay > 15))/5000
  # Plot histogram with secondary axis
  flights_sfo %>% filter(ArrDelay > 15) %>% ggplot(aes(ArrDelay)) + geom_histogram(stat = "bin", binwidth = 5) + xlim(1,240)
  flights_sfo %>% filter(ArrDelay > 15) %>% ggplot(aes(ArrDelay)) + geom_histogram(binwidth = 5) + ylim(0,5000) + stat_bin(aes(y=cumsum(..count..)/num_delayed),geom="line",color="blue") + scale_y_continuous(name = "Number of delayed flights", sec.axis = sec_axis(~./5000, name="Cumulative share of delayed flights")) + scale_x_continuous(name = "Delay time (minutes)", limits = c(1,240))

# Overall pdelayed by carrier
flights_sfo %>% group_by(UniqueCarrier, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% select(UniqueCarrier, pDelayed) %>% arrange() %>% ggplot(aes(reorder(UniqueCarrier, -pDelayed), pDelayed)) + geom_bar(stat = "identity") + xlab("Carrier") + ylab("Share of flights")

# Overall pdelayed by timeslot (hour)
delayed_by_timeslot <- flights_sfo %>% group_by(DepTimeslot, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% select(DepTimeslot, pDelayed) %>% arrange(DepTimeslot)
left_join(timeslots, delayed_by_timeslot) %>% ggplot(aes(DepTimeslot, pDelayed, group = 1)) + geom_line() + ylim(0,1) + xlab("Timeslot") + ylab("Share of flights delayed") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Overall pdelayed by week
flights_sfo %>% group_by(DepWeek, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% select(DepWeek, pDelayed) %>% arrange(DepWeek) %>% ggplot(aes(DepWeek, pDelayed, group = 1)) + geom_line() + ylim(0,1) + scale_x_continuous(breaks = 1:52) + geom_smooth() + xlab("Week") + ylab("Share of flights")

# Correlation between number of flights and pdelayed by week
  # Save summary of delay probability by week number
  pdelayed_by_week <- flights_sfo %>% group_by(DepWeek, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% select(DepWeek, pDelayed)
  
  # Save summary of number of flights by week number
  num_flight_by_week <- flights_sfo %>% group_by(DepWeek) %>% summarize(n = n())
  
  # Join delay probability and number of flights by week number
  num_flights_pdelayed_by_week <- inner_join(pdelayed_by_week, num_flight_by_week)
  
  # Calculate correlation coefficient for delay probability and number of flights
  cor.test(num_flights_pdelayed_by_week$pDelayed, num_flights_pdelayed_by_week$n)

# Boxplot with actual delay by carrier
flights_sfo %>% filter(Delayed == 1) %>% ggplot(aes(reorder(UniqueCarrier, ArrDelay, median), ArrDelay)) + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(flights_sfo$ArrDelay, c(0, 0.99))) + xlab("Carrier") + ylab("Delay (minutes)")

# Overall pdelayed by destination
flights_sfo %>% group_by(Dest, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% ggplot(aes(reorder(Dest, -pDelayed), pDelayed)) + geom_bar(stat = "identity") + ylim(0,1) + scale_x_discrete(labels = NULL, breaks = NULL) + xlab("Destination airport") + ylab("Share of flights delayed")

# Overall pdelayed by destination for top 30 destinations (~77,2% of flights)
# Create list of top 30 destinations by number of flights  
dest_top_30 <- flights_sfo %>% group_by(Dest) %>% summarize(n = n()) %>% top_n(30) %>% select(Dest) %>% unlist()

# Generate chart of pdelayed by top 30 destination
flights_sfo %>% group_by(Dest, Delayed) %>% summarize(n = n()) %>% mutate(pDelayed = n / sum(n)) %>% filter(Delayed == 1) %>% filter(Dest %in% dest_top_30) %>% ggplot(aes(reorder(Dest, -pDelayed), pDelayed)) + geom_bar(stat = "identity") + ylim(0,1) + xlab("Destination airport") + ylab("Share of flights delayed")

# Boxplot with actual delay by top 30 destination
flights_sfo %>% filter(Delayed == 1 & Dest %in% dest_top_30) %>% ggplot(aes(reorder(Dest, ArrDelay, median), ArrDelay)) + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(flights_sfo$ArrDelay, c(0, 0.99))) + xlab("Destination (top-30)") + ylab("Delay (minutes)") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot with distance by delay group
flights_sfo %>% ggplot(aes(Distance, Delayed)) + geom_boxplot() + xlab("Distance (miles)")

# Clean up redundant data
rm(pdelayed_by_week)
rm(num_flight_by_week)
rm(num_delayed)
rm(dest_top_30)
rm(num_flights_pdelayed_by_week)
rm(delayed_by_timeslot)
rm(flights_by_timeslot)

#### 6 MODEL DEVELOPMENT ####

# Define results table to show results
model_results <- tibble()

# Calculate overall pdelayed in imbalanced dataset
overall_delay_pct <- mean(train$Delayed == 1)

# Load data with trained models and predictions
load("rdata/model_fits_1.RData")
load("rdata/model_fits_2.RData")

## M1 - Logistic Regression model on delay yes/no

  # Fit models
  m1_fit <- glm(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot,family=binomial(link='logit'), data=train)
  m1_fit_bal <- glm(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot,family=binomial(link='logit'), data=train_bal)
  
  # Make prediction on test set (delay probability)
  m1_predict <- predict(m1_fit, test, type='response')
  m1_predict_bal <- predict(m1_fit_bal, test, type='response')
  
  # Plot predicted probability
  hist(m1_predict)
  hist(m1_predict_bal)
  
  # Transfer predicted probability into delay flag (yes/no)
  m1_predict_class <- as.factor(ifelse(m1_predict > overall_delay_pct,1,0)) # Using overall delay probability
  m1_predict_class_bal <- as.factor(ifelse(m1_predict_bal > 0.5,1,0))       # Using 0.5 probability because set is balanced with both classes 50%
  
  # Show model performance and save scores
  confusionMatrix(data=m1_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")
  confusionMatrix(data=m1_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")
  m1_f1 <- confusionMatrix(data=m1_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m1_bal_f1 <- confusionMatrix(data=m1_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m1_acc <- confusionMatrix(data=m1_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  m1_bal_acc <- confusionMatrix(data=m1_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  
  # Derrive top variables by importance
  m1_varimp <- varImp(m1_fit)
  m1_varimp$Variable <- row.names(m1_varimp)
  m1_varimp <- tibble(m1_varimp)
  m1_varimp <- m1_varimp %>% select(Variable, Overall) %>% arrange(desc(Overall))
   
  # Store scores in results table
  model_results <- bind_rows(model_results, tibble(Model="M1 - Logistic Regression on yes/no", Dataset="Imbalanced", F1 = m1_f1, Accuracy = m1_acc))
  model_results <- bind_rows(model_results, tibble(Model="M1 - Logistic Regression on yes/no", Dataset="Balanced", F1 = m1_bal_f1, Accuracy = m1_bal_acc))
  
## M2 - CART model on delay class

  # Fit model on imbalanced data
  m2_fit <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method = "rpart", data = train, trControl = trainControl(method = "cv", number = 10))
  m2_fit_bal <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method = "rpart", data = train_bal, trControl = trainControl(method = "cv", number = 10))
  
  # Make prediction on test set
  m2_predict <- predict(m2_fit, test, type='prob')
  m2_predict_bal <- predict(m2_fit_bal, test, type="prob")
  
  # Transfer predicted probability into delay flag (yes/no)
  m2_predict_class <- as.factor(ifelse(m2_predict$`1` > overall_delay_pct,1,0))   # Using overall delay probability
  m2_predict_class_bal <- as.factor(ifelse(m2_predict_bal$`1` > 0.5,1,0))         # Using 0.5 probability because set is balanced with both classes 50%
  
  # Show model performance and save scores
  confusionMatrix(m2_predict_class, test$Delayed, mode = "prec_recall", positive="1")
  confusionMatrix(m2_predict_class_bal, test$Delayed, mode = "prec_recall", positive="1")
  m2_f1 <- confusionMatrix(data=m2_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m2_bal_f1 <- confusionMatrix(m2_predict_class_bal, test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m2_acc <- confusionMatrix(data=m2_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  m2_bal_acc <- confusionMatrix(data=m2_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  
  # Plot resulting tree for imbalanced set
  plot(m2_fit$finalModel, uniform=TRUE, main="Classification Tree")
  text(m2_fit$finalModel, use.n.=TRUE, all=TRUE, cex=.7)
  
  # Plot resulting tree for balanced set
  plot(m2_fit_bal$finalModel, uniform=TRUE, main="Classification Tree")
  text(m2_fit_bal$finalModel, use.n.=TRUE, all=TRUE, cex=.7)
  
  # Store scores in results table
  model_results <- bind_rows(model_results, tibble(Model="M2 - Decision tree on yes/no", Dataset="Imbalanced", F1 = m2_f1, Accuracy = m2_acc))
  model_results <- bind_rows(model_results, tibble(Model="M2 - Decision tree on yes/no", Dataset="Balanced", F1 = m2_bal_f1, Accuracy = m2_bal_acc))
  
## M3 - Random forest on delay class

  # Start parallel computing track
  #library(doParallel)
  #cl <- makePSOCKcluster(2)
  #registerDoParallel(cl)
  
  # Fit models
  m3_fit <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method = "rf", data = train, tuneGrid = data.frame(mtry = 2))
  m3_fit_bal <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method = "rf", data = train_bal, tuneGrid = data.frame(mtry = 2))
  
  # Make prediction on test set
  m3_predict <- predict(m3_fit, test, type="prob")
  m3_predict_bal <- predict(m3_fit_bal, test, type="prob")
  
  # Transfer predicted probability into delay flag (yes/no)
  m3_predict_class <- as.factor(ifelse(m3_predict$`1` > overall_delay_pct,1,0))   # Using overall delay probability
  m3_predict_class_bal <- as.factor(ifelse(m3_predict_bal$`1` > 0.5,1,0))         # Using 0.5 probability because set is balanced with both classes 50%
  
  # Add missing levels
  levels(m3_predict_class) <- c("0", "1")
  levels(m3_predict_class_bal) <- c("0", "1")
  
  # Show model performance and save scores
  confusionMatrix(m3_predict_class, test$Delayed, mode = "prec_recall", positive="1")
  confusionMatrix(m3_predict_class_bal, test$Delayed, mode = "prec_recall", positive="1")
  m3_f1 <- confusionMatrix(data=m3_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m3_bal_f1 <- confusionMatrix(data=m3_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$byClass["F1"]
  m3_acc <- confusionMatrix(data=m3_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  m3_bal_acc <- confusionMatrix(data=m3_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  
  # Store scores in results table
  model_results <- bind_rows(model_results, tibble(Model="M3 - Random forest on yes/no", Dataset="Imbalanced", F1 = m3_f1, Accuracy = m3_acc))
  model_results <- bind_rows(model_results, tibble(Model="M3 - Random forest on yes/no", Dataset="Balanced", F1 = m3_bal_f1, Accuracy = m3_bal_acc))
  
  # Close parallel computing track
  #stopCluster(cl)

## M4 - KNN model on delay yes/no

  # Set parameters and fit model
  ctrl <- trainControl(method="repeatedcv",repeats = 1)
  m4_fit <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="knn", data=train, trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)
  m4_fit_bal <- train(Delayed ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="knn", data=train_bal, trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)
  
  # Make probability prediction on test set
  m4_predict <- predict(m4_fit, test, type='prob')
  m4_predict_bal <- predict(m4_fit_bal, test, type='prob')
  
  # Transform probability prediction to class prediction
  m4_predict_class <- as.factor(ifelse(m4_predict$`1` > overall_delay_pct,1,0))       # Using overall delay probability
  m4_predict_class_bal <- as.factor(ifelse(m4_predict_bal$`1` > 0.5,1,0))             # Using 0.5 probability because set is balanced with both classes 50%
  
  # Show model performance and save scores
  confusionMatrix(table(m4_predict_class, test$Delayed), mode = "prec_recall",positive="1")
  confusionMatrix(table(m4_predict_class_bal, test$Delayed), mode = "prec_recall",positive="1")
  m4_f1 <- confusionMatrix(table(m4_predict_class, test$Delayed), mode = "prec_recall", positive="1")$byClass["F1"]
  m4_bal_f1 <- confusionMatrix(table(m4_predict_class_bal, test$Delayed), mode = "prec_recall", positive="1")$byClass["F1"]
  m4_acc <- confusionMatrix(data=m4_predict_class, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  m4_bal_acc <- confusionMatrix(data=m4_predict_class_bal, reference=test$Delayed, mode = "prec_recall", positive="1")$overall["Accuracy"]
  
  # Store scores in results table
  model_results <- bind_rows(model_results, tibble(Model="M4 - k-Nearest neighbors on yes/no", Dataset="Imbalanced", F1 = m4_f1, Accuracy = m4_acc))
  model_results <- bind_rows(model_results, tibble(Model="M4 - k-Nearest neighbors on yes/no", Dataset="Balanced", F1 = m4_bal_f1, Accuracy = m4_bal_acc))
  
## M5 - PDA Regression model on delay category

  # Fit models
  control <- trainControl(method="cv", number=10, repeats=2, classProbs= TRUE, summaryFunction = multiClassSummary)
  m5_fit <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="pda", data=train, trControl = control)
  m5_fit_bal <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="pda", data=train_bal, trControl = control)
  
  # Make prediction on test set (delay probability)
  m5_predict <- predict(m5_fit, test, type='raw')
  m5_predict_bal <- predict(m5_fit_bal, test, type='raw')
  
  # Make sure all levels are present
  levels(m5_predict) <- c("a", "b", "c", "d", "e")
  levels(m5_predict_bal) <- c("a", "b", "c", "d", "e")
  
  # Show results and save accuracy
  confusionMatrix(table(m5_predict, test$DelayCategory), mode = "prec_recall")
  confusionMatrix(table(m5_predict_bal, test$DelayCategory), mode = "prec_recall")
  m5_acc <- confusionMatrix(table(m5_predict, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  m5_acc_bal <- confusionMatrix(table(m5_predict_bal, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  
  # Store accuracy value in results table
  model_results <- bind_rows(model_results, tibble(Model="M5 - PDA Regression on category (imbalanced)", Dataset="Imbalanced", Accuracy = m5_acc))
  model_results <- bind_rows(model_results, tibble(Model="M5 - PDA Regression on category (balanced)", Dataset="Balanced", Accuracy = m5_acc_bal))


## M6 - HDDA Regression model on delay category

  # Fit model on imbalanced training set
  m6_fit <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="hdda", data=train, trControl = control)
  m6_fit_bal <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="hdda", data=train_bal, trControl = control)
 
  # Make prediction on test set (delay probability)
  m6_predict <- predict(m6_fit, test, type='raw')
  m6_predict_bal <- predict(m6_fit_bal, test, type='raw')
  
  # Show results and save accuracy
  confusionMatrix(table(m6_predict, test$DelayCategory), mode = "prec_recall",)
  confusionMatrix(table(m6_predict_bal, test$DelayCategory), mode = "prec_recall",)
  m6_acc <- confusionMatrix(table(m6_predict, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  m6_acc_bal <- confusionMatrix(table(m6_predict_bal, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  
  # Store accuracy value in results table
  model_results <- bind_rows(model_results, tibble(Model="M6 - HDDA Regression on category (imbalanced)", Dataset="Imbalanced", Accuracy = m6_acc))
  model_results <- bind_rows(model_results, tibble(Model="M6 - HDDA Regression on category (balanced)", Dataset="Balanced", Accuracy = m6_acc_bal))

## M7 - KNN model on delay category

  # Fit model on imbalanced training set
  ctrl <- trainControl(method="repeatedcv",repeats = 1)
  m7_fit <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="knn", data=train, trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)
  m7_fit_bal <- train(DelayCategory ~ DepWeek + UniqueCarrier + Dest + DepTimeslot, method="knn", data=train_bal, trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)
  
  # Make prediction on test set (delay probability)
  m7_predict <- predict(m7_fit, test)
  m7_predict_bal <- predict(m7_fit_bal, test)
  
  # Show results and save accuracy
  confusionMatrix(table(m7_predict, test$DelayCategory), mode = "prec_recall")
  confusionMatrix(table(m7_predict_bal, test$DelayCategory), mode = "prec_recall")
  m7_acc <- confusionMatrix(table(m7_predict, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  m7_acc_bal <- confusionMatrix(table(m7_predict_bal, test$DelayCategory), mode = "prec_recall",)$overall["Accuracy"]
  
  # Store accuracy value in results table
  model_results <- bind_rows(model_results, tibble(Model="M7 - k-Nearest neighbors on category (imbalanced)", Dataset="Imbalanced", Accuracy = m7_acc))
  model_results <- bind_rows(model_results, tibble(Model="M7 - k-Nearest neighbors on category (balanced)", Dataset="Balanced", Accuracy = m7_acc_bal))
  
  # Show results table
  model_results

## ProbSpec model

  # Define results table to show results
  model_results_probspec <- tibble()
  
  m1_probspec <- calc_probspec(m1_predict_class, test$Delayed)
  m2_probspec <- calc_probspec(m2_predict_class, test$Delayed)
  m3_probspec <- calc_probspec(m3_predict_class, test$Delayed)
  m4_probspec <- calc_probspec(m4_predict_class, test$Delayed)
  m5_probspec <- calc_probspec(m5_predict, test$DelayCategory)
  m6_probspec <- calc_probspec(m6_predict, test$DelayCategory)
  m7_probspec <- calc_probspec(m7_predict, test$DelayCategory)
  m1_probspec_bal <- calc_probspec(m1_predict_class_bal, test$Delayed)
  m2_probspec_bal <- calc_probspec(m2_predict_class_bal, test$Delayed)
  m3_probspec_bal <- calc_probspec(m3_predict_class_bal, test$Delayed)
  m4_probspec_bal <- calc_probspec(m4_predict_class_bal, test$Delayed)
  m5_probspec_bal <- calc_probspec(m5_predict_bal, test$DelayCategory)
  m6_probspec_bal <- calc_probspec(m6_predict_bal, test$DelayCategory)
  m7_probspec_bal <- calc_probspec(m7_predict_bal, test$DelayCategory)
  
  # Calculate probspec and input into results table
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M1 - Logistic Regression on yes/no", Dataset="Imbalanced", ProbSpec = m1_probspec[1], FN = m1_probspec[2], TP = m1_probspec[3], Negatives = m1_probspec[4], Positives = m1_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M1 - Logistic Regression on yes/no", Dataset="Balanced", ProbSpec = m1_probspec_bal[1], FN = m1_probspec_bal[2], TP = m1_probspec_bal[3], Negatives = m1_probspec_bal[4], Positives = m1_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M2 - Decision tree on yes/no", Dataset="Imbalanced", ProbSpec = m2_probspec[1], FN = m2_probspec[2], TP = m2_probspec[3], Negatives = m2_probspec[4], Positives = m2_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M2 - Decision tree on yes/no", Dataset="Balanced", ProbSpec = m2_probspec_bal[1], FN = m2_probspec_bal[2], TP = m2_probspec_bal[3], Negatives = m2_probspec_bal[4], Positives = m2_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M3 - Random forest on yes/no", Dataset="Imbalanced", ProbSpec = m3_probspec[1], FN = m3_probspec[2], TP = m3_probspec[3], Negatives = m3_probspec[4], Positives = m3_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M3 - Random forest on yes/no", Dataset="Balanced", ProbSpec = m3_probspec_bal[1], FN = m3_probspec_bal[2], TP = m3_probspec_bal[3], Negatives = m3_probspec_bal[4], Positives = m3_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M4 - k-Nearest neighbors on yes/no", Dataset="Imbalanced", ProbSpec = m4_probspec[1], FN = m4_probspec[2], TP = m4_probspec[3], Negatives = m4_probspec[4], Positives = m4_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M4 - k-Nearest neighbors on yes/no", Dataset="Balanced", ProbSpec = m4_probspec_bal[1], FN = m4_probspec_bal[2], TP = m4_probspec_bal[3], Negatives = m4_probspec_bal[4], Positives = m4_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M5 - PDA Regression on category", Dataset="Imbalanced", ProbSpec = m5_probspec[1], FN = m5_probspec[2], TP = m5_probspec[3], Negatives = m5_probspec[4], Positives = m5_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M5 - PDA Regression on category", Dataset="Balanced", ProbSpec = m5_probspec_bal[1], FN = m5_probspec_bal[2], TP = m5_probspec_bal[3], Negatives = m5_probspec_bal[4], Positives = m5_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M6 - HDDA Regression on category", Dataset="Imbalanced", ProbSpec = m6_probspec[1], FN = m6_probspec[2], TP = m6_probspec[3], Negatives = m6_probspec[4], Positives = m6_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M6 - HDDA Regression on category", Dataset="Balanced", ProbSpec = m6_probspec_bal[1], FN = m6_probspec_bal[2], TP = m6_probspec_bal[3], Negatives = m6_probspec_bal[4], Positives = m6_probspec_bal[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M7 - k-Nearest neighbors on category", Dataset="Imbalanced", ProbSpec = m7_probspec[1], FN = m7_probspec[2], TP = m7_probspec[3], Negatives = m7_probspec[4], Positives = m7_probspec[5]))
  model_results_probspec <- bind_rows(model_results_probspec, tibble(Model="M7 - k-Nearest neighbors on category", Dataset="Balanced", ProbSpec = m7_probspec_bal[1], FN = m7_probspec_bal[2], TP = m7_probspec_bal[3], Negatives = m7_probspec_bal[4], Positives = m7_probspec_bal[5]))

  # Show rearranged results best to worst
  model_results_probspec %>% arrange(desc(ProbSpec))

# Save/load model fits and predictions due to large computing effort, in two different files given GitHub file size limit
save(m1_fit, m1_fit_bal, m1_predict, m1_predict_bal, m1_predict_class, m1_predict_class_bal, m2_fit, file = "rdata/model_fits_1.RData")
save(m2_fit_bal, m2_predict_class, m2_predict_class_bal, m3_fit, m3_fit_bal, m3_predict_class, m3_predict_class_bal, m4_fit, m4_fit_bal, m4_predict_class, m4_predict_class_bal, m5_fit, m5_fit_bal, m5_predict, m5_predict_bal, m6_fit, m6_fit_bal, m6_predict, m6_predict_bal, m7_fit, m7_fit_bal, m7_predict, m7_predict_bal, model_results, model_results_probspec, file = "rdata/model_fits_2.RData")
