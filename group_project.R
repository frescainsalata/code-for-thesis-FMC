## Group assignment Programming with R - 23-12-2020

## Josse Wannet - u153748
## Martine Hoevenberg - u1273819
## Rick van Hamond - u1247805
## Francesco Capuani - u456665

library(dplyr) 
library(ggplot2)
library(tidyr) 
library(caret)
## library for plotting decision trees nicely
library(rattle)
options(width = 80) 
## not a fan of scientific notation, so chaning it to show lots of 0.000's
options(scipen = 999)

## loading the data
model_data <- read.delim("input/hotel_bookings.csv", stringsAsFactors = TRUE, 
                         sep = ",")

## Preprocessing of the data
model_data <- model_data %>%
  na.omit(model_data) %>%   
  ## filtering out invalid values
  filter(adults > 0 & adults < 10) %>% 
  filter(adr < 2000 & adr > 0) %>% 
  filter(!(babies == 10 | babies == 9 | children == 10)) %>%
  ## this variable is already incorporated in lead time and the reservation 
  ## status is directly correlated with cancellation.
  select(-reservation_status_date, -reservation_status) %>%
  ## changing some variables to factor
  mutate(arrival_date_year = as.factor(arrival_date_year)) %>%
  mutate(is_canceled = as.factor(is_canceled)) %>%
  mutate(is_repeated_guest = as.factor(is_repeated_guest))

## renaming the factor levels for legibility
levels(model_data$is_canceled) <- list(Cancelled = 1, Not_Cancelled = 0)
levels(model_data$is_repeated_guest) <- list(Not_repeated = 0, repeated = 1)

## EDA -------------------------------------------------------------------------

## Figure 1. small table of hotel type, number of bookings, 
##           number of cancellations. See final document.

## Figure 2. Arrivals per Month
ggplot(model_data, aes(x = is_canceled, fill = is_canceled)) + 
  geom_bar() + 
  scale_x_discrete(name = "Canceled", breaks = c("0", "1"), 
                   labels = c("No", "Yes")) +
  labs(title = "Arrivals per Month", x = "Month", y = "Number of Bookings") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Canceled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) +
  ## nicely splitting the barplot per month
  facet_grid(. ~ factor(arrival_date_month, 
                        levels = c("January", "February", "March", "April", 
                                   "May", "June", "July", "August", "September", 
                                   "October", "November", "December")))

## Figure 3. Cancellations for Repeated vs First Time Guests
## this form of data manipulation is used to be able to transform the plots
## into proportion plots. Also used at figure 5, 6, 7. 
ggp1 <- model_data %>% 
  count(is_repeated_guest, is_canceled)

ggplot(ggp1, aes(x= is_repeated_guest, y = n, fill = is_canceled)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5)) +
  labs(title = "Cancellations for (non) Repeated Guests",
       x = "Repeated Guest", y = "Proportion") + 
  theme(legend.position = "right") +
  scale_fill_manual(name = "Canceled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) 

## Figure 4. Number of Days Between Booking and Arrival
ggplot(model_data, aes(x = lead_time, fill = factor(is_canceled)) ) +
  geom_histogram(binwidth = 20, position = "dodge") +
  labs(title = "Number of Days Between Booking Confirmation and Arrival Date",
       x = "Number of Days Between Booking and Arrival", 
       y = "Number of Bookings") +
  theme(legend.position = 'bottom') +
  scale_fill_manual(name = "Cancelled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) 

## Figure 5. Bookings with/without Children

## grouping the children variables into 0 and 1 or more for a nicer plot
simplified_bookings <- model_data %>%
  mutate(children = case_when(children == 0 ~ "None",
                              children >= 1 ~ "1 or more") %>%
           factor(levels = c("None", "1 or more")))

ggp4 <- simplified_bookings %>%
  count(children, is_canceled)

ggplot(ggp4, aes(x = children, y = n, fill = is_canceled)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  labs(title = "Cancellations for Bookings with/without Children") +
  scale_x_discrete("Number of Children") +
  scale_y_continuous("Proportion") +
  scale_fill_manual(name = "Canceled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) 

## Figure 6. Guests who did/did not Canceled Bookings Prior to the Current Booking

## again grouping variable into 0 and 1 or more for nicer plot
simplified_cancellations <- model_data %>%
  mutate(
    previous_cancellations = case_when(previous_cancellations == 0 ~ "None",
                                       previous_cancellations >= 1 ~ "1 or more") %>%
      factor(levels = c("None", "1 or more"))) 

ggp2 <- simplified_cancellations %>%
  count(previous_cancellations, is_canceled)

ggplot(ggp2, aes(x = previous_cancellations, y = n, fill = is_canceled)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = n),position = position_fill(vjust = 0.5)) +
  labs(title = "Guests who did/did not Cancel Previous Bookings") +
  scale_x_discrete("Number of Previous Cancellations") +
  scale_y_continuous("Proportion") +
  scale_fill_manual(name = "Canceled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) 

## Figure 7. Changes made in Bookings

## again grouping of the variables for a more descriptive plot
simplified_bookings <- model_data %>%
  mutate(booking_changes = case_when(booking_changes == 0 ~ "no booking changes",
                                     booking_changes >= 1 ~ "1 or more changes after booking") %>%
           factor(levels = c("no booking changes", "1 or more changes after booking")))

ggp3 <- simplified_bookings %>%
  count(booking_changes, is_canceled)

ggplot(ggp3, aes(x = booking_changes, y = n, fill = is_canceled)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  labs(title = "Changes in Booking") +
  scale_x_discrete("Number of Booking Changes") +
  scale_y_continuous("Proportion") +
  scale_fill_manual(name = "Canceled",
                    values = c("red", "green"), 
                    labels = c("Yes", "No")) 


## Modelling -------------------------------------------------------------------

## we now should split up the data into a training (70%) and testing (30%) subset.
## we will distribute the cancellation variable evenly to make sure the training
## and testing goes well. 
## also because the original data is so big, we will sample 20% of it to ease
## further modeling
set.seed(14)
cancel_frac <- sample_frac(model_data, size = 0.2, replace = F)

set.seed(14)
## the hotel variable had to be distributed evenly as well, otherwise the 
## indexing at the next step would result in one of the two hotel forms not
## to be included in the training set, resulting in failure of modelling
trn_index <- createDataPartition(y = c(cancel_frac$is_canceled, cancel_frac$hotel)
                                 , p = 0.70, list = F)
train_cancel <- cancel_frac[trn_index, ]
test_cancel <- cancel_frac[-trn_index, ]

## training first KNN model on our three selected variables
set.seed(14)
knn_three <- train(is_canceled ~ lead_time + previous_cancellations + 
                     booking_changes,
                   data = train_cancel,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   preProcess = c("center", "scale"),
                   na.action = na.omit
                   )
knn_three
## running the model on the test set
set.seed(14)
predict_knn_three <- predict(knn_three, test_cancel)
## creating confusion matrix
knn_three_confM <- confusionMatrix(predict_knn_three, 
                                   as.factor(test_cancel$is_canceled))
knn_three_confM

## KNN model with 6 variables based on the EDA
set.seed(14)
knn_six <- train(is_canceled ~ arrival_date_month + hotel + children +
                   booking_changes + previous_cancellations + lead_time,
                 data = train_cancel,
                 method = "knn",
                 trControl = trainControl(method = "cv", number = 5),
                 preProcess = c("center", "scale"),
                 na.action = na.omit
                 )
knn_six
## running on test set and creating confusion matrix
set.seed(14)
predict_knn_six <- predict(knn_six, test_cancel)
## creating confusion matrix
knn_six_confM <- confusionMatrix(predict_knn_six, 
                                   as.factor(test_cancel$is_canceled))
knn_six_confM

## KNN model with all data set variables with removal of a few variables
## to reduce running time drastically
set.seed(14)
knn_all <- suppressWarnings(train(is_canceled ~ . -country -company 
                                    -agent -arrival_date_year,
                                  data = train_cancel,
                                  method = "knn",
                                  trControl = trainControl(method = "cv", 
                                                           number = 5),
                                  preProcess = c("center", "scale"),
                                  na.action = na.omit
                                  ))
knn_all
## test set and confusion matrix
set.seed(14)
predict_knn_all <- predict(knn_all, test_cancel)
## creating confusion matrix
knn_all_confM <- confusionMatrix(predict_knn_all, 
                                 as.factor(test_cancel$is_canceled))
knn_all_confM

## training logistic reggression models using the same variables
## 3 variables
set.seed(14)
log_three <- train(is_canceled ~ lead_time + previous_cancellations + 
                     booking_changes,
                   data = train_cancel,
                   method = "glm",
                   family = binomial(link = "logit"),
                   trControl = trainControl(method = "cv", number = 5),
                   preProcess = c("center", "scale"),
                   na.action = na.omit
                   )
log_three
## test set and confusion matrix
set.seed(14)
predict_log_three <- predict(log_three, test_cancel)
log_three_confM <- confusionMatrix(predict_log_three, 
                                   as.factor(test_cancel$is_canceled))
log_three_confM

## log model with 6 variables
set.seed(14)
log_six <- train(is_canceled ~ arrival_date_month + hotel + children +
                   booking_changes + previous_cancellations + lead_time,
                 data = train_cancel,
                 method = "glm",
                 family = binomial(link = "logit"),
                 trControl = trainControl(method = "cv", number = 5),
                 preProcess = c("center", "scale"),
                 na.action = na.omit
                 )
log_six
## test set and confusion matrix
set.seed(14)
predict_log_six <- predict(log_six, test_cancel)
log_six_confM <- confusionMatrix(predict_log_six, 
                                   as.factor(test_cancel$is_canceled))
log_six_confM

## log model with all variables

## CAREFUL: THIS MODEL TAKES A WHILE TO RUN
set.seed(14)
log_all <- suppressWarnings(train(is_canceled ~ . -country -company 
                                    -agent -arrival_date_year,
                                  data = train_cancel,
                                  method = "glm",
                                  family = binomial(link = "logit"),
                                  trControl = trainControl(method = "cv", 
                                                           number = 5),
                                  preProcess = c("center", "scale"),
                                  na.action = na.omit
                                  ))
log_all
## test set and confusion matrix
set.seed(14)
predict_log_all <- predict(log_all, test_cancel)
log_all_confM <- confusionMatrix(predict_log_all, 
                                 as.factor(test_cancel$is_canceled))
log_all_confM

## Decision Tree Model with our selected variables
## Model with the 3 variables
set.seed(14)
dtree_three <- train(is_canceled ~ lead_time + previous_cancellations + 
                       booking_changes,
                     data = train_cancel,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 5),
                     na.action = na.omit
                     )
dtree_three
## plotting the decision tree: Figure 11
fancyRpartPlot(dtree_three$finalModel, sub = "", palettes = c("Reds", "Greens"))

## testing for the 3 var decision tree + confusion matrix
set.seed(14)
predict_dtree_three <- predict(dtree_three, test_cancel)
dtree_three_confM <- confusionMatrix(predict_dtree_three, 
                                     as.factor(test_cancel$is_canceled))
dtree_three_confM

## decision tree with 6 variables
set.seed(14)
dtree_six <- train(is_canceled ~ arrival_date_month + hotel + children +
                     booking_changes + previous_cancellations + lead_time,
                   data = train_cancel,
                   method = "rpart",
                   trControl = trainControl(method = "cv", number = 5),
                   na.action = na.omit
                   )
dtree_six
## plotting the decision tree: Figure 13
fancyRpartPlot(dtree_six$finalModel, sub = "", palettes = c("Reds", "Greens"))


## testing 6 var decision tree + confusion matrix
set.seed(14)
predict_dtree_six <- predict(dtree_six, test_cancel)
dtree_six_confM <- confusionMatrix(predict_dtree_six,
                                   as.factor(test_cancel$is_canceled))
dtree_six_confM


## decision tree with all variables but same removals as for knn_all and 
## log_all
set.seed(14)
dtree_all <- train(is_canceled ~ . -country -company -agent -arrival_date_year,
                   data = train_cancel,
                   method = "rpart",
                   trControl = trainControl(method = "cv", number = 5),
                   na.action = na.omit
                   )

dtree_all
## plotting the decision tree: Figure 15
fancyRpartPlot(dtree_all$finalModel, sub = "", palettes = c("Reds", "Greens"))

## testing the augmented all var decision tree model + confusion matrix
set.seed(14)
predict_dtree_all <- predict(dtree_all, test_cancel)
dtree_all_confM <- confusionMatrix(predict_dtree_all,
                                   as.factor(test_cancel$is_canceled))
dtree_all_confM

## From here on out we will extract the confusion matrix tables ($table)
## and other values of the confM, such as F1 / specifity / etc ($byClass)

## Appendix B:
## table 2
knn_three_confM$table
knn_three_confM$byClass
## table 3
knn_six_confM$table
knn_six_confM$byClass
## table 4
knn_all_confM$table
knn_all_confM$byClass
## table 6
log_three_confM$table
log_three_confM$byClass
## table 7
log_six_confM$table
log_six_confM$byClass
## table 8
log_all_confM$table
log_all_confM$byClass
## table 10
dtree_three_confM$table
dtree_three_confM$byClass
## table 11
dtree_six_confM$table
dtree_six_confM$byClass
## table 12
dtree_all_confM$table
dtree_all_confM$byClass

## extracting citation data from the packages
citation("dplyr")
citation("ggplot2")
citation("caret")
citation("tidyr")
citation("rattle")