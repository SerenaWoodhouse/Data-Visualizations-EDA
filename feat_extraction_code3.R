setwd("F:/Data Analytics 2/Market Dial")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("lubridate")
install.packages("ggcorrplot")
install.packages("caret")
install.packages("gridExtra")
install.packages("randomForest")
library(gridExtra)
library(randomForest)
library(caret)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(lubridate)

store_att <- read.csv("SUU_store_attributes_mav.csv", stringsAsFactors = F)
store_att <- store_att[,-1]
store_att <- store_att[1:336,]
lift <- read.csv("SUU_test_data_and_MarketDial_lift.csv")
lift <- lift[,-1]
lift$date_pre_start <- as.Date(lift$date_pre_start, format = "%Y-%m-%d")
lift$date_implementation_start <- as.Date(lift$date_implementation_start, format = "%Y-%m-%d")
lift$date_test_start <- as.Date(lift$date_test_start, format = "%Y-%m-%d")
lift$date_test_end <- as.Date(lift$date_test_end, format = "%Y-%m-%d")

test12 <- read.csv("test_12_revenue_data")
test12 <- test12[,-1]
test12$date_week <- as.Date(test12$date_week, format = "%Y-%m-%d")
test2 <- read.csv("test_2_revenue_data")
test2 <- test2[,-1]
test2$date_week <- as.Date(test2$date_week, format = "%Y-%m-%d")
test21 <- read.csv("test_21_revenue_data")
test21 <- test21[,-1]
test21$date_week <- as.Date(test21$date_week, format = "%Y-%m-%d")
test34 <- read.csv("test_34_revenue_data")
test34 <- test34[,-1]
test34$date_week <- as.Date(test34$date_week, format = "%Y-%m-%d")
test5 <- read.csv("test_5_revenue_data")
test5 <- test5[,-1]
test5$date_week <- as.Date(test5$date_week, format = "%Y-%m-%d")
test7 <- read.csv("test_7_revenue_data")
test7 <- test7[,-1]
test7$date_week <- as.Date(test7$date_week, format = "%Y-%m-%d")
test8 <- read.csv("test_8_revenue_data")
test8 <- test8[,-1]
test8$date_week <- as.Date(test8$date_week, format = "%Y-%m-%d")

str(store_att)
low_state <- names(table(store_att$state)[(table(store_att$state) < 5)])
store_att[store_att$state %in% low_state,"state"] <- "Other"

for (i in which(sapply(store_att, is.numeric))) {
  if (sum(is.na(store_att[,i])) < 5) {
    store_att[is.na(store_att[,i]),i] <- mean(store_att[,i], na.rm = T)
  }
}

# Manually fix NAs in factors

store_att[,which(sapply(store_att, is.character))] <- 
  lapply(store_att[,which(sapply(store_att, is.character))], factor)

store_att[is.na(store_att$sales_liquor_percent_retail_sales),
          "sales_liquor_percent_retail_sales"] <- 0

df <- store_att
ap <- unique(apply(df,FUN = is.na, MARGIN = 2))
rowSums(ap)
sapply(store_att, function(y) sum(length(which(is.na(y)))))
ap <- ap[rowSums(ap) != 0,]

store_att$NA1 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[1,]))) == ncol(df)
store_att$NA2 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[2,]))) == ncol(df)
store_att$NA3 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[3,]))) == ncol(df)
store_att$NA4 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[4,]))) == ncol(df)


for (i in which(sapply(store_att, is.numeric))) {
  if (sum(is.na(store_att[,i])) > 0) {
    store_att[is.na(store_att[,i]),i] <- mean(store_att[,i], na.rm = T)
  }
}



# Store attributes is ready to go!

min2 <- min(lift[lift$test_id == 2, "date_implementation_start"])
min5 <- min(lift[lift$test_id == 5, "date_implementation_start"])
min7 <- min(lift[lift$test_id == 7, "date_implementation_start"])
min8 <- min(lift[lift$test_id == 8, "date_implementation_start"])
min12 <- min(lift[lift$test_id == 12, "date_implementation_start"])
min21 <- min(lift[lift$test_id == 21, "date_implementation_start"])
min34 <- min(lift[lift$test_id == 34, "date_implementation_start"])

library(dplyr)
df2 <- test2 %>%
  filter(date_week < min2) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_2_direct_category_revenue),
            Store_Avg = mean(test_2_whole_store_revenue),
            Cat_Var = var(test_2_direct_category_revenue),
            Store_Var = var(test_2_whole_store_revenue),
            Pct_Cat = mean(test_2_direct_category_revenue) / 
              mean(test_2_whole_store_revenue))

df5 <- test5 %>%
  filter(date_week < min2) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_5_direct_category_revenue),
            Store_Avg = mean(test_5_whole_store_revenue),
            Cat_Var = var(test_5_direct_category_revenue),
            Store_Var = var(test_5_whole_store_revenue),
            Pct_Cat = mean(test_5_direct_category_revenue) / 
              mean(test_5_whole_store_revenue))

df7 <- test7 %>%
  filter(date_week < min7) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_7_direct_category_revenue),
            Store_Avg = mean(test_7_whole_store_revenue),
            Cat_Var = var(test_7_direct_category_revenue),
            Store_Var = var(test_7_whole_store_revenue),
            Pct_Cat = mean(test_7_direct_category_revenue) / 
              mean(test_7_whole_store_revenue))

df8 <- test8 %>%
  filter(date_week < min8) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_8_direct_category_revenue),
            Store_Avg = mean(test_8_whole_store_revenue),
            Cat_Var = var(test_8_direct_category_revenue),
            Store_Var = var(test_8_whole_store_revenue),
            Pct_Cat = mean(test_8_direct_category_revenue) / 
              mean(test_8_whole_store_revenue))

df12 <- test12 %>%
  filter(date_week < min12) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_12_direct_category_revenue),
            Store_Avg = mean(test_12_whole_store_revenue),
            Cat_Var = var(test_12_direct_category_revenue),
            Store_Var = var(test_12_whole_store_revenue),
            Pct_Cat = mean(test_12_direct_category_revenue) / 
              mean(test_12_whole_store_revenue))

df21 <- test21 %>%
  filter(date_week < min21) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_21_direct_category_revenue),
            Store_Avg = mean(test_21_whole_store_revenue),
            Cat_Var = var(test_21_direct_category_revenue),
            Store_Var = var(test_21_whole_store_revenue),
            Pct_Cat = mean(test_21_direct_category_revenue) / 
              mean(test_21_whole_store_revenue))

df34 <- test34 %>%
  filter(date_week < min34) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_34_direct_category_revenue),
            Store_Avg = mean(test_34_whole_store_revenue),
            Cat_Var = var(test_34_direct_category_revenue),
            Store_Var = var(test_34_whole_store_revenue),
            Pct_Cat = mean(test_34_direct_category_revenue) / 
              mean(test_34_whole_store_revenue))



## Other dataframes can be added

store_att2 <- merge(store_att, df2, by = "store_id")
store_att2 <- merge(store_att2, lift[lift$test_id == 2,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")

store_att5 <- merge(store_att, df5, by = "store_id")
store_att5 <- merge(store_att5, lift[lift$test_id == 5,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att5[1,44] <- mean(store_att5$store_size_square_footage) 

store_att7 <- merge(store_att, df7, by = "store_id")
store_att7 <- merge(store_att7, lift[lift$test_id == 7,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")

store_att8 <- merge(store_att, df8, by = "store_id")
store_att8 <- merge(store_att8, lift[lift$test_id == 8,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")

store_att12 <- merge(store_att, df12, by = "store_id")
store_att12 <- merge(store_att12, lift[lift$test_id == 12,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")

store_att21 <- merge(store_att, df21, by = "store_id")
store_att21 <- merge(store_att21, lift[lift$test_id == 21,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")

store_att34 <- merge(store_att, df34, by = "store_id")
store_att34 <- merge(store_att34, lift[lift$test_id == 34,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att34 <- store_att34[-23,]


#### Merge All Tests Together #### 
# First I'm going to try merging then splitting into train and test
store_att2$testId <- as.factor(2)
store_att5$testId <- as.factor(5)
store_att7$testId <- as.factor(7)
store_att8$testId <- as.factor(8)
store_att12$testId <- as.factor(12)
store_att21$testId <- as.factor(21)
store_att34$testId <- as.factor(34)
allTests <- rbind(store_att2, store_att5, store_att7, store_att8, store_att12, store_att21, store_att34)

#take out conflicting variables
allTests <- subset(allTests, select=-c(sales_beer_and_wine_percent_retail_sales,
                                        sales_candy_percent_retail_sales,
                                        sales_carbonated_packaged_bev_percent_retail_sales,
                                        sales_cigarettes_percent_retail_sales,
                                        sales_dairy_deli_percent_retail_sales,
                                        sales_food_service_percent_retail_sales,
                                        sales_fountain_cold_bev_percent_retail_sales,
                                        sales_general_merchandise_percent_retail_sales,
                                        sales_grocery_percent_retail_sales,
                                        sales_health_and_beauty_care_percent_retail_sales,
                                        sales_hot_beverage_percent_retail_sales,
                                        sales_ice_percent_retail_sales,
                                        sales_liquor_percent_retail_sales,
                                        sales_mugs_._promotion_percent_retail_sales,
                                        sales_non_carb_packaged_bev_percent_retail_sales,
                                        sales_noveltyentertnmntgifts_percent_retail_sales,
                                        sales_snacks_percent_retail_sales,
                                        sales_specialty_cold_percent_retail_sales,
                                        sales_tobacco_percent_retail_sales,
                                        sales_fuel_percent_retail_sales,
                                        NA1, NA2, NA3, NA4, testId, lift))

allTests$state <- as.numeric(allTests$state)
allTests$region <- as.numeric(allTests$region)
allTests$store_style <- as.numeric(allTests$store_style)

# Split into training and Testing
set.seed(12121)
sample <- sample.int(n = nrow(allTests), size = floor(.75*nrow(allTests)), replace = F)
allTestsTrain <- allTests[sample, ]
allTestsTest  <- allTests[-sample, ]


# Model tuning
set.seed(12121)
control <- trainControl(method="repeatedcv", number=4, repeats=3)
mtry <- sqrt(ncol(allTestsTrain))
tunegrid <- expand.grid(.mtry=mtry)

#rf models
#include store revenue and store style
#ntree < 500

library(caret)

allTestsTrain.pct_cat <- subset(allTestsTrain, select=-c(Cat_Avg, Cat_Var))
rfallTests.pct_cat <- train(Pct_Cat~., data=allTestsTrain.pct_cat, method="rf", tuneGrid=tunegrid, trControl=control , importance = T, ntree = 500)
rfallTests.pct_cat
pred.allTests.pct_cat <- predict(rfallTests.pct_cat, newdata = allTestsTest)
mean(abs(pred.allTests.pct_cat - allTestsTest$Pct_Cat))
pctcatimp <- varImp(rfallTests.pct_cat)
plot(pctcatimp)

allTestsTrain.cat_avg <- subset(allTestsTrain, select=-c(Pct_Cat, Cat_Var))
rfallTests.cat_avg <- train(Cat_Avg~., data=allTestsTrain.cat_avg, method="rf", tuneGrid=tunegrid, trControl=control , importance = T, ntree = 500)
rfallTests.cat_avg
pred.allTests.cat_avg <- predict(rfallTests.cat_avg, newdata = allTestsTest)
mean(abs(pred.allTests.cat_avg - allTestsTest$Cat_Avg))
catavgimp <- varImp(rfallTests.cat_avg)
plot(catavgimp)

allTestsTrain.cat_var <- subset(allTestsTrain, select=-c(Cat_Avg, Pct_Cat))
rfallTests.cat_var <- train(Cat_Var~., data=allTestsTrain.cat_var, method="rf", tuneGrid=tunegrid, trControl=control , importance = T, ntree = 500)
rfallTests.cat_var
pred.allTests.cat_var <- predict(rfallTests.cat_var, newdata = allTestsTest)
mean(abs(pred.allTests.cat_var - allTestsTest$Cat_Var))
catvarimp <- varImp(rfallTests.cat_var)
plot(catvarimp)

