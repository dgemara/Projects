library(tidyverse)
library(lubridate)

admissions <- read_csv('Admissions.csv')
lab <- read_csv('Lab.csv')
transfusions <- read_csv('Transfusions.csv')

# Impute the missing charlson_comorbidity_index values 
# in any way you see fit, with the intention that this 
# variable will be used as a predictor in a statistical model.

patients <- admissions %>%
  left_join(lab) %>%
  left_join(transfusions)

patients %>%
  count(charlson_comorbidity_index)

ggplot(aes(x = charlson_comorbidity_index, y = age, fill=factor(sex)), data = patients) +
  geom_boxplot()


## Answer to Question 1 ##

# I don't think it's necessary to impute the charlson_comorbidity_index.
# The index itself is a combination of other factors, such as age and other 
# risk factors. As indicated by the boxplot, there is no significant difference
# between the different the CCI and age and sex alone. More data would be useful
# to create a 'complete' CCI. As for its use in a statistical model, if using
# a tree model, the values would be 'allowed' to be NA, but if using a linear
# model (and CCI has to be used) then more data would be needed to predict 
# CCI based on the factors it needs (https://www.mdcalc.com/charlson-comorbidity-index-cci)

# Question 2:
# Determine if there is a significant difference in 
# sex between patients who had an rbc_transfusion 
# and patients that did not.

patients %>%
  count(rbc_transfusion, sex)
  

glm(rbc_transfusion ~ sex, data = patients, family = binomial) %>%
  summary()

## Answer to Question 2 ##

# Based on the logistic regression done between rbc_transfusion and sex, there is no 
# significance between male and female patients. sexM was not a significant variable
# in the model


# Question 3:
# Fit a linear regression model 
# using the result_value of the “Platelet Count” 
# lab tests as the dependent variable and age, sex, and hospital as the independent variables. 
# Briefly interpret the results.

patients %>%
  filter(test_name == 'Platelet Count') %>%
  select(result_unit) %>%
  distinct()

#only 1 type of unit

patients %>%
  filter(test_name == 'Platelet Count') %>%
  lm(result_value ~ age + sex + hospital, data = .) %>%
  summary()

patients %>%
  filter(test_name == 'Platelet Count') %>%
  lm(result_value ~ age, data = .) %>%
  summary()

# Answer to Question 3
# The model output is R^2 of 0.06697, which would indicate that the predictions 
# from this model would not be good. Male is extremely significant, while the hospital and age
# are not. However, a model on age alone is not good either (0.01 R^2). These are not the
# right variables to choose for platelet result value

# Question 4
# Create one or multiple plots that demonstrate the relationships 
# between length_of_stay (discharge date and time minus admission date and time), 
# charlson_comorbidity_index, and age.
test <- patients %>%
  mutate(admission_date = ymd(admission_date),
         admission_time = lubridate::hms(admission_time),
         admission = ymd_hms(paste(admission_date, admission_time), truncated = 3),
         discharge_date = mdy(discharge_date),
         discharge_time = lubridate::hms(discharge_time),
         discharge = ymd_hms(paste(discharge_date, discharge_time), truncated = 3)
         ) %>%
  mutate(length_of_stay = time_length(discharge - admission, "hours"))

test %>%
  filter(length_of_stay > 0) %>%
  ggplot(aes(x = charlson_comorbidity_index, y = length_of_stay)) +
  geom_boxplot()

test %>%
  filter(length_of_stay > 0) %>%
  ggplot(aes(x = age, y = length_of_stay)) +
  geom_point() +
  geom_smooth(method = 'lm')

#Answer to Question 4
#boxplot between CCI and length of stay definitely indicates an increasing length of 
#stay as CCI increases.

# scatterplot between age and length of stay (plus regeression line) shows there
# is not significant relationship between age and length of stay

# You are interested in evaluating the effect of platelet transfusions on a disease. 
# The patients with platelet_transfusion represent the selected treatment group. 
# Select a control group in any way you see fit. 
# access to any clinical variable you can think of?

#answer to question 5
#would compare between those who received platelet transfusions vs those who didnt.
# would run survival analysis, see the impact of transfusions vs survival/eradication
# of disease. Would need extra data of what happened AFTER transfusion.


# Question 6
# Fit a first-iteration statistical model of your choosing to predict 
# the result_value of the “Hemoglobin” lab tests and evaluate its performance. 
# How could you improve the model if you had more data and access 
# to any clinical variable you can think of?

patients %>%
  filter(test_name == 'Hemoglobin') %>%
  select(result_unit) %>%
  distinct()

data <- patients %>%
  filter(test_name == 'Hemoglobin') %>%
  mutate(admission_date = ymd(admission_date),
         admission_time = lubridate::hms(admission_date),
         discharge_date = mdy(discharge_date),
         discharge_time = lubridate::hms(discharge_time),
         admission_month = month(admission_date),
         admission_year = year(admission_date),
         admission_date = as.numeric(admission_date),
         result_date = ymd(result_date),
         result_month = month(result_date),
         result_year = year(result_date),
         result_date = as.numeric(result_date),
         result_value = ifelse(result_unit == 'g/dL', 
                               10*result_value,
                               result_value)
  ) %>%
  select(-c(discharge_date, discharge_time, test_name, 
            test_code, issue_date:plasma_transfusion,
            admission_time, result_time, result_unit))

library(tidymodels)
library(tidyverse)
library(catboost)
mset <- metric_set(rmse)

set.seed(2021)

split <- initial_split(data, prop = 0.7)
train <- training(split)
test <- testing(split)

features <- train %>% 
  select(-admission_id, -patient_id, -result_value) %>% 
  mutate(across(where(is.character), as.factor))
labels <- train$result_value %>% as.numeric()

train_pool <- catboost.load_pool(data = features, label = labels)

model <- catboost.train(train_pool,  NULL,
                        params = list(iterations = 10000, metric_period=10))

real_data <- test %>% 
  select(-result_value) %>%   
  mutate(across(where(is.character), as.factor))

real_pool <- catboost.load_pool(real_data)
prediction <- catboost.predict(model, real_pool)

test$pred <- prediction

mean(data$result_value, na.rm = T) #need to beat this score

ggplot(aes(x = result_value, y = pred), data = test) +
  geom_point() +
  geom_smooth(method = 'lm')

lm(result_value ~ pred, data = test) %>%
  summary()
  
# (max(data$result_value)-min(data$result_value))

test %>%
  explained_variance(result_value, pred)

importance <- catboost.get_feature_importance(model, 
                                              pool = NULL, 
                                              type = 'FeatureImportance',
                                              thread_count = -1) %>%
  as.data.frame() %>%
  rename(Importance = V1) %>%
  mutate(varnames = rownames(.))

ggplot(importance, aes(x=reorder(varnames, Importance), y=Importance)) + 
  geom_point() +
  xlab("Variable Name") +
  coord_flip()

mset <- metric_set(rmse)
set.seed(2021)
train_fold <- train %>%
  vfold_cv(10)

lin_spec <- linear_reg(penalty = tune()) %>%
  set_engine("glmnet")

lin_rec <- recipe(result_value ~ ., data = train) %>%
  step_rm(patient_id, admission_id) %>%
  step_other(hospital, threshold= 10) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

lin_wflow <- workflow() %>%
  add_recipe(lin_rec) %>%
  add_model(lin_spec)

cv <- lin_wflow %>%
  fit_resamples(train_fold)

cv %>%
  collect_metrics() 

tuned <- lin_wflow %>%
  tune_grid(train_fold,
            grid = crossing(penalty = 10 ^ seq(-7, -0.1, 0.1)),
            metrics = mset)

tuned %>%
  autoplot()

rf_spec <- rand_forest("regression",
                       mtry = tune(),
                       trees = tune()) %>%
  set_engine("ranger") %>%
  set_args(importance = "impurity")

rf_rec <- recipe(result_value ~ ., data = train) %>%
  step_impute_mean(all_numeric_predictors()) %>% 
  step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

rf_rec %>%
  prep() %>%
  juice() %>%
  View()

rf_wflow <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

rf_tune <- rf_wflow %>%
  tune_grid(train_fold,
            grid = crossing(trees = c(100, 200, 300),
                            mtry = 2:6),
            metrics = mset)

autoplot(rf_tune)



# Ran catboost to get base model, would've improved/done other methods such as
# xgboost, randomforest. Baseline RMSE for value was 141 in a blind model (avg
# result value in data). the model  found hospital,age , lap score and CCI to play
# the biggest roles. the RMSE for model was around 24, a massive improvement. more
# data could've helped was how long it took to get the test, time test was taken (I
# only used date of result, not time), to see if time of day makes a difference.

# didn't use data after hemoglobin test was taken (discharge date)