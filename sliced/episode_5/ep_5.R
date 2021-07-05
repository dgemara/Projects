library(tidymodels)
library(tidyverse)
library(stacks)
library(textrecipes)
library(lubridate)
theme_set(theme_light())
library(scales)
library(tidytext)

# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://loppsided.blog/posts/tasty-models/
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/board-games.Rmd
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/chopped.Rmd

real_data <- read_csv('AB_NYC_2019.csv') 

#https://www.r-bloggers.com/2020/08/how-to-use-catboost-with-tidymodels/
doParallel::registerDoParallel(cores = 6)

data <- read_csv("train.csv") %>%
  mutate(price_log = log(price + 0.01)) %>%
  select(-price) %>%
  mutate(max_review_date = max(last_review, na.rm = T),
         days_since_review = ifelse(is.na(last_review), 10000, max_review_date - last_review)) %>%
  select(-c(last_review, max_review_date)) %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month))

holdout <- read_csv("test.csv") %>%
  mutate(max_review_date = max(last_review, na.rm = T),
         days_since_review = ifelse(is.na(last_review), 10000, max_review_date - last_review)) %>%
  select(-c(last_review, max_review_date)) %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month)) %>%
  left_join(real_data %>% select(id, name, host_id, price)) %>%
  mutate(price_log = log(price + 0.01)) %>%
  select(-price) 
  
  

# %>%
#   mutate(last_review = lubridate::ymd(last_review) %>% as.numeric()) %>%
#   mutate(across(everything(), ~ replace_na(.x, 0)))

mset <- yardstick::metric_set(rmse)
#https://www.rdocumentation.org/packages/tune/versions/0.1.5/topics/tune_bayes
#https://www.hfshr.xyz/posts/2020-05-23-tidymodel-notes/
#https://www.tidymodels.org/learn/work/bayes-opt/


set.seed(2021)

split <- initial_split(data, prop = 0.6)
train <- training(split)
test <- testing(split)


# library(catboost)
# 
# features <- train %>% 
#   select(-price) %>% 
#   mutate(across(where(is.character), as.factor)) 
# labels <- train$price
# train_pool <- catboost.load_pool(data = features, label = labels)
# 
# model <- catboost.train(train_pool,  NULL,
#                         params = list(iterations = 1000, metric_period=10))
# 
# real_data <- holdout %>% 
#   mutate(across(where(is.character), as.factor))
# 
# real_pool <- catboost.load_pool(real_data)
# prediction <- catboost.predict(model, real_pool)
# 
# holdout$prediction <- prediction
# 
# importance <- catboost.get_feature_importance(model, 
#                                               pool = NULL, 
#                                               type = 'FeatureImportance',
#                                               thread_count = -1) %>%
#   as.data.frame()
# 
# importance %>%
#   arrange(V1)
# 
# 
# write_csv(holdout %>% select(id, price = prediction),
#           'episode_5_predictions.csv')


train_fold <- train %>%
  vfold_cv(10)

train %>%
  unnest_tokens(word, name) %>%
  group_by(word) %>%
  summarize(n_episodes = n_distinct(id)) %>%
  arrange(desc(n_episodes))

train %>%
  count(room_type)

lin_mod <- linear_reg(penalty = tune()) %>%
  set_engine('glmnet')

names(train)
rec <- recipe(
  price_log ~ .,  data = train
) %>%
  step_rm(id, host_name, host_id, neighbourhood_group) %>%
  step_tokenize(name) %>%
  step_stopwords(name) %>%
  step_tokenfilter(name, max_tokens = 50) %>%
  step_tf(name) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_other(neighbourhood, threshold = 0.01) %>%
  step_dummy(neighbourhood, room_type) %>%
  step_unknown(all_nominal_predictors()) 

rec %>%
  prep() %>%
  juice() %>%
  View()
  
xg_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(boost_tree("regression",
                       mtry = tune(),
                       trees = tune(),
                       learn_rate = .01) %>% set_engine("xgboost"))


xg_tune <- xg_wf %>%
  tune_grid(train_fold,
            grid = crossing(mtry = c(15, 18, 21),
                            trees = seq(300, 1600, 100)
                            ),
            metrics = mset)

autoplot(xg_tune)

xg_wf_best <- xg_wf %>%
  finalize_workflow(select_best(xg_tune))
xg_fit_best <- xg_wf_best %>%
  fit(train)
importances <- xgboost::xgb.importance(model = xg_fit_best$fit$fit$fit)

importances %>%
  mutate(Feature = fct_reorder(Feature, Gain)) %>%
  ggplot(aes(Gain, Feature)) +
  geom_point()

predict_on_holdout <- function(workflow) {
  workflow %>%
    fit(dataset) %>%
    augment(holdout) 
}
predictions <- xg_fit_best %>%
  predict(holdout)

holdout$predictions <- predictions

holdout %>%
  rmse(price_log, predictions$.pred)

