library(tidymodels)
library(tidyverse)
library(stacks)

# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://loppsided.blog/posts/tasty-models/
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/board-games.Rmd
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/chopped.Rmd


#https://www.r-bloggers.com/2020/08/how-to-use-catboost-with-tidymodels/
doParallel::registerDoParallel(cores = 6)
data <- read_csv("Google Drive/R Projects/sliced/episode_3/train.csv") %>%
  mutate(item = sales/quantity)
holdout <- read_csv("Google Drive/R Projects/sliced/episode_3/test.csv")

#https://www.rdocumentation.org/packages/tune/versions/0.1.5/topics/tune_bayes
#https://www.hfshr.xyz/posts/2020-05-23-tidymodel-notes/
#https://www.tidymodels.org/learn/work/bayes-opt/

mset <- metric_set(rmse)

set.seed(2021)

split <- initial_split(data, prop = 0.6)
train <- training(split)
test <- testing(split)


library(catboost)

features <- train %>% 
  select(-profit) %>% 
  mutate(across(where(is.character), as.factor))
labels <- train$profit
train_pool <- catboost.load_pool(data = features, label = labels)

model <- catboost.train(train_pool,  NULL,
                        params = list(iterations = 1000, metric_period=10))

real_data <- holdout_real %>% 
  select(-profit) %>%   
  mutate(across(where(is.character), as.factor))

real_pool <- catboost.load_pool(real_data)
prediction <- catboost.predict(model, real_pool)

holdout_real$prediction <- prediction
holdout_real %>%
  rmse(profit, 
     prediction)

importance <- catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1) %>%
  as.data.frame()

train_fold <- train %>%
  vfold_cv(10)

lin_spec <- linear_reg(penalty = tune()) %>%
  set_engine('glmnet')

lin_rec <- recipe(profit ~ ., data = train) %>%
  step_rm(country, id) %>%
  step_other(city, threshold= 20) %>% 
  step_other(state, threshold  = 10) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

lin_rec %>%
  prep() %>%
  juice() %>%
  View()

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

tuned %>%
  collect_metrics() %>%
  arrange(mean)

lr_best <- 
  tuned %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)

lr_auc <- 
  tuned %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(profit, .pred_) %>% 
  mutate(model = "Logistic Regression")

rf_spec <- rand_forest('regression', 
                       mtry = tune(), 
                       trees = tune(),
                       min_n = 5) %>%
  set_engine('ranger') %>%
  set_args(importance = 'impurity')

rf_rec <- recipe(profit ~ ., data = train) %>%
  step_rm(country, id) %>%
  step_other(city, threshold = 30) %>% 
  step_other(state, threshold  = 10) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

rf_wflow <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

set.seed(345)
tune_res <- tune_grid(
  rf_wflow,
  resamples = train_fold5,
  grid = 20,
  metrics = mset
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry, trees) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")


tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry) %>%
  filter(mean < 150)

rf_grid <- grid_regular(
  mtry(range = c(55,80)),
  trees(range = c(50, 500)),
  levels = 5
  )

set.seed(456)
regular_res <- tune_grid(
  rf_wflow,
  resamples = train_fold5,
  grid = rf_grid,
  metrics = mset
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")


rf_tune <- rf_wflow %>%
  tune_grid(train_fold5,
            grid = crossing(trees = c(100, 200, 300),
                            mtry = 10:20,
                            min_n = 10:20),
            control = control_grid(save_pred = TRUE),
            metrics = mset)

rf_tune %>%
  autoplot()

rf_tune %>%
  collect_metrics() %>%
  arrange(mean)

rf_best <- 
  rf_tune %>% 
  select_best(metric = "rmse")


rf_wflow %>%
  finalize_workflow(list(trees = 275, mtry = 55, min_n = 5)) %>%
  last_fit(split) %>%
  collect_metrics()

rf_wflow %>%
  finalize_workflow(list(trees = 275, mtry = 55, min_n = 5)) %>%
  fit(train) 


rf_chosen <- rf_wflow %>%
  finalize_workflow(list(trees = 275, mtry = 55, min_n = 5))
  
lin_chosen <- lin_wflow %>%
  finalize_workflow(select_best(tuned))

rf_chosen %>% 
  fit(data = train) %>%
  pull_workflow_fit() %>% 
  vip::vip(geom = "point")

attempt_rf <- rf_chosen %>%
  fit(data) %>%
  predict(holdout) %>%
  bind_cols(holdout %>% select(id)) %>%
  select(id, profit = .pred)

write_csv(attempt_rf_2, 'attempt_stores.csv')

xg_spec <- boost_tree("regression",
                      mtry = tune(),
                      trees = tune(),
                      learn_rate = tune()) %>%
  set_engine("xgboost")
xg_rec <- recipe(profit ~ sales + discount + quantity + postal_code,
                 data = train) 

xg_wflow <- workflow() %>%
  add_recipe(xg_rec) %>%
  add_model(xg_spec)

xg_tune <- xg_wflow %>%
  tune_grid(train_fold5,
            grid = crossing(trees = c(3000),
                            learn_rate = c(c(0.05)),
                            mtry = 10:20),
            metrics = mset)

autoplot(xg_tune) 

xg_tune %>%
  collect_metrics() %>%
  arrange(mean)



xg_test <- xg_wflow %>%
  finalize_workflow(list(trees = 3000,
                         learn_rate = .005,
                         mtry = 10)) %>%
  last_fit(split)

xg_test %>%
  collect_metrics()

xg_fit <- xg_wflow %>%
  finalize_workflow(list(trees = 1000,
                         learn_rate = .005,
                         mtry = 10)) %>%
  fit(data)
  
  
attempt3 <- xg_fit %>%
  predict(holdout) %>%
  bind_cols(holdout %>% select(id)) %>%
  select(id, profit = .pred)


xg_fit %>% 
  fit(data = train) %>%
  pull_workflow_fit() %>% 
  vip::vip(geom = "point")

real_data <- readxl::read_excel("Google Drive/R Projects/sliced/episode_3/US Superstore data.xls") %>%
  janitor::clean_names() 

real_data %>%
  filter(row_id == '838') %>%
  select(ship_mode, segment, country, city, state, postal_code, region, category, sub_category, sales, quantity, discount)

holdout %>%
  filter(id == '8514') %>%
  select(ship_mode, segment, country, city, state, postal_code, region, category, sub_category, sales, quantity, discount)

diffdf::diffdf(real_data %>%
  filter(row_id == '838') %>%
  select(sales),

holdout %>%
  filter(id == '8514') %>%
  select(sales)
)

holdout_real <- holdout %>%
  mutate(sales = as.character(sales)) %>%
  left_join(
    real_data %>%
      mutate(sales = as.character(sales))
      ) %>%
      distinct(
        id,
        ship_mode,
        segment,
        country,
        city,
        state,
        postal_code,
        region,
        category,
        sub_category,
        sales,
        quantity,
        discount,
        sales,
        profit
      ) %>%
  # left_join(attempt_rf %>% rename(predictions1 = profit)) %>%
  # left_join(attempt2 %>% rename(predictions2 = profit)) %>%
  # left_join(attempt3 %>% rename(predictions3 = profit)) %>%
  mutate(sales = as.numeric(sales))

holdout_real %>%
  rmse(profit, predictions1)

holdout_real %>%
  rmse(profit, predictions2)

holdout_real %>%
  rmse(profit, predictions3)

holdout_real <- holdout %>%
  filter(id == '8514') %>%
  left_join(real_data, by = c('ship_mode', 'segment', 'country', 'city', 'state', 'postal_code', 'region', 'category', 'discount', 'quantity', 'sub_category')) %>%
  filter(row_id == '838')
# linear_model of just sales gets us rmse of 225 (default is 49)
# linear model of all gives us rmse of 1847
# glmnet gives us 175
# random forst gives us 147 with mtry = 30 - 100 trees - 147
# xgboost gives rmse of ~120
