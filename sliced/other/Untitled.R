library(tidymodels)
library(tidyverse)
library(recipeselectors)

df <- read_csv('~/Downloads/train.csv') %>%
  mutate(damaged = factor(damaged))

holdout <- read_csv('~/Downloads/test.csv')

split <- initial_split(df, prop = 0.8)
train <- training(split)
test <- testing(split)

mset <- metric_set(mn_log_loss)

set.seed(2021-06-08)

train_5fold <- train %>%
  vfold_cv(v = 10)

linear_rec <- recipe(damaged ~ airport + flight_impact + species_name +
                       aircraft + species_id + flight_phase + operator_id
                     + height,
                     data = train) %>%
  step_other(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors(),) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_log(height, base = 2, offset = 1) %>%
  step_dummy(all_nominal_predictors())

linear_rec <- recipe(damaged ~ .,
                     data = train) %>%
  step_other(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors(),) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_log(height, base = 2, offset = 1) %>%
  step_dummy(all_nominal_predictors())

linear_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")
 
linear_wf <- workflow() %>%
  add_recipe(linear_rec) %>%
  add_model(linear_spec)

linear_tune <- linear_wf %>%
  tune_grid(train_5fold,
            grid = crossing(penalty = 10 ^ seq(-7, -.01, .05)),
            metrics = mset)
 
autoplot(linear_tune)

penalty <- linear_tune %>%
  collect_metrics()
 
best_penalty <- linear_tune %>%
  select_best("mn_log_loss")
 
best_penalty
 
final_wf <-
  linear_wf %>%
  finalize_workflow(best_penalty)

final_wf
 
final_tree <-
  final_wf %>%
  fit(data = train)

library(vip)
 
final_tree %>%
  pull_workflow_fit() %>%
  vip()
 
final_fit <-
  final_wf %>%
  last_fit(split)

final_fit %>%
  collect_metrics()
 
final_fit %>%
  collect_predictions() %>%
  roc_curve(damaged, .pred_0) %>%
  autoplot()

 
reg_res <-
  final_wf %>%
  # Attach the best tuning parameters to the model
  tune::finalize_workflow(best_penalty) %>%
  # Fit the final model to the training data
  parsnip::fit(data = train)

test_preds <- reg_res %>%
  predict(new_data = test, type = 'prob') %>%
  bind_cols(test, .) %>%
  select(id, damaged, .pred_0)

test_preds %>%
  mn_log_loss(damaged, .pred_0)

preds <- reg_res %>%
  predict(new_data = holdout, type = 'prob') %>%
  bind_cols(holdout, .) %>%
  select(id, damaged = .pred_1)

# write_csv(preds, 'test1.csv')
# 
# xgb_spec <- boost_tree(
#   mtry = 4,
#   trees = tune()
# ) %>% 
#   set_engine("xgboost") %>% 
#   set_mode("classification")
# 
# xg_rec <- recipe()
# 
# xgb_grid <- grid_latin_hypercube(
#   trees(),
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), train),
#   learn_rate(),
#   size = 1
# )
# 
# xgb_grid
# 
# xgb_wf <- workflow() %>%
#   add_formula(damaged ~ .) %>%
#   add_model(xgb_spec)
# 
# xgb_wf
# 
# doParallel::registerDoParallel()
# 
# set.seed(234)
# 
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = train_5fold,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )
# 
# xgb_res

#Basic Regression Model
base_rec <- recipe(damaged ~ ., data = train) %>%
  step_dummy(all_nominal(), one_hot = T)

base_rec %>% prep() %>% juice()

basic_dim_rec <- recipe(damaged ~ ., data = train) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal())

basic_dim_rec %>% prep() %>% juice() %>% ncol()


full_dim_rec <- recipe(damaged ~ ., data = train) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal())

full_dim_rec %>% prep() %>% juice() %>% ncol()


recipe(damaged ~., data = train) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  juice() %>%
  select(-damaged) %>%
  select_if(is.numeric) %>%
  cor() %>%
  as_tibble(rownames = 'features') %>%
  pivot_longer(-features) %>%
  filter(features > name) %>%
  drop_na() %>%
  arrange(desc(abs(value)))

recipe(damaged ~., data = train) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  juice() %>%
  select(-damaged) %>%
  select_if(is.numeric) %>%
  cor() %>%
  as_tibble(rownames = 'features') %>%
  pivot_longer(-features) %>%
  filter(features > name) %>%
  drop_na() %>%
  ggplot(aes(x=value)) +
  geom_histogram(color = 'white') +
  scale_x_continuous(labels = scales::label_percent())

simple_tree_model <- decision_tree(tree_depth = tune(),
                                   min_n = tune(),
                                   cost_complexity = tune()) %>%
  set_mode('classification') %>%
  set_engine('rpart')

random_forest_model <- rand_forest(min_n = tune(),
                                   trees = tune()) %>%
  set_mode('classification')  %>%
  set_engine('ranger')

xgboost_model <- boost_tree(trees = tune(),
                            tree_depth = tune()) %>%
  set_mode('classification')  %>%
  set_engine('xgboost')


wf <- workflow() %>%
  add_recipe(base_rec) %>%
  add_model(random_forest_model)

linear_tune <- wf %>%
  tune_grid(train_5fold,
            metrics = mset)

linear_tune <- linear_wf %>%
  tune_grid(train_5fold,
            grid = crossing(penalty = 10 ^ seq(-7, -.5, .05)),
            metrics = mset)


linear_tune <- linear_wf %>%
  tune_grid(train_5fold,
            grid = crossing(penalty = 10 ^ seq(-7, -.5, .05)),
            metrics = mset)

vip_rec <- recipe(damaged ~ ., data = train) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>%
  step_select_vip(all_predictors(), outcome = 'damaged',
                  model = linear_spec,
                  threshold = 0.9)
linear_spec

vip_rec %>%
  prep() %>% juice()
# 
# autoplot(linear_tune)
# 
# penalty <- linear_tune %>% 
#   collect_metrics()
# 
# best_penalty <- linear_tune %>%
#   select_best("mn_log_loss")
# 
# best_penalty
# 
# final_wf <- 
#   linear_wf %>% 
#   finalize_workflow(best_penalty)
# 
# final_wf
# 
# final_tree <- 
#   final_wf %>%
#   fit(data = train) 
#https://juliasilge.com/blog/xgboost-tune-volleyball/
#https://juliasilge.com/blog/palmer-penguins/
#https://juliasilge.com/blog/himalayan-climbing/
#https://juliasilge.com/blog/lasso-the-office/
#https://juliasilge.com/blog/tuition-resampling/
#https://www.tidymodels.org/start/tuning/
#https://cimentadaj.github.io/blog/2020-02-06-the-simplest-tidy-machine-learning-workflow/the-simplest-tidy-machine-learning-workflow/
#https://embed.tidymodels.org/reference/step_woe.html
