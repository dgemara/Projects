library(tidymodels)
library(tidyverse)
library(catboost)

data  <- read_csv("/Applications/Google/R Projects/sliced/episode_4/train.csv") %>%
  mutate(rain_tomorrow = as.factor(rain_tomorrow)) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date)) %>%
  mutate(day = lubridate::day(date)) %>%
  mutate(date = as.numeric(date))


holdout <- read_csv("/Applications/Google/R Projects/sliced/episode_4/test.csv") %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date)) %>%
  mutate(day = lubridate::day(date)) %>%
  mutate(date = as.numeric(date))

split <- initial_split(data, prop = 0.7)
train <- training(split)
test <- testing(split)

features <- train %>% 
  select(-rain_tomorrow, -id) %>% 
  mutate(across(where(is.character), as.factor))
labels <- train$rain_tomorrow %>% as.numeric()
train_pool <- catboost.load_pool(data = features, label = labels)

# model <- catboost.train(train_pool,  NULL,
#                         params = list(iterations = 1000,
#                                       metric_period=100,
#                                       loss_function = 'Logloss'))
# 
# real <- holdout %>%
#   mutate(across(where(is.character), as.factor))
# 
# real_pool <- catboost.load_pool(real)
# prediction <- catboost.predict(model, real_pool)

fit_control <- caret::trainControl(method = "cv",
                                   number = 4,
                                   classProbs = TRUE)

grid <- expand.grid(depth = c(4, 6, 8, 10, 12),
                    learning_rate = c(0.01, 0.05, 0.1),
                    iterations = 250,
                    l2_leaf_reg = 1e-3,
                    rsm = 0.95,
                    border_count = 64)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

report <- caret::train(features, make.names(labels),
                       method = catboost.caret,
                       logging_level = 'Verbose', preProc = NULL,
                       tuneGrid = grid, trControl = fit_control)

importance <- varImp(report, scale = FALSE)
print(importance)

ggplot2::ggplot(importance)

prediction_catboost <-  predict(report, holdout %>% mutate(across(where(is.character), as.factor)), type = "prob")

holdout$prediction_catboost <- prediction_catboost$X2

# prediction_catboost <-  predict(report, test %>% mutate(across(where(is.character), as.factor)), type = "prob")
# 
# test$pred <- prediction_catboost$X1
# 
# test %>%
#   mn_log_loss(rain_tomorrow %>% as.factor(),
#               pred)

write_csv(holdout %>% select(id, rain_tomorrow = prediction_catboost), 'episode_4.csv')

