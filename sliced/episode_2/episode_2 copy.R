library(tidymodels)
library(tidyverse)
library(catboost)

data  <- read_csv("Google Drive/R Projects/sliced/episode_2/train.csv") %>%
  mutate(damaged = as.factor(damaged))



data %>% 
  summarize(across(everything(), class)) %>%
  pivot_longer(cols = everything())


# newdf <- data %>% 
#   mutate(across(c(aircraft, aircraft_make), ~as.numeric(str_remove_all(.,"-")))) %>%
#   # rowwise() %>%
#   # mutate(total_na=sum(is.na(c_across((where(is.numeric)))))) %>%
#   # ungroup() %>%
#   mutate(total_na = rowSums(is.na(.))) %>%
#   mutate(engine1_position = ifelse(is.na(engine1_position), 
#                                    mean(engine1_position, na.rm = T), 
#                                    engine1_position)) 

holdout <- read_csv("Google Drive/R Projects/sliced/episode_2/test.csv") %>%
  unite("new_id", c(incident_year:aircraft, airport_id, airport, height, speed, species_id), remove = F)

real_data <- data.table::fread("Google Drive/R Projects/sliced/episode_2/database.csv") %>%
  janitor::clean_names() %>%
  rename(damaged = aircraft_damage) %>%
  select(record_id:damaged,
         -c(record_id, fatalities, injuries, warning_issued)) %>%
  distinct() %>%
  unite("new_id", c(incident_year:aircraft, airport_id, airport, height, speed, species_id), remove = F) %>%
  filter(new_id %in% holdout$new_id) %>%
  select(-new_id) %>%
  mutate(aircraft_model = as.numeric(aircraft_model),
         engine_model = as.numeric(engine_model),
         engine1_position = as.numeric(engine1_position),
         engine3_position = as.numeric(engine3_position)) %>%
  mutate(across(where(is.character), as.factor))

mset <- metric_set(mn_log_loss)

set.seed(2021)

split <- initial_split(data, prop = 0.7)
train <- training(split)
test <- testing(split)

library(catboost)

features <- train %>% 
  select(-damaged, -id) %>% 
  mutate(across(where(is.character), as.factor))
labels <- train$damaged
train_pool <- catboost.load_pool(data = features, label = labels)

model <- catboost.train(train_pool,  NULL,
                        params = list(iterations = 1000,
                                      metric_period=100,
                                      loss_function = 'Logloss'))

real <- real_data %>%
  select(-damaged) %>%
  mutate(across(where(is.character), as.factor))

real_pool <- catboost.load_pool(real)
prediction <- catboost.predict(model, real_pool)

real_data$prediction <- prediction

real_data %>%
  mn_log_loss(damaged %>% as.factor(), prediction)

fit_control <- caret::trainControl(method = "cv",
                            number = 4,
                            classProbs = TRUE)

grid <- expand.grid(depth = c(4, 6, 8),
                    learning_rate = 0.1,
                    iterations = 500,
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

print(report)

importance <- varImp(report, scale = FALSE)
print(importance)

ggplot2::ggplot(importance)

report

prediction_catboost <-  predict(report, real_data %>% mutate(across(where(is.character), as.factor)), type = "prob")

real_data$prediction_catboost <- prediction_catboost$X1

real_data %>%
  mn_log_loss(damaged %>% as.factor(), prediction_catboost)





