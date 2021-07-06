library(tidyverse)
library(tidymodels)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)


df <- read_csv('/Users/danielgemara/Downloads/train.csv') %>%
  mutate(target = as.factor(damaged)) %>%
  select(-damaged) 

holdout <- read_csv('/Users/danielgemara/Downloads/test.csv')

split <- initial_split(df, prop = 0.5)

train <- training(split)
test <- testing(split)

train %>% 
  head()

rec <- train %>%
  recipe(target ~ .) %>%
  step_rm(id) %>% 
  step_modeimpute(all_nominal()) %>% 
  step_dummy(all_nominal()) %>% 
  step_meanimpute(all_predictors()) %>%
  step_normalize(all_predictors()) %>% 
  prep(training = df)

rec


df2 <- juice(rec) 

train_index <- seq_len(nrow(train))
train2 <- df2[train_index,]
test2 <- df2[-train_index,]

lm_model <- 
  logistic_reg() %>% 
  set_engine("glm")

lm_form_fit <- 
  lm_model %>% 
  fit(factor(target_X1) ~ ., data = train2)

result <- predict(lm_form_fit, test2)
