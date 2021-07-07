library(tidyverse)
library(tidymodels)
library(textrecipes)
library(scales)
library(stacks)
theme_set(theme_light())
doParallel::registerDoParallel(cores = 4)

df <- read_csv("train.csv") %>%
  select(-Rank) 
df2 <- read_csv("test.csv")
sample_submission <- read_csv('sample_submission.csv')

release_date <- df %>%
  bind_rows(df2) %>%
  mutate(Month = str_pad(Month, 2, side = "left", pad = "0")) %>%
  unite("date", Year, Month, sep = "") %>%
  mutate(date = as.numeric(date)) %>%
  group_by(Game) %>%
  slice_min(date) %>%
  ungroup() %>%
  select(Game, date) %>%
  mutate(Year = substr(date, 0, 4) %>% as.numeric(),
         Month = substr(date, 5, 6) %>% as.numeric()) %>%
  select(-date) %>%
  mutate(is_new = 1)

previous_month <- df %>%
  bind_rows(df2) %>%
  select(Game, (Year), (Month), Hours_watched) %>%
  complete(Game, nesting(Year, Month), fill = list(Hours_watched = 0)) %>%
  group_by(Game) %>%
  mutate(previous_month = lag(Hours_watched),
         growth = Hours_watched - lag(Hours_watched)) %>%
  ungroup() %>%
  mutate(previous_month = ifelse(is.na(previous_month), 0, previous_month)) %>%
  mutate(growth = ifelse(is.na(growth), 0, growth))

common_games <- df %>%
  bind_rows(df2) %>%
  distinct(Game) %>%
  arrange(Game)

data <- df %>%
  left_join(release_date) %>%
  left_join(previous_month) %>%
  mutate(is_new = ifelse(is.na(is_new), 0, is_new)) %>%
  mutate(
    is_AOE = ifelse(str_detect(Game, 'Age of Empires'), 1, 0),
    is_Amnesia = ifelse(str_detect(Game, 'Amnesia'), 1, 0),
    is_AC = ifelse(str_detect(Game, 'Animal Crossing'), 1, 0),
    is_Ark = ifelse(str_detect(Game, 'Ark|ARK'), 1, 0),
    is_ASC = ifelse(str_detect(Game, "Assassin's Creed"), 1, 0),
    is_b = ifelse(str_detect(Game, "Battlefield"), 1, 0),
    is_bio = ifelse(str_detect(Game, "BioShock"), 1, 0),
    is_bord = ifelse(str_detect(Game, "Borderlands"), 1, 0),
    is_COD = ifelse(str_detect(Game, "Call of Duty"), 1, 0),
    is_Crash = ifelse(str_detect(Game, "Crash Bandicoot"), 1, 0),
    is_ds = ifelse(str_detect(Game, "Dark Soul"), 1, 0),
    is_dead = ifelse(str_detect(Game, "Dead Space"), 1, 0),
    is_devil = ifelse(str_detect(Game, "Devil May"), 1, 0),
    is_dia = ifelse(str_detect(Game, "Diablo"), 1, 0),
    is_dia = ifelse(str_detect(Game, "Diablo"), 1, 0),
    is_dk = ifelse(str_detect(Game, "Donkey Kong"), 1, 0),
    is_dragon = ifelse(str_detect(Game, "Dragon Ball|DRAGON BALL"), 1, 0),
    is_E3 = ifelse(str_detect(Game, "E3"), 1, 0),
    is_EA_Sports = ifelse(str_detect(Game, "EA Sports"), 1, 0),
    is_F1 = ifelse(str_detect(Game, "F1"), 1, 0),
    is_Fallout = ifelse(str_detect(Game, "Fallout"), 1, 0),
    is_Farming = ifelse(str_detect(Game, "Farming S"), 1, 0),
    is_FIFA = ifelse(str_detect(Game, "FIFA"), 1, 0),
    is_Final_Fantasy = ifelse(str_detect(Game, "Final Fantasy"), 1, 0),
    is_Fire_Emblem = ifelse(str_detect(Game, "Fire Emblem"), 1, 0),
    is_Football_Manager = ifelse(str_detect(Game, "Football Manager"), 1, 0),
    is_Forza = ifelse(str_detect(Game, "Forza"), 1, 0),
    
    is_Gears = ifelse(str_detect(Game, "Gears of War"), 1, 0),
    is_godwar = ifelse(str_detect(Game, "God of War"), 1, 0),
    is_Golf = ifelse(str_detect(Game, "Golf"), 1, 0),
    is_grandth = ifelse(str_detect(Game, "Grand Theft Auto"), 1, 0),
    is_guilt = ifelse(str_detect(Game, "Guilty Gear"), 1, 0),
    is_halo = ifelse(str_detect(Game, "Halo"), 1, 0),
    is_Heroes= ifelse(str_detect(Game, "Heroes of Might"), 1, 0),
    is_HITMAN = ifelse(str_detect(Game, "HITMAN"), 1, 0),
    is_justd = ifelse(str_detect(Game, "Just Dance"), 1, 0),
    is_Kingdom_Hearts = ifelse(str_detect(Game, "Kingdom Hearts"), 1, 0),
    is_Kirby = ifelse(str_detect(Game, "Kirby"), 1, 0),
    is_lol = ifelse(str_detect(Game, "League of Legends"), 1, 0),
    is_LEGO = ifelse(str_detect(Game, "LEGO"), 1, 0),
    is_lis = ifelse(str_detect(Game, "Life is Strange"), 1, 0),
    is_Lineage = ifelse(str_detect(Game, "Lineage"), 1, 0),
    is_LM = ifelse(str_detect(Game, "Little Nightmares"), 1, 0),
    is_Madden = ifelse(str_detect(Game, "Madden"), 1, 0),
    is_Mafia = ifelse(str_detect(Game, "Mafia"), 1, 0),
    is_Mario = ifelse(str_detect(Game, "Mario"), 1, 0),
    is_Marvel = ifelse(str_detect(Game, "Marvel"), 1, 0),
    is_Mass = ifelse(str_detect(Game, "Mass"), 1, 0),
    is_Mega = ifelse(str_detect(Game, "Mega"), 1, 0),
    is_Metal = ifelse(str_detect(Game, "Metal"), 1, 0),
    is_Metroid = ifelse(str_detect(Game, "Metroid"), 1, 0),
    is_Minecraft = ifelse(str_detect(Game, "Minecraft"), 1, 0),
    is_mlb = ifelse(str_detect(Game, "MLB"), 1, 0),
    is_Monster = ifelse(str_detect(Game, "Monster"), 1, 0),
    is_Mortal = ifelse(str_detect(Game, "Mortal"), 1, 0),
    is_Mount = ifelse(str_detect(Game, "Mount"), 1, 0),
    is_NBA = ifelse(str_detect(Game, "NBA"), 1, 0),
    is_NHL = ifelse(str_detect(Game, "NHL"), 1, 0),
    is_NieR = ifelse(str_detect(Game, "NieR"), 1, 0)
  ) %>%
  mutate(approx = Avg_viewer_ratio*Hours_Streamed)

holdout <- df2 %>%
  left_join(release_date) %>%
  left_join(previous_month) %>%
  mutate(is_new = ifelse(is.na(is_new), 0, is_new)) %>%
  filter(Month == 5 & Year == 2021) %>%
  mutate(
    is_AOE = ifelse(str_detect(Game, 'Age of Empires'), 1, 0),
    is_Amnesia = ifelse(str_detect(Game, 'Amnesia'), 1, 0),
    is_AC = ifelse(str_detect(Game, 'Animal Crossing'), 1, 0),
    is_Ark = ifelse(str_detect(Game, 'Ark|ARK'), 1, 0),
    is_ASC = ifelse(str_detect(Game, "Assassin's Creed"), 1, 0),
    is_b = ifelse(str_detect(Game, "Battlefield"), 1, 0),
    is_bio = ifelse(str_detect(Game, "BioShock"), 1, 0),
    is_bord = ifelse(str_detect(Game, "Borderlands"), 1, 0),
    is_COD = ifelse(str_detect(Game, "Call of Duty"), 1, 0),
    is_Crash = ifelse(str_detect(Game, "Crash Bandicoot"), 1, 0),
    is_ds = ifelse(str_detect(Game, "Dark Soul"), 1, 0),
    is_dead = ifelse(str_detect(Game, "Dead Space"), 1, 0),
    is_devil = ifelse(str_detect(Game, "Devil May"), 1, 0),
    is_dia = ifelse(str_detect(Game, "Diablo"), 1, 0),
    is_dia = ifelse(str_detect(Game, "Diablo"), 1, 0),
    is_dk = ifelse(str_detect(Game, "Donkey Kong"), 1, 0),
    is_dragon = ifelse(str_detect(Game, "Dragon Ball|DRAGON BALL"), 1, 0),
    is_E3 = ifelse(str_detect(Game, "E3"), 1, 0),
    is_EA_Sports = ifelse(str_detect(Game, "EA Sports"), 1, 0),
    is_F1 = ifelse(str_detect(Game, "F1"), 1, 0),
    is_Fallout = ifelse(str_detect(Game, "Fallout"), 1, 0),
    is_Farming = ifelse(str_detect(Game, "Farming S"), 1, 0),
    is_FIFA = ifelse(str_detect(Game, "FIFA"), 1, 0),
    is_Final_Fantasy = ifelse(str_detect(Game, "Final Fantasy"), 1, 0),
    is_Fire_Emblem = ifelse(str_detect(Game, "Fire Emblem"), 1, 0),
    is_Football_Manager = ifelse(str_detect(Game, "Football Manager"), 1, 0),
    is_Forza = ifelse(str_detect(Game, "Forza"), 1, 0),
    
    is_Gears = ifelse(str_detect(Game, "Gears of War"), 1, 0),
    is_godwar = ifelse(str_detect(Game, "God of War"), 1, 0),
    is_Golf = ifelse(str_detect(Game, "Golf"), 1, 0),
    is_grandth = ifelse(str_detect(Game, "Grand Theft Auto"), 1, 0),
    is_guilt = ifelse(str_detect(Game, "Guilty Gear"), 1, 0),
    is_halo = ifelse(str_detect(Game, "Halo"), 1, 0),
    is_Heroes= ifelse(str_detect(Game, "Heroes of Might"), 1, 0),
    is_HITMAN = ifelse(str_detect(Game, "HITMAN"), 1, 0),
    is_justd = ifelse(str_detect(Game, "Just Dance"), 1, 0),
    is_Kingdom_Hearts = ifelse(str_detect(Game, "Kingdom Hearts"), 1, 0),
    is_Kirby = ifelse(str_detect(Game, "Kirby"), 1, 0),
    is_lol = ifelse(str_detect(Game, "League of Legends"), 1, 0),
    is_LEGO = ifelse(str_detect(Game, "LEGO"), 1, 0),
    is_lis = ifelse(str_detect(Game, "Life is Strange"), 1, 0),
    is_Lineage = ifelse(str_detect(Game, "Lineage"), 1, 0),
    is_LM = ifelse(str_detect(Game, "Little Nightmares"), 1, 0),
    is_Madden = ifelse(str_detect(Game, "Madden"), 1, 0),
    is_Mafia = ifelse(str_detect(Game, "Mafia"), 1, 0),
    is_Mario = ifelse(str_detect(Game, "Mario"), 1, 0),
    is_Marvel = ifelse(str_detect(Game, "Marvel"), 1, 0),
    is_Mass = ifelse(str_detect(Game, "Mass"), 1, 0),
    is_Mega = ifelse(str_detect(Game, "Mega"), 1, 0),
    is_Metal = ifelse(str_detect(Game, "Metal"), 1, 0),
    is_Metroid = ifelse(str_detect(Game, "Metroid"), 1, 0),
    is_Minecraft = ifelse(str_detect(Game, "Minecraft"), 1, 0),
    is_mlb = ifelse(str_detect(Game, "MLB"), 1, 0),
    is_Monster = ifelse(str_detect(Game, "Monster"), 1, 0),
    is_Mortal = ifelse(str_detect(Game, "Mortal"), 1, 0),
    is_Mount = ifelse(str_detect(Game, "Mount"), 1, 0),
    is_NBA = ifelse(str_detect(Game, "NBA"), 1, 0),
    is_NHL = ifelse(str_detect(Game, "NHL"), 1, 0),
    is_NieR = ifelse(str_detect(Game, "NieR"), 1, 0)
  ) %>%
  mutate(approx = Avg_viewer_ratio*Hours_Streamed)


set.seed(2021)

split <- initial_split(data, prop = 0.75)
train <- training(split)
test <- testing(split)

lm(test$Hours_watched ~ test$approx) %>%
  summary()

ggplot(aes(x = approx, y = Hours_watched), data = test) + 
  geom_point() +
  geom_smooth(method = 'lm')



predict_holdout <- function(wf) {
  wf %>%
    augment(holdout) %>%
    group_by(Month, Year) %>%
    mutate(Rank = rank(desc(.pred))) %>%
    ungroup() %>%
    select(Game, Rank)
}

augment.model_stack <- function(x, data, ...) {
  bind_cols(data, predict(x, data, ...))
}

mset <- metric_set(rmse)
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)
set.seed(2021)
train_fold5 <- train %>%
  vfold_cv(5)

prep_juice <- function(d) juice(prep(d))

xg_rec <- recipe(Hours_watched ~ .,data = train) %>%
  step_tokenize(Game) %>%
  step_stopwords(Game) %>%
  step_tokenfilter(Game, max_tokens = 20) %>%
  step_tf(Game) 

xg_rec %>% prep_juice() %>% View()

xg_mod <- boost_tree("regression",
                     mtry = tune(),
                     trees = tune(),
                     learn_rate = .01) %>%
  set_engine("xgboost")

xg_wf <- workflow() %>%
  add_recipe(xg_rec) %>%
  add_model(xg_mod)

xg_tune <- xg_wf %>%
  tune_grid(
    train_fold5,
    metrics = mset,
    control = grid_control
    ,
    grid = crossing(mtry = c(5, 7, 9),
                    trees = 3000)
  )

autoplot(xg_tune)

xg_tune %>%
  collect_metrics() %>%
  arrange(mean)

xg_fit <- xg_wf %>%
  finalize_workflow(select_best(xg_tune)) %>%
  fit(train)

importances <- xgboost::xgb.importance(model = xg_fit$fit$fit$fit)

importances %>%
  mutate(Feature = fct_reorder(Feature, Gain)) %>%
  ggplot(aes(Gain, Feature)) +
  geom_col()

xg_fit %>%
  predict_holdout() %>%
  write_csv('ep_6_xgboost2_withprevious_higher_4.csv')

real_data <- read_csv('/Users/danielgemara/Downloads/Most watched games on Twitch - SullyGnome (1).csv') %>%
  bind_rows(read_csv('/Users/danielgemara/Downloads/Most watched games on Twitch - SullyGnome.csv')) %>%
  select(Game, Hours_Watched = `Watch time`)

output = xg_fit %>%
  augment(holdout) %>%
  left_join(real_data %>%
              mutate(Hours_Watched = case_when(
                'Game' == "Assassin's Creed Valhalla" ~ 995960,
                'Game' == "Pools, Hot Tubs, and Beaches" ~ 4709887,
                'Game' == "Call of Duty: Mobile" ~ 1351713,
                TRUE ~ Hours_Watched
              )), by = "Game") %>%
  mutate(model_rank = rank(desc(.pred)),
         real_rank = rank(desc(Hours_Watched))) %>%
  select(Game, Hours_Watched, real_rank, model_rank)
  filter(model_rank == real_rank)

holdout %>%
  mutate(Hours_Watched = Avg_viewer_ratio*Hours_Streamed) %>%
  select(Game, Hours_Watched) %>%
  mutate(Rank = rank(desc(Hours_Watched))) %>%
  select(Game, Rank) %>%
  left_join(real_data) %>%
  mutate(real_rank = rank(desc(Hours_Watched))) %>%
  filter(Rank == real_rank)
  
write_csv('final.csv')

