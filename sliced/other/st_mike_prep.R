library(tidyverse)

macro <-
  read_csv(
    "/Applications/Google/R Projects/ryerson/CMTH642 Y2J Y4J Y4K Y6J - Data Analytics Advanced Methods - W2019 - 1232019 - 509 PM/USDA_Macronutrients.csv"
  )

glimpse(macro)

micro <-
  read_csv(
    "/Applications/Google/R Projects/ryerson/CMTH642 Y2J Y4J Y4K Y6J - Data Analytics Advanced Methods - W2019 - 1232019 - 509 PM/USDA_Micronutrients.csv"
  )

glimpse(micro)

USDA <- macro %>%
  full_join(micro, by = 'ID')

glimpse(USDA)

USDA %>%
  summarize(across(everything(), class)) %>%
  pivot_longer(everything())



USDAclean <- USDA %>%
  mutate(across(c(Sodium, Potassium), ~as.numeric(str_remove_all(., ',')))) %>%
  mutate(na_counter = rowSums(is.na(.))) %>% #has 7057 rows 
  filter(na_counter <= 4) %>% #now only has 6877 
  mutate(Sugar = ifelse(is.na(Sugar), mean(Sugar, na.rm = T), Sugar)) %>%
  mutate(VitaminE = ifelse(is.na(VitaminE), mean(VitaminE, na.rm = T), VitaminE)) %>%
  mutate(VitaminD = ifelse(is.na(VitaminD), mean(VitaminD, na.rm = T), VitaminD)) %>%
  drop_na() %>%
  mutate(
    HighSodium = ifelse(Sodium > mean(Sodium, na.rm = T), 1, 0),
    HighCalories = ifelse(Calories > mean(Calories, na.rm = T), 1, 0),
    HighProtein = ifelse(Protein > mean(Protein, na.rm = T), 1, 0),
    HighSugar = ifelse(Sugar > mean(Sugar, na.rm = T), 1, 0),
    HighFat = ifelse(TotalFat > mean(TotalFat, na.rm = T), 1, 0)
  )   #now has 6310 rows

USDAclean <- USDA %>%
  mutate(across(c(Sodium, Potassium), ~as.numeric(str_remove_all(., ',')))) %>%
  mutate(na_counter = rowSums(is.na(.))) %>% #has 7057 rows 
  filter(na_counter <= 4) %>% #now only has 6877 
  mutate(across(c(Sugar, VitaminE, VitaminD), ~replace(., is.na(.), mean(., na.rm = TRUE)))) %>%
  drop_na() %>%
  mutate(
    HighSodium = ifelse(Sodium > mean(Sodium, na.rm = T), 1, 0),
    HighCalories = ifelse(Calories > mean(Calories, na.rm = T), 1, 0),
    HighProtein = ifelse(Protein > mean(Protein, na.rm = T), 1, 0),
    HighSugar = ifelse(Sugar > mean(Sugar, na.rm = T), 1, 0),
    HighFat = ifelse(TotalFat > mean(TotalFat, na.rm = T), 1, 0)
  )   #now has 6310 rows

USDAclean %>%
  slice_max(Sodium) #ID 2047,SALT, TABLE, with 38758

USDAclean %>%
  filter(VitaminC <= 100, VitaminC >= 0) %>%
  ggplot(aes(x = VitaminC)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  ggtitle('Distribution of Vitamin C')

USDAclean %>%
  select(ID, TotalFat, Protein, Carbohydrate) %>%
  pivot_longer(!ID, names_to = "type", values_to = "total") %>%
  ggplot(aes(x = total, y = type, fill = type)) +
  geom_boxplot()

USDAclean %>%
  ggplot(aes(x = TotalFat, y = Calories)) +
  geom_point() + 
  geom_smooth(method = 'lm')

USDAclean %>%
  glimpse()

USDAclean %>%
  filter(HighSodium == 1,
         HighFat == 1) #644

USDAclean %>%
  group_by(HighProtein) %>%
  summarize(mean_iron = mean(Iron, na.rm = T))

health_check <- function(df) {
  output <- df %>%
    mutate(
      healthcheck = case_when(
        HighSodium == 0 ~ 'Pass',
        HighSodium == 1 & HighSugar == 0 ~ 'Pass',
        HighSodium == 1 & HighSugar == 1 & HighFat == 0 ~ 'Pass',
        T ~ 'Fail'
      )
    )
  return(output)
}

USDAclean %>%
  health_check() %>%
  count(healthcheck) 

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

USDA_Selected_Features <- USDAclean[, c('Calories', 'Carbohydrate', 'Protein', 'TotalFat', 'Sodium')]
pairs(USDA_Selected_Features, lower.panel = panel.smooth, upper.panel = panel.cor)

model = lm(Calories ~ Carbohydrate + Protein + TotalFat + Sodium, data = USDAclean)

modelaov = anova(model)
modelaov


coef(model)

model

newrecord = data.frame(Carbohydrate = 425, Protein = 0.1, TotalFat = 40, Sodium = 430)
predict(model, newrecord)

newrecord = data.frame(Carbohydrate = 425, Protein = 0.1, TotalFat = 40, Sodium = 430*101)
predict(model, newrecord)

# If the value of Sodium increased 101 times from 430 to 43430, the value for 
# Calories would change by 13.97226. This represents a 0.72% change in the 
# value for Calories from when Sodium was equal to 430. To get this result 
# we multiply the difference in the Sodium value from before to after (43430 - 430 = 43000) 
# by the coefficient for Sodium from the model, which is 0.0003249363. 
# We use this value in our regression calculation. 
# The coefficient describes the change in the dependent variable 
# for each unit of change in the Sodium variable.


article1 = c(0.40, 0.72, 0, 0.36, 0.55)
article2 = c(0.77, 0.49, 0.66, 0.28, 0.38)
wilcox.test(article1, article2, paired = 0)

