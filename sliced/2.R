df <- c(1:300)
df1 <- df
df2 <- df

library(doParallel)  
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  

data <- crossing(df, df1, df2) %>%
  filter(df<df1,
         df<df2,
         df1<df2) %>%
  mutate(a = atan(1/df),
         b = atan(1/df1),
         c = atan(1/df2),
         d = a == (b+c)) %>%
  filter(d == T)

