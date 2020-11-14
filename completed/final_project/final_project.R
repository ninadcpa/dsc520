library("dplyr")
library("janitor")

options("width"=200)

df_jee03 <- read.csv("jeee16t03.csv")
df_jee03 <- df_jee03 %>% clean_names() %>%
              rename(state = state_and_type_of_government) %>%
              rename(population_k = population_2016_thousands)

df_jee03 <- df_jee03 %>% filter(population_k != "-")
df_jee03 <- df_jee03[-1,]
head(df_jee03[,1:5],10)

df_jee03$state <- sub("\\(.*", "",df_jee03$state )
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
df_jee03$state <- trim(df_jee03$state)
head(df_jee03[,1:5],10)

df_jee08 <- read.csv("jeee16t08.csv")
df_jee08 <-df_jee08 %>% clean_names() %>%
               rename(population = population_2016)

df_jee08 <-filter(df_jee08, state != "Total")
head(df_jee08[,1:3]) 

df_jee11 <- read.csv("jeee16t11.csv")
df_jee11 <- df_jee11 %>% clean_names() %>% filter(state != "Total")
head(df_jee11[,1:3]) 

df_consolidated <- inner_join(df_jee08, df_jee03, by = "state")
df_consolidated <- inner_join(df_consolidated, df_jee11, by = "state")

df_consolidated %>% select(-c(population))
str(df_consolidated)
