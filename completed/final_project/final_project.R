library("dplyr")
library("janitor")
library("skimr")
library("ggplot2")

options("width"=200)
options(scipen=999)
setwd("/home/npatkhe/dsc520/completed/final_project")


df_jee03 <- read.csv("jeee16t03.csv")
df_jee03 <- df_jee03 %>% clean_names() %>%
              rename(state = state_and_type_of_government) %>%
              rename(population_k = population_2016_thousands)

df_jee03 <- df_jee03 %>% filter(population_k != "-")
df_jee03$population_k <- as.numeric(as.character(df_jee03$population_k))
str(df_jee03)
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
df_jee11$pp_average_earnings <- as.numeric(as.character(df_jee11$pp_average_earnings))
str(df_jee11)
head(df_jee11[,1:3]) 

df_consolidated <- inner_join(df_jee08, df_jee03, by = "state")
df_consolidated <- inner_join(df_consolidated, df_jee11, by = "state")

df_consolidated <- df_consolidated %>% select(-c(population))
str(df_consolidated)
skim(df_consolidated)
summary(df_consolidated)

library("usmap")
# Population on US map
plot_usmap(data = df_consolidated, values = "population_k",  color = "grey", labels=TRUE) + 
  scale_fill_continuous( low = "white", high = "skyblue", 
                         name = "Population in Thousands", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Population in Thousands by State", caption = "Source: @https://bjs.gov")
# Direct expsense 
plot_usmap(data = df_consolidated, values = "total_justice_system_pc",  color = "grey", labels=FALSE) + 
  scale_fill_continuous( low = "white", high = "green", 
                         name = "Dollars", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Justice system cost per capita by State", caption = "Source: @https://bjs.gov")

plot_usmap(data = df_consolidated, values = "total_direct_expenditure",  color = "grey", labels=FALSE) + 
  scale_fill_continuous( low = "white", high = "green", 
                         name = "Dollars in Thousands", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Direct Expense in Thousands by State", caption = "Source: @https://bjs.gov")
cor(df_consolidated$population_k,df_consolidated$total_direct_expenditure)
cor(df_consolidated$population_k,df_consolidated$total_justice_system_pc)

direct_expense_lm <-  lm(df_consolidated$total_direct_expenditure ~ df_consolidated$population_k)
summary(direct_expense_lm)

total_justice_system_pc_lm <-  lm(df_consolidated$total_justice_system_pc ~ df_consolidated$population_k)
summary(total_justice_system_pc_lm)

cor(df_consolidated$population_k,df_consolidated$tjs_average_earnings)
cor(df_consolidated$population_k,df_consolidated$pp_average_earnings)

df_consolidated$pp_jl_emp_ratio <- df_consolidated$pp_total_employees/df_consolidated$jl_total_employees
head(df_consolidated[,c('state','pp_jl_emp_ratio')],60)
head(df_consolidated[,c('state','pp_average_earnings')],60)


p <- ggplot(df_consolidated, aes(x=pp_average_earnings)) +
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue",binwidth = 500) +
  geom_density(alpha=.2, fill="#FF6666")
p
ggplot(df_consolidated, aes(x=as.numeric(population_k), y=as.numeric(pp_average_earnings))) +
  geom_point(aes(color = as.numeric(pp_average_earnings))) +
  geom_smooth(se = FALSE, method = "lm") +
  labs(
    title = "Population vs Police Earnings",
    caption = "Data from Scores.csv",
    x = "Population in Thousands",
    y = "Police Earnings"
  ) 


p <- ggplot(df_consolidated, aes(x=as.numeric(pp_average_earnings))) + 
  geom_density() +
  labs(
    title = "Density view",
    subtitle = "Scores of Regular and Sports Students",
    caption = "Data from Scores.csv"
  ) +
  geom_vline(aes(xintercept=mean(as.numeric(pp_average_earnings))),
             color="blue", linetype="dashed", size=1) 
p
+ # Add mean line
  facet_grid(~Section)
library("usmap")
library("evaluate")
plot_usmap(data = df_consolidated, values = 'pp_average_earnings',  color = 'grey', labels=FALSE) + 
  scale_fill_continuous( low = 'white', high = 'skyblue', 
                         name = 'Popularity', label = scales::comma
  ) + 
  theme(legend.position = 'right') + 
  theme(panel.background = element_rect(colour = 'black')) + 
  labs(title = 'Police Earnings by State', caption = 'Source: @littlemissdata')

library("plotly")

g <- ggplot(df_consolidated) +
  geom_sf(aes(fill=population_k)) +
  scale_fill_distiller("Population in Thousands", palette="Spectral") +
  ggtitle("Population by State")
ggplotly(g)