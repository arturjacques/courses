library(readr)

covid_df <- read_csv("covid19.csv")

dim(covid_df)
vector_cols <- colnames(covid_df)
print(vector_cols)

head(covid_df)


library(tibble)
glimpse(covid_df)


library(dplyr)
covid_df_all_states <- covid_df %>% 
  filter(Province_State == "All States") %>% 
  select(-Province_State)

covid_df_all_states_daily <- select(covid_df_all_states,
                                    Country_Region, active, hospitalizedCurr,
                                    daily_tested, daily_positive)


covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%  
  summarize(tested = sum(daily_tested),
           positive = sum(daily_positive),
           active = sum(active),
           hospitalized = sum(hospitalizedCurr)) %>% 
  arrange(-tested)

covid_df_all_states_daily_sum

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <-  covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <-  covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

positive_tested_top_3 <- sort(positive_cases/tested_cases, decreasing = T)
print(positive_tested_top_3)
positive_tested_top_3 <- positive_tested_top_3[c(1,2,3)] %>% round(2)
print(positive_tested_top_3)

united_kingdom <-  c(0.11, 1473672, 166909, 0, 0)
united_states  <-  c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)

colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

print(covid_mat)

question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

dataframes <- c(covid_df, covid_df_all_states, covid_df_all_states_daily, 
                covid_df_all_states_daily_sum)
matrices <- c(covid_mat)
vectores <- c(countries, tested_cases, positive_cases, active_cases,
              hospitalized_cases, united_kingdom, united_states, turkey)

data_structure_list <- list("dataframes" = dataframes, "matrix"= matrices, "vector" = vectores)

covid_analysis_list <- list(question, answer, data_structure_list)

print(covid_analysis_list[2])
