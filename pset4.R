library(dbplyr)
library(tidyverse)
library(janitor)
library(gt)
library(lubridate)
library(readxl)

poll <- read_csv("ps_4_elections-poll-nc09-3.csv")

# Question 1

# Madlib 1 
dem <- poll %>% 
  filter(response == "Dem")
nrow(dem)


# Madlib 2
rep <- poll %>% 
  filter(hrep_fav == "Favorable")

undecided <- poll %>% 
  filter(response == "Und")

nrow(rep) - nrow(undecided)


# Madlib 3 
gender_diff <- poll %>%
  filter(gender != gender_combined)
nrow(gender_diff)

# Madlib 4
white_diff <- poll %>% 
  filter(race_eth == "White") %>% 
  filter(file_race_black != "White")
nrow(white_diff)

# Madlib 5
rep_time <- poll %>% 
  filter(response == "Rep") %>% 
  arrange(timestamp) %>% 
  slice(1) %>% 
  pull(timestamp)

dem_time <- poll %>% 
  filter(response == "Dem") %>% 
  arrange(timestamp) %>% 
  slice(1) %>% 
  pull(timestamp)

minute(rep_time) - minute(dem_time)

# Quesiton 2
table <- poll %>% 
  select(response, final_weight, race_eth) %>% 
  group_by(race_eth, response) %>% 
  summarize(total = sum(final_weight)) %>%   
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  spread(key =  response, value = total, fill=0) %>% 
  mutate(all = Dem + Rep + Und + `3`) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all) %>% 
  select(-all, -`3`) %>% 
  arrange(factor(race_eth, levels = c("White","Black","Hispanic","Asian","Other"))) %>% 
  ungroup()

# gt() %>% 
#   tab_header(
#     title = "Polling Results in North Carolina 9th Congressional District") %>% 
#   
#   cols_label(
#     race_eth = "Race",
#     Dem = "DEM.",
#     Rep = "REP.",
#     Und = "UND."
#   ) %>%
#   
#   fmt_percent(columns = vars(Dem, Rep, Und),
#               decimals = 0) %>% 
#   as_raw_html() %>% as.character() %>% cat()
#   
  
# Question 3
violin <- poll %>%
  select(final_weight, educ) %>% 
  filter(educ != "[DO NOT READ] Refused")

violin$educ <- fct_rev(factor(violin$educ, levels = c("Graduate or Professional Degree", "Bachelors' degree", "Some college or trade school", "High school", "Grade school")))


ggplot(violin, aes(x=educ, y= final_weight)) + geom_violin() +  geom_jitter(alpha = .5, width = .2) +  coord_flip() + labs(title ="More Educated Matter Less in North Carolina 9th", subtitle ="Poll gives more weight for people who are less likely to participate in polls", x = NULL, y = "Weight Given to Respondent in Calculating Poll Results", caption="New York Times Upshot/Siena College 2018 live polls" )


# Question 4
response_by_ager <- poll %>% 
  filter(ager != "[DO NOT READ] Refused") %>% 
  filter(response != 3) %>%
  group_by(ager , response) %>% 
  summarize(N = n()) %>% 
  mutate(freq = N / sum(N), pct = round((freq*100), 0))
  
response_by_ager %>% 
  summarize(total = sum(pct))

ggplot(response_by_ager, aes(x = ager, y = pct, fill = response)) + 
  geom_col(position = "dodge2") + labs(x = "Age Group",y = "Percent of Responses", fill = "Response") +
  scale_fill_manual(values = c("blue", "red", "grey")) +
  theme(legend.position = "top")

  