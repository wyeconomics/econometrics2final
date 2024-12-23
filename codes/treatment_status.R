# treatment year data per each emd(읍면동)

# Preambles----
library(tidyverse)


# Data Loading----
data <- read_csv('data/subway_1fin.csv')

ggplot(data, aes(year_built, fill = factor(level), group = factor(level))) +
  geom_bar() +
  scale_fill_discrete(name = NULL, labels = c('underground', 'on-ground')) +
  theme_minimal() +
  theme(legend.position = 'bottom')
ggsave('figures/f1.png')

data %>% count(year_built) %>% arrange(year_built)
data %>% group_by(locationD) %>% summarise(time = min(year_built)) %>% 
  ungroup() %>% count(time) %>% arrange(time) %>% print(n=57)


ggplot(data %>% count(locationD), aes(n)) + geom_bar() +
  labs(x = 'number of subway stations', y = 'number of emds') +
  theme_minimal()
ggsave('figures/f2.png')

# Treatment dataset
df1 <- data %>% 
  group_by(locationD) %>% 
  summarise(time = min(year_built)) %>% ungroup() %>% 
  filter(!is.na(time)) %>% 
  left_join(data %>% select(name, level, year_built, locationD), by = c('time' = 'year_built', 'locationD'))

df1 <- df1 %>% group_by(locationD) %>% distinct(level, .keep_all = T) %>% 
  mutate(n = n_distinct(level),
         level = if_else(n==2 , 2, level)) %>% 
  distinct(level, .keep_all = T) %>% ungroup() %>% select(-n)

write_rds(df1, 'data/treatment(emd).rds')

# Treatment dataset(limited to one station emds)
temp <- data %>% count(locationD) %>% mutate(exclude = if_else(n>1, 1, 0))

df2 <- data %>%
  group_by(locationD) %>% 
  summarise(time = min(year_built)) %>% ungroup() %>% 
  filter(!is.na(time)) %>% 
  left_join(data %>% select(name, level, year_built, location, locationD), by = c('time' = 'year_built', 'locationD')) %>% 
  left_join(temp, by = c('locationD')) %>% 
  distinct(locationD, .keep_all = T)
df2
write_rds(df2, 'data/treatment(emd)-2.rds')
