library(tidyverse)

data <- read_csv('data/소상공인_우용_변환.csv')
data %>% names()
data <- data %>% filter(`상권업종대분류명` %in% c('음식', '숙박'))
write_csv(data, 'data/소상공인_우용_변환.csv')

data <- read_csv('data/Seoul Subway Facilities.csv', locale = locale(encoding = "EUC-KR"))
data %>% head()

