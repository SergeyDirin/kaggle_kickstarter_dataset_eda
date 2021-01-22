#Loading files

library(tidyverse)
library(lubridate)

d1 <- read_csv("../ks-projects-201612.csv")
d2 <- read_csv("../ks-projects-201801.csv")

#checking the difference in files
unique(substr(d1$launched,1,4))
unique(substr(d2$launched,1,4))

table(substr(d1$launched,1,4))
table(substr(d2$launched,1,4))

# no need to use 2016, 2018 is cleaned 2016 with additional information

d <- d2

#cleaning

#NA and NULLs
sapply(d, function(x) sum(is.na(x)))


#checking dates
table(substr(d$launched,1,4))

d %>% 
  filter(year(d$launched) == 1970)  


d$launched[year(d$launched) == 1970] <- d$deadline[year(d$launched) == 1970] - days(30)
