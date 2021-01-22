#Loading files

d1 <- read.csv("../ks-projects-201612.csv")
d2 <- read.csv("../ks-projects-201801.csv")

str(d1)
str(d2)

summary(d1)
dim(d1)

summary(d2)
dim(d2)

#checking nas
sapply(d1, function(x) sum(is.na(x)))

d1[is.na(d1$name),]
d1[is.na(d1$name),"name"] <- "Unknown"

sapply(d1, function(x) sum(is.na(x)))
dim(d1)

d1[-is.na(d1$x.3),]

head(unique(d1$deadline))

nrow(d1)
nrow(d1) - nrow(d1[is.null(d1$x),])
nrow(d1) - nrow(d1[is.null(d1$x.1),])
nrow(d1) - nrow(d1[is.null(d1$x.2),])
nrow(d1) - nrow(d1[is.null(d1$x.3),])

d1$x

d1[-is.null(d1$x.3),]


colnames(d1)

d1 <- d1[,-c(14,15,16,17)]

str(d1)

#testing better presentation
table(sapply(d1, function(x) sum(is.na(x))))
b <- barplot(sapply(d1, function(x) sum(is.na(x))), las=1, horiz = T)

# best presentation
sapply(d1, function(x) sum(is.na(x)))


#checking dates

head(d1$launched)

unique(substr(d1$launched,1,4))

nrow(d1) - nrow(d1[!is.na(as.numeric(substr(d1$launched,1,4))),])

d1[is.na(as.numeric(substr(d1$launched,1,4))),]

#remove the not correctly read names
d1 <- d1[!is.na(as.numeric(substr(d1$launched,1,4))),]



unique(substr(d1$launched,1,4))
nrow(d1[as.numeric(substr(d1$launched,1,4))>2020,])
head(d1[as.numeric(substr(d1$launched,1,4))>2020,])


library(tidyverse)
t1 <- read_csv("../ks-projects-201612.csv")
t2 <- read_csv("../ks-projects-201801.csv")

library(lubridate)
unique(substr(t2$launched,1,4))
unique(substr(t1$launched,1,4))

nrow(t1)
nrow(t2)

table(substr(t1$launched,1,4))
table(substr(s2$launched,1,4))

d2 <- d2 %>% 
  filter(as.numeric(substr(d2$launched,1,4)) == 1970)



t1 <- d %>% 
  filter(year(launched) == 1970)  

d[1:5,]

t1 

t1 <- union_all(t1, d[1:5,])

t1

#backup t1
t2 <- t1

t1 %>% 
  mutate(
    launched = as_datetime(ifelse(year(launched) == 1970, deadline - days(30), launched))
  )

as_datetime(444)

d$launched[year(d$launched) == 1970] <- d$deadline[year(d$launched) == 1970] - days(30)

t1 %>% 
  mutate(
    launched = ifelse(year(launched) == 1970, as_datetime(deadline - days(30)), launched)
  ) %>%
  mutate(
    launched = as_datetime(launched)
  )

#checking columns
colnames(d)

nrow(table(d$name))

table(d$currency)

barplot(table(d$category))
barplot(table(d$main_category), horiz=T, las=1)


opar <- par(no.readonly=TRUE)
par(mar = c(5, 6, 4, 2))
par(opar)

opar <- par(no.readonly=TRUE)
par(mar = c(5, 6, 4, 2))
barplot(table(d$main_category), horiz=T, las=1)
par(opar)



summary(d)

barplot(table(d$main_category))

d %>%
  group_by(main_category) %>%
  summarise(cnt = count()) %>%
  sort(cnt, descending = T)

d %>%
  count(main_category, state, wt = NULL, sort = T)

main_cat <- d %>%
  count(main_category, state, wt = NULL, sort = T)
  

main_cat
main_cat$n

ggplot(d, aes(main_category)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(d) +
  geom_bar(aes(main_category))+
  coord_flip()

ggplot(main_cat) +
  geom_bar(aes(x = reorder(main_category, n), y = n, fill = state), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()



