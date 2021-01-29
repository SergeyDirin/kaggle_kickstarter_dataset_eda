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

# count of sub categories
d %>%
  count(main_category, state, wt = NULL, sort = T)

d %>% 
  group_by(main_category) %>%
  summarise(subCats = length(unique(category)))

subcats_count <- d %>% 
  group_by(main_category) %>%
  summarise(subCats = length(unique(category)))

ggplot(subcats_count) +
  geom_bar(aes(x = reorder(main_category, subCats), y = subCats), stat = "identity") + 
  xlab("Main Category") + 
  ylab("SubCategory Count")+
  coord_flip()

#currency
curr_summ <- d %>%
  count(currency, state, wt = NULL, sort = T)

ggplot(curr_summ) +
  geom_bar(aes(x = reorder(currency, n), y = n, fill = state), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()

#deadline
deadline_by_month <- d %>% 
  filter(state %in% c("canceled", "failed", "successful")) %>%
  mutate(deadline_month = floor_date(deadline, unit = "month")) %>% 
  count(deadline_month, state, wt = NULL, sort = T)

ggplot(deadline_by_month) +
  geom_line(aes(x = deadline_month, y=n)) +
  facet_grid(state~.) +
  xlab("Deadline") + 
  ylab("Projects Count") + 
  ggtitle("Projects over time dynamics")

launched_by_month <- d %>% 
  filter(state %in% c("canceled", "failed", "successful")) %>%
  mutate(launched_month = floor_date(launched, unit = "month")) %>% 
  count(launched_month, state, wt = NULL, sort = T)

ggplot(launched_by_month) +
  geom_line(aes(x = launched_month, y=n)) +
  facet_grid(state~.) +
  xlab("Launched") + 
  ylab("Projects Count") + 
  ggtitle("Projects over time dynamics")

summary(d)
# goal

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal <= 100000) %>%
  ggplot() +
    geom_histogram(aes(y = goal))+
    facet_grid(state~.) +
    coord_flip() + 
  ggtitle("Distribution of Goals below $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal >= 100000 & goal < 1000000) %>%
  ggplot() +
    geom_histogram(aes(y = goal))+
    facet_grid(state~.) +
    coord_flip() + 
    ggtitle("Distribution of Goals from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal >= 1000000) %>%
  ggplot() +
    geom_histogram(aes(y = goal))+
    facet_grid(state~.) +
    coord_flip() + 
    ggtitle("Distribution of Goals over $1,000,000")


# pledged

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged <= 100) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged below $100")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 100 & pledged <= 100000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $100 till $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 100000 & pledged <= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged over $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed")) %>%
  filter(pledged >= 1000000)

summary(d)

#backers
d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers <= 50) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers below 50")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 50 & backers < 1000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 1000 & backers < 25000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 25000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  group_by(state)  %>%
  summarise(max_backers = max(backers)) 

t1 <- d %>%
  filter(state %in% c("canceled")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 
t2 <- d %>%
  filter(state %in% c("failed")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 
t3 <- d %>%
  filter(state %in% c("successful")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 

t2

t1_2 <- left_join(t1,t2, by = "backers")
backers_counts <- left_join(t1_2, t3, by = "backers")

colnames(backers_counts) <- c("backers", "canceled", "failed", "successful")
backers_counts[20:25,]

summary(d)

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers <= 25) %>%
  group_by(state, backers)  %>%
  summarise(n = n()) %>%
  ggplot(aes(x=backers, y=n, group=state)) +
  geom_line(aes(color=state),size = 1.3)

length(unique(d$country))

d %>%
  count(country) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(country, n), y = n), stat = "identity") + 
  xlab("Country") + 
  ylab("Projects Count")+
  coord_flip() +
  ggtitle("Counties by number of projects")

#is being over the goal success?
d %>%
  filter(pledged < goal & state == "successful") %>%
  mutate(over_goal = goal - pledged) %>%
  select( ID, main_category, launched, goal, over_goal, backers)

d %>%
  filter(pledged > goal & state != "successful") %>%
  mutate(over_goal = goal - pledged) %>%
  count(state) %>%
  ggplot(aes(x = state, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.25)

# what is undefined state?
d %>%
  filter(state == "undefined" & backers > 0) %>%
  mutate(over_goal = goal - pledged) 
# all undefined have 0 backers

# get distribution on projects without backers

d %>%
  filter(state == "undefined") %>%
  mutate(launched_year = year(launched)) %>%
  count(launched_year) %>%
  ggplot(aes(x = launched_year, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.25)



format_money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

# check the projects that made money



d %>%
  filter(state == "successful") %>%
  summarise(mean_goal = mean(usd_goal_real))



d %>%
  filter(state == "successful") %>%
  group_by(main_category) %>%
  summarise(mean_goal = mean(usd_goal_real)) %>%
  ggplot(aes(x = reorder(main_category, mean_goal), y = mean_goal)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = format_money(mean_goal), fontface = "bold"), hjust = "inward") +
  ylab("Average Goal") +
  xlab("Main Category") +
  geom_hline(yintercept = 9533) +
  annotate(geom = "text", y = 9200, x = 1, label = "$9,533 Overall Average", angle = 90, 
           hjust = "inward", vjust = "bottom", nudge_y = 2) +
  ggtitle("Average Goals by Categoory")



# percentage of successful by category

d %>%
  group_by(main_category) %>%
  summarise(
    count = n(), 
    success_count = sum(state == "successful"),
    success_rate = sum(state == "successful") / n()
    ) %>%
  ggplot(aes(x = reorder(main_category, success_rate), y = success_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = paste0(formatC(success_rate, format="f", digits = 2),"%")), nudge_y = 0.025) +
  xlab("Main Category") +
  ylab("Success Rate") +
  ggtitle("Success Rate by Category")


d %>%
  group_by(main_category) %>%
  summarise(
    pledged = sum(usd_pledged_real), 
    goal = sum(usd_goal_real),
  ) %>%
  pivot_longer(!main_category, names_to = "pledged_goal", values_to = "amount") %>%
  ggplot(aes(x = reorder(main_category, amount), y = amount, fill = pledged_goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_text(aes(label = paste(format_money(amount/1000000),"M")), size = 3.8, 
            position = position_dodge(width = 1), hjust = "inward") +
  xlab("Main Category") +
  ylab("Amount in USD") +
  ggtitle("Goal and Pledget Amount Comparison")

summary(d)
dim(d)

d %>% 
  arrange(desc(backers)) %>%
  top_n(20, backers) %>%
  ggplot(aes(x = backers, y = usd_pledged_real)) +
  geom_point(aes(color = main_category, shape = main_category)) + scale_color_brewer(palette="Dark2") +
  geom_text(aes( label = paste0 ("$",round(usd_pledged_real / backers, 0) )), vjust = "bottom", nudge_y = 550000) +
  xlab("Backers") +
  ylab("Pledged USD $") +
  ggtitle("Average Pledge for top 20 Backed Porjects")


#top backed Project
d %>%
  top_n(1, backers) %>%
  t()
