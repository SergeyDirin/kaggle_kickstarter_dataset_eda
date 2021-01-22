
d_summ <- d %>%
  count(main_category, state, wt = NULL, sort = T)

ggplot(main_cat) +
  geom_bar(aes(x = reorder(main_category, n), y = n), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()

ggplot(main_cat) +
  geom_bar(aes(x = reorder(main_category, n), y = n, fill = state), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()
