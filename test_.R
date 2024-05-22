x <- DB_forGraph %>% 
  select(region) %>%
  rename(selected_var = region) %>%
  group_by(selected_var) %>%
  mutate(nb_top = n(), 
         pop = "A") %>%
  ungroup() %>%
  bind_rows(DB_forGraph %>% 
  select(top_defaillance,region) %>%
  rename(selected_var = region) %>%
  filter(top_defaillance == 1) %>% 
  group_by(selected_var) %>%
  mutate(nb_top = n(), 
         pop = "B") %>%
  ungroup() %>% 
  select(selected_var, nb_top, pop)
  ) %>%
  group_by(selected_var, pop) %>% 
  mutate(nb_var = n(), 
         perc = round(nb_var / nb_top,2)*100) %>% 
  select(selected_var, pop,nb_top, nb_var, perc) %>% 
  distinct() 

xbis <- x %>% 
  select(selected_var,top_defaillance, perc) %>%
  pivot_wider(names_from = "top_defaillance", 
                          values_from = "perc")
xbis$top_statut <- ifelse(
  (xbis$`1`-xbis$`0`)>1,"sur-representé",
  ifelse((xbis$`0`-xbis$`1`)>1, "sous-representé", 
         "equi-representé"))

x <- x %>% left_join(xbis %>% select(selected_var, top_statut), by= "selected_var")


x$top_defaillance <- factor(x$top_defaillance, 
                            levels = c(0,1), 
                            labels = c("Non en difficulté","En difficulté"))

ggplot(data=x, aes(x=selected_var, y = perc, col = top_statut, fill = top_statut)) +
  theme_test() +
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = perc), nudge_y = 1) + 
  labs(title = "Ventilation de la variable x par defaillance") + 
  xlab("") + 
  ylab("Proportion (en %)") +
  facet_wrap(~top_defaillance) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


