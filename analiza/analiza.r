# 4. faza: Analiza podatkov

# podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                 gostota.naselij=naselja/povrsina) %>%
#   left_join(povprecja, by="obcina")
# row.names(podatki) <- podatki$obcina
# podatki$obcina <- NULL
# 
# # Število skupin
# n <- 5
# skupine <- hclust(dist(scale(podatki))) %>% cutree(n)

# Država Nigeria
#Izračun podatkov za napoved
podatki.napoved <- zdruzeni.podatki %>% filter(Drzava == 'Nigeria', Leto %in% 2000:2018)
fit <- lm(data = podatki.napoved, Rodnost ~ Leto)
prihodnost <- data.frame(Leto=seq(2019, 2025))
predict(fit, prihodnost)
napoved <- prihodnost %>% mutate(Rodnost=predict(fit, .))


#Graf
graf.napoved <- ggplot(podatki.napoved, aes(x=Leto, y=Rodnost)) +
  theme(axis.title=element_text(size=11), plot.title=element_text(size=15, hjust=0.5)) +
  geom_col(data=napoved, aes(x=Leto, y=Rodnost), fill="grey43", alpha=0.6, width = 0.6, colour="grey0") +
  labs(title="Napoved rodnosti Nigerija", y="Rodnost") +
  geom_col(fill="dodgerblue", width = 0.6, colour="lightblue2") +
  geom_smooth(method=lm, colour = "red", linetype = "11", size=2, fullrange = TRUE, se=FALSE)
print(graf.napoved)


