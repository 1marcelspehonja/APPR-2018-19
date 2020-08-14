library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)

# 3. faza: Vizualizacija podatkov

# # Uvozimo zemljevid.
# zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                              pot.zemljevida="OB", encoding="Windows-1250")
# levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#   { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
# zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
# zemljevid <- fortify(zemljevid)
# 
# # Izračunamo povprečno velikost družine
# povprecja <- druzine %>% group_by(obcina) %>%
#   summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))


#graf1 Rodnost naključnih 40 držav leta 2018
drzave <- filter(zdruzeni.podatki.krajse,Leto==2018)[sample(nrow(filter(zdruzeni.podatki.krajse,Leto==2018)), 40), 1]
zdruzeni.podatki.krajse <- filter(zdruzeni.podatki.krajse, Leto %in% c(2000,2018), Drzava %in% drzave)

graf1 <- dotchart(filter(arrange(zdruzeni.podatki.krajse, Rodnost), Leto==2018)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse, Rodnost), Leto==2018)[,1], cex=.7, 
                  main="Rodnost leta 2018", xlab="Stevilo rojstev na 1000 prebivalcev")

#graf2 Rodnost naključnih 40 držav leta 2000
graf2 <- dotchart(filter(arrange(zdruzeni.podatki.krajse, Rodnost), Leto==2000)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse, Rodnost), Leto==2000)[,1], cex=.7, 
                  main="Rodnost leta 2000", xlab="Stevilo rojstev na 1000 prebivalcev")
remove(zdruzeni.podatki.krajse, drzave)


#graf3 Rodnost sedmih držav skozi leta
zdruzeni.podatki$Leto <- as.numeric(as.character(zdruzeni.podatki$Leto))
graf3 <- ggplot(zdruzeni.podatki %>% filter(Drzava %in% c('Slovenia',
                                                          'Germany',
                                                          'China', 
                                                          'Japan',
                                                          'Nigeria', 
                                                          'Iraq',
                                                          'South Africa',
                                                          'India')),
                aes(x=Leto,y=Rodnost,color=Drzava)) +
  geom_point(shape=21, size=2.5, fill= "black") +
  geom_line(size = 1.2) + 
  theme_classic() +
  labs(x="Leto", y ="Stevilo rojstev na 1000 prebivalcev", title ="Rodnost") +
  theme(axis.title=element_text(size=11), plot.title=element_text(size=15, hjust=0.5)) + 
  scale_colour_manual(values=c("dodgerblue3","purple","springgreen4","black","orangered3","dark grey","yellow", "tan1"), 
                    name="Drzava",
                    breaks=c('Slovenia', 'Germany', 'China', 'Japan', 'Nigeria',  'Iraq', 'South Africa', 'India'),
                    labels=c('Slovenia', 'Germany', 'China', 'Japan', 'Nigeria', 'Iraq', 'South Africa', 'India'))
  

#graf4 rodnost skozi leta glede na religijo
religija.povprecje$Leto <- as.numeric(as.character(religija.povprecje$Leto))
graf4 <- ggplot(religija.povprecje,
                aes(x=Leto, y=religija.povprecje$"Povprecje rodnost", color=Religija)) +
  geom_point(shape=1, size=2.5, fill= "black") +
  geom_line(size = 1.2) + 
  theme_bw() +
  labs(x="Leto", y ="Povprecno stevilo rojstev na 1000 prebivalcev", title ="Rodnost glede na vero") +
  scale_colour_manual(values=c("dodgerblue3","purple","springgreen4","black","tan1","dark grey","maroon"))


#graf5 Rodnost skozi leta glede na celino

#celina.povprecje$Leto <- as.numeric(as.character(celina.povprecje$Leto))
graf5 <- ggplot(zdruzeni.podatki %>% filter(Leto==2018) %>% drop_na(Celina), aes(x=Celina, y=Rodnost)) +
  geom_boxplot() + theme_classic() +
  scale_y_log10() + geom_point() +
  labs(x="Celina", y ="Stevilo rojstev na 1000 prebivalcev", title ="Rodnost glede na celino leta 2018") +
  theme(plot.title = element_text(hjust = 0.5)) 


#graf6 
graf6 <- ggplot(zdruzeni.podatki %>% filter(Leto==2018), 
                aes(x=filter(zdruzeni.podatki,Leto==2018)$"GDP PPP ($)", y=filter(zdruzeni.podatki,Leto==2018)$Rodnost)) +
  theme_classic() +
  geom_point(shape=21, size=2.5, fill= "black") +
  labs(x="BDP PPP ($)", y ="Stevilo rojstev na 1000 prebivalcev", title ="Povezava rodnosti in BDP-ja") +
  geom_smooth(method = 'loess', se = FALSE)


#graf7






