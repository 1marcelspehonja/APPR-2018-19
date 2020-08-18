library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(tidyverse)

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
drzave <- filter(zdruzeni.podatki.krajse,Leto==2018)[sample(nrow(filter(zdruzeni.podatki.krajse,Leto==2018)), 30), 1]
zdruzeni.podatki.krajse1 <- filter(zdruzeni.podatki.krajse, Leto %in% c(2000,2018), Drzava %in% drzave)

graf1 <- dotchart(filter(arrange(zdruzeni.podatki.krajse1, Rodnost), Leto==2018)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse1, Rodnost), Leto==2018)[,1], cex=.7, 
                  main="Rodnost leta 2018", xlab="Stevilo novorojenih otrok na 1000 prebivalcev")

#graf2 Rodnost naključnih 40 držav leta 2000
graf2 <- dotchart(filter(arrange(zdruzeni.podatki.krajse1, Rodnost), Leto==2000)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse1, Rodnost), Leto==2000)[,1], cex=.7, 
                  main="Rodnost leta 2000", xlab="Stevilo novorojenih otrok na 1000 prebivalcev")
rm(zdruzeni.podatki.krajse1, drzave)



#graf3 Rodnost osmih držav skozi leta
zdruzeni.podatki$Leto <- as.numeric(as.character(zdruzeni.podatki$Leto))
graf3 <- ggplot(zdruzeni.podatki %>% filter(Drzava %in% c('Slovenia',
                                                          'Germany',
                                                          'China', 
                                                          'Japan',
                                                          'Nigeria', 
                                                          'Iraq',
                                                          'South Africa',
                                                          'India',
                                                          'Brazil')),
                aes(x=Leto,y=Rodnost,color=Drzava)) +
  geom_point(shape=21, size=2.5, fill= "black") +
  geom_line(size = 1.2) + 
  theme_classic() +
  labs(x="Leto", y ="Stevilo novorojenih otrok na 1000 prebivalcev", title ="Rodnost izbranih držav") +
  theme(axis.title=element_text(size=11), plot.title=element_text(size=13, hjust=0.5)) + 
  scale_colour_manual(values=c("orangered3","dark grey","yellow","tan1","orchid2","springgreen4","dodgerblue3","purple","black"), 
                    name="Drzava",
                    breaks=c('Nigeria', 'Iraq', 'South Africa', 'India', 'Brazil', 'China', 'Germany', 'Slovenia', 'Japan'),
                    labels=c('Nigeria', 'Iraq', 'South Africa', 'India', 'Brazil', 'China', 'Germany', 'Slovenia', 'Japan'))
print(graf3)


#graf4 rodnost skozi leta glede na religijo
religija.povprecje$Leto <- as.numeric(as.character(religija.povprecje$Leto))
graf4 <- ggplot(religija.povprecje,
                aes(x=Leto, y=religija.povprecje$"Povprecje rodnost", color=Religija)) +
  geom_point(shape=1, size=2.5, fill= "black") +
  geom_line(size = 1.2) + 
  theme_bw() +
  labs(x="Leto", y ="Povprecno stevilo novorojenih otrok na 1000 prebivalcev", title ="Rodnost glede na vero") +
  scale_colour_manual(values=c("dodgerblue3","purple","springgreen4","black","tan1","dark grey","maroon"))
print(graf4)


#graf5 Rodnost skozi leta glede na celino
graf5 <- ggplot(zdruzeni.podatki %>% filter(Leto==2018) %>% drop_na(Celina), aes(x=Celina, y=Rodnost)) +
  geom_boxplot() + theme_classic() +
  scale_y_log10() + geom_point() +
  labs(x="Celina", y ="Stevilo novorojenih otrok na 1000 prebivalcev", 
       title ="Rodnost glede na celino leta 2018") +
  theme(plot.title = element_text(hjust = 0.5)) 
print(graf5)


#graf6 Povezava med Rodnostjo in BDP, Pricakovano zivljenjsko dobo ter umrljivostjo novorojenckov
mid<-mean(filter(zdruzeni.podatki,Leto==2018)$"Pricakovana zivljenjska doba")
graf6 <- ggplot(zdruzeni.podatki %>% filter(Leto==2018), 
                aes(x = filter(zdruzeni.podatki,Leto==2018)$"GDP PPP ($)", 
                    y = filter(zdruzeni.podatki,Leto==2018)$Rodnost,
                    color = filter(zdruzeni.podatki,Leto==2018)$"Pricakovana zivljenjska doba",
                    size=filter(zdruzeni.podatki,Leto==2018)$"Umrljivost novorojenckov na 1000 rojstev")) +
  theme_classic() +
  geom_point() +
  scale_size(range = c(1,6)) +
  labs(x="BDP PPP ($)", y ="Stevilo novorojenih otrok na 1000 prebivalcev", 
       title ="Povezava rodnosti, BDP, zivljenjske dobe ter umrljivostjo novorojenckov leta 2018", 
       colour="Pricakovana zivljenjska doba",
       size="Umrljivost novorojenckov \nna 1000 rojstev") +
  theme(plot.title=element_text(size=10, hjust=0.5)) +
  geom_smooth(method = 'loess', se = FALSE, color="black", size=1.5) +
  scale_color_gradient2(midpoint=mid, low="blue", high="yellow", mid="red")
print(graf6)
rm(mid)


#graf7 Povezava med Rodnostjo in stopnjo izobrazbe
zdruzeni.izobrazba <- zdruzeni.podatki %>% drop_na("procent vpisa v srednjo solo")
graf7 <- ggplot(zdruzeni.izobrazba %>% filter(Leto==2018), 
                aes(x=filter(zdruzeni.izobrazba,Leto==2018)$"procent vpisa v srednjo solo", 
                    y=filter(zdruzeni.izobrazba,Leto==2018)$Rodnost)) +
  theme_classic() +
  geom_point(size=2.5) +
  labs(x="Procent vpisa v srednjo solo", y ="Stevilo novorojenih otrok na 1000 prebivalcev", 
       title ="Povezava rodnosti in stopnje izobrazbe") +
  geom_smooth(method = 'loess', se = FALSE, color="red", size=1.2)
print(graf7)
#remove(zdruzeni.izobrazba)


#graf8 Povezava med rodnostjo in številom splavov
zdruzeni.podatki$new <- zdruzeni.podatki$"St. splavov na leto" / zdruzeni.podatki$Prebivalci * 10000
graf8 <- ggplot(zdruzeni.podatki %>% filter(Leto==2010), 
                aes(x=filter(zdruzeni.podatki,Leto==2010)$new, 
                    y=filter(zdruzeni.podatki,Leto==2010)$Rodnost)) +
  theme_classic() +
  geom_point(size=2.5) +
  labs(x="Stevilo splavov na 10.000 prebivalcev na leto", 
       y ="Stevilo novorojenih otrok na 1000 prebivalcev", 
       title ="Povezava rodnosti in stevila splavov leta 2010") +
  geom_smooth(method = 'loess', se = FALSE, color="red", size=1.2)
print(graf8)



# Zemljevid sveta - Rodnost
source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")

zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", mapa = "zemljevidi", pot.zemljevida = "", encoding = "UTF-8") %>% fortify()

rodnost.2018 <- zdruzeni.podatki %>% filter(Leto==2018) %>% select(c(Drzava, Rodnost) )%>% 
  mutate_all(funs(str_replace(., "Russian Federation", "Russia"))) %>%
  mutate_all(funs(str_replace(., "United States", "United States of America")))%>%
  mutate_all(funs(str_replace(., "Egypt, Arab Rep.", "Egypt")))%>%
  mutate_all(funs(str_replace(., "Congo, Dem. Rep.", "Dem. Rep. Congo")))%>%
  mutate_all(funs(str_replace(., "Central African Republic", "Central African Rep.")))
rodnost.2018$Rodnost <- as.numeric(as.character(rodnost.2018$Rodnost))

zemljevid.rodnost <- ggplot() + geom_polygon(data=zemljevid %>% left_join(rodnost.2018, by=c("NAME"="Drzava")),
                                                aes(x=long, y=lat, group=group, fill=Rodnost),alpha = 0.8, color = "black") + 
  coord_cartesian(xlim=c(-150, 170), ylim=c(-50, 80)) +
  scale_fill_gradient2(low ="yellow", mid = "red", high = "blue",midpoint = 25, na.value = "white")+
  xlab("") + ylab("") + ggtitle("Rodnost po drzavah sveta") +
  theme(plot.title = element_text(hjust = 0.5))



#____________________________________________________________________________________________
rodnost.vse <- zdruzeni.podatki %>% select(c(Drzava, Leto, Rodnost) )%>% 
  mutate_all(funs(str_replace(., "Russian Federation", "Russia"))) %>%
  mutate_all(funs(str_replace(., "United States", "United States of America")))%>%
  mutate_all(funs(str_replace(., "Egypt, Arab Rep.", "Egypt")))%>%
  mutate_all(funs(str_replace(., "Congo, Dem. Rep.", "Dem. Rep. Congo")))%>%
  mutate_all(funs(str_replace(., "Central African Republic", "Central African Rep.")))
rodnost.vse$Rodnost <- as.numeric(as.character(rodnost.vse$Rodnost))



