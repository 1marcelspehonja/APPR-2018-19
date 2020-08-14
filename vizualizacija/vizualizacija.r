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


#graf1
zdruzeni.podatki.krajse1 <- filter(zdruzeni.podatki.krajse,Leto==2018)[sample(nrow(filter(zdruzeni.podatki.krajse,Leto==2018)), 40), ]

graf1 <- dotchart(filter(arrange(zdruzeni.podatki.krajse1,Rodnost), Leto==2018)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse1,Rodnost), Leto==2018)[,1], cex=.7, 
                  main="Rodnost", xlab="Stevilo rojstev na 1000 prebivalcev")
remove(zdruzeni.podatki.krajse1)

#graf2
zdruzeni.podatki.krajse2 <- filter(zdruzeni.podatki.krajse,Leto==2000)[sample(nrow(filter(zdruzeni.podatki.krajse,Leto==2018)), 40), ]

graf2 <- dotchart(filter(arrange(zdruzeni.podatki.krajse2,Rodnost), Leto==2000)$Rodnost,
                  labels=filter(arrange(zdruzeni.podatki.krajse2,Rodnost), Leto==2000)[,1], cex=.7, 
                  main="Rodnost", xlab="Stevilo rojstev na 1000 prebivalcev")
remove(zdruzeni.podatki.krajse2)


#graf3 
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
  
#graf4
religija.povprecje$Leto <- as.numeric(as.character(religija.povprecje$Leto))
graf4 <- ggplot(religija.povprecje,
                aes(x=Leto, y=religija.povprecje$"Povprecje rodnost", color=Religija)) +
  geom_point(shape=1, size=2.5, fill= "black") +
  geom_line(size = 1.2) + 
  theme_bw() +
  labs(x="Leto", y ="Povprecno stevilo rojstev na 1000 prebivalcev", title ="Rodnost glede na vero") +
  scale_colour_manual(values=c("dodgerblue3","purple","springgreen4","black","tan1","dark grey","maroon"))


#graf5


