library(dplyr)
library(readr)
library(tidyr)
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)


# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke o BDP iz csv dokumenta
podatki.gdp <- read_csv("podatki/GDP_PPP.csv", skip=5, col_names=c('Drzava', 'Kratica', 'GDP PPP ($)', 'Koda', 1960:2019), 
                        na=":",locale=locale(encoding="Windows-1250"))

podatki.gdp <- podatki.gdp %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)

podatki.gdp <- melt(podatki.gdp, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='GDP PPP ($)', variable.name='Leto', 
                    na.rm=FALSE) %>% arrange(Drzava)



# Funkcija, ki uvozi podatke o rodnosti
podatki.rodnost <- read_csv("podatki/število_rojstev_na_1000.csv", skip=5, col_names=c('Drzava', 'Kratica', 'St. rojstev na 1000', 'Koda', 1960:2019), 
                               na=":",locale=locale(encoding="Windows-1250"))

podatki.rodnost <- podatki.rodnost %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)

podatki.rodnost <- melt(podatki.rodnost, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='St. rojstev na 1000 preb.', variable.name='Leto', 
                                 na.rm=FALSE) %>% arrange(Drzava)



# Funkcija, ki uvozi podatke pričakovani življenjski dobi
podatki.zivlj.doba <- read_csv("podatki/pricakovana_zivlj_doba.csv", skip=5, col_names=c('Drzava', 'Kratica', 'Pricakovana zivlj. doba', 'Koda', 1960:2019), 
                            na=":",locale=locale(encoding="Windows-1250"))

podatki.zivlj.doba <- podatki.zivlj.doba %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)

podatki.zivlj.doba <- melt(podatki.zivlj.doba, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='Pricakovana zivljenjska doba', variable.name='Leto', 
                        na.rm=FALSE) %>% arrange(Drzava)



# Funkcija, ki uvozi podatke o umrljivosti novorojenčkov
podatki.umrljivost.novorojenckov <- read_csv("podatki/umrljivost_novorojenckov_na_1000.csv", skip=5, col_names=c('Drzava', 'Kratica', 'St. smrti novorojenckov na 1000 rojstev', 'Koda', 1960:2019), 
                            na=":",locale=locale(encoding="Windows-1250"))

podatki.umrljivost.novorojenckov <- podatki.umrljivost.novorojenckov %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)

podatki.umrljivost.novorojenckov <- melt(podatki.umrljivost.novorojenckov, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='Umrljivost novorojenckov na 1000 rojstev', variable.name='Leto', 
                           na.rm=FALSE) %>% arrange(Drzava)



# Funkcija, ki uvozi podatke o splošni izobrazbi
podatki.izobrazba <- read_csv("podatki/proc_vpis_v_srednjo_solo.csv", skip=5, col_names=c('Drzava', 'Kratica', 'Procent vpisa v srednjo solo', 'Koda', 1960:2019), 
                                             na=":",locale=locale(encoding="Windows-1250"))

podatki.izobrazba <- podatki.izobrazba %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)

podatki.izobrazba <- melt(podatki.izobrazba, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='procent vpisa v srednjo solo', variable.name='Leto', 
                                         na.rm=FALSE) %>% arrange(Drzava)



# Funkcija, ki uvozi podatke o številu splavov
podatki.splavi <- read_csv("podatki/st_splavov_na_leto.csv", skip=26, col_names=c('Kratica', 'Koda', 'Spol', 'Leto', 'St. splavov na leto'), 
                            na=":",locale=locale(encoding="Windows-1250"))

podatki.splavi <- podatki.splavi %>% filter(Leto >= 2000, Leto <= 2018, Kratica != "")
  
podatki.splavi <- podatki.splavi %>% select('Kratica', 'Leto', 'St. splavov na leto')



# Funkcija, ki uvozi podatke o glavni religiji posamezne države
podatki.religija <- read_csv("podatki/main-religion-of-the-country-in.csv", skip=1, col_names=c('Drzava', 'Kratica', 'Leto', 'Religija'), 
                           na=":",locale=locale(encoding="Windows-1250"))



#Združimo v eno tabelo
zdruzeni.podatki <- inner_join(podatki.gdp, select(podatki.rodnost, c(1,3,4)), by = c('Drzava', 'Leto')) %>% 
  inner_join(., select(podatki.zivlj.doba, c(1,3,4)), by=c('Drzava', 'Leto')) %>%
  inner_join(., select(podatki.umrljivost.novorojenckov, c(1,3,4)), by=c('Drzava', 'Leto')) %>%
  left_join(., select(podatki.izobrazba, c(1,3,4)), by=c('Drzava', 'Leto')) %>%
  left_join(., select(podatki.religija, c(1,4)), by=c('Drzava')) %>%
  left_join(., podatki.splavi, by=c('Kratica', 'Leto')) 

zdruzeni.podatki %>% filter(is.na(Religija)) %>% View



# # Funkcija, ki uvozi podatke o številu novorojenih otrok iz csv dokumenta
# podatki.otroci.na.zensko <- read_csv("podatki/št_otrok_na_žensko.csv", skip=5, 
#                                      col_names=c('Drzava', 'Kratica', 'St. otrok na zensko', 'Koda', 1960:2019), 
#                                      na=":",locale=locale(encoding="Windows-1250"))
# 
# podatki.otroci.na.zensko <- podatki.otroci.na.zensko %>% select('Drzava', 'Kratica', 45:63) %>% drop_na(11:21)
# 
# podatki.otroci.na.zensko <- melt(podatki.otroci.na.zensko, id.vars=c('Drzava', 'Kratica'), measure.vars=3:21, value.name='St. otrok na zensko', variable.name='Leto', 
#                                  na.rm=FALSE) %>% arrange(Drzava)
