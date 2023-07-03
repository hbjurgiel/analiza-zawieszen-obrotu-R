# Wgranie wymaganych repozytoriów

required_packages <- c("quantmod", "readxl", "tidyverse","bizdays", "RQuantLib")
sapply(required_packages, require, character.only = TRUE)
rm(required_packages)

# Przygotowanie kalendarza biznesowego

load_quantlib_calendars("Poland", from = "2016-01-01", to = "2022-12-31")

# Wgranie bazy danych o zawieszeniach spółeki zmiana formatu daty - spółki zawieszone (2016-2022), dane od 2019 z KNF
# Należy podać właściwą ścieżkę do pobranej bazy w formacie .xlsx

knf_komunikaty <- data.frame(read_excel("DYSK:/FOLDER//Baza danych_Zawieszenia.xlsx"))

knf_komunikaty$Data.odwieszenia <- as.Date(knf_komunikaty$Data.odwieszenia)

knf_komunikaty$Data.zawieszenia <- as.Date(knf_komunikaty$Data.zawieszenia)

knf_komunikaty$liczba_dni <- bizdays(knf_komunikaty$Data.zawieszenia, knf_komunikaty$Data.odwieszenia, cal = "QuantLib/Poland")


knf_komunikaty %>% subset(!is.na(liczba_dni)) %>%  
  ggplot(aes(reorder(Rok, as.integer(Rok)), liczba_dni))+
  geom_bar(stat = "summary", fun = "mean")+
  labs(x="", y="Średnia ilość dni bez notowań")+
  theme_bw()

knf_komunikaty %>% subset(!is.na(liczba_dni)) %>%  
  ggplot(aes(reorder(Rok, as.integer(Rok)), liczba_dni))+
  geom_bar(stat = "summary", fun = "median")+
  labs(x="", y="Średnia ilość dni bez notowań")+
  theme_bw()


knf_komunikaty %>% subset(!is.na(liczba_dni)) %>%  
  ggplot(aes(reorder(Rok, as.integer(Rok)), liczba_dni))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(0, 400, by = 50))+
  labs(x="Lata", y="Ilość dni bez notowań")+
  theme_bw()
# Należy podać folder docelowego zapisu
write.csv(knf_komunikaty, file = "DYSK:/FOLDER/knf_komunikaty_liczba_dni.csv" ,col.names = FALSE)

