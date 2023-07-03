# Wgranie wymaganych repozytoriów

required_packages <- c("quantmod", "readxl", "tidyverse","bizdays", "RQuantLib")
sapply(required_packages, require, character.only = TRUE)
rm(required_packages)

# Przygotowanie kalendarza biznesowego

load_quantlib_calendars("Poland", from = "2019-01-01", to = "2022-12-31")

# Wgranie bazy danych spółek zawieszonych i zmiana formatu daty - spółki zawieszone (2019-2020)
# Należy podać właściwą ścieżkę do pobranej bazy w formacie .xlsx

knf_baza <- data.frame(read_excel("DYSK:/FOLDER/Spółki zawieszone_Notowania.xlsx"))

knf_baza$data_odwieszenia <- as.Date(knf_baza$data_odwieszenia)

knf_baza$data_zawieszenia <- as.Date(knf_baza$data_zawieszenia)

# dodanie kolumn z plus/minus 5 dniami roboczymi (odwieszenie rozumiane jako +1)

for (i in 1:5) {
  knf_baza[[paste0("data_minus_", i)]] <- offset(knf_baza$data_zawieszenia, -i, cal = "QuantLib/Poland")
}

for (i in 2:5) {
  knf_baza[[paste0("data_plus_", i)]] <- offset(knf_baza$data_odwieszenia, i, cal = "QuantLib/Poland")
}

rm(i)

knf_baza <- knf_baza %>% mutate(across(3:13, as.Date))

# Załadowanie tickerów do Quantmod w celu pobrania notowań i utworzenie nowego środowiska

symbols <- unique(as.vector(knf_baza$ticker))

data_env <- new.env()

# Pobranie symboli

getSymbols(symbols, env = data_env, from = "2019-01-01", to = "2022-12-31")

rm(symbols)

close_data <- do.call(merge, eapply(data_env, Cl)) 

rm(data_env)

# Wyfiltrowanie odpowiednich dat i przerobienie bazy danych z notowaniami

selected_dates <- as.Date(unique(
                  c(knf_baza$data_zawieszenia, knf_baza$data_odwieszenia, knf_baza$data_minus_1,
                    knf_baza$data_minus_2, knf_baza$data_minus_3, knf_baza$data_minus_4,
                    knf_baza$data_minus_5, knf_baza$data_plus_2,
                    knf_baza$data_plus_3, knf_baza$data_plus_4, knf_baza$data_plus_5)))
              
              
close_data_filtered <- data.frame(date = index(close_data[selected_dates]), (close_data[selected_dates]))

remove(close_data)

remove(selected_dates)

for ( col in 1:ncol(close_data_filtered)){
  colnames(close_data_filtered)[col] <-  sub(".Close", "", colnames(close_data_filtered)[col])
}

rm(col)

close_data_pivot <- close_data_filtered %>%
                    pivot_longer(!date, names_to = "ticker", values_to = "value") %>% 
                    mutate(id = paste(date, ticker)) %>% 
                    distinct(id, .keep_all = TRUE) %>%
                    select(1:3) 

rm(close_data_filtered)


# Wyszukanie odpowiednich notowań do wybranych dat i tickerów

zaw_cl <- c()

odw_cl <- c()

minus_1_cl <- c()

minus_2_cl <- c()

minus_3_cl <- c()

minus_4_cl <- c()

minus_5_cl <- c()

plus_2_cl <- c()

plus_3_cl <- c()

plus_4_cl <- c()

plus_5_cl <- c()

for (n in 1:nrow(knf_baza)) {
  
  ticker_knf <-as.character(knf_baza$ticker[n])
  
  data_zaw <- as.numeric(knf_baza$data_zawieszenia[n])
  
  data_odw <- as.numeric(knf_baza$data_odwieszenia[n])
  
  data_minus_1 <- as.numeric(knf_baza$data_minus_1[n])
  
  data_minus_2 <- as.numeric(knf_baza$data_minus_2[n])
  
  data_minus_3 <- as.numeric(knf_baza$data_minus_3[n])
  
  data_minus_4 <- as.numeric(knf_baza$data_minus_4[n])
  
  data_minus_5 <- as.numeric(knf_baza$data_minus_5[n])
  
  data_plus_2 <- as.numeric(knf_baza$data_plus_2[n])
  
  data_plus_3 <- as.numeric(knf_baza$data_plus_3[n])
  
  data_plus_4 <- as.numeric(knf_baza$data_plus_4[n])
  
  data_plus_5 <- as.numeric(knf_baza$data_plus_5[n])
  
  zaw_cl <- append(zaw_cl, close_data_pivot %>% 
            filter(ticker == ticker_knf, date == data_zaw) %>% 
            select(3) %>% 
            as.numeric())

  odw_cl <- append(odw_cl, close_data_pivot %>% 
            filter(ticker == ticker_knf, date == data_odw) %>% 
            select(3) %>% 
            as.numeric())
  
  minus_1_cl <- append(minus_1_cl, close_data_pivot %>% 
                         filter(ticker == ticker_knf, date == data_minus_1) %>% 
                         select(3) %>% 
                         as.numeric())
  
  minus_2_cl <- append(minus_2_cl, close_data_pivot %>% 
                         filter(ticker == ticker_knf, date == data_minus_2) %>% 
                         select(3) %>% 
                         as.numeric())
  
  minus_3_cl <- append(minus_3_cl, close_data_pivot %>% 
                         filter(ticker == ticker_knf, date == data_minus_3) %>% 
                         select(3) %>% 
                         as.numeric())
  
  minus_4_cl <- append(minus_4_cl, close_data_pivot %>% 
                         filter(ticker == ticker_knf, date == data_minus_4) %>% 
                         select(3) %>% 
                         as.numeric())
  
  minus_5_cl <- append(minus_5_cl, close_data_pivot %>% 
                         filter(ticker == ticker_knf, date == data_minus_5) %>% 
                         select(3) %>% 
                         as.numeric())
  
  plus_2_cl <- append(plus_2_cl, close_data_pivot %>% 
                        filter(ticker == ticker_knf, date == data_plus_2) %>% 
                        select(3) %>% 
                        as.numeric())
  
  plus_3_cl <- append(plus_3_cl, close_data_pivot %>% 
                        filter(ticker == ticker_knf, date == data_plus_3) %>% 
                        select(3) %>% 
                        as.numeric())
  
  plus_4_cl <- append(plus_4_cl, close_data_pivot %>% 
                        filter(ticker == ticker_knf, date == data_plus_4) %>% 
                        select(3) %>% 
                        as.numeric())
  
  plus_5_cl <- append(plus_5_cl, close_data_pivot %>% 
                        filter(ticker == ticker_knf, date == data_plus_5) %>% 
                        select(3) %>% 
                        as.numeric())
  
}

rm(close_data_pivot)

# Utworzenie bazy danych zawierającej cenę akcji w danym dniu

quotes <- data.frame(knf_baza$ticker)

quotes$zaw_cl <- zaw_cl

quotes$odw_cl <- odw_cl

quotes$minus_1_cl <- minus_1_cl

quotes$minus_2_cl <- minus_2_cl

quotes$minus_3_cl <- minus_3_cl

quotes$minus_4_cl <- minus_4_cl

quotes$minus_5_cl <- minus_5_cl

quotes$plus_2_cl <- plus_2_cl

quotes$plus_3_cl <- plus_3_cl

quotes$plus_4_cl <- plus_4_cl

quotes$plus_5_cl <- plus_5_cl


# Utworzenie bazy danych zawierającej stopy zwrotu w danym dniu i liczbę dni
# Datą odniesienia w przypadku stóp zwrotu jest data zawieszenia, 
# daty przed dniem zawieszenia są pomnożone przez -1

returns <- data.frame(knf_baza$ticker)

returns$diff_dni <- bizdays(knf_baza$data_zawieszenia, knf_baza$data_odwieszenia, cal = "QuantLib/Poland")

returns$minus_1_ret <- as.numeric(-100 * ((quotes$minus_1_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$minus_2_ret <- as.numeric(-100 * ((quotes$minus_2_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$minus_3_ret <- as.numeric(-100 * ((quotes$minus_3_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$minus_4_ret <- as.numeric(-100 * ((quotes$minus_4_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$minus_5_ret <- as.numeric(-100 * ((quotes$minus_5_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$plus_1_ret <- as.numeric(100 * ((quotes$odw_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$plus_2_ret <- as.numeric(100 * ((quotes$plus_2_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$plus_3_ret <- as.numeric(100 * ((quotes$plus_3_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$plus_4_ret <- as.numeric(100 * ((quotes$plus_4_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$plus_5_ret <- as.numeric(100 * ((quotes$plus_5_cl - quotes$zaw_cl) / quotes$zaw_cl))

returns$diff_dni_grupa[returns$diff_dni <= 10] <- "1-10"

returns$diff_dni_grupa[returns$diff_dni > 10 & returns$diff_dni <= 20] <- "11-20"

returns$diff_dni_grupa[returns$diff_dni > 20 & returns$diff_dni <= 30] <- "21-30"

returns$diff_dni_grupa[returns$diff_dni > 30] <- "30+"

cols_to_pivot <- c("minus_1_ret", "minus_2_ret", "minus_3_ret" , "minus_4_ret", "minus_5_ret", "plus_1_ret", "plus_2_ret", "plus_3_ret", "plus_4_ret", "plus_5_ret")

returns_days <- returns %>% select(3:12) %>% 
                pivot_longer(cols = cols_to_pivot, names_to = "day_diff", values_to = "return")

returns_days$day_diff <- sub("minus_", "-", returns_days$day_diff)

returns_days$day_diff <- sub("plus_", "", returns_days$day_diff)

returns_days$day_diff <- sub("_ret", "", returns_days$day_diff)

returns_days$day_diff <- as.character(returns_days$day_diff)

grupa <- c()



# Zapisanie baz danych 

write.csv(knf_baza, file = "D:/Praca końcowa/knf_baza.csv" ,col.names = FALSE)

write.csv(quotes, file = "D:/Praca końcowa/quotes.csv" ,col.names = FALSE)

write.csv(returns, file = "D:/Praca końcowa/returns.csv" ,col.names = FALSE)

# Wykresy

#Ilość dni od do odwieszenia a stopa zwrotu

mean_plus_1_ret  <- mean(returns$plus_1_ret)

mean_days <- mean(returns$diff_dni)

data_breaks <- data.frame(start = c(0, 2, 5, 9),  # Create data with breaks
                          end = c(2, 5, 9, 10),
                          colors = factor(1:4))



returns %>% 
  ggplot(aes(diff_dni, plus_1_ret))+
  geom_jitter(size = 2)+
  labs(x ="Ilość dni do odwieszenia", y = "Stopa zwrotu w %")+
  scale_y_continuous(breaks = seq(-25, 50, by = 5))+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  geom_hline( yintercept = mean_plus_1_ret, linetype="dashed", size = 0.5, color = "red")+
  geom_vline( xintercept = mean_days, linetype="dashed", size = 0.5, color = "blue")+
  annotate("text",x= 80, y=mean_plus_1_ret+1.5, label="Średnia stopa zwrotu", size = 2.5 , color = "red")+
  annotate("text",x= mean_days-2.5, y=30, label="Średnia ilość dni", size = 2.5 , color = "blue", angle = 90)+
  theme_bw()

# Stopy zwrotu w przedziałach

returns %>% 
  ggplot(aes(diff_dni_grupa, plus_1_ret))+
  geom_boxplot()+
  labs(x="Ilość dni do odwieszenia w przedziałach", y="Stopa zwrotu w %")+
  scale_y_continuous(breaks = seq(-25, 50, by = 10))+
  theme_bw()

# Stopy zwrotu - w zależności od ilości dni od zawiedzenia/odwieszenia

returns_days %>% 
  ggplot(aes(reorder(day_diff, as.integer(day_diff)), return))+
  geom_boxplot()+
  labs(x="Ilość dni od zawieszenia/odwieszenia", y="Stopa zwrotu w %")+
  scale_y_continuous(breaks = seq(-50, 150, by = 10))+
  theme_bw()


returns_days %>% 
  ggplot(aes(reorder(day_diff, as.integer(day_diff)), return))+
  geom_point()+
  labs(x="Ilość dni od zawieszenia/odwieszenia", y="Stopa zwrotu w %")+
  scale_y_continuous(breaks = seq(-50, 150, by = 10))+
  theme_bw()




