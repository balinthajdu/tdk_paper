# Working Directory:
setwd('C:/Users/Bálint/OneDrive/Dokumentumok/TDK-Szakdolgozat')

# Adatbázis betöltése:
library(readxl)
data <- as.data.frame(read_excel("Adatbazis.xlsx"))
str(data)
summary(data)
View(data)

# Adatbázis szükséges átalakítása:
data$comp_level <- as.factor(data$comp_level)
data$comp_level <- relevel(data$comp_level, "Premier League")
data$international_reputation <- as.factor(data$international_reputation)
data$injury_prone <- as.factor(data$injury_prone)
data$future_prospect <- as.factor(data$future_prospect)
library(MASS)
contrasts(data$comp_level) <- contr.sum(5)
contrasts(data$international_reputation) <- contr.sum(5)

# Leíró statisztika
library(stargazer)
stargazer(data, type = 'text',median = TRUE)
library(psych)
describe(data)

# Eredményváltozó vizsgálata:
hist(data$market_value_in_eur_y)
hist(data $log_market_value_in_eur) # Logaritmálni kell.

# Magyarázóváltozó 1: Tavalyi piaci értékek
hist(data$market_value_2022)
hist(data$log_market_value_2022)

# Magyarázóváltozó 2: Wikipédia keresések száma
hist(data$total_pageviews)
hist(data$log_total_pageviews)

# Modellben használt változók leszûkítése:
library(dplyr)

used_data <- data %>% 
  dplyr::select(age, age_square, comp_level,log_market_value_in_eur, minutes_pct, goals_per90, assists_per90,xg_per90, xg_a_per90,passes_per90, passes_pct, aerials_won_pct,progressive_passes_per90,prog_pass_pct,log_market_value_2022, take_ons_per90,challenge_tackles_pct, international_reputation, injury_prone, future_prospect, log_total_pageviews)
summary(used_data)

# Modellépítés:
# I. Kutatási kérdés modellje: Minden változó, kivéve az advanced statok
modell1 <- lm(log_market_value_in_eur ~ .-xg_per90-xg_a_per90-prog_pass_pct-progressive_passes_per90,data = used_data)
summary(modell1)
library(car)
vif(modell1) # Nincsen multikollinearitás. Csak a struktúrált.
hist(modell1$residuals)
ks.test(modell1$residuals, "pnorm") # Hibatagok nem normális eloszlásúak
white(modell1, interactions = FALSE)$p.value # Heteroszekdasztikus
white(modell1, interactions = TRUE)$p.value # Heteroszkedasztikus
bptest(modell1) # Heteroszkedasztikus
coeftest(modell1, vcov = hccm(modell1))

# II. Kutatási kérdés modelljei:
# Advanced data: xG
modell2<- lm(log_market_value_in_eur ~ .-goals_per90-xg_a_per90-prog_pass_pct-progressive_passes_per90,data = used_data)
summary(modell2)
vif(modell2) # Nincsen multikollinearitás. Csak a struktúrált.
hist(modell2$residuals)
ks.test(modell2$residuals, "pnorm") # Hibatagok nem normális eloszlásúak
white(modell2, interactions = FALSE)$p.value # Heteroszekdasztikus
white(modell2, interactions = TRUE)$p.value # Heteroszkedasztikus
bptest(modell2) # Heteroszkedasztikus
coeftest(modell2, vcov = hccm(modell2))

# Advanced data: xA
modell3 <- lm(log_market_value_in_eur ~ .-xg_per90-assists_per90-prog_pass_pct-progressive_passes_per90,data = used_data)
summary(modell3)
vif(modell3) # Passes_per90 éppen, hogy átlépi az 5-ös VIF értéket.
ks.test(modell3$residuals, "pnorm") # Hibatagok nem normális eloszlásúak
white(modell3, interactions = FALSE)$p.value # Heteroszekdasztikus
white(modell3, interactions = TRUE)$p.value # Heteroszkedasztikus
bptest(modell3) # Heteroszkedasztikus
coeftest(modell3, vcov = hccm(modell3))

# Advanced data: Progresszív passzok és arányuk
modell4 <- lm(log_market_value_in_eur ~ .-xg_per90-xg_a_per90-passes_pct-passes_per90,data = used_data)
summary(modell4)
vif(modell4) # Nincsen multikollinearitás. Csak a struktúrált.
ks.test(modell4$residuals, "pnorm") # Hibatagok nem normális eloszlásúak
white(modell4, interactions = FALSE)$p.value # Heteroszekdasztikus
white(modell4, interactions = TRUE)$p.value # Heteroszkedasztikus
bptest(modell4) # Heteroszkedasztikus
coeftest(modell4, vcov = hccm(modell4))

# Összehasonlítások:
AIC(modell1,modell2,modell3,modell4)
BIC(modell1,modell2,modell3,modell4)

# Korrelációs index (I): alapmodell
View(used_data)
predicted <- predict(modell1)
mean <- mean(data$log_market_value_in_eur)

felso <- sum((data$log_market_value_in_eur-predicted)^2)
also <- sum((data$log_market_value_in_eur-mean)^2)
gyok_alatt <- 1-(felso/also)
I_korrelacios_index1 <- sqrt(gyok_alatt)
I_korrelacios_index1

# Korrelációs index (I): xG
predicted <- predict(modell2)
mean <- mean(data$log_market_value_in_eur)

felso <- sum((data$log_market_value_in_eur-predicted)^2)
also <- sum((data$log_market_value_in_eur-mean)^2)
gyok_alatt <- 1-(felso/also)
I_korrelacios_index2 <- sqrt(gyok_alatt)
I_korrelacios_index2

# Korrelációs index (I): xA
predicted <- predict(modell3)
mean <- mean(data$log_market_value_in_eur)

felso <- sum((data$log_market_value_in_eur-predicted)^2)
also <- sum((data$log_market_value_in_eur-mean)^2)
gyok_alatt <- 1-(felso/also)
I_korrelacios_index3 <- sqrt(gyok_alatt)
I_korrelacios_index3

# Korrelációs index (I): Progresszív passzok
predicted <- predict(modell4)
mean <- mean(data$log_market_value_in_eur)

felso <- sum((data$log_market_value_in_eur-predicted)^2)
also <- sum((data$log_market_value_in_eur-mean)^2)
gyok_alatt <- 1-(felso/also)
I_korrelacios_index4 <- sqrt(gyok_alatt)
I_korrelacios_index4

### VÉGE ###

### Innentõl csak adatvizualizációs kód! ###





















### Adatvizualizáció ###

# Felhasznált adatvizualizációk a TDK dolgozatban:
# 1. Ábra:
library(ggplot2)
library(dplyr)
library(readxl)
Statista_1 <- read_excel("Statista_1.xlsx", 
                         sheet = "Munka1")
ggplot(Statista_1, aes(x = Latogatasszam, y = reorder(Oldal, Latogatasszam))) +
  geom_col(fill = "deepskyblue3") +
  geom_text(aes(label = Latogatasszam), color = 'black', hjust = -0.1, size = 3, vjust = 0.5) + 
  labs(x = "Látogatások száma (millió darab)", y = "Hírportálok") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 20)) +  # Hírportálok neveinek tördelése 20 karakterenként
  theme(panel.background = element_rect(fill = "white")) +  # Háttér beállítása
  scale_x_continuous(expand = c(0, 0), limits = c(0, 615), breaks = seq(0, 600, by = 100)) +  # X tengely 0-tól kezdése és maximum 600-ig
  theme(panel.grid.major.x = element_line(color = "gray", size = 0.1),  # Csak az X tengelynek rácsvonalak beállítása
        axis.title.x = element_text(hjust = 0.5))  # X tengely címének középre igazítása
library(stringr)
str(data)

# 3. ábra:
# Sima
ggplot(data, aes(x = market_value_in_eur_y)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(labels = function(x) paste0(x/1e6, " M ???"), 
                     breaks = seq(0, 160e6, by = 40e6)) +
  labs(y = "Gyakoriság [db]", x = "Piaci érték [millió euró]") +
  theme(panel.background = element_rect(fill = "white"))
# Logaritmált
ggplot(data, aes(x = log_market_value_in_eur)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  labs(y = "Gyakoriság [db]", x = "Piaci érték logaritmusa") +
  theme(panel.background = element_rect(fill = "white"))

# 4. ábra:
# Sima
ggplot(data, aes(x = market_value_2022)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(labels = function(x) paste0(x/1e6, " M ???"), 
                     breaks = seq(0, 160e6, by = 40e6)) +
  labs(y = "Gyakoriság [db]", x = "Piaci érték [millió euró]") +
  theme(panel.background = element_rect(fill = "white"))
# Logaritmált
ggplot(data, aes(x = log_market_value_2022)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  labs(y = "Gyakoriság [db]", x = " Piaci érték logaritmusa") +
  theme(panel.background = element_rect(fill = "white"))

# 5. ábra:

# Sima
ggplot(data, aes(x = total_pageviews)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(labels = function(x) paste0(x/1e6, " M db"), breaks = seq(0, 25e6, by = 5e6)) +
  labs(y = "Gyakoriság [db]", x = "Wikipedia keresések száma [millió darab]") +
  theme(panel.background = element_rect(fill = "white"))

# Logaritmált
ggplot(data, aes(x = log_total_pageviews)) +
  geom_histogram(fill = "deepskyblue3", color = "black", bins = 10) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  labs(y = "Gyakoriság [db]", x = " Wikipedia keresések számának logaritmusa") +
  theme(panel.background = element_rect(fill = "white"))

# 6. ábra:
ggplot(data, aes(x = age, y = log_market_value_in_eur)) +
  geom_point(color = "deepskyblue3") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, df = 2), color = "black") +  
  labs(y = "Piaci érték logaritmusa", x = "Életkor") +
  theme(panel.background = element_rect(fill = "white"))

# 3. táblázat:
library(psych)
describe(select_if(data, is.numeric))
library(writexl)
write_xlsx(describe(select_if(data, is.numeric)), "leiro_stat.xlsx")
leiro_stat <- read_excel("leiro_stat.xlsx")
library(stargazer)
stargazer(data[,1:10], flip = TRUE)
stargazer(data, type = "html", out="table1.htm", flip = TRUE)

# 4. Táblázat
stargazer(coeftest(modell1, vcov = hccm(modell1)), type = 'text') # Csak megnézd
stargazer(coeftest(modell1, vcov = hccm(modell1)), type = "html", out="table1.htm")
summary(modell1)

# 5. táblázat:
stargazer(modell1_sg, modell2_sg, modell3_sg, modell4_sg,
          type = 'html', out="modellek_osszehas .htm")

modell1_sg <- coeftest(modell1, vcov = hccm(modell1))
modell2_sg <- coeftest(modell2, vcov = hccm(modell2))
modell3_sg <- coeftest(modell3, vcov = hccm(modell3))
modell4_sg <- coeftest(modell4, vcov = hccm(modell4))

# Ábrák:
ggplot(data, aes(x = comp_level, y = market_value_in_eur_y)) +
  geom_boxplot()

# Boxplot készítése
ggplot(data, aes(x = comp_level, y = market_value_in_eur_y)) +
  geom_boxplot(fill = c("coral1", "deepskyblue3", "olivedrab2", "darkgoldenrod2", "mediumorchid2")) +
  ylab("Piaci értékek millió euróban") + 
  xlab("Bajnokságok") +
  scale_y_continuous(limits = c(0, 125000000), breaks = seq(0, 125000000, by = 25000000), 
                     labels = paste0(seq(0, 125, by = 25), "M ???")) +
  theme_minimal() +
  theme(axis.line.y = element_line(color = "gray"))    

# Sérülékenység
ggplot(data, aes(x = injury_prone, y = market_value_in_eur_y)) +
  geom_boxplot(fill = c("coral1", "deepskyblue3")) +
  ylab("Sérülékenység") + 
  xlab("Piaci értékek (millió euró)") +
  scale_y_continuous(limits = c(0, 125000000), breaks = seq(0, 125000000, by = 25000000), 
                     labels = paste0(seq(0, 125, by = 25), "M ???")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray"))

ggplot(data, aes(x = injury_prone, y = market_value_in_eur_y)) +
  geom_boxplot(fill = c("coral1", "deepskyblue3")) +
  ylab("Piaci értékek millió euróban") + 
  xlab("Sérülékenység") +
  scale_y_continuous(limits = c(0, 125000000), breaks = seq(0, 125000000, by = 25000000), 
                     labels = paste0(seq(0, 125, by = 25), "M ???")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray")) +
  scale_x_discrete(labels = c("Nem sérülékeny" , "Sérülékeny" ))

# Heteroszked ábra
hetero_data <- as.data.frame(modell1$residuals^2)
hetero_data$log_market <- data$log_market_value_in_eur



ggplot(hetero_data, aes(x = log_market, y = modell1$residuals^2)) + 
  geom_point(color = "deepskyblue3") +
  ylab("Hibatagok négyzete") + 
  xlab("Piaci érték logaritmált értékei") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(hetero_data, aes(x = log_market, y = modell1$residuals^2)) + 
  geom_point(color = "deepskyblue3") +
  ylab("Hibatagok négyzete") + 
  xlab("Piaci érték logaritmált értékei") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"))

v1 <- vif(modell1)
v3 <- vif(modell3)

stargazer(v1,v3, type = 'html',out="multikol.htm")

# Stat_2
top5 <- read_excel("top5.xlsx", 
                         sheet = "Data")
str(top5)

ggplot(top5, aes(x = ertek, y = reorder(Bajnoksag, ertek))) +
  geom_col(fill = "deepskyblue3") +
  geom_text(aes(label = ertek), color = 'black', hjust = -0.1, size = 3, vjust = 0.5) + 
  labs(x = "Összes játékos piaci értéke (millió euró)", y = "Bajnokságok") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "lightgray")) +
  scale_x_continuous(limits = c(0, 14000), breaks = seq(0, 14000, by = 2000))
