## ANALISIS CON DATOS DEL 2016
library(tidyverse)
library(caret)
library(countrycode)
library(janitor)
library(naniar)
library(corrplot)
library(leaps)
library(car)

# IMPORTACION DE LOS DATOS -----
df1 <- read.csv("Data2016/PBI2011.csv")
df2 <- read.csv("Data2016/GPC_GINI_LABOR_HIGHTECH.csv")
df3 <- read.csv("Data2016/PATEN_MEDHIGHEXP.csv")

df1 <- df1 %>% 
  select(COUNTRY = Country.Name, 
         GDP2011 = GDP.per.capita..PPP..constant.2011.international.....NY.GDP.PCAP.PP.KD.,
         GDP2005 = GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.,
         POPAGE = Population.ages.15.64....of.total...SP.POP.1564.TO.ZS.,
         GINI = GINI.index..World.Bank.estimate...SI.POV.GINI.,
         HTEXPORT = High.technology.exports....of.manufactured.exports...TX.VAL.TECH.MF.ZS.)

df2 <- df2 %>% 
  select(COUNTRY = Country.Name,
         ADVLABOR = Labor.force.with.advanced.education....of.total.working.age.population.with.advanced.education...SL.TLF.ADVN.ZS.)

df3 <- df3 %>% 
  select(COUNTRY = Country.Name,
         PATENT = Patent.applications..residents..IP.PAT.RESD.,
         MHTEXPORT = Medium.and.high.tech.exports....manufactured.exports...TX.MNF.TECH.ZS.UN.)

df <- inner_join(df1, df2)
df <- inner_join(df, df3)

df <- df %>% 
  select(COUNTRY, GDP2005, GDP2011, POPAGE, ADVLABOR, GINI, PATENT, MHTEXPORT)


# CONSTRUCCI?N DE FACTORES ------
# AMERICA DEL SUR
codigos_am_s <- c("AR","BO","BR","CL","CO","EC","FK","GF","GY","PY","PE","SR","UY","VE")
paises_am_s <- countrycode(codigos_am_s, "iso2c", "country.name")
codigos_am_nc <- c("AI","AG","AW","BS","BB","BZ","BM","VG","CA","KY","CR","CU","CW","DM","DO","SV","GL","GD","GP","GT","HT","HN","JM","MQ","MX","MS","NI","PA","PR","BL","KN","LC","MF","PM","VC","SX","TT","TC","US","VI")
paises_am_nc <- countrycode(codigos_am_nc, "iso2c", "country.name")

# EUROPA
codigos_europa <- c("AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","XK","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA")
paises_europa <- countrycode(codigos_europa, "iso2c", "country.name", custom_match = list(XK = "Kosovo"))
paises_europa <- unlist(paises_europa)

# AFRICA
codigos_africa <- c("DZ","AO","BJ","BW","BF","BI","CM","CV","CF","TD","KM","CD","CG","CI","DJ","EG","GQ","ER","ET","GA","GM","GH","GN","GW","KE","LS","LR","LY","MG","MW","ML","MR","MU","YT","MA","MZ","NA","NE","NG","RW","ST","SN","SC","SL","SO","ZA","SS","SD","SZ","TZ","TG","TN","UG","EH","ZM","ZW")
paises_africa <- countrycode(codigos_africa, "iso2c", "country.name")

# ASIA 
codigos_asia <- c("AF","AM","AZ","BH","BD","BT","BN","KH","CN","CY","GE","HK","IN","ID","IR","IQ","IL","JP","JO","KZ","KW","KG","LA","LB","MO","MY","MV","MN","MM","NP","KP","OM","PK","PS","PH","QA","SA","SG","KR","LK","SY","TW","TJ","TH","TL","TR","TM","AE","UZ","VN","YE")
paises_asia <- countrycode(codigos_asia, "iso2c", "country.name")

# OCEANIA
codigos_oceania <- c("AS","AU","CK","FJ","PF","GU","KI","MH","FM","NR","NC","NZ","NU","MP","PW","PG","PN","WS","SB","TK","TO","TV","UM","VU","WF")
paises_oceania <- countrycode(codigos_oceania, "iso2c", "country.name")

# ANTARTIDA
codigos_antartida <- c("AQ")
paises_antartida <- countrycode(codigos_antartida, "iso2c", "country.name")

# Guardo dataframe



paises_am_nc <- c(paises_am_nc, 
                  "Virgin Islands (U.S.)",
                  "Antigua and Barbuda", 
                  "Bahamas, The",
                  "Sint Maarten (Dutch part)",
                  "St. Kitts and Nevis",
                  "Turks and Caicos Islands")

paises_am_s <- c(paises_am_s,
                 "Curacao",
                 "Trinidad and Tobago",
                 "Venezuela, RB")

paises_europa <- c(paises_europa,
                   "Channel Islands",
                   "Bosnia and Herzegovina",
                   "Slovak Republic",
                   "St. Martin (French part)",
                   "St. Vincent and the Grenadines",
                   "Turkiye")

paises_asia <- c(paises_asia,
                 "Brunei Darussalam",
                 "Hong Kong SAR, China",
                 "Iran, Islamic Rep.",
                 "Korea, Dem. People's Rep.",
                 "Korea, Rep.",
                 "Kyrgyz Republic",
                 "Lao PDR",
                 "Macao SAR, China",
                 "Myanmar",
                 "Russian Federation",
                 "Syrian Arab Republic",
                 "West Bank and Gaza", 
                 "Yemen, Rep.")

paises_africa <- c(paises_africa,
                   "Congo, Dem. Rep.",
                   "Congo, Rep.",
                   "Egypt, Arab Rep.",
                   "Gambia, The",
                   "Cabo Verde",
                   "Cote d'Ivoire",
                   "Sao Tome and Principe",
                   "Sub-Saharan Africa (excluding high income)")

paises_oceania <- c(paises_oceania, 
                    "Micronesia, Fed. Sts.")

df$CONT <- case_when(
  df$COUNTRY %in% paises_africa == TRUE ~ "AFRICA",
  df$COUNTRY %in% paises_am_s == TRUE ~ "S.AMERICA",
  df$COUNTRY %in% paises_am_nc == TRUE ~ "N.AMERICA",
  df$COUNTRY %in% paises_antartida == TRUE ~ "ANTARTIDA",
  df$COUNTRY %in% paises_asia == TRUE ~ "ASIA",
  df$COUNTRY %in% paises_europa == TRUE ~ "EUROPA",
  df$COUNTRY %in% paises_oceania == TRUE ~ "OCEANIA")

df[,2:8] <- apply(df[,2:8], MARGIN = 2, FUN = as.double) %>% 
  data.frame()

rm(CONT, dummy, paises_africa, paises_am_nc, paises_am_s, paises_antartida, paises_asia, paises_europa, paises_oceania, codigos_africa, codigos_am_s, codigos_am_nc, codigos_antartida, codigos_asia, codigos_europa, codigos_oceania, df1, df2, df3)

### ANALISIS DESCRIPTIVO -----
colnames(df)
miss_var_summary(df)

# Selecci?n de la variable objetivo
df <- df %>% 
  filter(!is.na(df$GDP2011)) %>%
  select(!GDP2005)

write.csv(df, "data/2016_obs.csv")

df <- df %>% 
  filter(!is.na(df$CONT))
  
shapiro.test(log(df$GDP2011))

# Analisis descriptivo de todas las variables con na reemplazado ---------
# df$GINI[is.na(df$GINI)] <- mean(df$GINI, na.rm=TRUE)
# df$ADVLABOR[is.na(df$ADVLABOR)] <- mean(df$ADVLABOR, na.rm=TRUE)
# df$PATENT[is.na(df$PATENT)] <- mean(df$PATENT, na.rm=TRUE)
# df$HTEXPORT[is.na(df$HTEXPORT)] <- mean(df$HTEXPORT, na.rm=TRUE)
# df$MHTEXPORT[is.na(df$MHTEXPORT)] <- mean(df$MHTEXPORT, na.rm=TRUE)
# df$POPAGE[is.na(df$POPAGE)] <- mean(df$POPAGE, na.rm=TRUE)
# 
# corrplot(cor(df[,1:7]), method = 'number', diag = FALSE)

# Analisis descriptivo de las correlaciones frente a variables sin reemplazo de NA ---------

# Variables con menor pct de na
# POPAGE, MHTEXPORT, HTEXPORT

dftrunc <- df %>% 
  select(COUNTRY, GDP2011, CONT, MHTEXPORT, POPAGE)

dftrunc <- na.omit(dftrunc)

# Mutaci?n a log
dftrunc <- dftrunc %>%
  mutate(GDP2011log = log(GDP2011))

dftrunc <- dftrunc %>% 
  filter(GDP2011 > quantile(dftrunc$GDP2011, .05))

shapiro.test(log(dftrunc$GDP2011))

dftrunc %>% 
  select_if(is.numeric) %>% 
  select(!GDP2011) %>% 
  cor()

# Con Contes agrupados
dftrunc$CONT <- case_when(
  dftrunc$CONT %in% c("N.AMERICA", "S.AMERICA") == TRUE ~ "AMERICA",
  dftrunc$CONT %in% c("N.AMERICA", "S.AMERICA") == FALSE ~ dftrunc$CONT
)

dftrunc %>% 
  ggplot(aes(x = GDP2011log)) +
  geom_histogram(fill = "orange", color = "black", bins = 12) +
  xlab("PBI PER CAPITA log") +
  theme_bw()

dftrunc %>% 
  ggplot(aes(y = GDP2011log, fill = CONT, group = CONT)) +
  geom_boxplot() +
  ylab("PBI Per Capita log") +
  guides(fill=guide_legend(title="CONTe")) +
  theme_bw()


tabyl(dftrunc$CONT) %>% 
  arrange(-percent)

par(mfrow=c(1,1))

dftrunc %>% 
  filter(!(COUNTRY  %in% c("Congo, Rep.", "Moldova"))) %>% 
  select(!c(GDP2011, COUNTRY)) %>% 
  filter(CONT != "OCEANIA") %>% 
  pivot_longer(-c(GDP2011log, CONT), values_to = "val", names_to = "name") %>% 
  ggplot(aes(x = val, y = GDP2011log, color = CONT)) +
  geom_point(size = 2) +
  ylab("PBI Per Capita log") +
  guides(color = guide_legend(title="CONTe")) +
  xlab("") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme_bw()

dftrunc %>% 
  filter(CONT == "AFRICA", MHTEXPORT > 75)

dftrunc %>% 
  filter(CONT == "EUROPA") %>% 
  ggplot()+
  geom_point(aes(x = POPAGE, y = GDP2011log)) 

dftrunc %>% 
  filter(CONT == "EUROPA" & (POPAGE > 72.5 | GDP2011log < 9))

# Elimino algunos casos at?picos
dftrunc <- dftrunc %>% 
  filter(!(COUNTRY %in% c("Congo, Rep.", "Moldova", "Afghanistan")))

tabyl(dftrunc$CONT) %>% 
  arrange(-percent)

# MODELO CON DEVOLUCIONES ######
dftrunc <- dftrunc

CONT <- dftrunc %>% 
  select(CONT)

dummy <- dummyVars(" ~ .", data = CONT)

# perform one-hot encoding on data frame
CONT <- data.frame(predict(dummy, newdata=dftrunc))

# Unir al df y limpiar casos nulos 
dfm <- cbind(dftrunc, CONT)
dfm <- dfm %>% 
  select(!c(COUNTRY, CONT, GDP2011)) %>%
  select(GDP2011log, MHTEXPORT, POPAGE, CONTAFRICA, CONTAMERICA, CONTASIA, CONTEUROPA, CONTOCEANIA)

dfm <- dfm %>% 
  select(GDP2011log, MHTEXPORT, POPAGE, CONTAFRICA, CONTAMERICA, CONTASIA, CONTOCEANIA, CONTEUROPA)
rownames(dfm) <- NULL

write.csv(dfm, "model_2016.csv")

m2 <- lm(data = dftrunc2, GDP2011log ~ MHTEXPORT + POPAGE + as.factor(CONT))
summary(m2)

# RANGO DE LA MATRIZ ------------
library(Matrix)
x <- dfm %>% 
  select(-GDP2011log) %>% 
  as.matrix()

rankMatrix(x)[1]

# modelo con factores-----
dftrunc2 <- dftrunc %>% 
  select(-COUNTRY) %>% 
  mutate(MHTEXPORT = MHTEXPORT/ 100,
         POPAGE = POPAGE/100)

library(MASS) 

X <- model.matrix(m2)

a <- car::linearHypothesis(m2, c("as.factor(CONT)ASIA-as.factor(CONT)AMERICA-as.factor(CONT)EUROPA=0"))
cme <- 51.551/127

# Intervalos de confianza sobre la función estimable
0.573 +


# ------

# Intervalo de confianza para ver pbi de america vs asia.

# mu + america - mu + asia = .... +- t_n-r, 95% * sqrt(CME)

((2.369 + 0.028) - (2.369 + (-0.086)))
((2.369 + 0.028) - (2.369 + (-0.086))) - qt(0.05, df = 127) * sqrt(51.551/127)
((2.369 + 0.028) - (2.369 + (-0.086))) + qt(0.05, df = 127) * sqrt(51.551/127)
2.369-0.086- 2.369- 0.028


# Modelo con Interacci?n -----
m2int <- lm(data = dftrunc2, GDP2011log ~ MHTEXPORT + POPAGE + as.factor(CONT) * MHTEXPORT)
summary(m2int)

anova(m2int)




shapiro.test(m2int$residuals)

# Comprobaci?n de supuestos 
dataset <- dftrunc2[, 2:5]

yest <- predict(m2, dataset)
ypred <- dfm$GDP2011

windows()

par(mfrow=c(1,2))

windows()
ggplot() +
  aes(x = yest, y = log(ypred)) +
  geom_point(col = "darkred") + 
  geom_smooth(method = "lm") +
  xlab("Logaritmo de PBI Per C?pita Observado") +
  ylab("Logaritmo de PBI Per C?pita Estimado") + 
  theme_bw()

windows()
ggplot() +
  aes(x = exp(yest), y = ypred) +
  geom_point(col = "darkred") +
  xlab("Pbi Per C?pita Observado") +
  ylab("Pbi Per C?pita Estimado") +
  geom_smooth(method = "lm") +
  theme_bw()


par(mfrow = c(2,2))
plot(m2)

shapiro.test(m2$residuals)







# MISCELANEA -------- 
# define one-hot encoding function

# Rango de la matriz
mat <- dftrunc %>% 
  select(!c(COUNTRY, GDP2011log, CONT)) %>% 
  mutate(int = dfm$CONTEUROPA*MHTEXPORT) %>% 
  as.matrix()

b1 <- rep(1, 132)
mat <- cbind(b1, mat)

library(Matrix)
rankMatrix(mat)

rm(CONT, dummy, mat, b1)

# Modelo completo
mfull <- regsubsets(data = dfm, GDP2011log ~., nvmax = 7)

windows()
par(mfrow=c(1,3))
plot(mfull,scale='adjr2')
plot(mfull,scale='Cp')
plot(mfull,scale='bic')

# Modelo 2 
mfull <- lm(data = dfm, GDP2011log ~
              MHTEXPORT + 
              POPAGE + 
              CONTAMERICA*MHTEXPORT + 
              CONTASIA*MHTEXPORT + 
              CONTEUROPA*MHTEXPORT + 
              CONTAFRICA*MHTEXPORT)

mfull2 <- olsrr::ols_step_all_possible(mfull)

# MHTEXPORT
# POPAGE
# CONTAMERICA
# CONTAFRICA
# MHTEXPORT:CONTEUROPA

# Modelo 3
m3 <- lm(data = dfm, GDP2011log ~
           MHTEXPORT +
           POPAGE + 
           CONTEUROPA)

anova(m3)
summary(m3)


## Matriz solo con interacci?n
b0 <- 1 





# Y estimado vs. Y predicho

exp(yest)
