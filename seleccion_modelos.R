library(tidyverse)
library(janitor)
# Imputacion por mediana
df <- read_delim("data/df.csv") %>% 
  select(!COUNTRY)

# PBI Per Capita
df$GDPPC[is.na(df$GDPPC)] <- mean(df$GDPPC, na.rm=TRUE)

# Alta tecnologia
df$HIGH.TECH.EXP[is.na(df$HIGH.TECH.EXP)] <- median(df$HIGH.TECH.EXP, na.rm=TRUE)
df$HIGH.TECH.EXP %>% 
  hist()

# Media Tecnologia
df$MEDHIGH.TECH.EXP %>% 
  hist()

df$MEDHIGH.TECH.EXP[is.na(df$MEDHIGH.TECH.EXP)] <- mean(df$HIGH.TECH.EXP, na.rm=TRUE)

# Gini
df$GINI %>% 
  hist()

df$GINI[is.na(df$GINI)] <- mean(df$GINI, na.rm=TRUE)

# journals
df$SCIENCE.JOURNALS[is.na(df$SCIENCE.JOURNALS)] <- mean(df$SCIENCE.JOURNALS, na.rm=TRUE)
miss_var_summary(df)

# Primer modelo
m1 <- lm(log(GDPPC) ~ ., data = df)
summary(m1)

# Modelo con variables continente
library(caret)

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=df)

#perform one-hot encoding on data frame
continents <- data.frame(predict(dummy, newdata=df))

#view final data frame
df2 <- continents %>% 
  select(!c(HIGH.TECH.EXP, SCIENCE.JOURNALS))

## Resumen de los mejores modelos
library(leaps)
mfull <- regsubsets(data = df, log(GDPPC) ~., nvmax = 7)

windows()
par(mfrow=c(3,1))
plot(mfull,scale='adjr2')
plot(mfull,scale='Cp')
plot(mfull,scale='bic')



# Segundo mejor modelo
m2 <- lm(data = df2, log(GDPPC) ~ CONTINENTAFRICA + TECH.RD.PER.M)
summary(m2)

