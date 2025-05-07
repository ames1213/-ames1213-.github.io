## Load necessary libraries

install.packages("neonUtilities")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(neonUtilities)
library(dplyr)
library(ggplot2)
library(tidyr)


## LOAD AND COMBINE SOIL TEMP — SOAP SITE

soil_temp <- read.csv("soiltempsoap22_24.csv")
soil_temp2 <- read.csv("soiltempsoap20_22.csv")
soil_temp3 <- read.csv("soiltempsoap18_20.csv")
soil_temp4 <- read.csv("soiltempsoap16_218.csv")

soil_temp_all <- bind_rows(
  soil_temp[, c("startDateTime", "soilTempMean")],
  soil_temp2[, c("startDateTime", "soilTempMean")],
  soil_temp3[, c("startDateTime", "soilTempMean")],
  soil_temp4[, c("startDateTime", "soilTempMean")]
)

soil_temp_all$startDateTime <- as.POSIXct(soil_temp_all$startDateTime)
soil_temp_all$year <- format(soil_temp_all$startDateTime, "%Y")
soap_soil_avg <- aggregate(soilTempMean ~ year, data = soil_temp_all, FUN = mean, na.rm = TRUE)

write.csv(soil_temp_all, "combinedsoapsoiltemp.csv")
write.csv(soap_soil_avg, "soapsoiltempavg.csv")


## PLOT: SOAP Soil Temp

ggplot(soap_soil_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(title = "SOAP Site Yearly Avg Soil Temp (2016–2024)", x = "Year", y = "Avg Soil Temp (°C)") +
  theme_minimal()


## LOAD AND COMBINE SOIL TEMP — ABBY SITE

asoil_temp <- read.csv("soiltempabby20_24.csv")
asoil_temp2 <- read.csv("soiltempabby16_20.csv")

asoil_all <- bind_rows(
  asoil_temp[, c("startDateTime", "soilTempMean")],
  asoil_temp2[, c("startDateTime", "soilTempMean")]
)

asoil_all$startDateTime <- as.POSIXct(asoil_all$startDateTime)
asoil_all$year <- format(asoil_all$startDateTime, "%Y")
abby_soil_avg <- aggregate(soilTempMean ~ year, data = asoil_all, FUN = mean, na.rm = TRUE)

write.csv(asoil_all, "combinedasoapsoiltemp.csv")
write.csv(abby_soil_avg, "abbysoiltempavg.csv")


## PLOT: ABBY Soil Temp

ggplot(abby_soil_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(title = "ABBY Site Yearly Avg Soil Temp (2016–2024)", x = "Year", y = "Avg Soil Temp (°C)") +
  theme_minimal()


## LOAD AND COMBINE PRECIPITATION — ABBY SITE

aprecip <- precipabby16_20x[, c("startDateTime", "secPrecipBulk")]
aprecip2 <- precipabby20_24x[, c("startDateTime", "secPrecipBulk")]
combinedabbyprecip <- rbind(aprecip, aprecip2)
combinedabbyprecip$startDateTime <- as.POSIXct(combinedabbyprecip$startDateTime)
combinedabbyprecip$year <- format(combinedabbyprecip$startDateTime, "%Y")
yearly_precipabby <- aggregate(secPrecipBulk ~ year, data = combinedabbyprecip, FUN = sum, na.rm = TRUE)
yearly_precipabby$site <- "ABBY"

write.csv(combinedabbyprecip, "combinedabbyprecip.csv")
write.csv(yearly_precipabby, "yearly_precipabby.csv")


## LOAD AND COMBINE PRECIPITATION — SOAP SITE

sprecip <- read.csv("precipsoap20_24.csv")
sprecip2 <- read.csv("precipsoap16_20.csv")

sprecipsub  <- sprecip[, c("startDateTime", "secPrecipBulk")]
sprecipsub2 <- sprecip2[, c("startDateTime", "secPrecipBulk")]
combinedsoapprecip <- rbind(sprecipsub, sprecipsub2)
combinedsoapprecip$startDateTime <- as.POSIXct(combinedsoapprecip$startDateTime)
combinedsoapprecip$year <- format(combinedsoapprecip$startDateTime, "%Y")
yearly_precip <- aggregate(secPrecipBulk ~ year, data = combinedsoapprecip, FUN = sum, na.rm = TRUE)
yearly_precip$site <- "SOAP"

write.csv(combinedsoapprecip, "combinedsoapprecip.csv")
write.csv(yearly_precip, "soapprecipavg.csv")


## PLOT: Combined Precipitation Trends

combined_precipsites <- rbind(yearly_precip, yearly_precipabby)
combined_precipsites$year <- as.numeric(as.character(combined_precipsites$year))

ggplot(combined_precipsites, aes(x = year, y = secPrecipBulk, color = site)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("ABBY" = "black", "SOAP" = "firebrick")) +
  labs(title = "Yearly Total Precipitation: SOAP vs ABBY",
       x = "Year", y = "Total Precipitation (mm)", color = "Site") +
  theme_minimal()


## LOAD AND CLEAN VEGETATION DATA

shrubs <- read.csv("shrubs")
nonwoody <- read.csv("non-woody")

shrubs_clean <- shrubs %>%
  select(date, scientificName, meanHeight, siteID)

nonwoody_clean <- nonwoody %>%
  select(date, scientificName, basalStemDiameter, siteID) %>%
  mutate(date = as.Date(date), year = year(date))

nw_abby <- nonwoody_clean %>%
  filter(siteID == "ABBY")

nw_abby_summary <- nw_abby %>%
  group_by(year) %>%
  summarise(mean_basal = mean(basalStemDiameter, na.rm = TRUE))


## JOIN PRECIP AND NON-WOODY DATA — ABBY SITE

yearly_precipabby$year <- as.numeric(as.character(yearly_precipabby$year))
df_combined <- left_join(nw_abby_summary, yearly_precipabby, by = "year")

## PLOT: RELATIONSHIP — Basal Stem vs Precip

ggplot(df_combined, aes(x = secPrecipBulk, y = mean_basal, color = factor(year))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "ABBY: Basal Stem Diameter vs Total Precipitation",
    x = "Total Precipitation (mm)",
    y = "Mean Basal Stem Diameter (cm)",
    color = "Year"
  ) +
  theme_minimal()


## LINEAR MODEL STATS BY YEAR

df_clean <- df_combined %>%
  filter(!is.na(mean_basal) & !is.na(secPrecipBulk))

model_results <- df_clean %>%
  group_by(year) %>%
  summarise(
    model = list(lm(mean_basal ~ secPrecipBulk)),
    r_squared = summary(lm(mean_basal ~ secPrecipBulk))$r.squared
  )

print(model_results)

