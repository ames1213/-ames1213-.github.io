# 🌱 NEON Ecological Data Analysis: ABBY & SOAP Sites

This project explores the relationships between **non-perennial vegetation cover**, **soil temperature**, and **precipitation** using National Ecological Observatory Network (NEON) data from the ABBY and SOAP sites.

---

##  Objective

To assess how environmental variables, specifically **soil temperature** and **precipitation**, correlate with **non-perennial vegetation cover** across seasons and years at two NEON field sites.

---

## 📍 Study Sites

- **ABBY**: Abbee Field Site, Oregon, USA
- **SOAP**: Soaproot Saddle, California, USA

---

## Key Findings

- **Seasonal Patterns**: Soil temperature and vegetation cover both show clear seasonal trends.
- **Precipitation-Vegetation Link**: Higher precipitation corresponds with increased non-perennial vegetation.
- **Site Differences**: SOAP tends to show more pronounced seasonal extremes in both vegetation and temperature.

---

## Visualizations

- **Visualized** using ggplot 2, plots were created for each site to display the average basal stem diameter and precipitation or soil temperature, respectively. Plots are available on the poster. 

---

## Abstract

- 	Primary productivity forms the foundation of all food webs and ecosystems, and autotrophic success reflects ecosystem health. However, rapid and dramatic changes in meteorological conditions due to climate change can disrupt the seasonal cycles of plants or reduce growth rates. This study utilizes public ecological datasets available on the National Ecological Observatory Network (NEON), specifically plant productivity data on non-herbaceous perennial vegetation and meteorological data on average annual precipitation and soil temperature from the past eight years (2016-2024). NEON’s standardized, long-term monitoring enables comprehensive site comparisons and facilitates the analysis of ecological trends across various time scales. The data will be narrowed to shrub groups and non-woody perennial vegetation in two sites, one in California and one in Washington, selected based on regional data availability, aiming to identify a correlation between changes in plant size and weather conditions. 
The predicted correlation is that locations experiencing frequent meteorological changes uncharacteristic to the region, such as soil temperature variations and altered precipitation patterns, will display a reduction in primary productivity success, specifically reduced size and density of plant species. Coastal climates are particularly vulnerable to climate change due to their oceanic proximity, increasing their susceptibility to natural disasters that alter precipitation patterns. Changes in primary productivity can also disrupt food chains; linking these shifts to climate change may help predict future trophic trends and guide ecosystem management strategies to mitigate biodiversity loss.

 ---

 ## Code Used For Cleaning NEON Data 

library(ggplot2)
library(dplyr)
library(lubridate)


### LOAD AND COMBINE SOIL TEMP — SOAP SITE

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


### PLOT: SOAP Soil Temp

ggplot(soap_soil_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(title = "SOAP Site Yearly Avg Soil Temp (2016–2024)", x = "Year", y = "Avg Soil Temp (°C)") +
  theme_minimal()


### LOAD AND COMBINE SOIL TEMP — ABBY SITE

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


# PLOT: ABBY Soil Temp

ggplot(abby_soil_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(title = "ABBY Site Yearly Avg Soil Temp (2016–2024)", x = "Year", y = "Avg Soil Temp (°C)") +
  theme_minimal()


### LOAD AND COMBINE PRECIPITATION — ABBY SITE

aprecip <- precipabby16_20x[, c("startDateTime", "secPrecipBulk")]
aprecip2 <- precipabby20_24x[, c("startDateTime", "secPrecipBulk")]
combinedabbyprecip <- rbind(aprecip, aprecip2)
combinedabbyprecip$startDateTime <- as.POSIXct(combinedabbyprecip$startDateTime)
combinedabbyprecip$year <- format(combinedabbyprecip$startDateTime, "%Y")
yearly_precipabby <- aggregate(secPrecipBulk ~ year, data = combinedabbyprecip, FUN = sum, na.rm = TRUE)
yearly_precipabby$site <- "ABBY"

write.csv(combinedabbyprecip, "combinedabbyprecip.csv")
write.csv(yearly_precipabby, "yearly_precipabby.csv")


### LOAD AND COMBINE PRECIPITATION — SOAP SITE

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


### PLOT: Combined Precipitation Trends

combined_precipsites <- rbind(yearly_precip, yearly_precipabby)
combined_precipsites$year <- as.numeric(as.character(combined_precipsites$year))

ggplot(combined_precipsites, aes(x = year, y = secPrecipBulk, color = site)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("ABBY" = "black", "SOAP" = "firebrick")) +
  labs(title = "Yearly Total Precipitation: SOAP vs ABBY",
       x = "Year", y = "Total Precipitation (mm)", color = "Site") +
  theme_minimal()


### LOAD AND CLEAN VEGETATION DATA

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


### JOIN PRECIP AND NON-WOODY DATA — ABBY SITE

yearly_precipabby$year <- as.numeric(as.character(yearly_precipabby$year))
df_combined <- left_join(nw_abby_summary, yearly_precipabby, by = "year")


### PLOT: RELATIONSHIP — Basal Stem vs Precip

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


### LINEAR MODEL STATS BY YEAR

df_clean <- df_combined %>%
  filter(!is.na(mean_basal) & !is.na(secPrecipBulk))

model_results <- df_clean %>%
  group_by(year) %>%
  summarise(
    model = list(lm(mean_basal ~ secPrecipBulk)),
    r_squared = summary(lm(mean_basal ~ secPrecipBulk))$r.squared
  )

print(model_results)


 ---

  ## Results

   - None of the P-values were statistically significant, though there was an indication of correlation between the success of the plant (growth represented by basal stem diameter) and higher precipitation and lower soil temperature. 
