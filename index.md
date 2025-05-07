# Load necessary libraries

install.packages("neonUtilities")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(neonUtilities)
library(dplyr)
library(ggplot2)
library(tidyr)

# ================================
# Soil Temperature Distribution Plot
# ================================
ggplot(combined_data, aes(x = soilTempMean, color = dataset)) +
  geom_density() +
  labs(title = "Soil Temperature Distributions",
       x = "Soil Temperature Mean",
       y = "Density") +
  theme_minimal()

# ================================
# Precipitation Data for ABBY
# ================================
combinedabbyprecip <- bind_rows(precipabby16_20x, precipabby20_24x)

# ================================
# SOAP Soil Temperature: Combine and Average
# ================================
soil_temp_list <- list(
  read.csv("soiltempsoap22_24.csv"),
  read.csv("soiltempsoap20_22.csv"),
  read.csv("soiltempsoap18_20.csv"),
  read.csv("soiltempsoap16_218.csv")
)

combined_soil_temp <- bind_rows(lapply(soil_temp_list, function(df) df[, c("startDateTime", "soilTempMean")]))
combined_soil_temp$startDateTime <- as.POSIXct(combined_soil_temp$startDateTime)
combined_soil_temp$year <- format(combined_soil_temp$startDateTime, "%Y")

soap_yearly_avg <- aggregate(soilTempMean ~ year, data = combined_soil_temp, FUN = mean, na.rm = TRUE)
write.csv(soap_yearly_avg, "soapsoiltempavg.csv")

ggplot(soap_yearly_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "SOAP Site Yearly Average Soil Temperature (2016–2024)",
       x = "Year", y = "Average Soil Temp (°C)") +
  theme_minimal()

# ================================
# ABBY Soil Temperature
# ================================
abby_soil_temp <- bind_rows(
  read.csv("soiltempabby20_24.csv")[, c("startDateTime", "soilTempMean")],
  read.csv("soiltempabby16_20.csv")[, c("startDateTime", "soilTempMean")]
)

abby_soil_temp$startDateTime <- as.POSIXct(abby_soil_temp$startDateTime)
abby_soil_temp$year <- format(abby_soil_temp$startDateTime, "%Y")

abby_yearly_avg <- aggregate(soilTempMean ~ year, data = abby_soil_temp, FUN = mean, na.rm = TRUE)
write.csv(abby_yearly_avg, "abbysoiltempavg.csv")

ggplot(abby_yearly_avg, aes(x = as.numeric(year), y = soilTempMean)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "ABBY Site Yearly Average Soil Temperature (2016–2024)",
       x = "Year", y = "Average Soil Temp (°C)") +
  theme_minimal()

# ================================
# ABBY Precipitation
# ================================
aprecip <- precipabby16_20x[, c("startDateTime", "secPrecipBulk")]
aprecip2 <- precipabby20_24x[, c("startDateTime", "secPrecipBulk")]

combinedabbyprecip <- rbind(aprecip, aprecip2)
combinedabbyprecip$startDateTime <- as.POSIXct(combinedabbyprecip$startDateTime)
combinedabbyprecip$year <- format(combinedabbyprecip$startDateTime, "%Y")

yearly_precipabby <- aggregate(secPrecipBulk ~ year, data = combinedabbyprecip, FUN = sum, na.rm = TRUE)
write.csv(yearly_precipabby, "yearly_precipabby.csv")

ggplot(yearly_precipabby, aes(x = as.numeric(year), y = secPrecipBulk)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "ABBY Total Yearly Precipitation (2016–2024)",
       x = "Year", y = "Total Precipitation (mm)") +
  theme_minimal()

# ================================
# SOAP Precipitation
# ================================
sprecip <- read.csv("precipsoap20_24.csv")
sprecip2 <- read.csv("precipsoap16_20.csv")

sprecipsub <- sprecip[, c("startDateTime", "secPrecipBulk")]
sprecipsub2 <- sprecip2[, c("startDateTime", "secPrecipBulk")]

combinedsoapprecip <- rbind(sprecipsub, sprecipsub2)
combinedsoapprecip$startDateTime <- as.POSIXct(combinedsoapprecip$startDateTime)
combinedsoapprecip$year <- format(combinedsoapprecip$startDateTime, "%Y")

yearly_precip <- aggregate(secPrecipBulk ~ year, data = combinedsoapprecip, FUN = sum, na.rm = TRUE)
write.csv(yearly_precip, "soapprecipavg.csv")

ggplot(yearly_precip, aes(x = as.numeric(year), y = secPrecipBulk)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "SOAP Total Yearly Precipitation (2016–2024)",
       x = "Year", y = "Total Precipitation (mm)") +
  theme_minimal()

# ================================
# Combined Site Precipitation Plot
# ================================
yearly_precip$site <- "SOAP"
yearly_precipabby$site <- "ABBY"
combined_precipsites <- rbind(yearly_precip, yearly_precipabby)
combined_precipsites$year <- as.numeric(as.character(combined_precipsites$year))

ggplot(combined_precipsites, aes(x = year, y = secPrecipBulk, color = site)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2016:2024) +
  scale_color_manual(values = c("ABBY" = "black", "SOAP" = "firebrick")) +
  labs(title = "Yearly Total Precipitation: SOAP vs ABBY",
       x = "Year", y = "Total Precipitation (mm)",
       color = "Site") +
  theme_minimal()

# ================================
# Shrub and Non-Woody Plant Data
# ================================
shrubs <- read.csv("shrubs")
nonwoody <- read.csv("non-woody")

shrubs_clean <- shrubs %>% select(date, scientificName, meanHeight, siteID)
shrubs_abby <- shrubs_clean %>% filter(siteID == "ABBY")
shrubs_soap <- shrubs_clean %>% filter(siteID == "SOAP")

nw_clean <- nonwoody %>% select(date, scientificName, basalStemDiameter, siteID)
nw_abby <- nw_clean %>% filter(siteID == "ABBY")
nw_soap <- nw_clean %>% filter(siteID == "SOAP")

# Add year column
nw_abby <- nw_abby %>% mutate(year = lubridate::year(date))

# Summarize by year
nw_abby_summary <- nw_abby %>%
  group_by(year) %>%
  summarise(mean_basal = mean(basalStemDiameter, na.rm = TRUE))

# Merge with precipitation
precip_siteabby <- yearly_precipabby %>% mutate(year = as.numeric(year))
df_combined <- left_join(nw_abby_summary, precip_siteabby, by = "year")

# ================================
# Scatter + Regression Plot
# ================================
ggplot(df_combined, aes(x = secPrecipBulk, y = mean_basal, color = factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1), se = FALSE, color = "black") +
  labs(
    title = "Basal Stem Diameter vs Precipitation by Year (ABBY)",
    x = "Total Yearly Precipitation (mm)",
    y = "Mean Basal Stem Diameter (cm)",
    color = "Year"
  ) +
  theme_minimal()

# ================================
# Yearly Linear Regression Summaries
# ================================
df_clean <- df_combined %>%
  filter(!is.na(mean_basal) & !is.na(secPrecipBulk))

model_summaries <- df_clean %>%
  group_by(year) %>%
  summarize(
    model = list(lm(mean_basal ~ secPrecipBulk)),
    r_squared = summary(lm(mean_basal ~ secPrecipBulk))$r.squared
  )
