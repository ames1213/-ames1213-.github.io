install.packages("neonUtilities")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(neonUtilities)
library(dplyr)
library(ggplot2)
library(tidyr)

#downloading the large vegetation structure data for California and Washington
#focusing on 10-year span of 2014 to 2024
vegetation <- loadByProduct(dpID="DP1.10098.001", 
                            site=c("SOAP","ABBY"),
                            startdate="2014-08", 
                            enddate="2024-08")
names(vegetation)
idk <- vegetation$"vst_apparentindividual"
#isolating Non-herbaceous perennial vegetation structure foe eagle ferns
perennialnw <- vegetation$"vst_non-woody"
write.csv(perennialnw, "non-woody")

#viewing downloaded data and identifying the focus group, vst_shrubgroup
shrubs <- vegetation$"vst_shrubgroup"

cleanshrubs <- shrubs %>% drop_na(meanHeight)

#identifying and cleaning second focus group
cleannw <- perennialnw %>% drop_na(basalStemDiameter, meanLeafLength)
write.csv(shrubs, "shrubs")

#plot1
ggplot(cleanshrubs, aes(x = as.factor(date), y = meanHeight)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Mean Leaf Length by Leaf Number", 
       x = "date", 
       y = "meanHeight") +
  theme_minimal()

#plot to visualize changes in leaf length across the years of observations
ggplot(cleannw, aes(x = date, y = meanLeafLength)) +
  geom_line(color = "blue") +  
  labs(title = "Mean Leaf Length Over Time",
       x = "Date",
       y = "Mean Leaf Length") +
  theme_minimal()

#potting the stem diameter of the ferns against time
ggplot(cleannw, aes(x = date, y = basalStemDiameter)) +
  geom_line(color = "blue") +  
  labs(title = "Basal Stem Diameter Over Time",
       x = "Date",
       y = "Basal Stem Diameter") +
  theme_minimal()

#visualizing outliers in the data set
boxplot(cleanfr$basalStemDiameter, main = "Boxplot of Basal Stem Diameter")
#removing top 1%
clean <- cleanfr %>% filter(basalStemDiameter < quantile(basalStemDiameter, 0.99))

#improving appearance of graph
ggplot(perennialnw, aes(x = date, y = basalStemDiameter)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1) +
  labs(title = "Basal Stem Diameter Over Time",
       x = "Date",
       y = "Basal Stem Diameter") +
  theme_minimal()

#uploading precipitation data for my SOAP
precipsoap20_24 <- loadByProduct(dpID="DP1.00006.001", 
                        site=c("SOAP"),
                        startdate="2020-09", 
                        enddate="2024-08",
                        timeIndex=30)
precipsoap20_24x <- precipsoap20_24$"SECPRE_30min"
write.csv(precipsoap20_24x, "precipsoap20_24.csv")

precipsoap16_20 <- loadByProduct(dpID="DP1.00006.001", 
                                 site=c("SOAP"),
                                 startdate="2016-09", 
                                 enddate="2020-09",
                                 timeIndex=30)
precipsoap16_20x <- precipsoap16_20$"SECPRE_30min"
write.csv(precipsoap16_20x, "precipsoap16_20.csv")

#precipitation data for ABBY LEFT OFF HERE
precipabby20_24 <- loadByProduct(dpID="DP1.00006.001", 
                                 site=c("ABBY"),
                                 startdate="2020-09", 
                                 enddate="2024-08",
                                 timeIndex=30)
precipabby20_24x <- precipabby20_24$"SECPRE_30min"
write.csv(precipabby20_24x, "precipabby20_24.csv")

precipabby16_20 <- loadByProduct(dpID="DP1.00006.001", 
                                 site=c("ABBY"),
                                 startdate="2016-09", 
                                 enddate="2020-09",
                                 timeIndex=30)
precipabby16_20x <- precipsoap16_20$"SECPRE_30min"
write.csv(precipabby16_20x, "precipsoap16_20.csv")

#uploading soil temperature data for SOAP CA site, all sites were too large for upload                                  
#2022-2024
soiltempsoap22_24 <- loadByProduct(dpID="DP1.00041.001", 
                                   site=c("SOAP"),
                                   startdate="2022-09", 
                                   enddate="2024-08",
                                   timeIndex=30,
                                   check.size=TRUE)
soiltempsoap22_24 <- soiltempsoap22_24$"ST_30_minute"
write.csv(soiltempsoap22_24, "soiltempsoap22_24.csv")


#2020-2022
soiltempsoap20_22 <- loadByProduct(dpID="DP1.00041.001", 
                              site=c("SOAP"),
                              startdate="2020-09", 
                              enddate="2022-08",
                              timeIndex=30,
                              check.size=TRUE)
soiltempsoap20_22 <- soiltempsoap20_22$"ST_30_minute"
write.csv(soiltempsoap20_22, "soiltempsoap20_22.csv")

#2018-2020
soiltempsoap18_20 <- loadByProduct(dpID="DP1.00041.001", 
                                   site=c("SOAP"),
                                   startdate="2018-09", 
                                   enddate="2020-08",
                                   timeIndex=30,
                                   check.size=TRUE)
soiltempsoap18_20 <- soiltempsoap18_20$"ST_30_minute"
write.csv(soiltempsoap18_20, "soiltempsoap18_20.csv")

#2016-2018
soiltempsoap16_18 <- loadByProduct(dpID="DP1.00041.001", 
                                   site=c("SOAP"),
                                   startdate="2022-09", 
                                   enddate="2024-08",
                                   timeIndex=30,
                                   check.size=TRUE)
soiltempsoap16_18 <- soiltempsoap16_18$"ST_30_minute"
write.csv(soiltempsoap16_18, "soiltempsoap16_218.csv")

#next site
soiltempabby20_24 <- loadByProduct(dpID="DP1.00041.001", 
                                   site=c("ABBY"),
                                   startdate="2020-09", 
                                   enddate="2024-08",
                                   timeIndex=30,
                                   check.size=TRUE)
soiltempabby20_24 <- soiltempabby20_24$"ST_30_minute"
write.csv(soiltempabby20_24, "soiltempabby20_24.csv")

soiltempabby16_20 <- loadByProduct(dpID="DP1.00041.001", 
                                   site=c("ABBY"),
                                   startdate="2016-09", 
                                   enddate="2020-09",
                                   timeIndex=30,
                                   check.size=TRUE)
soiltempabby16_20 <- soiltempabby16_20$"ST_30_minute"
write.csv(soiltempabby16_20, "soiltempabby16_20.csv")

#SAVING THESE DAMN FILES
save.image(file = "my_workspace.Rdata")
load(file = "my_workspace.Rdata")

#isolating 30 minute increments for the soil data and saving csv
soiltempsoap30 <- soiltempsoap$"ST_30_minute"
write.csv(soiltempsoap30, "soiltempsoap22_24.csv")
soiltempsoap22_24 <- read.csv("soiltempsoap22_24.csv")


#practice plot of temp and time
ggplot(data = soiltempsoap22_24, aes(x = startDateTime, y = soilTempMean)) +
  geom_point() +                       
  labs(title = "Soil Temperature vs Time", 
       x = "height", 
       y = "temp") +
  theme_minimal()   

# Add a new column to each dataset to label the dataset source
soiltempabby20_24$dataset <- "Abby20-24"
soiltempsoap20_22$dataset <- "Soap20-22"

# Combine the two datasets
combined_data <- bind_rows(soiltempabby20_24, soiltempsoap20_22)
