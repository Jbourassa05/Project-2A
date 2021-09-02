# GDP Data And Emission Efficencies
library(readr)
library(tidyverse)
#GDP Prep
Prov_GDP_Industry <- read_csv("Projet 2A/04-Data/CSV/GDP/Prov_GDP_Industry.csv")

GDP<-Prov_GDP_Industry%>%
  spread(Industry, GDP.Million)%>%
  mutate(`Crop and Animal Production` = `Crop production` + `Animal production and aquaculture`)%>%
  rename(`Animal Production` = `Animal production and aquaculture`)%>%
  gather(Industry, GDP.Million, 3:6)

# Agriculture Emissions:

CanadianEconomicSector_Ag <- read_csv("Projet 2A/04-Data/CSV/Emissions/CanadianEconomicSector_Ag.csv")

Ag.Emissions<-CanadianEconomicSector_Ag%>%
  select(-Measure)%>%
  spread(Source, Mt.CO2eq)%>%
  select(-Agriculture, -`On Farm Fuel Use`)%>%
  mutate(`Crop and Animal Production` = `Crop Production` + `Animal Production`)%>%
  filter(Region %in% c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan"))%>%
  rename (Geo = Region,
          `Crop production` = `Crop Production`)%>%
  gather(Industry, Mt.CO2eq, 3:5)

#Total Emissions Estimate
IPCC_EM <- read_csv("Projet 2A/04-Data/CSV/Emissions/IPCC_EM.csv")

IPCC_EM<-IPCC_EM%>%
  select(-Measure)%>%
  filter(Category %in% c("Total"))%>%
  spread(Category, Mt.CO2eq)%>%
  rename(`All industries` = Total,
         Geo = Region)%>%
  gather(Industry, Mt.CO2eq, 3:3)

# Full Dataset
Ag.Emissions <- rbind(Ag.Emissions, IPCC_EM)%>%
  filter(Year>=2000)

GDP <-full_join(GDP, Ag.Emissions, by = c("Year"="Year", "Geo"="Geo", "Industry"="Industry"))%>%
  filter(Geo %in% c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan"),
         Year != 2020)

#Intensity measures

GDP_Data <- GDP%>%
  mutate(t.CO2eq = Mt.CO2eq*1000000,
         t.CO2eq.GDP = t.CO2eq/GDP.Million)


# Exporting Data

write.csv(GDP_Data,'GDP_Emissions_Data.csv')
