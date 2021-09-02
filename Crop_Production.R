# Total Area by Province - 1990-2019

library(readr)
Fruit_Area <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Fruit_Area.csv")
Vegetable_Area <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Vegetable_Area.csv")
Pasture_Estimate <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Pasture_Estimate.csv")
FieldCrop_Area <- read_csv("Projet 2A/04-Data/CSV/Crop Production/FieldCrop_Area.csv")
Potatoes_Area <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Potatoes_Area.csv")
library(tidyverse)
Potatoes_Area<-select(Potatoes_Area, -Area)
Fruit_Area<-select(Fruit_Area, -Area)
FieldCrop_Area<-select(FieldCrop_Area, -Area)
Vegetable_Area<-select(Vegetable_Area, -Area)

Potatoes_Area<-Potatoes_Area%>%
  group_by(Year, Geo)%>%
  summarise(Potatoes = sum(Ha))

Fruit_Area<-Fruit_Area%>%
  group_by(Year, Geo)%>%
  summarise(Fruit_Area = sum(Ha))

FieldCrop_Area<-FieldCrop_Area%>%
  group_by(Year, Geo)%>%
  summarise(FieldCrop_Area = sum(Ha))

Vegetable_Area<-Vegetable_Area%>%
  group_by(Year, Geo)%>%
  summarise(Vegetable_Area = sum(Ha))

Pasture_Area<-Pasture_Estimate%>%
  gather(Geo, Pasture_Area, 2:11)

Total.ha <-full_join(Potatoes_Area,Fruit_Area, by=c("Year"="Year", "Geo"="Geo"))
Total.ha <-full_join(Total.ha ,FieldCrop_Area, by=c("Year"="Year", "Geo"="Geo"))
Total.ha <-full_join(Total.ha ,Vegetable_Area, by=c("Year"="Year", "Geo"="Geo"))
Total.ha <-full_join(Total.ha ,Pasture_Area, by=c("Year"="Year", "Geo"="Geo"))

Total.ha<-Total.ha%>%
  replace(is.na(.), 0)%>%
  mutate(Total.Ha = Potatoes + Fruit_Area + FieldCrop_Area+ Vegetable_Area +Pasture_Area,
         Crop.Area = Potatoes + Fruit_Area + FieldCrop_Area+ Vegetable_Area)%>%
  select(Year, Geo, Total.Ha, Crop.Area )

# Total Production

Vegetable_Production <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Vegetable_Production.csv")
Potatoes_Production <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Potatoes_Production.csv")
Fruit_Production <- read_csv("Projet 2A/04-Data/CSV/Crop Production/Fruit_Production.csv")
FieldCrop_Production <- read_csv("Projet 2A/04-Data/CSV/Crop Production/FieldCrop_Production.csv")

Potatoes_Production<-select(Potatoes_Production, -Production)%>%
  group_by(Year, Geo)%>%
  summarise(Potatoes = sum(Metric.Tonne))
Vegetable_Production <-select(Vegetable_Production , -Production)%>%
  group_by(Year, Geo)%>%
  summarise(Vegetable = sum(Metric.Tonne))
Fruit_Production <-select(Fruit_Production , -Production)%>%
  group_by(Year, Geo)%>%
  summarise(Fruit = sum(Metric.Tonne))
FieldCrop_Production <-select(FieldCrop_Production , -Production)%>%
  group_by(Year, Geo)%>%
  summarise(FieldCrop = sum(Metric.Tonne))

Total.mt <-full_join(Potatoes_Production,Vegetable_Production, by=c("Year"="Year", "Geo"="Geo"))
Total.mt <-full_join(Total.mt,Fruit_Production, by=c("Year"="Year", "Geo"="Geo"))
Total.mt <-full_join(Total.mt,FieldCrop_Production, by=c("Year"="Year", "Geo"="Geo"))

Total.mt<-Total.mt%>%
  replace(is.na(.), 0)%>%
  mutate(Total.mt = Potatoes + Vegetable + Fruit+ FieldCrop)%>%
  select(Year, Geo, Total.mt)

# Grouping Production and Area Data

Crop_Production<-full_join(Total.mt,Total.ha,by=c("Year"="Year", "Geo"="Geo"))


# Emission Data - Canadian Economic Sector

CanadianEconomicSector_Ag <- read_csv("Projet 2A/04-Data/CSV/Emissions/CanadianEconomicSector_Ag.csv")
View(CanadianEconomicSector_Ag)

CP_CES_EM<-CanadianEconomicSector_Ag%>%
  select(-Measure)%>%
  filter(Source == "Crop Production")%>%
  select(-Source)%>%
  mutate(CES.t.CO2eq = Mt.CO2eq*1000000)%>%
  rename(Geo = Region)%>%
  select(-Mt.CO2eq)%>%
  filter(Geo %in% c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan"))

Crop_Production<-full_join(Crop_Production,CP_CES_EM,by=c("Year"="Year", "Geo"="Geo"))

# Emission Data - IPCC Sector
IPCC_Ag_EM <- read_csv("Projet 2A/04-Data/CSV/Emissions/IPCC_Ag_EM.csv")
View(IPCC_Ag_EM)

CP_IPCC_EM<- IPCC_Ag_EM%>%
  filter(Region %in% c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan"),
         Category != "Agriculture",
         Category != "Enteric Fermentation",
         Category != "Manure Management")%>%
  mutate(t.CO2eq = Mt.CO2eq*1000000)%>%
  select(-Mt.CO2eq)%>%
  spread(Category, t.CO2eq)%>%
  mutate(IPCC_t.CO2eq_Total = `Direct Sources - Agricultural Soils`+`Indirect Sources - Agricultural Soils`+`Field Burning of Agricultural Residues`+`Liming, Urea Application and Other Carbon-containing Fertilizers`,
         IPCC_t.CO2eq_Ag.Soil = `Direct Sources - Agricultural Soils`+ `Indirect Sources - Agricultural Soils`)%>%
  rename(Geo = Region)%>%
  select(Year, Geo, IPCC_t.CO2eq_Total, IPCC_t.CO2eq_Ag.Soil)

Crop_Production<-full_join(Crop_Production,CP_IPCC_EM,by=c("Year"="Year", "Geo"="Geo"))

# Production Data

Crop_Production<-Crop_Production %>%
  #mutate(#Crop.EM.Production = CES.t.CO2eq/Total.mt,
         #Crop.EM.Area = CES.t.CO2eq/Total.Ha,
         #Ag.Soil.Production = IPCC_t.CO2eq_Ag.Soil/Total.mt,
         #Ag.Soil.Area = IPCC_t.CO2eq_Ag.Soil/Total.Ha)%>%
  filter(Year <=2019)
  
# Exporting Data

write.csv(Crop_Production,'Crop_Production_Data.csv')
