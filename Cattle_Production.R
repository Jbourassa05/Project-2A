# Animal Production
library(readr)
library(tidyverse)

Cattle_Population <- read_csv("Projet 2A/04-Data/CSV/Animal Production/Cattle.Population.csv")
View(Cattle_Population)

Cattle_Population<-select(Cattle_Population, -VALUE)%>%
  spread(Cattle.Type, Head.Cattle)%>%
  mutate(Dairy.Cattle = `Dairy cows`+`Heifers for dairy replacement`,
         Non.Dairy.Cattle = `Total cattle` - Dairy.Cattle)%>%
  select(Year, GEO, Dairy.Cattle, Non.Dairy.Cattle)%>%
  gather(Cattle.Type, Population, 3:4)

#Emision factors

Cattle_EM <- read_csv("Projet 2A/04-Data/CSV/Animal Production/Cattle._EM.csv")

Cattle_EM<-gather(Cattle_EM, Cattle.Type, Emission.Factor, 2:3)
Cattle_Production <- full_join(Cattle_Population,Cattle_EM, by = c("Year"= "Year", "Cattle.Type"="Cattle.Type"))

Cattle_Production <- Cattle_Production %>%
  mutate(kg.CO2eq = Population*Emission.Factor,
         kt.CO2eq = kg.CO2eq/1000000)%>%
  filter(Year <=2019)

# Estimating Average emissions per head of cattle

T.Population <- Cattle_Production%>%
  group_by(Year, GEO)%>%
  summarise(Total.Population = sum(Population))
T.Emissions <-Cattle_Production%>%
  group_by(Year, GEO)%>%
  summarise(Total.Emissions = sum(kg.CO2eq))

Total.Cattle <-full_join(T.Population, T.Emissions, by = c("Year" = "Year", "GEO" = "GEO"))

Total.Cattle<-mutate(Total.Cattle,
                     Avg.Emissions = Total.Emissions/Total.Population,
                     Avg.Emissions.tCO2eq = Avg.Emissions/1000)
## Not best comparision but does highlight differences in emissions between beef and dairy production\
ggplot(Total.Cattle, aes(Year, Avg.Emissions.tCO2eq, color = GEO))+
  geom_line()

#Final Consolidation and Cleaning
Cattle_Production <- full_join(Cattle_Production,Total.Cattle, by = c("Year"= "Year", "GEO"="GEO"))

write.csv(Cattle_Production,'Cattle_Production.csv')
