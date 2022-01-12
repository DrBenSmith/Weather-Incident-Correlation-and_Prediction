# ---

# Ben Smith 
# January 2021 
# REACH BGD Mission

# ---

# The aim of this script is to generate an output displaying the recorded 
# daily rainfall for CXB against the incident data for the camps. This 
# should then be able to inform actors as to the likely result, and 
# therefore response, required for incoming rainfall. Once completed,
# this should be presented to the IMAWG and the EPRWG for feedback and 
# dissemination.


# Preamble ----------------------------------------------------------------

# install.packages("readxl")
library(readxl)
# install.packages("xlsx")

# install.packages("tidyr")
library(tidyr) # For rearranging the incident dataset 

# install.packages("dplyr")
library(dplyr) # For rearranging the incident dataset

# install.packages("plotly")
library(plotly)

df_incident <- read_xlsx("../../01 - Input Data/01_incident_reports/210916_SM_Daily_Incident_From2018_To2021-Sep14.xlsx")

# rename the column names to remove spaces:
colnames(df_incident) <- gsub(pattern = " ", replacement = "_", x = colnames(df_incident))

# Load in the rainfall data:
df_cxb_2019 <- read_xlsx("../../06 - Output and Published Datasets/reach_bgd_nathaz_tf_hydrometeorlogical_incidents_jan2019_-_jan_2020_v2.xlsx", sheet = "Database")

df_cxb_2020 <- read_xlsx("../../06 - Output and Published Datasets/REACH_BGD_NatHazTWG - Hydro-Meteorological Incident Database 2020.xlsx", sheet = "Hydro-Met Incident DB (daily)")

df_cxb_2021 = read.csv("../../02 - Reformatted Data/BMD Weather Data - 2021 - Cox Bazar.csv")

# Reformat the rainfall rainfall data:
  
  # Convert 2019 to daily
  df_cxb_2019 <- df_cxb_2019 %>% 
    mutate(Date = as.Date(Date_time, format = "%Y-%m-%d")) %>%
    select(Date, "Interval_GSB Coxs Bazaar-1227") %>%
    group_by(Date) %>%
    summarise(Daily_Rainfall = sum(`Interval_GSB Coxs Bazaar-1227`, na.rm = T)) %>%
    filter(format(Date, "%Y") != "2020") %>%
    mutate(Date = as.POSIXct(Date))

  # Convert 2020 date to POSIXct, rainfall to numeric and remove mysterious duplicate:
  df_cxb_2020$Date = as.POSIXct(df_cxb_2020$Date)
  df_cxb_2020$Daily_Rainfall <- as.numeric(df_cxb_2020$`rain.GSB Cox's Bazaar-1227`)
  df_cxb_2020 = df_cxb_2020[-which(duplicated(df_cxb_2020$Date)),]

  # Convert 2021 to POSIXct and numeric:
  df_cxb_2021$Date = as.POSIXct(as.Date(df_cxb_2021$`ï..Date`,  "%d/%m/%Y"))
  df_cxb_2021$Daily_Rainfall = as.numeric(gsub(pattern = "Tr", 
                                               replacement = "0.5", 
                                               x = df_cxb_2021$`Rainfall..mm.`))

df_rainfall <- rbind(df_cxb_2019, 
                     df_cxb_2020 %>% select(Date, Daily_Rainfall),
                     df_cxb_2021 %>% select(Date, Daily_Rainfall))

# Fill the missing rainfall dates:
dates = data.frame(Date = seq.POSIXt(min(df_rainfall$Date), max(df_rainfall$Date), 
                                   by = "day"))
df_rainfall = dates %>% full_join(df_rainfall)


# Check the Input Data ----------------------------------------------------
View(head(df_incident))

df_flood <- df_incident %>% 
  filter(Type_of_incident == "Flood") %>%
  select(Camp, Date_of_incident, Number_of_incidents, 
         Incident_Details, Affected_HHs, Affected_individuals, 'Casualties_(ind)',
         Injured_ind, Displaced_HH, Displaced_ind,
         Partially_Damaged_shelters, Totally_Damaged_shelters, Damaged_Shelter_2018,
         Damaged_waterpoints, Damaged_health_facilities, Damaged_food_distribution_site)


# Reformat the Incident Data to Daily -------------------------------------

df_flood_daily = df_flood %>% 
  select(-Incident_Details) %>%
  group_by(Date_of_incident) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
  
# Aggregate the text columns into single entries per day, for later...
temp_text <- df_flood %>% 
  select(Date_of_incident, Camp, Incident_Details) %>%
  group_by(Date_of_incident) %>% 
  mutate(Camp = paste0(Camp, collapse = "; "),
         Incident_Details = paste0(Incident_Details, collapse = "; ")) %>%
  distinct()
  
df_flood_daily <- df_flood_daily %>% left_join(temp_text, by = "Date_of_incident") 

rm(temp_text)

# Create Graphs -----------------------------------------------------------

plot_ly(x = df_flood_daily$Date_of_incident, 
        y = df_flood_daily$Affected_individuals,
        type = 'scatter', mode = 'lines+markers',
        hovertext = df_flood_daily$Incident_Details)

vlines = data.frame(
  x = c(t(data.frame(a = df_flood_daily$Date_of_incident, 
                     b = df_flood_daily$Date_of_incident,
                     c = rep(NA, nrow(df_flood_daily))))),
  y = c(matrix(rep(c(1,-20,NA), length=3*nrow(df_flood_daily)), nrow=3)))


plot_ly(x = vlines$x, y = vlines$y,
        type = "scatter", mode = "lines",
        name = "Flooding_incidents") %>%
  add_trace(x = df_rainfall$Date, y = df_rainfall$Daily_Rainfall,
            mode = "lines", type = "scatter",
            name = "Rainfall")



df_merge = df_rainfall %>% left_join()  
  # add_trace(x = df_flood_daily$Date_of_incident,
  #           y = df_flood_daily$Affected_individuals,
  #           type = 'scatter', mode = 'markers',
  #           name = "Incidents", yaxis = "y2",
  #           hovertext = df_flood_daily$Incident_Details) %>%
  # layout(yaxis2 = list(side = "right",  overlaying = "y", sharey = T))
  

# Create Output -----------------------------------------------------------


