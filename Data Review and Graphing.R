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

install.packages("readxl")
library(readxl)
# install.packages("xlsx")

install.packages("tidyr")
library(tidyr) # For rearranging the incident dataset 

install.packages("dplyr")
library(dplyr) # For rearranging the incident dataset

install.packages("plotly")
library(plotly)

df_incident <- read_xlsx("D:/Bangladesh/05_hydrological_meteorological/01 - Input Data/01_incident_reports/210916_SM_Daily_Incident_From2018_To2021-Sep14.xlsx")

# rename the column names to remove spaces:
colnames(df_incident) <- gsub(pattern = " ", replacement = "_", x = colnames(df_incident))


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


# Reformat the Rainfall / Weather Data to Daily ---------------------------


# Create Graphs -----------------------------------------------------------

plot_ly(x = df_flood_daily$Date_of_incident, 
        y = df_flood_daily$Affected_individuals,
        type = 'scatter', mode = 'lines+markers',
        hovertext = df_flood_daily$Incident_Details)

# Create Output -----------------------------------------------------------


