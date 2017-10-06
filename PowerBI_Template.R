
# Sample Script and Data to Create Power BI GEDSB Template ----------------

library (tidyverse)
library(readxl)
TEMP <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/PowerBI/TemplateData.xlsx", 
                   sheet = "Sheet1")  %>%
  gather (key, 
          value, 
          -SchoolMident, 
          -StudentOEN) %>%
  mutate (Raw = substr(key, 18, 20),
          Raw = ifelse (Raw == "Dot", "Raw", "Level"), 
          Assessment  = substr (key,1,1),
          Assessment = ifelse (Assessment == "R", "Reading",
                               ifelse (Assessment == "W", "Writing",
                                       "Math"))) %>%
  select (Mident = SchoolMident,
          OEN = StudentOEN,
          Assessment, 
          Raw,
          value)
