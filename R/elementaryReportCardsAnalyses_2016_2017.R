
# 2015-16 Elementary Report Card Analyses -----------------------------------------

setwd("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/20162017_School_Reports")
#setwd("H:/Student Data/ReportCard/schoolReports")
library(readxl)
library(foreign)
source ("https://raw.githubusercontent.com/grousell/Misc/master/R/preamble.R")

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)


# Load & Clean Data ---------------------------------------------------------------

student <- read.dbf ("C:/Users/grousell/OneDrive - Grand Erie DSB/Student Data/DBF from Planning/Student_Detail_2017_03_All.dbf",
                     as.is = TRUE) %>%
  select (SchoolMident = SchoolCode,
          OEN = StudentID,
          Gender = SexCode,
          SchoolName,
          IEP = Exceptiona) %>%
  mutate (OEN = as.numeric(OEN))

student$SchoolName <- gsub("/", "-", student$SchoolName)

student$schoolCode <- paste0(student$SchoolMident, "_", student$SchoolName)

eleT1 <- read_csv("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Elementary/20162017_Elem_Report_Card_R1.csv") %>%
  rename(Year = `School Year`,
         School = `Current School`,
         MIDENT = SchoolID,
         Name = `Student Name`,
         Grade = `Student Grade Level`)%>%
  filter (Grade >0)

eleT2 <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Elementary/20162017_Elem_Report_Card R2.xlsx",
                    sheet = "20162017_Elem_Report_Card_R2") %>%
  filter (`Student Grade Level` >0)


# Clean Term 2 so each student has own row --------------------------------

# Temp 1 ---------------
temp1 <- eleT2 %>%
  select (Year = `School Year`,
          School = `Current School`,
          SchoolID,
          Name = `Student Name`,
          Grade  = `Student Grade Level`,
          OEN,
          `Reporting Period`) %>%
  mutate (MISSING = na_zero(OEN)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)
# Temp 2 ---------------
temp2 <- eleT2 %>%
  select (OEN, 
          Responsibility,
          `Independent Work`,
          `Initiative`,
          `Organization`,
          `Collaboration`,
          `Self Regulation`) %>%
  mutate (MISSING = na_zero(Responsibility)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 3 ---------------
temp3 <- eleT2 %>%
  select (OEN,
          Reading,
          Writing,
          `Oral Communication`,
          `Media Literacy`) %>%
  mutate (MISSING = na_zero(Reading)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 4 ---------------
temp4 <- eleT2 %>%
  select (OEN,
          `French Listening`,
          `French Speaking`,
          `French Reading`,
          `French Writing`) %>%
  mutate (MISSING = na_zero(`French Listening`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 5 ---------------

temp5 <- eleT2 %>%
  select (OEN,
          `Number Sense and Numeration`,
          `Measurement`,
          `Geometry and Spatial Sense`,
          `Patterning and Algebra`,
          `Data Management`) %>%
  mutate (MISSING = na_zero(`Number Sense and Numeration`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup) 

# Temp 6 ---------------
temp6 <- eleT2 %>%
  select (OEN,
          `Science and Technology`) %>%
  mutate (MISSING = na_zero(`Science and Technology`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 7 ---------------
temp7 <- eleT2 %>%
  select (OEN,
          `Social Studies`) %>%
  mutate (MISSING = na_zero(`Social Studies`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 8 ---------------
temp8 <- eleT2 %>%
  select (OEN,
          `Health Education`) %>%
  mutate (MISSING = na_zero(`Health Education`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 9 ---------------
temp9 <- eleT2 %>%
  select (OEN,
          `Physical Education`) %>%
  mutate (MISSING = na_zero(`Physical Education`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 10 ---------------
temp10 <- eleT2 %>%
  select (OEN,
          Dance) %>%
  mutate (MISSING = na_zero(Dance)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 11 ---------------
temp11 <- eleT2 %>%
  select (OEN,
          Drama) %>%
  mutate (MISSING = na_zero(Drama)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 12 ---------------
temp12 <- eleT2 %>%
  select (OEN,
          Music) %>%
  mutate (MISSING = na_zero(Music)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Temp 13 ---------------
temp13 <- eleT2 %>%
  select (OEN,
          `Visual Arts`) %>%
  mutate (MISSING = na_zero(`Visual Arts`)) %>%
  filter (MISSING != 0) %>%
  mutate (dup = duplicated(OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-MISSING, -dup)

# Combine back to single data frame ---------------
eleT2 <-temp1 %>%
  left_join(temp2, by = c ("OEN")) %>%
  left_join(temp3, by = c ("OEN"))%>%
  left_join(temp4, by = c ("OEN"))%>%
  left_join(temp5, by = c ("OEN"))%>%
  left_join(temp6, by = c ("OEN"))%>%
  left_join(temp7, by = c ("OEN"))%>%
  left_join(temp8, by = c ("OEN"))%>%
  left_join(temp9, by = c ("OEN"))%>%
  left_join(temp10, by = c ("OEN"))%>%
  left_join(temp11, by = c ("OEN"))%>%
  left_join(temp12, by = c ("OEN"))%>%
  left_join(temp13, by = c ("OEN"))

remove (temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12, temp13 )



eleT1 <- inner_join(eleT1 %>%
                      mutate (OEN = parse_number(OEN)), 
                    student, by = c ("OEN")) 

eleT2 <- inner_join(eleT2 %>%
                      mutate (OEN = parse_number(OEN)), 
                    student, by = c ("OEN")) 

remove (student)

# Recode Variables T1 and T2 --------------------------------------------------------

eleT1 <- eleT1 %>%
  mutate (Grade = recode(Grade, 
                         "1" = "Grade 1",
                         "2" = "Grade 2",
                         "3" = "Grade 3",
                         "4" = "Grade 4",
                         "5" = "Grade 5",
                         "6" = "Grade 6",
                         "7" = "Grade 7",
                         "8" = "Grade 8"),
          School = gsub ("Public", "", School),
          School = gsub("School", "", School),
          School = gsub("Elementary", "", School),
          School = trim.trailing(School))

eleT2 <- eleT2 %>%
  mutate (Grade = recode(Grade, 
                         "1" = "Grade 1",
                         "2" = "Grade 2",
                         "3" = "Grade 3",
                         "4" = "Grade 4",
                         "5" = "Grade 5",
                         "6" = "Grade 6",
                         "7" = "Grade 7",
                         "8" = "Grade 8"),
          School = gsub ("Public", "", School),
          School = gsub("School", "", School),
          School = gsub("Elementary", "", School),
          School = trim.trailing(School))

# Separate Learning Skills and Courses ------------------------------------

eleT1Learn <- eleT1 %>%
  select (Year, 
          School, 
          OEN, 
          Name, 
          Grade, 
          IEP,
          Responsibility, 
          `Independent Work`, 
          Initiative, 
          Organization, 
          Collaboration,
          `Self Regulation`)

eleT2Learn <- eleT2 %>%
  select (Year, 
          School, 
          OEN, 
          Name, 
          Grade, 
          IEP,
          Responsibility, 
          `Independent Work`, 
          Initiative, 
          Organization, 
          Collaboration,
          `Self Regulation`)

eleT1Course <- eleT1 %>%
  select(Year, 
         School, 
         OEN, 
         Name, 
         Grade, 
         IEP,
         `French Listening`,
         `French Speaking`,
         `French Reading`,
         `French Writing`,
         Reading,
         Writing,
         `Oral Communication`,
         `Media Literacy`,
         `Number Sense` = `Number Sense and Numeration`,
         Measurement,
         Geometry = `Geometry and Spatial Sense`,
         `Patterning and Algebra`,
         `Data Management`,
         `Science and Technology`,
         `Social Studies`,
         `Health Education`,
         `Physical Education`,
         Dance,
         Drama,
         Music,
         `Visual Arts` 
  ) 

eleT2Course <- eleT2 %>%
  select(Year, 
         School, 
         OEN, 
         Name, 
         Grade, 
         IEP,
         `French Listening`,
         `French Speaking`,
         `French Reading`,
         `French Writing`,
         Reading,
         Writing,
         `Oral Communication`,
         `Media Literacy`,
         `Number Sense` = `Number Sense and Numeration`,
         Measurement,
         Geometry = `Geometry and Spatial Sense`,
         `Patterning and Algebra`,
         `Data Management`,
         `Science and Technology`,
         `Social Studies`,
         `Health Education`,
         `Physical Education`,
         Dance,
         Drama,
         Music,
         `Visual Arts` 
  ) 


# Create Long Table Term 1-------------------------------------------------------

longT1 <- eleT1Course %>%
  gather(Course, Mark, 7:ncol(eleT1Course)) %>%
  mutate (Mark = ifelse (Mark == "ALT", "R", Mark), 
          Mark_R = substr(Mark,1,1),
          Mark_Level = recode (Mark_R, 
                               "A" = "4",
                               "B" = "3",
                               "C" = "2",
                               "D" = "1",
                               "1" = "4",
                               "9" = "4",
                               "8" = "4",
                               "7" = "3",
                               "6" = "2",
                               "5" = "1",
                               "I" = "R",
                               "R" = "R"))



# Reorder Mark levels -------------------------------------- 
longT1$Mark_Level <- factor(longT1$Mark_Level,
                           levels=c("R", "1", "2", "3", "4"))
# Reorder Grade -------------------------------------------

longT1$Grade <- factor (longT1$Grade,
                        levels = c ("Grade 1",
                                    "Grade 2",
                                    "Grade 3",
                                    "Grade 4",
                                    "Grade 5",
                                    "Grade 6",
                                    "Grade 7",
                                    "Grade 8"))

# Reorder Courses --------------------------------------------

longT1$Course <- factor (longT1$Course,
                         levels = c ("Reading", 
                                     "Writing", 
                                     "Oral Communication", 
                                     "Media Literacy",
                                     "Data Management",
                                     "Geometry",
                                     "Measurement",
                                     "Number Sense",
                                     "Patterning and Algebra",
                                     "French Reading",
                                     "French Speaking",
                                     "French Listening",
                                     "French Writing", 
                                     "Social Studies",
                                     "Science and Technology",
                                     "Health Education",
                                     "Physical Education",
                                     "Dance",
                                     "Drama",
                                     "Music",
                                     "Visual Arts"
                         ))



# Create Long Table Term 2-------------------------------------------------------

longT2 <- eleT2Course %>%
  gather(Course, Mark, 7:ncol(eleT2Course)) %>%
  mutate (Mark = ifelse (Mark == "ALT", "R", Mark), 
          Mark_R = substr(Mark,1,1),
          Mark_Level = recode (Mark_R, 
                               "A" = "4",
                               "B" = "3",
                               "C" = "2",
                               "D" = "1",
                               "1" = "4",
                               "9" = "4",
                               "8" = "4",
                               "7" = "3",
                               "6" = "2",
                               "5" = "1",
                               "I" = "R",
                               "R" = "R"))



# Reorder Mark levels -------------------------------------- 
longT2$Mark_Level <- factor(longT2$Mark_Level,
                            levels=c("R", "1", "2", "3", "4"))
# Reorder Grade -------------------------------------------

longT2$Grade <- factor (longT2$Grade,
                        levels = c ("Grade 1",
                                    "Grade 2",
                                    "Grade 3",
                                    "Grade 4",
                                    "Grade 5",
                                    "Grade 6",
                                    "Grade 7",
                                    "Grade 8"))

# Reorder Courses --------------------------------------------

longT2$Course <- factor (longT2$Course,
                         levels = c ("Reading", 
                                     "Writing", 
                                     "Oral Communication", 
                                     "Media Literacy",
                                     "Data Management",
                                     "Geometry",
                                     "Measurement",
                                     "Number Sense",
                                     "Patterning and Algebra",
                                     "French Reading",
                                     "French Speaking",
                                     "French Listening",
                                     "French Writing", 
                                     "Social Studies",
                                     "Science and Technology",
                                     "Health Education",
                                     "Physical Education",
                                     "Dance",
                                     "Drama",
                                     "Music",
                                     "Visual Arts"
                         ))






# Create Long Table T1 Learning Skills ------------------------------------

longT1Learn <- eleT1Learn %>%
  gather(Skill, Mark, 7:ncol(eleT1Learn)) %>%
  mutate (Skill = factor (Skill,levels = c(
                               "Responsibility",
                               "Organization",
                               "Independent Work",
                               "Initiative",
                               "Self Regulation",
                               "Collaboration"
                             )),
          Mark = recode (Mark,
                         "E" = "Excellent", 
                         "G" = "Good", 
                         "S" = "Satisfactory",
                         "N" = "Needs Improvement"),
          Mark = factor (Mark, levels = c("Needs Improvement",
                                          "Satisfactory",
                                          "Good",
                                          "Excellent")),
          Grade = factor (Grade,
                          levels = c ("Grade 1",
                                       "Grade 2",
                                       "Grade 3",
                                       "Grade 4",
                                       "Grade 5",
                                       "Grade 6",
                                       "Grade 7",
                                       "Grade 8")))

# Create Long Table T2 Learning Skills ------------------------------------

longT2Learn <- eleT2Learn %>%
  gather(Skill, Mark, 7:ncol(eleT2Learn)) %>%
  mutate (Skill = factor (Skill,levels = c(
    "Responsibility",
    "Organization",
    "Independent Work",
    "Initiative",
    "Self Regulation",
    "Collaboration")),
  Mark = recode (Mark,
                 "E" = "Excellent", 
                 "G" = "Good", 
                 "S" = "Satisfactory",
                 "N" = "Needs Improvement"),
  Mark = factor (Mark, levels = c("Needs Improvement",
                                  "Satisfactory",
                                  "Good",
                                  "Excellent")),
  Grade = factor (Grade,
                  levels = c ("Grade 1",
                              "Grade 2",
                              "Grade 3",
                              "Grade 4",
                              "Grade 5",
                              "Grade 6",
                              "Grade 7",
                              "Grade 8")))



# Data Summary Tables Term 1-----------------------------------------------------

# Data Table Summary - Learning Skills ------------------------------------

# Learning SKills - By Board ------------------------------
RCsummaryBoardT1Learn <-  longT1Learn %>%
  filter(!is.na(Skill))  %>%              #remove NAs to get an accurate count
  group_by(Grade, Skill) %>%                         #Group columns by school then grade then strand
  summarise(counts.N = sum(Mark == "Needs Improvement", na.rm=TRUE), #Counting number of instances of value R
            counts.S = sum(Mark == "Satisfactory", na.rm=TRUE), #Counting number of instances of values 1
            counts.G = sum(Mark == "Good", na.rm=TRUE), #Counting number of instances of values 2
            counts.E = sum(Mark == "Excellent", na.rm=TRUE),
            count = (counts.N + counts.S + counts.G + counts.E),
            perc.N = round(((counts.N/count)*100),0),
            perc.S = round (((counts.S/count)*100),0),
            perc.G = round (((counts.G/count)*100),0),
            perc.E = round (((counts.E/count)*100),0)
  )

RCsummaryBoardT1Learn$School <- "GEDSB"
# Learning SKills - By School --------------------------------------------

RCsummaryT1Learn <-  longT1Learn %>%
  filter(!is.na(Skill))  %>%              #remove NAs to get an accurate count
  group_by(School,Grade, Skill) %>%                         #Group columns by school then grade then strand
  summarise(counts.N = sum(Mark == "Needs Improvement", na.rm=TRUE), #Counting number of instances of value R
            counts.S = sum(Mark == "Satisfactory", na.rm=TRUE), #Counting number of instances of values 1
            counts.G = sum(Mark == "Good", na.rm=TRUE), #Counting number of instances of values 2
            counts.E = sum(Mark == "Excellent", na.rm=TRUE),
            count = (counts.N + counts.S + counts.G + counts.E),
            perc.N = round(((counts.N/count)*100),0),
            perc.S = round (((counts.S/count)*100),0),
            perc.G = round (((counts.G/count)*100),0),
            perc.E = round (((counts.E/count)*100),0)
  )

RCsummaryT1Learn <- rbind(RCsummaryT1Learn, RCsummaryBoardT1Learn)

RCsummaryT1Learn$School <- factor (RCsummaryT1Learn$School,
                              levels = c("GEDSB",
                                         "Agnes G. Hodge",
                                         "Anna Melick Memorial",
                                         "Banbury Heights",
                                         "Bellview",
                                         "Bloomsburg",
                                         "Boston",
                                         "Branlyn Community",
                                         "Brier Park",
                                         "Burford District",
                                         "Caledonia Centennial",
                                         "Cedarland",
                                         "Centennial-Grand Woodlands",
                                         "Central",
                                         "Cobblestone",
                                         "Courtland",
                                         "Delhi",
                                         "Dufferin",
                                         "Ecole Confederation",
                                         "Elgin Avenue",
                                         "Fairview Avenue",
                                         "Glen Morris Central",
                                         "Graham Bell-Victoria",
                                         "Grandview",
                                         "Grandview Central",
                                         "Greenbrier",
                                         "Hagersville",
                                         "Houghton",
                                         "J. L. Mitchener",
                                         "James Hillier",
                                         "James Hillier - Woodview",
                                         "Jarvis",
                                         "King George",
                                         "Lakewood",
                                         "Langton",
                                         "Lansdowne-Costain",
                                         "Lynndale Heights",
                                         "Major Ballachey",
                                         "Mt. Pleasant",
                                         "North Ward",
                                         "Oakland-Scotland",
                                         "Oneida Central",
                                         "Onondaga-Brant",
                                         "Paris Central",
                                         "Port Rowan",
                                         "Prince Charles",
                                         "Princess Elizabeth",
                                         "Rainham Central",
                                         "River Heights",
                                         "Russell Reid",
                                         "Ryerson Heights",
                                         "Seneca Central",
                                         "St. George-German",
                                         "Teeterville",
                                         "Thompson Creek",
                                         "Walpole North",
                                         "Walsh",
                                         "Walter Gretzky",
                                         "Waterford",
                                         "West Lynn",
                                         "Woodman-Cainsville/Echo Place")
)

# Course - By Board -------------------------------------------------------

RCsummaryBoardT1 <-  longT1 %>%
  filter(!is.na(Course))  %>%              #remove NAs to get an accurate count
  group_by(Grade, Course) %>%                         #Group columns by school then grade then strand
  summarise(counts.R = sum(Mark_Level == "R", na.rm=T), #Counting number of instances of value R
            counts.1 = sum(Mark_Level == "1", na.rm=T), #Counting number of instances of values 1
            counts.2 = sum(Mark_Level == "2", na.rm=T), #Counting number of instances of values 2
            counts.3 = sum(Mark_Level == "3", na.rm=T), #Counting number of instances of values 3
            counts.4 = sum(Mark_Level == "4", na.rm=T), #Counting number of instances of values 4
            count = (counts.R + counts.1 + counts.2 + counts.3 + counts.4),
            perc.R = round(((counts.R/count)*100),0),
            perc.1 = round (((counts.1/count)*100),0),
            perc.2 = round (((counts.2/count)*100),0),
            perc.3 = round (((counts.3/count)*100),0),
            perc.4 = round (((counts.4/count)*100),0)
  )

RCsummaryBoardT1$School <- "GEDSB"
# Course - By School -------------------------------------------------------

RCsummaryT1 <-  longT1 %>%
  filter(!is.na(Course))  %>%              #remove NAs to get an accurate count
  group_by(School, Grade, Course) %>%                         #Group columns by school then grade then strand
  summarise(counts.R = sum(Mark_Level == "R", na.rm=T), #Counting number of instances of value R
            counts.1 = sum(Mark_Level == "1", na.rm=T), #Counting number of instances of values 1
            counts.2 = sum(Mark_Level == "2", na.rm=T), #Counting number of instances of values 2
            counts.3 = sum(Mark_Level == "3", na.rm=T), #Counting number of instances of values 3
            counts.4 = sum(Mark_Level == "4", na.rm=T), #Counting number of instances of values 4
            count = (counts.R + counts.1 + counts.2 + counts.3 + counts.4),
            perc.R = round(((counts.R/count)*100),0),
            perc.1 = round (((counts.1/count)*100),0),
            perc.2 = round (((counts.2/count)*100),0),
            perc.3 = round (((counts.3/count)*100),0),
            perc.4 = round (((counts.4/count)*100),0)
  )

RCsummaryT1 <- rbind(RCsummaryT1, RCsummaryBoardT1)

RCsummaryT1$School <- factor (RCsummaryT1$School,
                              levels = c("GEDSB",
                                         "Agnes G. Hodge",
                                         "Anna Melick Memorial",
                                         "Banbury Heights",
                                         "Bellview",
                                         "Bloomsburg",
                                         "Boston",
                                         "Branlyn Community",
                                         "Brier Park",
                                         "Burford District",
                                         "Caledonia Centennial",
                                         "Cedarland",
                                         "Centennial-Grand Woodlands",
                                         "Central",
                                         "Cobblestone",
                                         "Courtland",
                                         "Delhi",
                                         "Dufferin",
                                         "Ecole Confederation",
                                         "Elgin Avenue",
                                         "Fairview Avenue",
                                         "Glen Morris Central",
                                         "Graham Bell-Victoria",
                                         "Grandview",
                                         "Grandview Central",
                                         "Greenbrier",
                                         "Hagersville",
                                         "Houghton",
                                         "J. L. Mitchener",
                                         "James Hillier",
                                         "James Hillier - Woodview",
                                         "Jarvis",
                                         "King George",
                                         "Lakewood",
                                         "Langton",
                                         "Lansdowne-Costain",
                                         "Lynndale Heights",
                                         "Major Ballachey",
                                         "Mt. Pleasant",
                                         "North Ward",
                                         "Oakland-Scotland",
                                         "Oneida Central",
                                         "Onondaga-Brant",
                                         "Paris Central",
                                         "Port Rowan",
                                         "Prince Charles",
                                         "Princess Elizabeth",
                                         "Rainham Central",
                                         "River Heights",
                                         "Russell Reid",
                                         "Ryerson Heights",
                                         "Seneca Central",
                                         "St. George-German",
                                         "Teeterville",
                                         "Thompson Creek",
                                         "Walpole North",
                                         "Walsh",
                                         "Walter Gretzky",
                                         "Waterford",
                                         "West Lynn",
                                         "Woodman-Cainsville/Echo Place")
                              )

# Data Summary Tables Term  2-----------------------------------------------------

# Learning SKills - By Board ------------------------------
RCsummaryBoardT2Learn <-  longT2Learn %>%
  filter(!is.na(Skill))  %>%              #remove NAs to get an accurate count
  group_by(Grade, Skill) %>%                         #Group columns by school then grade then strand
  summarise(counts.N = sum(Mark == "Needs Improvement", na.rm=TRUE), #Counting number of instances of value R
            counts.S = sum(Mark == "Satisfactory", na.rm=TRUE), #Counting number of instances of values 1
            counts.G = sum(Mark == "Good", na.rm=TRUE), #Counting number of instances of values 2
            counts.E = sum(Mark == "Excellent", na.rm=TRUE),
            count = (counts.N + counts.S + counts.G + counts.E),
            perc.N = round(((counts.N/count)*100),0),
            perc.S = round (((counts.S/count)*100),0),
            perc.G = round (((counts.G/count)*100),0),
            perc.E = round (((counts.E/count)*100),0)
  )

RCsummaryBoardT2Learn$School <- "GEDSB"
# Learning SKills - By School --------------------------------------------

RCsummaryT2Learn <-  longT2Learn %>%
  filter(!is.na(Skill))  %>%              #remove NAs to get an accurate count
  group_by(School,Grade, Skill) %>%                         #Group columns by school then grade then strand
  summarise(counts.N = sum(Mark == "Needs Improvement", na.rm=TRUE), #Counting number of instances of value R
            counts.S = sum(Mark == "Satisfactory", na.rm=TRUE), #Counting number of instances of values 1
            counts.G = sum(Mark == "Good", na.rm=TRUE), #Counting number of instances of values 2
            counts.E = sum(Mark == "Excellent", na.rm=TRUE),
            count = (counts.N + counts.S + counts.G + counts.E),
            perc.N = round(((counts.N/count)*100),0),
            perc.S = round (((counts.S/count)*100),0),
            perc.G = round (((counts.G/count)*100),0),
            perc.E = round (((counts.E/count)*100),0)
  )

RCsummaryT2Learn <- rbind(RCsummaryT2Learn, RCsummaryBoardT2Learn)

RCsummaryT2Learn$School <- factor (RCsummaryT2Learn$School,
                                   levels = c("GEDSB",
                                              "Agnes G. Hodge",
                                              "Anna Melick Memorial",
                                              "Banbury Heights",
                                              "Bellview",
                                              "Bloomsburg",
                                              "Boston",
                                              "Branlyn Community",
                                              "Brier Park",
                                              "Burford District",
                                              "Caledonia Centennial",
                                              "Cedarland",
                                              "Centennial-Grand Woodlands",
                                              "Central",
                                              "Cobblestone",
                                              "Courtland",
                                              "Delhi",
                                              "Dufferin",
                                              "Ecole Confederation",
                                              "Elgin Avenue",
                                              "Fairview Avenue",
                                              "Glen Morris Central",
                                              "Graham Bell-Victoria",
                                              "Grandview",
                                              "Grandview Central",
                                              "Greenbrier",
                                              "Hagersville",
                                              "Houghton",
                                              "J. L. Mitchener",
                                              "James Hillier",
                                              "James Hillier - Woodview",
                                              "Jarvis",
                                              "King George",
                                              "Lakewood",
                                              "Langton",
                                              "Lansdowne-Costain",
                                              "Lynndale Heights",
                                              "Major Ballachey",
                                              "Mt. Pleasant",
                                              "North Ward",
                                              "Oakland-Scotland",
                                              "Oneida Central",
                                              "Onondaga-Brant",
                                              "Paris Central",
                                              "Port Rowan",
                                              "Prince Charles",
                                              "Princess Elizabeth",
                                              "Rainham Central",
                                              "River Heights",
                                              "Russell Reid",
                                              "Ryerson Heights",
                                              "Seneca Central",
                                              "St. George-German",
                                              "Teeterville",
                                              "Thompson Creek",
                                              "Walpole North",
                                              "Walsh",
                                              "Walter Gretzky",
                                              "Waterford",
                                              "West Lynn",
                                              "Woodman-Cainsville/Echo Place")
)

# Course - By Board -------------------------------------------------------

RCsummaryBoardT2 <-  longT2 %>%
  filter(!is.na(Course))  %>%              #remove NAs to get an accurate count
  group_by(Grade, Course) %>%                         #Group columns by school then grade then strand
  summarise(counts.R = sum(Mark_Level == "R", na.rm=T), #Counting number of instances of value R
            counts.1 = sum(Mark_Level == "1", na.rm=T), #Counting number of instances of values 1
            counts.2 = sum(Mark_Level == "2", na.rm=T), #Counting number of instances of values 2
            counts.3 = sum(Mark_Level == "3", na.rm=T), #Counting number of instances of values 3
            counts.4 = sum(Mark_Level == "4", na.rm=T), #Counting number of instances of values 4
            count = (counts.R + counts.1 + counts.2 + counts.3 + counts.4),
            perc.R = round(((counts.R/count)*100),0),
            perc.1 = round (((counts.1/count)*100),0),
            perc.2 = round (((counts.2/count)*100),0),
            perc.3 = round (((counts.3/count)*100),0),
            perc.4 = round (((counts.4/count)*100),0)
  )

RCsummaryBoardT2$School <- "GEDSB"
# Course - By School -------------------------------------------------------
RCsummaryT2 <-  longT2 %>%
  filter( !is.na(Course))  %>%              #remove NAs to get an accurate count
  group_by(School, Grade, Course) %>%                         #Group columns by school then grade then strand
  summarise(counts.R = sum(Mark_Level == "R", na.rm=T), #Counting number of instances of value R
            counts.1 = sum(Mark_Level == "1", na.rm=T), #Counting number of instances of values 1
            counts.2 = sum(Mark_Level == "2", na.rm=T), #Counting number of instances of values 2
            counts.3 = sum(Mark_Level == "3", na.rm=T), #Counting number of instances of values 3
            counts.4 = sum(Mark_Level == "4", na.rm=T), #Counting number of instances of values 4
            count = (counts.R + counts.1 + counts.2 + counts.3 + counts.4),
            perc.R = round(((counts.R/count)*100),0),
            perc.1 = round (((counts.1/count)*100),0),
            perc.2 = round (((counts.2/count)*100),0),
            perc.3 = round (((counts.3/count)*100),0),
            perc.4 = round (((counts.4/count)*100),0)
  )

RCsummaryT2 <- rbind(RCsummaryT2, RCsummaryBoardT2)

RCsummaryT2$School <- factor (RCsummaryT2$School,
                              levels = c("GEDSB",
                                         "Agnes G. Hodge",
                                         "Anna Melick Memorial",
                                         "Banbury Heights",
                                         "Bellview",
                                         "Bloomsburg",
                                         "Boston",
                                         "Branlyn Community",
                                         "Brier Park",
                                         "Burford District",
                                         "Caledonia Centennial",
                                         "Cedarland",
                                         "Centennial-Grand Woodlands",
                                         "Central",
                                         "Cobblestone",
                                         "Courtland",
                                         "Delhi",
                                         "Dufferin",
                                         "Ecole Confederation",
                                         "Elgin Avenue",
                                         "Fairview Avenue",
                                         "Glen Morris Central",
                                         "Graham Bell-Victoria",
                                         "Grandview",
                                         "Grandview Central",
                                         "Greenbrier",
                                         "Hagersville",
                                         "Houghton",
                                         "J. L. Mitchener",
                                         "James Hillier",
                                         "James Hillier - Woodview",
                                         "Jarvis",
                                         "King George",
                                         "Lakewood",
                                         "Langton",
                                         "Lansdowne-Costain",
                                         "Lynndale Heights",
                                         "Major Ballachey",
                                         "Mt. Pleasant",
                                         "North Ward",
                                         "Oakland-Scotland",
                                         "Oneida Central",
                                         "Onondaga-Brant",
                                         "Paris Central",
                                         "Port Rowan",
                                         "Prince Charles",
                                         "Princess Elizabeth",
                                         "Rainham Central",
                                         "River Heights",
                                         "Russell Reid",
                                         "Ryerson Heights",
                                         "Seneca Central",
                                         "St. George-German",
                                         "Teeterville",
                                         "Thompson Creek",
                                         "Walpole North",
                                         "Walsh",
                                         "Walter Gretzky",
                                         "Waterford",
                                         "West Lynn",
                                         "Woodman-Cainsville/Echo Place")
)

# Reshape Data file for ggplot --------------------------------------------


# T1 Learning Skills ------------------------------------------------------

RCsummarylongPercT1Learn <- tbl_df(select(RCsummaryT1Learn,
                                          School,
                                          Grade, 
                                          Skill,
                                          perc.N,
                                          perc.S,
                                          perc.G,
                                          perc.E)) %>%
  gather(Levels, 
         Num, 
         perc.N,
         perc.S,
         perc.G,
         perc.E) 

RCsummarylongPercT1Learn$Levels <- factor(RCsummarylongPercT1Learn$Levels, 
                                    levels = c("perc.N",
                                               "perc.S",
                                               "perc.G",
                                               "perc.E"),
                                    labels = c ("Needs Improvement",
                                                "Satisfactory",
                                                "Good",
                                                "Excellent")) 
# T2 Learning Skills ------------------------------------------------------

RCsummarylongPercT2Learn <- tbl_df(select(RCsummaryT2Learn,
                                          School,
                                          Grade, 
                                          Skill,
                                          perc.N,
                                          perc.S,
                                          perc.G,
                                          perc.E)) %>%
  gather(Levels, 
         Num, 
         perc.N,
         perc.S,
         perc.G,
         perc.E) 

RCsummarylongPercT2Learn$Levels <- factor(RCsummarylongPercT2Learn$Levels, 
                                          levels = c("perc.N",
                                                     "perc.S",
                                                     "perc.G",
                                                     "perc.E"),
                                          labels = c ("Needs Improvement",
                                                      "Satisfactory",
                                                      "Good",
                                                      "Excellent")) 

# T1 & T2 Courses --------------------------------------------------------------

RCsummarylongNumT1 <- tbl_df(select(RCsummaryT1,School,Grade, Course,   #Reshaping Counts for plotting
                                  counts.R, 
                                  counts.1, 
                                  counts.2, 
                                  counts.3,
                                  counts.4)) %>%
  gather(Levels, 
         Num, 
         counts.1, 
         counts.2, 
         counts.3, 
         counts.4) 

RCsummarylongNumT1$Levels <- factor(RCsummarylongNumT1$Levels, 
                                    levels = c("counts.1", 
                                               "counts.2",
                                               "counts.3",
                                               "counts.4"),
                                    labels = c ("Level 1",
                                                "Level 2",
                                                "Level 3",
                                                "Level 4")) 

RCsummarylongPercT1 <- tbl_df(select(RCsummaryT1,School,Grade, Course,  #Reshaping Percentages for plotting
                                   perc.R, 
                                   perc.1, 
                                   perc.2, 
                                   perc.3,
                                   perc.4)) %>%
  gather(Levels, 
         Num, 
         perc.1, 
         perc.2, 
         perc.3, 
         perc.4) 

RCsummarylongPercT1$Levels <- factor(RCsummarylongPercT1$Levels, 
                                    levels = c("perc.1", 
                                               "perc.2",
                                               "perc.3",
                                               "perc.4"),
                                    labels = c ("Level 1",
                                                "Level 2",
                                                "Level 3",
                                                "Level 4")) 


RCsummarylongNumT2 <- tbl_df(select(RCsummaryT2,School,Grade, Course,   #Reshaping Counts for plotting
                                    counts.R, 
                                    counts.1, 
                                    counts.2, 
                                    counts.3,
                                    counts.4)) %>%
  gather(Levels, 
         Num, 
         counts.1, 
         counts.2, 
         counts.3, 
         counts.4) 

RCsummarylongNumT2$Levels <- factor(RCsummarylongNumT2$Levels, 
                                    levels = c("counts.1", 
                                               "counts.2",
                                               "counts.3",
                                               "counts.4"),
                                    labels = c ("Level 1",
                                                "Level 2",
                                                "Level 3",
                                                "Level 4")) 



RCsummarylongPercT2 <- tbl_df(select(RCsummaryT2,School,Grade, Course,  #Reshaping Percentages for plotting
                                     perc.R, 
                                     perc.1, 
                                     perc.2, 
                                     perc.3,
                                     perc.4)) %>%
  gather(Levels, 
         Num, 
         perc.1, 
         perc.2, 
         perc.3, 
         perc.4) 

RCsummarylongPercT2$Levels <- factor(RCsummarylongPercT2$Levels, 
                                     levels = c("perc.1", 
                                                "perc.2",
                                                "perc.3",
                                                "perc.4"),
                                     labels = c ("Level 1",
                                                 "Level 2",
                                                 "Level 3",
                                                 "Level 4")) 

# French Language ---------------------------------------------------------

frenchCountsT1 <- RCsummarylongNumT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "French Listening" |
           Course == "French Reading" |
           Course == "French Speaking" |
           Course == "French Writing") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

frenchCountsT2 <- RCsummarylongNumT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "French Listening" |
           Course == "French Reading" |
           Course == "French Speaking" |
           Course == "French Writing") %>%
    arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

frenchPercT1 <- RCsummarylongPercT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "French Listening" |
           Course == "French Reading" |
           Course == "French Speaking" |
           Course == "French Writing") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

frenchPercT2 <- RCsummarylongPercT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "French Listening" |
           Course == "French Reading" |
           Course == "French Speaking" |
           Course == "French Writing") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

# English -----------------------------------------------------------------

engCountsT1 <- RCsummarylongNumT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Reading" |
           Course == "Writing" |
           Course == "Oral.Communication" |
           Course == "Media.Literacy") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

engCountsT2 <- RCsummarylongNumT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Reading" |
           Course == "Writing" |
           Course == "Oral.Communication" |
           Course == "Media.Literacy") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

engPercT1 <- RCsummarylongPercT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Reading" |
           Course == "Writing" |
           Course == "Oral.Communication" |
           Course == "Media.Literacy") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

engPercT2 <- RCsummarylongPercT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Reading" |
           Course == "Writing" |
           Course == "Oral.Communication" |
           Course == "Media.Literacy") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

# Mathematics -------------------------------------------------------------

mathCountsT1 <- RCsummarylongNumT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Data Management" |
           Course == "Geometry" |
           Course == "Measurement" |
           Course == "Number Sense" |
           Course == "Patterning and Algebra") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

mathCountsT2 <- RCsummarylongNumT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Data Management" |
           Course == "Geometry" |
           Course == "Measurement" |
           Course == "Number Sense" |
           Course == "Patterning and Algebra") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

mathPercT1 <- RCsummarylongPercT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Data Management" |
           Course == "Geometry" |
           Course == "Measurement" |
           Course == "Number Sense" |
           Course == "Patterning and Algebra") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels


mathPercT2 <- RCsummarylongPercT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Data Management" |
           Course == "Geometry" |
           Course == "Measurement" |
           Course == "Number Sense" |
           Course == "Patterning and Algebra") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

# Science / Social Sudies -----------------------------------------------------------

socStudiesCountsT1 <- RCsummarylongNumT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Social Studies" |
           Course == "Science and Technology") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

socStudiesCountsT2 <- RCsummarylongNumT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Social Studies" |
           Course == "Science and Technology") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

socStudiesPercT1 <- RCsummarylongPercT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Social Studies" |
           Course == "Science and Technology") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels


socStudiesPercT2 <- RCsummarylongPercT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Social Studies" |
           Course == "Science and Technology") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

# Phys Ed / Art  -----------------------------------------------------------

artPhysEdCountsT1 <- RCsummarylongNumT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Health Education" |
           Course == "Physical Education" |
           Course == "Music" |
           Course == "Visual Arts") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

artPhysEdCountsT2 <- RCsummarylongNumT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Health Education" |
           Course == "Physical Education" |
           Course == "Music" |
           Course == "Visual Arts") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

artPhysEdPercT1 <- RCsummarylongPercT1 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Health Education" |
           Course == "Physical Education" |
           Course == "Music" |
           Course == "Visual Arts") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels


artPhysEdPercT2 <- RCsummarylongPercT2 %>%                  #Use RCsummarylongNum  THEN
  filter(Course == "Health Education" |
           Course == "Physical Education" |
           Course == "Music" |
           Course == "Visual Arts") %>%
  arrange(School,Grade, Course, Levels)                   #Sort by Grade, Strand and Levels

# Loop --------------------------------------------------------------------

# eleT1 <- eleT1 %>%
#   filter (schoolCode == "456764_Princess Elizabeth Public School")

#data.frame (table (eleT1$schoolCode))

Year <- "2016-2017"

# Term_name <- "June"

for (SchName in unique(eleT1$schoolCode)){
  knit2pdf("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/20162017_School_Reports/reportCardSchoolReport.rnw",
           output=paste0(SchName, '_', Year, '_Report_Card_Summary.tex'),
           compiler = 'xelatex')
  }

files.tex <- list.files(pattern %in% c("\\.tex$", "\\.log$", "\\.aux$"))

files.tex <- list.files(pattern = "\\.tex$")
if(file.exists(files.tex)) file.remove(files.tex)

files.log <- list.files(pattern = "\\.log$")
if(file.exists(files.log)) file.remove(files.log)

files.aux <- list.files(pattern = "\\.aux$")
if(file.exists(files.aux)) file.remove(files.aux)