#Attendence Source Code
# this is a test

#updating missing information for each attendance dataset
OR_Attendance$Cohort <- "OR"
DFSS_Attendance$LLE.Role <- "Instructional Leader"
IL_Attendance$Cohort <- "IL"
TN_Attendance$Cohort <- "TN"
DFSS_Attendance$Cohort <- DFSS_Attendance$LLE.Cohort

#Making all of the variables be in the same order
Vars <- c("Cohort","Full.Name", "LLE.Role", "Session.Start.Date.Time", "Session.Name")
OR_Small <- OR_Attendance[Vars]
DFSS_Small <- DFSS_Attendance[Vars]
IL_Small <- IL_Attendance[Vars]
TN_Small <- TN_Attendance[Vars]

print("binding all of the cohorts attendance together")
Attendance <- rbind(OR_Small, DFSS_Small, IL_Small, TN_Small) 
print(nrow(Attendance))


print("Removing duplicate Foundational Training assessments") 
Attendance <- Attendance %>% distinct(Cohort, Full.Name, LLE.Role, Session.Start.Date.Time,Session.Name) 
print(nrow(Attendance))

print("Removing System Leaders")
Attendance <- subset(Attendance, Attendance$LLE.Role != "System Leader")
print(nrow(Attendance))

print("If an error appears here please contact Jenna Hille - there should be no duplicates of attendence data.")

#Check to see if there are duplicates



#Creating widedataset
Attendance_Wide <- spread(Attendance,Session.Name, Session.Start.Date.Time, fill = NA, convert = FALSE) 


#Creating widedataset
#Attendance_Wide <- spread(Attendance,Session.Name, Session.Start.Date.Time, fill = NA, convert = FALSE) 

#Creating Dummy Variables
Attendance_Wide$Foundational_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Foundational Training`), 1, 0)
Attendance_Wide$Core1_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel Core 1: Data Dialogues`), 1, 0)
Attendance_Wide$Core2_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel Core 2: Team Lesson Planning`), 1, 0)
Attendance_Wide$Pre_Found_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Pre-Foundational Webinar`), 1, 0)
Attendance_Wide$Post_Found_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Post Foundational Skill-building`), 1, 0)
Attendance_Wide$Pre_Core1_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Pre-Core 1 Webinar (Data Dialogues)`), 1, 0)
Attendance_Wide$Post_Core1_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Post-Core 1 Skill-building (Data Dialogues)`), 1, 0)
Attendance_Wide$Pre_Core2_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Pre-Core 2 Webinar (Team Lesson Planning)`), 1, 0)
Attendance_Wide$Post_Core2_Attendence <- ifelse(!is.na(Attendance_Wide$`Lead Learn Excel: Post-Core 2 Skill-building (Team Lesson Planning)`), 1, 0)

#Changing Names for Matching
Attendance_Wide$Full.Name <- as.character(Attendance_Wide$Full.Name)
Attendance_Wide <-mutate(Attendance_Wide,LastName=sapply(strsplit(Attendance_Wide$Full.Name, split=' ', fixed=TRUE),function(x) (x[2])))
Attendance_Wide <- extract(Attendance_Wide, Full.Name, c("FirstName", "LastName"), "([^ ]+) (.*)")
Attendance_Wide$FNLN <- paste(Attendance_Wide$LastName, Attendance_Wide$FirstName)

Attendance_Wide$FNLN <- gsub(" ", "", Attendance_Wide$FNLN, fixed = TRUE)
Attendance_Wide$FNLN <- gsub("-", "", Attendance_Wide$FNLN, fixed = TRUE)
Attendance_Wide$FNLN <- gsub("'", "", Attendance_Wide$FNLN, fixed = TRUE)
Attendance_Wide$FNLN <- toupper(Attendance_Wide$FNLN)


#Updating Var names
Attendance_Wide$Attendence_Cohort <- Attendance_Wide$Cohort
Attendance_Wide$Attendence_LLE.Role<- Attendance_Wide$LLE.Role
Attendance_Wide$Cohort <- NULL
Attendance_Wide$LLE.Role <- NULL
Attendance_Wide$FirstName <- NULL
Attendance_Wide$LastName <- NULL

#Rename dataset
Attendence <- Attendance_Wide

Attendence$Found_Date <- Attendence$`Lead Learn Excel: Foundational Training`
Attendence$Pre_Found_Date <- Attendence$`Lead Learn Excel: Pre-Foundational Webinar`
Attendence$Post_Found_Date <- Attendence$`Lead Learn Excel: Post Foundational Skill-building`
Attendence$Core1_Date <- Attendence$`Lead Learn Excel Core 1: Data Dialogues`
Attendence$Pre_Core1_Date <- Attendence$`Lead Learn Excel: Pre-Core 1 Webinar (Data Dialogues)`
Attendence$Post_Core1_Date <- Attendence$`Lead Learn Excel: Post-Core 1 Skill-building (Data Dialogues)`
Attendence$Core2_Date <- Attendence$`Lead Learn Excel Core 2: Team Lesson Planning`
Attendence$Pre_Core2_Date <- Attendence$`Lead Learn Excel: Pre-Core 2 Webinar (Team Lesson Planning)`
Attendence$Post_Core2_Date <- Attendence$`Lead Learn Excel: Post-Core 2 Skill-building (Team Lesson Planning)`


reorder <- c("FNLN", "Attendence_Cohort", "Attendence_LLE.Role","Found_Date","Foundational_Attendence", "Pre_Found_Date", "Pre_Found_Attendence",
             "Post_Found_Date", "Post_Found_Attendence", "Core1_Date","Core1_Attendence","Pre_Core1_Date", "Pre_Core1_Attendence", "Post_Core1_Date", "Post_Core1_Attendence","Core2_Date","Core2_Attendence",
             "Pre_Core2_Date", "Pre_Core2_Attendence", "Post_Core2_Date", "Post_Core2_Attendence")
Attendence <- Attendence[reorder]
#removing other datasets
rm(Attendance_Wide, DFSS_Attendance, IL_Attendance, OR_Attendance, TN_Attendance, OR_Small, DFSS_Small, IL_Small, TN_Small, Attendance)

print("Data is done being reformatted: First and Last names have been updated and new attendance variables have been created, NO other cleaning has been done!")
