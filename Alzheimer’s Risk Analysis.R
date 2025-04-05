library(dplyr)
library(ggplot2)
library(WVPlots) 
library(scales)

#load dataset
dataset <- read.csv(file="alzheimers_disease_data.csv", stringsAsFactors = TRUE)


#Create data frame for selected variables
patient_data <- dataset %>%
  select(Age, EducationLevel,FamilyHistoryAlzheimers, Gender, MMSE, Diagnosis)
head(patient_data)

#factor categorical data
patient_data <- patient_data %>%
  mutate(
    Gender = as.factor(Gender),
    FamilyHistoryAlzheimers = as.factor(FamilyHistoryAlzheimers),
    Diagnosis = as.factor(Diagnosis)
  )

  #Descriptive statistics Individual Attributes

#Age
summary(patient_data$Age)
#Skewness
(3*(mean(patient_data$Age) - median(patient_data$Age)))/sd(patient_data$Age)
#variance
var(patient_data$Age)
#standard dev
sd(patient_data$Age)


#Education Level
summary(patient_data$EducationLevel)
#Skewness
(3*(mean(patient_data$EducationLevel) - median(patient_data$EducationLevel)))/sd(patient_data$EducationLevel)
#variance
var(patient_data$EducationLevel)
#standard dev
sd(patient_data$EducationLevel)

  #MMSE
summary(patient_data$MMSE)
#Skewness
(3*(mean(patient_data$MMSE) - median(patient_data$MMSE)))/sd(patient_data$MMSE)
#variance
var(patient_data$MMSE)
#standard dev
sd(patient_data$MMSE)

#Gender
summary(patient_data$Gender)

#Family History Alzheimer
summary(patient_data$FamilyHistoryAlzheimers)


#Diagnosis
summary(patient_data$Diagnosis)


 #Relationships between attributes

#Gender and Diagnosis
ggplot(patient_data, aes(x = Gender, fill = Diagnosis)) +
  geom_bar(position = "stack") +
  labs(title = "Gender and Diagnosis", x = "Gender", y = "Diagnosed", fill = "Diagnosis") 

#Family History and Diagnosis
ggplot(patient_data, aes(x = FamilyHistoryAlzheimers, fill= Diagnosis)) +
  geom_bar(position = "dodge") +
  labs(title = "Side-by-Side Bar Plot: Family History vs. Diagnosis", 
       x = "Family History of Alzheimer’s", y = "Count",fill = "Diagnosis")
 
#age and MMSE
age_MMSE <- patient_data %>%
  mutate(age_group = cut(Age, breaks = c(60, 70, 80, 100), labels = c("60-70", "70-80", "80+")))
  ggplot(age_MMSE, aes(x = MMSE, fill = age_group)) +
        geom_density(alpha = 0.3) +
        labs(title = "MMSE and Age", x = "MMSE Score", y = "Density", fill = "Age Group") 
    
#Education and Diagnosis
ggplot(patient_data, aes(x = EducationLevel, fill = Diagnosis)) +
  geom_bar(position = "stack") +
  labs(title = "Education Level and Diagnosis", x = "Education Level", y = "Diagnosed", fill = "Diagnosis") 
    

  #Inidvidual visualizations   

#Age visualization
ggplot(patient_data, aes(x=Age)) + 
  geom_histogram(fill = "yellow", color="black") +
  labs(title = "Patient Age Disribution", x = "Age", y = "Number of Patients")

#Education level
ggplot(patient_data, aes(x=EducationLevel)) + 
  geom_histogram(fill = "green", color = "black") +
  labs(title = "Patient Education Level", x = "Education Level", y = "Number of Patients")

#FamilyHistoryAlzheimers
ggplot(patient_data, aes(x=FamilyHistoryAlzheimers)) + 
  geom_bar(fill= c("green","red"), color = "black") +
  labs(title = "Patient Family History of Alzheimers", x = "Family History Alzheimers", y= "Number of Patients")

#MMSE
ggplot(patient_data, aes(y = MMSE)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Patient MMSE Scores", y = "MMSE Score")

#Gender
ggplot(patient_data, aes(x=Gender)) + 
  geom_bar(fill= c("blue","pink"), color = "black") +
  labs(title = "Gender Distribusion", x = "Gender", y= "Number of Patients")

#Diagnosis
ggplot(patient_data, aes(x=Diagnosis)) + 
  geom_bar(fill= c("Orange", "grey"), color = "black") +
  labs(title = "Patient Alzheimers Diagnosis", x = "Alzheimers Diagnosis", y= "Number of Patients")


