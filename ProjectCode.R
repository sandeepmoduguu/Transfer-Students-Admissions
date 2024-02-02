library(ggplot2)
getwd()
setwd("/Users/saiarvindatluri/Downloads/Fundamentals of Data Analytics/Project ")

# Loading the Excel Files 
library(readxl)
F21 <- read_excel("F21-Admitted-IUTs.xlsx")
F22 <- read_excel("F22-Admitted-IUTs.xlsx")
S21 <- read_excel("S21-Admitted-IUTs.xlsx")
S22 <- read_excel("S22-Admitted-IUTs.xlsx")
S23 <- read_excel("S23-Admitted-IUTs.xlsx")

# Cleaning the column names with the same names to load the data in a single file 
library(data.table)
setnames(F22, "Stats", "Stats Course not MATH 2510")
setnames(S21, "Stats Course not 2510", "Stats Course not MATH 2510")
setnames(S22, "Stats Course not 2510", "Stats Course not MATH 2510")
setnames(S23, "Stats Course not 2510", "Stats Course not MATH 2510")

# Loading the files into a single data file 
all_data <- rbind(F21, S21, F22, S22, S23)

# Updating all the data of TA & TB to A & B respectively using DF
all_data[all_data == 'TA'] <- 'A'
all_data[all_data == 'TA-'] <- 'A-'
all_data[all_data == 'TB'] <- 'B'
all_data[all_data == 'TB-'] <- 'B-'
all_data[all_data == 'TB+'] <- 'B+'
all_data[all_data == "P+ (B)"]<-"B"
all_data[all_data == "P+ (B-)"]<-"B-"
all_data[all_data == "P+ (B+)"]<-"B+"
all_data[all_data == "B (P+)"]<-"B"
all_data[all_data == "B- (P+)"]<-"B-"
all_data[all_data == "P (C+)"]<-"C+"
all_data[all_data == "P (B-)"]<-"B-"
all_data[all_data =="B+ (SU20)"]<-"B+"


(all_data$`Cumulative GPA`) 

# Method 1

summary(all_data$`Cumulative GPA`)

transfer_students <- nrow(all_data)

# Transfer Students for below 25 %
transfer_students_25 <- transfer_students * 0.25

sum(all_data$`Cumulative GPA` == 4.00)
mean(all_data$`Cumulative GPA` == 4.00)

sum(all_data$`Cumulative GPA` < 4.00 & all_data$`Cumulative GPA` > 3.50)
mean(all_data$`Cumulative GPA` < 4.00 & all_data$`Cumulative GPA` > 3.50)

sum(all_data$`Cumulative GPA` <= 3.50 & all_data$`Cumulative GPA` >= 3.14)  
mean(all_data$`Cumulative GPA` <= 3.50 & all_data$`Cumulative GPA` >= 3.14)

sum(all_data$`Cumulative GPA` <= 3.14)
mean(all_data$`Cumulative GPA` <= 3.0)

# Based on the data min GPA has to be 3.15 to enter to Leeds using the 25% reduction model 

# Graph Rep 
data_GPA <- data.frame(GPA = c("3.14 - 3.50", "3.51 - 3.99", "4.00"), Percent_Eligible = c(46.0, 27.6, 0.02), Count_Eligible = c(451,251,14))
ggplot(data_GPA, aes(x=GPA, y=Percent_Eligible, fill = GPA)) + geom_col() + 
  geom_text(aes(label=paste(Percent_Eligible,"% (", Count_Eligible,")")), vjust=-0.5) + 
  scale_fill_manual(values=c("grey","firebrick4", "black")) +
  labs(title="25% Reduction in Transfer Student Eligibility", x="Proposed Minimum GPA Threshold", y="Percent of Eligible Students")


# Transfer Students for below 50%
transfer_students_50 <- transfer_students * 0.50

sum(all_data$`Cumulative GPA` <= 4.00 & all_data$`Cumulative GPA` > 3.50)
mean(all_data$`Cumulative GPA` <= 4.00 & all_data$`Cumulative GPA` > 3.50)

sum(all_data$`Cumulative GPA` <= 3.50 & all_data$`Cumulative GPA` > 3.30)  
mean(all_data$`Cumulative GPA` <= 3.50 & all_data$`Cumulative GPA` > 3.30)

sum(all_data$`Cumulative GPA` <= 3.30)

# Graph Rep 
data_GPA <- data.frame(GPA = c("3.30 - 3.50", "3.51 - 4.00"), Percent_Eligible = c(29.2, 22.1), Count_Eligible = c(265, 201))

ggplot(data_GPA, aes(x=GPA, y=Percent_Eligible, fill = GPA)) + geom_col() + 
       geom_text(aes(label=paste(Percent_Eligible,"% (", Count_Eligible,")")), vjust=-0.5) + 
       scale_fill_manual(values=c("grey","firebrick4")) +
       labs(title="50% Reduction in Transfer Student Eligibility", x="Proposed Minimum GPA Threshold", y="Percent of Eligible Students")

# Method 2 
# 25% reduction 
nrow(all_data)
New_ECON_Mandatory_25 <-subset(all_data, all_data$`ECON 2010`!="B-"&all_data$`ECON 2020`!="B-"&all_data$`ECON 2020`!="C+")
table(New_ECON_Mandatory$`ECON 2010`)
nrow(New_ECON_Mandatory_25)

orig_students <- nrow(all_data)
new_students <- nrow(New_ECON_Mandatory_25)

data <- data.frame(Category = c("Original", "25% Reduction"), Count = c(orig_students, new_students))

ggplot(data, aes(x=Category, y=Count, fill = Category)) + 
  geom_col() + scale_fill_manual(values = c("black", "grey")) +
  labs(title="Students Before and After 25% Reduction",
       x = "Student Group",
       y = "Number of Students") 

# 50% Reduction  
New_ECON_Mandatory_50 <-subset(all_data ,all_data$`ECON 2010`!="B-"&all_data$`ECON 2010`!="B"&all_data$`ECON 2020`!="C+"&all_data$`ECON 2020`!="B-")
table(New_ECON_Mandatory_50$`ECON 2010`)
table(New_ECON_Mandatory_50$`ECON 2020`)
nrow(New_ECON_Mandatory_50)

orig_students <- nrow(all_data)
new_students <- nrow(New_ECON_Mandatory_50)


data <- data.frame(Category = c("Original", "50% Reduction"), Count = c(orig_students, new_students))

ggplot(data, aes(x=Category, y=Count, fill = Category)) + 
  geom_col() + scale_fill_manual(values = c("black", "grey")) +
  labs(title="Students Before and After 50% Reduction",
       x = "Student Group",
       y = "Number of Students") 

orig_students <- nrow(all_data)
new_students <- nrow(New_ECON_Mandatory_25)


data <- data.frame(Category = c("Original", "25% Reduction"), Count = c(orig_students, new_students))

ggplot(data, aes(x=Category, y=Count, fill = Category)) + 
  geom_col() + scale_fill_manual(values = c("black", "grey")) +
  labs(title="Students Before and After 25% Reduction",
       x = "Student Group",
       y = "Number of Students") 

