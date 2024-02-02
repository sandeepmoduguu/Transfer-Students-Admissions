# Initializing the necessary libraries to manipulate data and work with excel files

library(openxlsx)
library(rio)

# Listing all the excel files in the specified directory

files <- list.files(pattern = ".xlsx")

#Creating a new excel workbook

wb <- createWorkbook()

#Looping through the each file, reading the data, and writing it to a new worksheet in the workbook

for(file in files) {
  df <- read.xlsx(file)
  addWorksheet(wb,file)
  writeData(wb,file,df)
}

# saving the appended workbook as 'combines.xlsx' in the specified directory

saveWorkbook(wb,'combined.xlsx',overwrite=TRUE)

# Defining the path to the combined Excel file.

path<-"/Users/sandeepreddymodugu/Downloads/R Project/Raw Data/combined.xlsx"

# Importing the combined Excel file into a data frame named 'CU_trf_stu'.

CU_trf_stu<-import_list(path, rbind=TRUE)

# Removing spaces from all character class values in the 'CU_trf_stu' data frame

CU_trf_stu<-replace(CU_trf_stu," ","")

# Replacing specific project codes with standardized codes

CU_trf_stu[CU_trf_stu=="TA"]<-"A" 
CU_trf_stu[CU_trf_stu=="TB"]<-"B" 
CU_trf_stu[CU_trf_stu=="P+ (B)"]<-"B"
CU_trf_stu[CU_trf_stu=="P+ (B-)"]<-"B-" 
CU_trf_stu[CU_trf_stu=="TB-"]<-"B-"
CU_trf_stu[CU_trf_stu=="TA-"]<-"A-"
CU_trf_stu[CU_trf_stu=="P+ (B+)"]<-"B+"
CU_trf_stu[CU_trf_stu=="B (P+)"]<-"B"
CU_trf_stu[CU_trf_stu=="B- (P+)"]<-"B-"
CU_trf_stu[CU_trf_stu=="TB+"]<-"B+"
CU_trf_stu[CU_trf_stu=="P (C+)"]<-"C+"
CU_trf_stu[CU_trf_stu=="P (B-)"]<-"B-" 
CU_trf_stu[CU_trf_stu=="B+ (SU20)"]<-"B+"

# Displaying the modified 'CU_trf_stu' data frame and counting the number of rows.

CU_trf_stu
nrow(CU_trf_stu)

# Creating a subset named 'ECON_25' to model approximately 25% reduction in transfer students

ECON_25<-subset(CU_trf_stu,CU_trf_stu$ECON.2010!="B-"&CU_trf_stu$ECON.2020!="B-"&CU_trf_stu$ECON.2020!="C+")

# Generating frequency tables for 'ECON.2010' and 'ECON.2020' column values within the 'ECON_25' subset.

table(ECON_25$ECON.2010)
table(ECON_25$ECON.2020)

# Counting the number of rows in the 'ECON_25' data frame

nrow(ECON_25)

# Creating a subset named 'ECON_50' to model close to 50% reduction in transfer students

ECON_50<-subset(CU_trf_stu,CU_trf_stu$ECON.2010!="B-"&CU_trf_stu$ECON.2010!="B"&CU_trf_stu$ECON.2020!="C+"&CU_trf_stu$ECON.2020!="B-")

# Generating frequency tables for 'ECON.2010' and 'ECON.2020' column values within the 'ECON_50' subset.

table(ECON_50$ECON.2010)
table(ECON_50$ECON.2020)

# Counting the number of rows in the 'ECON_50' data frame
nrow(ECON_50)

table(CU_trf_stu$ECON.2010)
table(CU_trf_stu$ECON.2020)


barplot(c(nrow(CU_trf_stu),nrow(ECON_25%),nrow(ECON_50%)), names=c("CU_trf_stu","ECON_25%","ECON_50%"), col=c("blue","red","green"))

ECON_25%change_ECON2010=(length(CU_trf_stu$ECON.2010)-length(ECON_25%$ECON.2010))/length(CU_trf_stu$ECON.2010)
ECON_25%change_ECON2020=(length(CU_trf_stu$ECON.2020)-length(ECON_25%$ECON.2020))/length(CU_trf_stu$ECON.2020)

ECON_50%change_ECON2010=(length(CU_trf_stu$ECON.2010)-length(ECON_50%$ECON.2010))/length(CU_trf_stu$ECON.2010)
ECON_50%change_ECON2020=(length(CU_trf_stu$ECON.2020)-length(ECON_50%$ECON.2020))/length(CU_trf_stu$ECON.2020)


# Adjust margins and plot size
par(mfrow = c(1, 3))  # Arrange plots in 1 row and 3 columns
par(mar = c(5, 4, 4, 8))  # Adjust margins (bottom, left, top, right)
options(repr.plot.width = 10, repr.plot.height = 3)  # Adjust plot width and height

# Data for the first bar plot
data1 <- c(length(CU_trf_stu$ECON.2010), length(CU_trf_stu$ECON.2020))
names1 <- c("ECON 2010", "ECON 2020")
colors1 <- c("blue", "red")

# Data for the second bar plot
data2 <- c(ECON_25%change_ECON2010, ECON_25%change_ECON2020)
names2 <- c("ECON 2010", "ECON 2020")
colors2 <- c("yellow", "green")

# Data for the third bar plot
data3 <- c(ECON_50%change_ECON2010, ECON_50%change_ECON2020)
names3 <- c("ECON 2010", "ECON 2020")
colors3 <- c("cyan", "orange")


# Create the first bar plot
bp1 <- barplot(data1, names.arg = names1, col = colors1, main = "Plot 1")
text(bp1, data1 + 5, labels = data1, pos = 3)  # Adding labels above bars

# Create the second bar plot
bp2 <- barplot(data2, names.arg = names2, col = colors2, main = "Plot 2")
text(bp2, data2 + 5, labels = data2, pos = 3)  # Adding labels above bars

# Create the third bar plot
bp3 <- barplot(data3, names.arg = names3, col = colors3, main = "Plot 3")
text(bp3, data3 + 5, labels = data3, pos = 3)  # Adding labels above bars

table(CU_trf_stu$ECON.2010)

