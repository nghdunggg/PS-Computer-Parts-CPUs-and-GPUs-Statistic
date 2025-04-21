install.packages("pastecs") # tidyr library
install.packages("DescTools") # Mode function
install.packages("zoo") #function "na.locf"
install.packages("corrplot")

library(tidyverse)
library(zoo)
library(DescTools)
library(pastecs)
library(tidyr)
library(dplyr)
library(corrplot)

install.packages("corrplot")

# Load the corrplot package


# Set the URL of the dataset you want to download
url <- "https://huggingface.co/datasets/vantien06/PSAssignment/resolve/main/Intel_CPUs.csv?download=true"

# Set the destination file path where you want to save the dataset
directory_name = "data"
dir.create(directory_name)

# Check if the directory was created successfully
destination <- "./data/Intel_CPUs.csv"

# Download the dataset
download.file(url, destination)


Intel_CPUs<-read.csv("./data/Intel_CPUs.csv", na.strings = c("","N/A"))
#view(Intel_CPUs)
head(Intel_CPUs,10)

#select specific column
data<-Intel_CPUs[,c(1,2,4,6,8,10,12,14,36,40)]
# Calculate the missing ratio of each data column and represent it graphically
emptycellrate <- (colMeans(is.na(Intel_CPUs)))*100
mrdf <- data.frame(Category = names(emptycellrate), percentage = emptycellrate)

# Draw a column chart
ggplot(mrdf, aes(x = Category, y = percentage)) +
geom_bar(stat = "identity", fill = "lightblue") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ggtitle("Empty-Cell Ratio of the data")

# Check for missing data
print(apply(is.na(data),2,sum))
head(data)
write.csv(data, "./data/filter1.csv", row.names = TRUE)


Cache_Size_Clean <- function(size){
  if(grepl('K',size)){
    return (as.double(gsub(" K","",size)) /1024)
  }
  else{
    return (as.double(gsub(" M","",size)))
  }
}
# tách dữ liệu thành 2 cột gồm loại cache và size của nó
data <- separate(data,Cache,into = c("Cache_Size","Cache_Type"),sep="B")
# xử lý chuỗi và đưa về kiểu số thực
data$Cache_Size <- sapply(data$Cache_Size,Cache_Size_Clean)
data$Cache_Size <- log(data$Cache_Size)
# vì lệnh separate nên loại thường không được thêm vào
data$Cache_Type <- ifelse(data$Cache_Type == "", "Normal", sub(" ","",data$Cache_Type))
data <- data[complete.cases(data$Cache_Size), ]

data$Instruction_Set <- na.fill(data$Instruction_Set,"64")
data$Instruction_Set<- as.numeric(gsub("-bit", "", data$Instruction_Set))
data$Instruction_Set <- na.fill(data$Instruction_Set,"64")
colnames(data)[11] <- "Instruction_Set_Bit"

data$Lithography<- as.numeric(gsub("nm", "", data$Lithography))
colnames(data)[4] <- "Lithography_nm"
Ex<-mean(data$Lithography_nm, na.rm = TRUE)
data$Lithography_nm[is.na(data$Lithography_nm)] <- Ex

data$TDP<- as.numeric(gsub("W", "", data$TDP))
colnames(data)[9] <- "TDP_W"
Ex <- mean(data$TDP_W, na.rm = TRUE)
data$TDP_W[is.na(data$TDP_W)] <- Ex

base_frequency <-function(f){
  if (grepl(" GHz",f)) {
    return (as.double(gsub(" GHz","",f))*1000)
  }
  return (as.double(gsub(" MHz","",f)))
}
data$Processor_Base_Frequency <-as.integer( sapply(data$Processor_Base_Frequency,base_frequency))
data$Processor_Base_Frequency <- as.numeric(gsub("GHz", "", data$Processor_Base_Frequency))
colnames(data)[6] <- "Processor_Base_Frequency_MHz"
Ex <- mean(data$Processor_Base_Frequency_MHz, na.rm = TRUE)
data$Processor_Base_Frequency_MHz[is.na(data$Processor_Base_Frequency_MHz)] <- Ex

# Convert T column to numeric after removing "°C"
data$T <- as.numeric(gsub("°C", "", data$T))

# Rename the column to "T"
colnames(data)[which(colnames(data) == "T")] <- "T_Celsius"

# Impute missing values in T_Celsius column with mean
mean_T <- mean(data$T_Celsius, na.rm = TRUE)
data$T_Celsius[is.na(data$T_Celsius)] <- mean_T

# Display first few rows of the dataset
head(data$T_Celsius)

head(data) #show ra là tui clean ht data r nè
# Print count of missing values in each column
print(apply(is.na(data), 2, sum))

summary(data)

#Lithography
hist(data$Lithography_nm,main="Histogram of Lithography")

#nb_of_cores
hist(data$nb_of_Cores,main="Histogram of nb_of_Cores")

#Processor_Base_Frequency
hist(data$Processor_Base_Frequency_MHz,main="Histogram of Processor_Base_Frequency(MHz) ")

#Cache
hist(data$Cache_Size,main="Histogram of Cache Size")

#TDP
hist(data$TDP_W,main="Histogram of TDP")

#T
hist(data$T_Celsius,main="Histogram of T")

#Instruction_Set
hist(data$Instruction_Set_Bit,main="Histogram of Instruction_Set(bit)")

##finding the outcome variable by heatmap
temp=cor(data[,sapply(data,is.numeric)])
corrplot(temp,method="number")

data <- data[, c("Product_Collection", "Vertical_Segment","Status", "Cache_Type","Lithography_nm","nb_of_Cores","Processor_Base_Frequency_MHz", "Cache_Size", "TDP_W", "T_Celsius", "Instruction_Set_Bit" )]
pairs(data[, 5:11],
      main = "Matrix Scatter Plot",
      diag.panel = NULL, # remove diagonal panels
      upper.panel = NULL, # remove upper panels
      lower.panel = panel.smooth) # add smoother to lower panels


mlr<-lm(formula=TDP_W~Lithography_nm+nb_of_Cores+Processor_Base_Frequency_MHz
     +Cache_Size+T_Celsius+Instruction_Set_Bit,data=data)
VIF(mlr)
summary(mlr)
#=>Adjusted R-squared:   0.7521 best choice

newpredictdata<-data.frame(Lithography_nm=c(30),nb_of_Cores=c(30),
                           Processor_Base_Frequency_MHz=c(3)
                           ,Cache_Size=c(10),T_Celsius=c(60),Instruction_Set_Bit=c(64))
TDP_prediction<-predict(mlr,newpredictdata,interval = "confidence")
TDP_prediction