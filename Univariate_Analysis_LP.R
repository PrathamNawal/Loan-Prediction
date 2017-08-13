#Load the datasets
library(readr)
train <- read_csv("~/Downloads/train_u6lujuX_CVtuZ9i.csv")
test <- read_csv("~/Downloads/test_Y3wMUE5_7gLdaTN.csv")

#combine the datasets for preprocessing
data <- rbind.data.frame(train[,-13],test)

#Exploratory Data Analysis
dim(data)

#Structure of the data
str(data)

#Head of the data
head(data)

#Tail of the data
tail(data)

#Univariate Analysis
#Gender
class(data$Gender)

unique(data$Gender)
sum(is.na(data$Gender))

#Imputation of missing values
library(DMwR)
data$Gender[is.na(data$Gender)] = "Male"

#Conversion into a factor variable
data$Gender = as.factor(data$Gender)

#Proportion of Gender
round(prop.table(table(data$Gender)),digits = 2)

#Categorise into 0 and 1
levels(data$Gender)[levels(data$Gender) == "Male"] <- 0
levels(data$Gender)[levels(data$Gender) == "Female"] <- 1


#Married
class(data$Married)

#Check for missing values
unique(data$Married)
sum(is.na(data$Married))

#Proportion of Marital Status
round(prop.table(table(data$Married)),digits = 3)*100

#Imputation of missing values
library(DMwR)
data$Married[is.na(data$Married)] = "Yes"

#Conversion into a factor variable
data$Married = as.factor(data$Married)

#Categorise into 0 and 1
levels(data$Married)[levels(data$Married) == "Yes"] <- 1
levels(data$Married)[levels(data$Married) == "No"] <- 0

#Visualization
library(ggplot2)
ggplot(data=data, aes(x=Gender, y=Married)) + geom_bar(stat="identity")
table(data$Gender,data$Married)

#DO LEVELLING OF THE VARIABLES AT THE LAST STEP

#Dependents
class(data$Dependents)

unique(data$Dependents)

#Missing Values in NA
sum(is.na(data$Dependents))

#Proportion of Dependents
round(prop.table(table(data$Dependents)),digits = 3)*100

#Collect all the missing Values
a <- cbind(data$Loan_ID[is.na(data$Dependents)],data$Dependents[is.na(data$Dependents)])
a = as.data.frame(a)
names(a) <- c("Loan_ID","Dependents")

#Imputing the missing values proportion-wise
a$Dependents = as.character(a$Dependents)

a[c(5,10,15,20,25,3,8,13,18,23,2,6,11,16,19),2] = "0"
a[c(1,7,12,17),2] = "1"
a[c(4,14,21,24),2] = "2"
a[c(9,22),2] = "3+"

#Putting the imputed values in the data
data$Dependents[is.na(data$Dependents)] = a$Dependents

#Visualizing the Dependents
ggplot(data,aes(data$Dependents)) + geom_bar() 

#Converting into factor variable
data$Dependents = as.factor(data$Dependents)

#Levelling
levels(data$Dependents)[levels(data$Dependents) == "3+"] <- 3

#Education
class(data$Education)
unique(data$Education)

#Missing Values in Education Category
sum(is.na(data$Education))

#proportion of Education
round(prop.table(table(data$Education)),digits = 3)*100

#Converting into factor Variable
data$Education = as.factor(data$Education)

#Levelling
levels(data$Education)[levels(data$Education)== "Graduate"] <- 0
levels(data$Education)[levels(data$Education)== "Not Graduate"] <- 1

#Visualizing the Education Scenario
ggplot(data,aes(data$Education)) + geom_bar()

#Self-Employed
class(data$Self_Employed)
unique(data$Self_Employed)

#Proportion of Employment
round(prop.table(table(data$Self_Employed)),digits = 3)*100

#Missing Values in Employed
sum(is.na(data$Self_Employed))
a <- cbind(data$Loan_ID[is.na(data$Self_Employed)],data$Self_Employed[is.na(data$Self_Employed)])
a <- as.data.frame(a)
names(a) <- c("Loan_ID","Self_Employed")
a$Self_Employed = as.character(a$Self_Employed)

#Imputing missing values according to the proportion
a[1:48,2] <- "No"
a[49:55,2] <- "Yes"

#Putting the imputed values in the data
data$Self_Employed[is.na(data$Self_Employed)] = a$Self_Employed

#Converting into a factor variable
data$Self_Employed = as.factor(data$Self_Employed)

#Levelling
levels(data$Self_Employed)[levels(data$Self_Employed) == "No"] <- 0
levels(data$Self_Employed)[levels(data$Self_Employed) == "Yes"] <- 1

#Applicant-Income
class(data$ApplicantIncome)

#Missing values in the income
sum(is.na(data$ApplicantIncome))

#Visualisation
hist(data$ApplicantIncome,col = "Green",xlab = "Amount(Rs)",ylab = "No of people",main = "First_Applicant_Income")
ggplot(data,aes(ApplicantIncome)) + geom_histogram(binwidth = 5000,colour = "Blue") + geom_density() + ggtitle("Income_First_Person")


#Standardization
STD_Applicant <- as.data.frame((data$ApplicantIncome - mean(data$ApplicantIncome))/sd(data$ApplicantIncome))
#Normalization
NRM_Applicant = as.data.frame((data$ApplicantIncome - min(data$ApplicantIncome))/(max(data$ApplicantIncome) - min(data$ApplicantIncome)))

#Not Included in the data currently

#Co-Applicant Income
p <-as.data.frame(data$CoapplicantIncome[data$CoapplicantIncome == 0])

#Nearly half of the Coapplicants aren't earning
#44%

#Missing Values in the Coapplicant Income
sum(is.na(data$CoapplicantIncome))

#Visualization
hist(data$CoapplicantIncome,col = "Green",xlab = "Amount(Rs)",ylab = "No of people",main = "Second_Applicant_Income")
ggplot(data,aes(CoapplicantIncome)) + geom_histogram(binwidth = 5000,colour = "Blue") + geom_density() + ggtitle("Income_Second_Person")


#Standardization
STD_Coapplicant <- as.data.frame((data$CoapplicantIncome - mean(data$CoapplicantIncome))/sd(data$CopplicantIncome))
#Normalization
NRM_Coaaplicant = as.data.frame((data$CoapplicantIncome - min(data$CoapplicantIncome))/(max(data$CoapplicantIncome) - min(data$CoapplicantIncome)))

#Not Included in the data currently

#New- Variable
#Total Income
Total_Income <- as.data.frame(data$ApplicantIncome + data$CoapplicantIncome)
names(Total_Income) <- c("TotalIncome")

#Loan Amount
class(data$LoanAmount)

#Missing Values in Loan Amount
sum(is.na(data$LoanAmount))

#Visualization
hist(data$LoanAmount,col = "Green",xlab = "Amount(Rs)",ylab = "No of people",main = "Loan_Amount")

#Imputing Missing Values
library(DMwR)
centralValue(data$LoanAmount)
data$LoanAmount[is.na(data$LoanAmount)] = 126

#Loan Amount Term
class(data$Loan_Amount_Term)
unique(data$Loan_Amount_Term)

#Visualization
hist(data$Loan_Amount_Term,col = "Green",xlab = "Amount(Rs)",ylab = "No of people",main = "Loan_Amount_Term")

#Missing Values in Loan_Amount_Term
sum(is.na(data$Loan_Amount_Term))

#Imputation of missing values
library(DMwR)
centralValue(data$Loan_Amount_Term)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] = 360


#Count in Each Category
table(data$Loan_Amount_Term)

#Binning
data$Loan_Amount_Term[data$Loan_Amount_Term == 6] <- 60
data$Loan_Amount_Term[data$Loan_Amount_Term == 12] <- 60
data$Loan_Amount_Term[data$Loan_Amount_Term == 36] <- 60
data$Loan_Amount_Term[data$Loan_Amount_Term == 84] <- 60
data$Loan_Amount_Term[data$Loan_Amount_Term == 350] <- 360

#Levelling
data$Loan_Amount_Term[(data$Loan_Amount_Term == 60)] <- 0
data$Loan_Amount_Term[(data$Loan_Amount_Term == 120)] <- 1
data$Loan_Amount_Term[(data$Loan_Amount_Term == 180)] <- 2
data$Loan_Amount_Term[(data$Loan_Amount_Term == 240)] <- 3
data$Loan_Amount_Term[(data$Loan_Amount_Term == 300)] <- 4
data$Loan_Amount_Term[(data$Loan_Amount_Term == 360)] <- 5
data$Loan_Amount_Term[(data$Loan_Amount_Term == 480)] <- 6

#Included in the data

#Converting into a factor Variable
data$Loan_Amount_Term = as.factor(data$Loan_Amount_Term)

#Visualize the factor Loan Amount Term
ggplot(data, aes_string(x = data$Loan_Amount_Term, y = data$Married)) +
  geom_bar(stat="identity",fill= "DarkSlateBlue") + theme_bw() +
  xlab("No of Terms")  + ylab("Count")
  ggtitle("Loan_Amount_Term") +  theme(text=element_text(size=15))
  
#Credit History
class(data$Credit_History)
unique(data$Credit_History)

#Missing Values in Credit History
sum(is.na(data$Credit_History))

#Proportion of Credit History
table(data$Credit_History)
round(prop.table(table(data$Credit_History)),digits = 3)*100

a <- as.data.frame(data$Credit_History[is.na(data$Credit_History)])

#Imputation of Missing Values proportion wise
a[1:66,] = 1  #84%
a[67:79,] = 0  #16%
names(a) <- c("Imputed_Values")

#Putting back the imputed values in the data
data$Credit_History[is.na(data$Credit_History)] = a$Imputed_Values

#Converting into the factor variable
data$Credit_History = as.factor(data$Credit_History)

#Property_Area
class(data$Property_Area)
unique(data$Property_Area)

#Missing Values in the Property Area
sum(is.na(data$Property_Area))

#Proportion of the Area type
round(prop.table(table(data$Property_Area)),digits = 3)*100

#Levelling
levels(data$Property_Area)[levels(data$Property_Area)== "Rural"] <- 0
levels(data$Property_Area)[levels(data$Property_Area)== "Semiurban"] <- 1
levels(data$Property_Area)[levels(data$Property_Area)== "Urban"] <- 2

#Not included in the data currently

#Piechart Visualization
library(graphics)
stripplot(data$Property_Area)


