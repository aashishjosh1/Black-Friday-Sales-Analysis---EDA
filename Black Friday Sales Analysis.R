
                                                  # ANALYSIS OF BLACK FRIDAY SALES

# Authors      : Aashish Joshi
# Project Goal: To Understand the Customers Purchase Behavior
# Date        : 04/20/2018

# Set Work Directory
setwd("C:/Data Analysis Projects/Black Friday Sales Analysis")

# Load Packages
library(data.table)
library(DataExplorer) # For initial exploratory data analysis
library(dplyr) # For data manipulation
library(xda) # For Exploratory data analysis
library(ggplot2)
library(vcd)
library(rpart)

# Load The Dataset
blackf_data <- fread("sales.csv")

# STEP 1: DATA PROFILING

# Basic Statistics: The following questions will be explored:

    # A. What is the size of the dataset?
object.size(blackf_data) # The data size is 37.6 MB

    # B. How many rows and columns are there in the dataset?
dim(blackf_data) # 550068 rows and 12 columns

    # C. What does my dataset look like?
head(blackf_data, 10)
# From the first 10 rows it is evident that, the same user ID is repeated the number of times purchases were made
# The Age variable is reported as a range of values
#There are 3 city categories namely A, B, & C
# The stay in current ranges from 1 - 4 years
# The customers comprised of both married (1) and singles (0)
# There are 3 different product categories available to customers

    # D. What is the structure of the data?
str(blackf_data) # Text
plot_str(blackf_data) # Network Graph
# The dataset are made up of factors and integers
# Marital status should probably be a factor and not an integer, hence, it must be converted
# Purchase, age, and Stay in current city should be numeric
# Product ID, gender should be a factor

    # E. Are there any missing values?
sapply(blackf_data, function(x) sum(is.na(x)))
plot_missing(blackf_data)

    # F. What is the data distribution? i.e. what are the continous and discrete features?
# Continous Features
  #Did the married visited the store more?
prop.table(table(blackf_data$Marital_Status))
plot_histogram(blackf_data)

# Discrete Features

  # How many male and female customers?
prop.table(table(blackf_data$Gender))
  # Customers age group
prop.table(table(blackf_data$Age))
  # Which city category has the highest customers?
prop.table(table(blackf_data$City_Category))
  # What effect does the duration of stay in current has on the store visit?
prop.table(table(blackf_data$Stay_In_Current_City_Years))
plot_bar(blackf_data)

# STEP 2: DATA CLEANING AND TRANSFORMATION

# Transform the data types

blackf_data$User_ID <- as.factor(blackf_data$User_ID)
blackf_data$Product_ID <- as.factor(blackf_data$Product_ID)
blackf_data$Gender <- as.factor(if_else(blackf_data$Gender == 'M', 'Male', 'Female'))
blackf_data$Age <- as.factor(blackf_data$Age)
blackf_data$Occupation <- as.factor(blackf_data$Occupation)
blackf_data$City_Category <- as.factor(blackf_data$City_Category)
blackf_data$Stay_In_Current_City_Years <- as.factor(blackf_data$Stay_In_Current_City_Years)
blackf_data$Marital_Status <- as.factor(if_else(blackf_data$Marital_Status == 1, 'Married', 'Single'))
blackf_data$Product_Category_1 <- as.integer(blackf_data$Product_Category_1)
blackf_data$Product_Category_2 <- as.integer(blackf_data$Product_Category_2)
blackf_data$Product_Category_3 <- as.integer(blackf_data$Product_Category_3)
blackf_data$Purchase <- as.numeric(blackf_data$Purchase)

# Impute the missing values

fit <- rpart(Product_Category_2 ~ User_ID + Product_ID + Age + Gender,
                                data = blackf_data[!is.na(blackf_data$Product_Category_2),], 
                                method = "anova")
blackf_data$Product_Category_2[is.na(blackf_data$Product_Category_2)] <- 
  predict(fit, blackf_data[is.na(blackf_data$Product_Category_2),])

fit_1 <- rpart(Product_Category_3 ~ User_ID + Product_ID + Age + Gender,
             data = blackf_data[!is.na(blackf_data$Product_Category_3),], 
             method = "anova")
blackf_data$Product_Category_3[is.na(blackf_data$Product_Category_3)] <- 
  predict(fit_1, blackf_data[is.na(blackf_data$Product_Category_3),])

# Check for any missing values
plot_missing(blackf_data)
# Everything is now clean, hence, the analysis can now begin.

# STEP 3: EXPLORATORY DATA ANALYSIS

# How many unique User_IDs are there in the dataset?
length(unique(blackf_data$User_ID))# The store had 5891 customers

# How many items did each customer purchased?
Unique_UserID <- as.data.frame(table(blackf_data$User_ID))
names(Unique_UserID) <- c("User_ID", "Customer_Purchase_Count")
head(Unique_UserID)

# Due to the large dataset, the average values were used for this analysis (using dplyr's chaining method)
new_data <- blackf_data %>%   
  group_by(User_ID, Age, Gender, Occupation, City_Category, Stay_In_Current_City_Years, Marital_Status) %>% 
  summarise_each(funs(mean), Product_Category_1, Product_Category_2, Product_Category_3, Purchase)

# Rename the average values accordingly
colnames(new_data)[8] <- "Product_Cat_1_Avg" 
colnames(new_data)[9] <- "Product_Cat_2_Avg"
colnames(new_data)[10] <- "Product_Cat_3_Avg"
colnames(new_data)[11] <- "Avg_Purchase_Amount"

# Explore the age and gender variables versus the product categories and the purchase amount

# 1. Which Age group/gender had the highest purchase by product category?

# Product Category 1:
ggplot(new_data, aes(Gender, Product_Cat_1_Avg, fill = Gender)) + geom_col(width = 0.4) + facet_wrap(~ Age) + 
  labs(title = "Age Group/Gender Vs Product Category 1")

# Product Category 2:
ggplot(new_data, aes(Gender, Product_Cat_2_Avg, fill = Gender)) + geom_col() + facet_wrap(~ Age) +  
  labs(title = "Age Group/Gender Vs Product Category 2")

# Product Category 3:
ggplot(new_data, aes(Gender, Product_Cat_3_Avg, fill = Gender)) + geom_col() + facet_wrap(~ Age) +  
  labs(title = "Age Group/Gender Vs Product Category 3")

# Age group versus Average purchase amount
ggplot(new_data, aes(Age, Avg_Purchase_Amount, fill = Gender)) + geom_col() + facet_wrap(~ Gender) +  
  labs(title = "Age Group/Gender Vs Average Purchase Amount")

# 2. Which product category raked in the most money wrapped with age?
# Product Category 1:
ggplot(new_data, aes(Product_Cat_1_Avg, Avg_Purchase_Amount, color = Age)) + geom_point() + facet_wrap(~ Age) +
  labs(title = "Product_Cat_1_Avg Vs Avg_Purchase_Amount")

# Product Category 2:
ggplot(new_data, aes(Product_Cat_2_Avg, Avg_Purchase_Amount, color = Age)) + geom_point() + facet_wrap(~ Age) + 
  labs(title = "Product_Cat_2_Avg Vs Avg_Purchase_Amount")

# Product Category 3:
ggplot(new_data, aes(Product_Cat_3_Avg, Avg_Purchase_Amount, color = Age)) + geom_point() + facet_wrap(~ Age) + 
  labs(title = "Product_Cat_3_Avg Vs Avg_Purchase_Amount")

# Explore the occupation variable versus the product categories and the purchase amount

# 1. Which occupation had more influence on product purchase?
# Product Category 1:
ggplot(new_data, aes(Occupation, Product_Cat_1_Avg, fill = Occupation)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "Occupation Vs Product Category 1")

# Product Category 2:
ggplot(new_data, aes(Occupation, Product_Cat_2_Avg, fill = Occupation)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "Occupation Vs Product Category 2")

# Product Category 3:
ggplot(new_data, aes(Occupation, Product_Cat_3_Avg, fill = Occupation)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "Occupation Vs Product Category 3")  

# Which occupation spent the most money?
ggplot(new_data, aes(Occupation, Avg_Purchase_Amount, fill = Occupation)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "Occupation Vs Average Purchase Amount")  

# Explore the city category variable versus the product categories and the purchase amount

# Product category 1:
ggplot(new_data, aes(City_Category, Product_Cat_1_Avg, fill = City_Category)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "City Category Vs Product_Category 1")  

# Product Category 2:
ggplot(new_data, aes(City_Category, Product_Cat_2_Avg, fill = City_Category)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "City Category Vs Product_Category 2")  

# Product Category 3:
ggplot(new_data, aes(City_Category, Product_Cat_3_Avg, fill = City_Category)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "City Category Vs Product_Category 3")  

# City Category versus Average purchase amount
ggplot(new_data, aes(City_Category, Avg_Purchase_Amount, fill = City_Category)) + geom_col() +  facet_wrap(~ Age) +
  labs(title = "City Category Vs Average Purchase Amount")  

# Explore the Stay in current city variable versus the product categories and the purchase amount

# Product Category 1:
ggplot(new_data, aes(Stay_In_Current_City_Years, Product_Cat_1_Avg, fill = Stay_In_Current_City_Years)) + geom_col() +  
  facet_wrap(~ Age) + labs(title = "Stay in current city Vs Product_Category 1")  

# Product category 2:
ggplot(new_data, aes(Stay_In_Current_City_Years, Product_Cat_2_Avg, fill = Stay_In_Current_City_Years)) + geom_col() +  
  facet_wrap(~ Age) + labs(title = "Stay in current city Vs Product_Category 2")

# Product category 3:
ggplot(new_data, aes(Stay_In_Current_City_Years, Product_Cat_3_Avg, fill = Stay_In_Current_City_Years)) + geom_col() +  
  facet_wrap(~ Age) + labs(title = "Stay in current city Vs Product_Category 3")

# Stay in current city versus Average purchase amount
ggplot(new_data, aes(Stay_In_Current_City_Years, Avg_Purchase_Amount, fill = Stay_In_Current_City_Years)) + geom_col() +  
  facet_wrap(~ Age) + labs(title = "Stay in current city Vs Avg_Purchase_Amount")


# Explore the marital status variable.

# Product Category 1:
ggplot(new_data, aes(Marital_Status, Product_Cat_1_Avg, fill = Marital_Status)) + geom_col() +  
  facet_wrap(~ City_Category) + labs(title = "Marital Status Vs Product_Category 1 (wrapped with city category)")  

# Product Category 2:
ggplot(new_data, aes(Marital_Status, Product_Cat_2_Avg, fill = Marital_Status)) + geom_col() +  
  facet_wrap(~ City_Category) + labs(title = "Marital Status Vs Product_Category 2 (wrapped with city category)")  

# Product Category 3:
ggplot(new_data, aes(Marital_Status, Product_Cat_3_Avg, fill = Marital_Status)) + geom_col() +  
  facet_wrap(~ City_Category) + labs(title = "Marital Status Vs Product_Category 3 (wrapped with city category)")  

# Marital status versus Average purchase amount
ggplot(new_data, aes(Marital_Status, Avg_Purchase_Amount, fill = Marital_Status)) + geom_col() +  
  facet_wrap(~ City_Category) + labs(title = "Marital Status Vs Avg_Purchase_Amount")


# CONCLUSION

# 1. THERE WERE MORE MALES (75%) CUSTOMERS THAN FEMALES (25%)

# 2. THE 3 MAJOR CUSTOMERS CLASSIFIED UNDER THE AGE GROUP VARIABLE ARE
      # A. 26-35 YEARS OLD --- 40%
      # B. 36-45 YEARS OLD --- 20%
      # C. 18-25 YEARS OLD --- 18%
    # THE LEAST WAS THE 0 -17 YEARS OLD WITH JUST 3%, FOLLOWING THAT CLOSELY WAS THE 55+ WITH 4%, 
    # THE 46-50 AND 51-55 WERE CLOSELY MATCHED WITH 8% AND 7% RESPECTIVELY

# 3. CITY CATEGORY B HAD THE MOST CUSTOMERS(42%), FOLLOWED BY C (31%), AND LASTLY A (27%)

# 4. pEOPLE WHO HAD STAYED IN THE CITY FOR 1 YEAR CONSTITUTED MAJORITY OF THE CUSTOMERS

# 5. sINGLES MADE UP 59 % WHILE MARRIED MADE UP 41% OF THE CUSTOMERS

# 6. MORE MALES THAN FEMALES PURCHASED ALL 3 PRODUCT CATEGORIES, ALTHOUGH FOR THE 0-17 YEARS OLD, IT WAS CLOSELY MATCHED

# 7. AS EXPECTED 26-35 YEAR OLDS SPENT MORE MONEY

# 8. IN ALL THE CITY CATEGORIES, THE SINGLES SPENT THE MOST MONEY

# 9. FOR THE PRODUCT CATEGORIES WITH THE HIGHEST PURCHASE, THE PLOTS PRESENT THEM IN DETAILS

