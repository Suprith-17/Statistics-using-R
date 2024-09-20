
#Installing nnet for multinomial distribution

install.packages("nnet")  
library(nnet)
library(ggplot2)   #For graphical representation


#Q1.(a)	Describe the data set using appropriate plots/curves/charts


#Graphical representation of gender column

shopping_trends <- read_excel("C:/Users/sarve/Downloads/shopping_trends.xlsx")
Gender_Count <- table(shopping_trends[["Gender"]])
Gender_Count_df <- as.data.frame(Gender_Count)
Gender_Count_df$Var1 <- as.factor(Gender_Count_df$Var1)


ggplot(Gender_Count_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total count of Male and female in shop", x = "Gender", y = "Count") +
  theme_minimal()


#Graphical representation of Category column

Category_Count <- table(shopping_trends[["Category"]])
Category_Count_df <- as.data.frame(Category_Count)
Category_Count_df$Var1 <- as.factor(Category_Count_df$Var1)

ggplot(Category_Count_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of sales in each category", x = "Categories", y = "Total sales") +
  theme_minimal()


#Graphical representation of season column

Season_Count <- table(shopping_trends[["Season"]])
Season_Count_df <- as.data.frame(Season_Count)
Season_Count_df$Var1 <- as.factor(Season_Count_df$Var1)

ggplot(Season_Count_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total number of sales in each season", x = "Seasons", y = "Total sales") +
  theme_minimal()


#Graphical representation of Payment method column

Payment_Method_Count <- table(shopping_trends[["Payment Method"]])
Payment_Method_Count_df <- as.data.frame(Payment_Method_Count)
Payment_Method_Count_df$Var1 <- as.factor(Payment_Method_Count_df$Var1)

pie_chart <- ggplot(Payment_Method_Count_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Total count of each payment method") +
  geom_text(aes(label = paste(Var1, "\n", Freq)), position = position_stack(vjust = 0.5))
print(pie_chart)


#Graphical representation of Frequency of purchases column

Frequency_of_Purchases_Count <- table(shopping_trends[["Frequency of Purchases"]])
Frequency_of_Purchases_Count_df <- as.data.frame(Frequency_of_Purchases_Count)
Frequency_of_Purchases_Count_df$Var1 <- as.factor(Frequency_of_Purchases_Count_df$Var1)

ggplot(Frequency_of_Purchases_Count_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Frequency of Purchases", x = "Frequency of Purchases", y = "Total count") +
  theme_minimal()



#--------------------------------------------------------------------------------------------------------



#Q1.(b)	Consider one of continuous attributes, and compute central and variational measures.

# Central measures
mean_purchase_amount <- mean(shopping_trends$"Purchase Amount (USD)")
median_purchase_amount <- median(shopping_trends$"Purchase Amount (USD)")
print(mean_purchase_amount)
print(median_purchase_amount)

# Variational measures
sd_purchase_amount <- sd(shopping_trends$"Purchase Amount (USD)")
range_purchase_amount<- max(shopping_trends$"Purchase Amount (USD)") - min(shopping_trends$"Purchase Amount (USD)")
print(sd_purchase_amount)
print(range_purchase_amount)



#----------------------------------------------------------------------------------------------------



# Q.1(c)	For a particular variable of the data set, use Chebyshev's rule, and propose one-sigma interval. Based on your proposed interval, specify the outliers if any.                                                          

purchase_amount <- shopping_trends$"Purchase Amount (USD)"
k <- 1
lower_limit <- mean_purchase_amount - k * sd_purchase_amount
upper_limit <- mean_purchase_amount + k * sd_purchase_amount
outliers <- purchase_amount[purchase_amount < lower_limit | purchase_amount > upper_limit]
Outliers_total_count <- length(outliers)

cat("Proposed one sigma interval :",lower_limit,"to",upper_limit)

cat("Outliers:",Outliers_total_count,"\n")



#-------------------------------------------------------------------------------------------------------



#Q1.(d)	Explain how the box-plot technique can be used to detect outliers. Apply this technique for one attribute of the data set

boxplot(purchase_amount, main = "Box Plot with Outliers", ylab = "Purchase amount")


points(which(purchase_amount < lower_limit | purchase_amount > upper_limit), purchase_amount[purchase_amount < lower_limit | purchase_amount > upper_limit], col = "red", pch = 16)



#---------------------------------------------------------------------------------------------------------




# Q2.
# a)	Select four variables of the dataset, and propose an appropriate probability model to quantify uncertainty of each variable.           
#                                       
# b)	 For each model in part (a), estimate the parameters of model.   
#  
# c)	Express the way in which each model can be used for the predictive analytics, then find the prediction for each attribute.                              



#putting data set into a data frame

df <- data.frame(shopping_trends)


# Implementing Bernoulli Distribution Probability model to subscription column

df$Subscription.Status <- as.numeric(df$Subscription.Status == "Yes")
head(df)
subscription_model <- glm(Subscription.Status ~ 1, data = df, family = "binomial")

# Extracting the estimated probability

prediction_subscription <- predict(subscription_model, type = "response")

model_parameters_Subscription <- summary(subscription_model)$coefficients

# Printing the estimated probability

cat("Estimated Probability of Subscription (p):", prediction_subscription[1:10])

#Printing model Parameters

cat("\nModel Parameters:\n")
print(model_parameters_Subscription)



#---------------------------------------------------------------------------------------------------------



# Implementing Gaussian (Normal) Distribution Probability model to Purchase amount (USD) column


purchase_amount_model <- lm(df$Purchase.Amount..USD. ~ 1, data = df)

# Extracting the estimated parameters

mean_purchase_amount <- coef(purchase_amount_model)[1]
sd_purchase_amount <- sd(residuals(purchase_amount_model))

# Printing the estimated parameters

cat("Estimated Mean (μ) of Purchase Amount:", mean_purchase_amount, "\n")
cat("Estimated Standard Deviation (σ) of Purchase Amount:", sd_purchase_amount, "\n")

# Predict purchase amounts for new data

new_data <- data.frame(ID = 101:110)  # Assuming 10 new observations
predicted_purchase <- predict(purchase_amount_model, newdata = new_data)

# Print the predicted purchase amounts

cat("Predicted Purchase Amounts for New Data:", predicted_purchase, "\n")



#--------------------------------------------------------------------------------------------------------



# Implementing Multinomial Distribution Probability model to Frequency of Purchases  column


df$Frequency.of.Purchases <- as.factor(df$Frequency.of.Purchases)


# Fit a multinomial logistic regression model

frequency_model <- multinom(df$Frequency.of.Purchases ~ 1, data = df)


# Extracting the estimated probabilities for each category

probabilities <- predict(frequency_model, type = "probs")


# Printing the estimated probabilities for the first few observations

cat("Estimated Probabilities for Frequency:\n")
print(probabilities[1:5, ])

# Making predictions for new data

new_data <- data.frame(Constant = 1)
new_predictions <- predict(frequency_model, newdata = new_data, type = "probs")


# Printing the predicted probabilities for each category

cat("Predicted Probabilities for New Data:\n")
print(new_predictions)



#--------------------------------------------------------------------------------------------------



# Implementing Gaussian (Normal) Distribution Probability model to Age column


# Fit a linear regression model for Age

age_model <- lm(df$Age ~ 1, data = df)


# Extract the estimated parameters

coefficient_age <- coef(age_model)[1]
sd_age <- sd(residuals(age_model))


# Print the estimated parameters

cat("Estimated Mean (μ) of Age:", coefficient_age, "\n")
cat("Estimated Standard Deviation (σ) of Age:", sd_age, "\n")


# Predict Age for new observations

new_data <- data.frame(ID = 101:110)  # Example new data
predicted_age <- predict(age_model, newdata = new_data)


# Print the predicted Age

cat("Predicted Age for new observations:", predicted_age, "\n")



#---------------------------------------------------------------------------------------------



# Q3.(a)Consider two categorical variables of the dataset, develop a binary decision making strategy to check whether two variables are independent at the significant level alpha=0.01.  To do so,
# 
# i.State the hypotheses. 
# 
# ->Null Hypothesis: The two variables are independent.
# ->Alternative Hypothesis: The two variables are not independent.



# Creating a contingency table

contingency_table <- table(df$Gender, df$Subscription.Status)


# Perform the chi-squared test

chi_squared_test <- chisq.test(contingency_table)


# Print the test result

print(chi_squared_test)


#	Finding the statistic and critical values.  


# Extracting the p-value

p_value <- chi_squared_test$p.value


# Comparing the p-value to the significance level

alpha <- 0.01


# interpretation by using decision making statement

if (p_value < alpha) {
  cat("Reject the null hypothesis. The variables are not independent.\n")
} else {
  cat("Fail to reject the null hypothesis. The variables are independent.\n")
}


# iii.Explain your decision and Interpret results   
# 
# ->If we reject the null hypothesis, then we find that there is a significant association between the two variables.
# ->If we fail to reject the null hypothesis, then we find that there is no significant association between the two variables.



#---------------------------------------------------------------------------------------------



#Q3.(b)Consider one categorical variable, apply goodness of fit test to evaluate whether a candidate set of probabilities can be appropriate to quantify the uncertainty of class frequency at the significant level alpha=0.05

total_count <- nrow(df)

# Count the occurrences of each season

season_counts <- table(df$Season)


# Calculate the probability of each season

prob_Fall <- season_counts['Fall'] / total_count
prob_Spring <- season_counts['Spring'] / total_count
prob_Summer <- season_counts['Summer'] / total_count
prob_Winter <- season_counts['Winter'] / total_count


# Assuming your data frame is named 'df' and the column is 'Season'

observed_freq <- table(df$Season)


# Define the candidate set of probabilities

expected_probs <- c(prob_Fall, prob_Spring, prob_Summer, prob_Winter)


# Perform the goodness-of-fit test

chi_squared_test <- chisq.test(observed_freq, p = expected_probs)


# Print the test result

print(chi_squared_test)


# Extract the p-value

p_value <- chi_squared_test$p.value


# Compare the p-value to the significance level

alpha <- 0.05


# #interpretation by using decision making statement

if (p_value < alpha) {
  cat("Reject the null hypothesis. Observed frequencies do not match expected frequencies.\n")
} else {
  cat("Fail to reject the null hypothesis. Observed frequencies are consistent with expected frequencies.\n")
}



#---------------------------------------------------------------------------------------------------------------



#Q3.(c)Consider one continuous variable in the dataset, and apply test of mean for a proposed candidate of population mean at the significant level alpha=0.05.



candidate_mean <- 30 


# Performing one-sample t-test

t_test_result <- t.test(df$Age, mu = candidate_mean)


# Printing the test result

print(t_test_result)


#extracting p-value

p_value <- t_test_result$p.value


# Comparing the p-value to the significance level

alpha <- 0.05


#interpretation by using decision making statement

if (p_value < alpha) {
  cat("Reject the null hypothesis. Population mean is not equal to the proposed candidate mean.\n")
} else {
  cat("Fail to reject the null hypothesis. Population mean is equal to the proposed candidate mean.\n")
}