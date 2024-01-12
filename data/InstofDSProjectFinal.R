
library(Sleuth3)
library(tidyverse)
library(car)
library (mosaic)
library (ggthemes)
library (gmodels)
library (DescTools)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidymodels)
library(tidyr)
library(modelr)
library(rpart.plot)
library(estimatr)
library(robust)

setwd("/Users/abigailtata/Desktop/inst doe DS/country data ")
#read in country data  
LE1<- read_csv("https://raw.githubusercontent.com/josh-greene-54/IDS-proj-grp/main/data/countrydata.csv", TRUE)
view(LE1)


# read in income class data 
incomeclass<- read_csv("https://raw.githubusercontent.com/josh-greene-54/IDS-proj-grp/main/data/incomeclass.csv", TRUE)
view(incomeclass)

# take out NAs in the dataset 
LE1 <- na.omit(LE1)


# make all variables numeric 
LE1 <- LE1 %>%
  mutate(
    GDP.per.capita = as.numeric(as.character(GDP.per.capita)),
    Population = as.numeric(as.character(Population )),
    Life.expectancy = as.numeric(as.character(Life.expectancy)),
    Current.health.expenditure.per.gdp = as.numeric(as.character(Current.health.expenditure.per.gdp)),
    Current.health.expenditure.per.capita = as.numeric(as.character(Current.health.expenditure.per.capita)),
    Compulsory.education.duration = as.numeric(as.character(Compulsory.education.duration )),
)


# log

# log  GDP
LE1$logGDP<- log(LE1$GDP.per.capita)
# log Current health expenditure per capita
LE1$logHCExpCapita<- log(LE1$Current.health.expenditure.per.capita)
view(LE1)


# exploratory scatter plots with correlations 

# scatter plot of Compulsory.education.duration and loggdp
ggplot(data = LE1) +
  geom_point(mapping = aes(x = GDP.per.capita , y = Compulsory.education.duration)) +
  geom_smooth(method = lm,mapping = aes(x = logGDP , y = Compulsory.education.duration), se=FALSE)+
  ggtitle("                               Scatter Plot Compulsory.education.duration and loggdp")

cor(LE1$logGDP,LE1$Compulsory.education.duration, use = "complete.obs")


#view GDP.per.capita and Current.health.expenditure.per.capita
ggplot(data = LE1) +
  geom_point(mapping = aes(y = Current.health.expenditure.per.capita , x = GDP.per.capita)) +
  geom_smooth(method = lm,mapping = aes(x = GDP.per.capita , y = Current.health.expenditure.per.capita), se=FALSE)+
  ggtitle("                               Scatter Plot GDP.per.capita and Current.health.expenditure.per.capita")

cor(LE1$logGDP,LE1$Current.health.expenditure.per.capita, use = "complete.obs")


#### view Current.health.expenditure.per.capita and logGDP
ggplot(data = LE1) +
  geom_point(mapping = aes(y = logHCExpCapita , x = logGDP)) +
  geom_smooth(method = lm,mapping = aes(y = logHCExpCapita , x = logGDP), se=FALSE)+
  ggtitle("                               Scatter Plot Current.health.expenditure.per.capita and logGDP")

cor(LE1$logGDP,LE1$logHCExpCapita, use = "complete.obs")



#### view life expectancy and logGDP
ggplot(data = LE1) +
  geom_point(mapping = aes(y = Life.expectancy , x = logGDP)) +
  geom_smooth(method = lm,mapping = aes(x = logGDP , y = Life.expectancy), se=FALSE)+
  ggtitle("                  Scatter Plot  life expectancy and logGDP")


cor(LE1$Life.expectancy,LE1$logGDP, use = "complete.obs")



# lm model with summary of log GDP and Life expenctancy 
edulm<-lm(LE1$Life.expectancy~LE1$logGDP)
edulm
summary(edulm)
plot(edulm)

model <- lm_robust(Life.expectancy ~ logGDP, data = LE1)
# Print the summary of the model
#with robust standard errors 
summary(model)
plot(model)

#use cook distance package to show the 61 and 64 outliers and their SD

# boxplot to see outliers 
boxplot(LE1$logGDP)
boxplot(LE1$Life.expectancy)
#significant amount of outliers in gdp

#summary stats of the merged data set 
lm_out <- lm(Life.expectancy ~ logGDP, data = merged_df)
summary(lm_out)

#add residuals to data set 
merged_df <- merged_df %>%
  add_residuals(lm_out)

#residual plot 
ggplot(merged_df, aes(logGDP, resid)) + geom_point()
+ geom_hline(yintercept = 0, linetype = 2, color = "red")

#addpredictions to data set 
merged_df <- merged_df %>%
  add_predictions(lm_out)
merged_df$pred

#loggdp and residuals
ggplot(merged_df, aes(logGDP, resid)) + geom_point(aes(color = class)) + geom_hline(yintercept = 0, linetype = 2)
summary(lm_out)

# t test H vs. L
sample_list <- split(merged_df$GDP.per.capita, merged_df$class)
group_H <- sample_list$`High Income`
group_L <- sample_list$`Lower Income`
t.test(group_H, group_L)

#anova test 
anova_result <- aov(logGDP ~ class, data = merged_df)
summary(anova_result)


# multiple linear regression exploration

model1 <- lm(Life.expectancy ~ logGDP + logHCExpCapita+ Population + Compulsory.education.duration, data = merged_df)

model3 <- lm(Life.expectancy ~ logGDP + logHCExpCapita, data = merged_df)

# Display the summary of the model
summary(model1)
summary(model3)

set.seed(123)  # Setting a seed for reproducibility
split <- initial_split(merged_df, prop = 0.7, strata = Life.expectancy)
train_data <- training(split)
test_data <- testing(split)

# Create a decision tree model using the 'rpart' package
tree_model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Train the model
trained_model <- tree_model %>% fit(Life.expectancy ~ logGDP+logHCExpCapita, data = train_data)
# Make predictions on the test set
predictions <- predict(trained_model, new_data = test_data)
# View the predictions
print(predictions)
#visualize tree
rpart.plot(trained_model$fit, roundint = FALSE)

predict_life_expectancy <- function(trained_model, log_gdp_value, log_hc_exp_value) {
  new_data <- data.frame(logGDP = log_gdp_value, logHCExpCapita = log_hc_exp_value)
  new_predictions <- predict(trained_model, new_data)
  new_predictions <- as.numeric(new_predictions[[1]])
  GDP <- exp(log_gdp_value)
  cat(" Countrys GDP", GDP, "\n")
  cat("Predicted Average Life Expectancy:", new_predictions, "\n")
  return(list(PredictedLifeExpectancy = new_predictions))
}
#input values
log_gdp_value <- 11
log_hc_exp_value <- 10
predicted_life_expectancy <- predict_life_expectancy(trained_model, log_gdp_value, log_hc_exp_value)


