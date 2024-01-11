
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
#setwd("/Users/abigailtata/Desktop/inst doe DS/country data ")
#read in country data  
LE1<- read_csv("https://raw.githubusercontent.com/josh-greene-54/IDS-proj-grp/main/data/countrydata.csv", TRUE)
view(LE1)


# read in income class data 
incomeclass<- read_csv("https://raw.githubusercontent.com/josh-greene-54/IDS-proj-grp/main/data/incomeclass.csv", TRUE)
view(incomeclass)

# take out NAs in the dataset 
LE1 <- na.omit(LE1)
sum(is.na(LE1$GDP.per.capita))
sum(is.na(LE1$Current.health.expenditure.per.capita))


# make all variables numeric 
LE1 <- LE1 %>%
  mutate(
    GDP.per.capita = as.numeric(as.character(GDP.per.capita)),
    Population = as.numeric(as.character(Population )),
    Life.expectancy = as.numeric(as.character(Life.expectancy)),
    Current.health.expenditure.per.gdp = as.numeric(as.character(Current.health.expenditure.per.gdp)),
    Current.health.expenditure.per.capita = as.numeric(as.character(Current.health.expenditure.per.capita)),
    Compulsory.education.duration = as.numeric(as.character(Compulsory.education.duration )),
    Birth.rate.p1000  = as.numeric(as.character(Birth.rate.p1000 )),
    Death.rate.p1000 = as.numeric(as.character(Death.rate.p1000 ))
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
  geom_point(mapping = aes(x = logGDP , y = Compulsory.education.duration)) +
  geom_smooth(method = lm,mapping = aes(x = logGDP , y = Compulsory.education.duration), se=FALSE)+
  ggtitle("                               Scatter Plot Compulsory.education.duration and loggdp")

cor(LE1$logGDP,LE1$Compulsory.education.duration, use = "complete.obs")


#view GDP.per.capita and Current.health.expenditure.per.capita
ggplot(data = LE1) +
  geom_point(mapping = aes(y = GDP.per.capita , x = Current.health.expenditure.per.capita)) +
  geom_smooth(method = lm,mapping = aes(y = GDP.per.capita , x = Current.health.expenditure.per.capita), se=FALSE)+
  ggtitle("                               Scatter Plot GDP.per.capita and Current.health.expenditure.per.capita")

cor(LE1$logGDP,LE1$Current.health.expenditure.per.capita, use = "complete.obs")


#### view Current.health.expenditure.per.capita and logGDP
ggplot(data = LE1) +
  geom_point(mapping = aes(x = logHCExpCapita , y = logGDP)) +
  geom_smooth(method = lm,mapping = aes(x = logHCExpCapita , y = logGDP), se=FALSE)+
  ggtitle("                               Scatter Plot Current.health.expenditure.per.capita and logGDP")

cor(LE1$logGDP,LE1$logHCExpCapita, use = "complete.obs")



#### view life expectancy and logGDP
ggplot(data = LE1) +
  geom_point(mapping = aes(x = Life.expectancy , y = logGDP)) +
  geom_smooth(method = lm,mapping = aes(x = Life.expectancy , y = logGDP), se=FALSE)+
  ggtitle("                               Scatter Plot  life expectancy and logGDP")


cor(LE1$Life.expectancy,LE1$logGDP, use = "complete.obs")

# visualizing correlation between birth rate and life expectancy
ggplot(data = LE1) +
  geom_point(mapping = aes(x = Life.expectancy , y = Birth.rate.p1000)) +
  geom_smooth(method = lm,mapping = aes(x = Life.expectancy , y = Birth.rate.p1000), se=FALSE)+
  ggtitle("                               Scatter Plot birth rate and life expectancy")

cor(LE1$Life.expectancy,LE1$Birth.rate.p1000, use = "complete.obs")



# lm model with summary of log GDP and Life expenctancy 
edulm<-lm(LE1$logGDP~LE1$Life.expectancy)
edulm
summary(edulm)
plot(edulm)

#use cook distance package to show the 61 and 64 outliers and their SD

# boxplot to see outliers 
boxplot(LE1$logGDP)
boxplot(LE1$Life.expectancy)
#significant amount of outliers in gdp



# merge both data sets to be able to look at class in LE1
merged_df <- merge(incomeclass, LE1, by = "Country.Name")
view(merged_df)



# mutate class variable to long name and get rid of GDPrange

merged_df <- merged_df %>%
  select(-GDPrange)
merged_df <- merged_df %>%
  mutate(class = case_when(
    class == "L" ~ "Lower Income",
    class == "LM" ~ "Lower Middle Income",
    class == "UM" ~ " Upper Middle Income",
    class == "H" ~ "High Income"
  ))

view(merged_df)
str(merged_df)

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

model1 <- lm(Life.expectancy ~ logGDP + logHCExpCapita + Birth.rate.p1000 + Death.rate.p1000+ Population + Compulsory.education.duration, data = merged_df)

model2 <- lm(Life.expectancy ~  Birth.rate.p1000 + Death.rate.p1000, data = merged_df)

model3 <- lm(Life.expectancy ~ logGDP + logHCExpCapita, data = merged_df)

# Display the summary of the model
summary(model1)
summary(model2)
summary(model3)

# looking at the multiple linear regression we are seeing that with all of the independent variables 
#together LogGDP is not considered to be significant, instead, logHCExpCapita,Birth.rate.p1000 and Death.rate.p1000 
#are the only significant ones. when logGDP and logHCExpCapita are alone in a model theyre both significant
#this goes to show that although logGDP has a strong correlation with life expectancy in isolation, with other variables
#it does not appear to have a significant relationship with life expectancy. 
#this could be due to confounding variables or multicollinearity.


#machine learning to try to predict life expectancy from loggdp

set.seed(123)  # Setting a seed for reproducibility
split <- initial_split(merged_df, prop = 0.7, strata = Life.expectancy)
train_data <- training(split)
test_data <- testing(split)

# Create a decision tree model using the 'rpart' package
tree_model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Train the model
trained_model <- tree_model %>% fit(Life.expectancy ~ logGDP, data = train_data)

# Make predictions on the test set
predictions <- predict(trained_model, new_data = test_data)

# View the predictions
print(predictions)

# try a logGDP prediction 
new_data <- data.frame(logGDP = 11)
new_predictions <- predict(trained_model, new_data)

#show decision tree
print(new_predictions)
rpart.plot(trained_model$fit)



