#Loading Data Set of Life Expectancy 
data<- read.csv(file='C:/Users/91909/OneDrive/Desktop/Modelling/Life_Expectancy_Data.csv')
#This package contains a set of functions related to exploratory data analysis, data preparation, and model performance.
library(funModeling)
#The tidyverse package is designed to make it easy to install and load core packages from the tidyverse in a single command.
#ggplot2, for data visualisation.
library(ggplot2)
#dplyr, for data manipulation.
library(dplyr)
#tidyr, for data tidying.
#readr, for data import.
library(tidyverse)
#Harrell Miscellaneous-replace tables with graphics
library(Hmisc)
library(psych)
#Calculates correlation of variables and displays the results graphically.
library(corrgram)
# Usedfor performing basic statistical tests, including t-test, Wilcoxon test, ANOVA, Kruskal-Wallis and correlation analyses.
library('rstatix')
library(corrplot)
library(faraway)
#Tools designed to make it easier for users, particularly beginner/intermediate R users to build ordinary least squares regression models
library('olsrr')
#leaps() performs an exhaustive search for the best subsets of the variables in x for predicting y in linear regression, using an efficient branch-and-bound algorithm
library('leaps')
#plyr is an R package that makes it simple to split data apart,common data-manipulation step. 
library(plyr)
#Generate a nicely formatted table for LaTeX 
library(xtable)

head(data)
#calculate summary statistics of each column of dataframe  in R 
summary(data)
#str displays structures of R objects. str are mostly used for displaying the contents of a list.
#str () is an alternative function to display the summary of the output produced, especially when the data set is huge.
str(data)
#Finding missing data
rowSums(is.na(data))
colSums(is.na(data))
freq(data)

#Checking distribution
install.packages(ggplot2)
library(ggplot2)

#Rename column names
names(data)[1]<-'Country_name'
names(data)[4]<- "Life_expectancy"
names(data)[5]<- "Electricity"
names(data)[6]<-"net_national_income"
names(data)[7]<-"net_national_income_percapita"
names(data)[8]<-"HIV"
names(data)[9]<-"Children_out_school"
names(data)[10]<-"Education_primary"
names(data)[11]<-"Education_Bachelors"
names(data)[12]<-"Mortality_rate"
names(data)[13]<-"Primary_completion_rate"
names(data)[14]<-"Literacy_rate"
names(data)[15]<-"Real_interest_rate"
names(data)[16]<-"Population_growth"
names(data)[17]<-"Population_density"
names(data)[18]<-"Total_population"
names(data)[19]<-"Current_health_exp_per_capita"
names(data)[20]<-"Current_health_exp"
names(data)[21]<-"Unemplyoment_total"
names(data)[22]<-"GDP_growth"
names(data)[23]<-"GDP_per_capita"
names(data)[24]<-"Birth_rate"
names(data)[25]<-"Renewable _energy_consumption"
names(data)[26]<-"Adults"
names(data)[27]<-"Drinking_water"
names(data)[28]<-"Poverty_headcount"
names(data)[29]<-"Compulsory_education"

# Building histogram
numerical_data = data %>% dplyr::select(4:29)
ggplot(gather(numerical_data[,-c(2,7,12,15,16,25)]), aes(value)) + 
  geom_histogram(bins = 10,color="darkblue", fill="lightblue") + 
  facet_wrap(~key, scales = 'free_x')


#Dropping columns having categorical variable
#Renewable energy consumption,CountryName, Code, Continent
df<-data %>%select(-c(1,2,3,25))
head(df)
#cor_mat : compute correlation matrix with p-values.
corr <- cor_mat(df)
corr %>% View()
#These attributes are choosen based on co-relation value closer to 1 and -1
df1<- df %>%select(c("Life_expectancy","Electricity","Mortality_rate","Primary_completion_rate","Literacy_rate",
                     "Population_growth","Current_health_exp_per_capita","GDP_per_capita","Birth_rate",
                     "Drinking_water","Poverty_headcount"))


corr_1 <- cor_mat(df1)
corr_1
corr_1 %>% View()
View(corr_1)

modelx<- lm(Life_expectancy~ Electricity+Mortality_rate+Primary_completion_rate+Literacy_rate+Population_growth
            +Current_health_exp_per_capita+GDP_per_capita+Birth_rate+Drinking_water+Poverty_headcount, data=df1)

vif(modelx)
#based on vif we eliminate Current_health_exp_per_capita,GDP_per_capita,Birth_rate
modelx1<- lm(Life_expectancy~ Electricity+Mortality_rate+Primary_completion_rate+Literacy_rate+Population_growth+
               Drinking_water+Poverty_headcount, data=df1)
summary(modelx1)
vif(modelx1)

new_df<- data %>%select(c("Country_name","Country.Code","Continent","Life_expectancy","Electricity","Mortality_rate","Primary_completion_rate","Literacy_rate",
                          "Population_growth",'Drinking_water',"Poverty_headcount"))
library(data.table)
setDT(new_df)
new_df[, Life_expectancy := ifelse(is.na(Life_expectancy),
                                   median(Life_expectancy, na.rm = TRUE),
                                   Life_expectancy), by='Continent']
new_df[, Electricity := ifelse(is.na(Electricity),
                               median(Electricity, na.rm = TRUE),
                               Electricity), by='Continent']
new_df[, Mortality_rate := ifelse(is.na(Mortality_rate),
                                  median(Mortality_rate, na.rm = TRUE),
                                  Mortality_rate), by='Continent']
new_df[, Primary_completion_rate := ifelse(is.na(Primary_completion_rate),
                                           median(Primary_completion_rate, na.rm = TRUE),
                                           Primary_completion_rate), by='Continent']
new_df[, Literacy_rate := ifelse(is.na(Literacy_rate),
                                 median(Literacy_rate, na.rm = TRUE),
                                 Literacy_rate), by='Continent']
new_df[, Population_growth := ifelse(is.na(Population_growth),
                                     median(Population_growth, na.rm = TRUE),
                                     Population_growth), by='Continent']
new_df[, Poverty_headcount := ifelse(is.na(Poverty_headcount),
                                     median(Poverty_headcount, na.rm = TRUE),
                                     Poverty_headcount), by='Continent']
new_df[, Drinking_water := ifelse(is.na(Drinking_water),
                                  median(Drinking_water, na.rm = TRUE),
                                  Drinking_water), by='Continent']

colSums(is.na(new_df))
head(new_df)
numerical_data2 = new_df %>% dplyr::select(3:9)
ggplot(gather(numerical_data2[,-c(1,2)]), aes(value)) + 
  geom_histogram(bins = 10,color="darkblue", fill="lightblue") + 
  facet_wrap(~key, scales = 'free_x')
#-----------------------------------------QUESTION 3---------------------------
#bulilding model after imputing missing values
new_model<- lm(Life_expectancy~  Electricity+Mortality_rate+Primary_completion_rate+Population_growth+
                 Drinking_water, data=new_df)
summary(new_model)
vif(new_model)

#after multiplying t-value and estimate the max value is seen in  Mortality_rate and Population_growth
new_model1 <- lm(Life_expectancy~Mortality_rate+Population_growth, data=new_df)
summary(new_model1)
vif(new_model1)

#taking maximum three
new_model2 <- lm(Life_expectancy~ Mortality_rate+Population_growth+Drinking_water, data=new_df)
summary(new_model2)
vif(new_model2)

#On based on Adjusted R-sqaure value we take two models: new_model,new_model2
#plots the standardised residuals against fitted values for new_model
stdres_newmodel<-rstandard(new_model)
plot(new_model$fitted.values,stdres_newmodel,pch=16,
     ylab="Standardized Residuals",xlab="fitted y",ylim=c(-3,3),main="Full model")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

#plots the QQ-plot for the new_model - Data that aligns closely to the dotted line indicates a normal distribution.
#If the points skew drastically from the line, you could consider adjusting your model by adding or removing other
#variables in the regression model.
qqnorm(stdres_newmodel, ylab="Standardized Residuals",
       xlab="Normal Scores", main="QQ Plot for Full model" )
qqline(stdres_newmodel)


AIC(new_model)
AIC(new_model2)
AIC(new_model1)

ols_mallows_cp(new_model,new_model2)

full.model.cp <- lm(Life_expectancy ~ Electricity+Mortality_rate+Primary_completion_rate+Population_growth+
                      Drinking_water, data=new_df,x=TRUE)
X <- full.model.cp$x
y <-new_df$Life_expectancy
all.models <- leaps(X, y, int = FALSE, strictly.compatible = FALSE, method="Cp")
plot(all.models$size, all.models$Cp, log="y", xlab="|M|", ylab=expression(C[p]),ylim=c(1,200))
#we log transfrom the y axis as the C_p value can get quite large
lines(all.models$size, all.models$size) # this plots the line C_p=|M|
min.cp <- all.models$Cp == min(all.models$Cp) #this finds the smallest C_p value
min(all.models$Cp) #gives the min C_p value
min.cp <- all.models$which[min.cp, ] #this finds the corresponding model with the smallest C_p
min.cp #this lists the paramters included in the model

final_model <- lm(Life_expectancy~  Electricity+Mortality_rate+Population_growth+
                    Drinking_water, data=new_df)
summary(final_model)
AIC(final_model)
AIC(new_model)
AIC(new_model1)
ols_mallows_cp(final_model,new_model)


#plots the standardised residuals against fitted values for REDUCED model
stdres_final_model<-rstandard(final_model)
plot(final_model$fitted.values,stdres_final_model,pch=16,ylab="Standardized Residuals",xlab="fitted y",ylim=c(-3,3),main="reduced model")
abline(h=2,lty=2)
abline(h=-2,lty=2)
#plots the QQ-plot for the REDUCED model
qqnorm(stdres_final_model, ylab="Standardized Residuals",
       xlab="Normal Scores", main="QQ Plot for Reduced model")
qqline(stdres_final_model)


df1$predicted_mlr <- predict(final_model)
df1$residuals_mlr <- residuals(final_model)
plot(final_model)

df1 %>%
  gather(key = "iv", value = "x", -Life_expectancy, -predicted_mlr, -residuals_mlr) %>%
  ggplot(aes(x = x, y = Life_expectancy)) + # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted_mlr), alpha = .2) +
  geom_point(aes(color = residuals_mlr)) +
  geom_point(aes(y = predicted_mlr), shape = 1) +
  facet_grid(~ iv, scales = "free_x") + # Split panels here by `iv`
  theme_bw()
#---------------------------------QUESTION 4TH------------------------------------
df2 <- new_df %>%select(c("Continent","Life_expectancy","Electricity","Mortality_rate","Population_growth",'Drinking_water'))
setDF(df2)  
avgLifeExpByContinent <- ddply(df2, ~Continent, summarize, avgLifeExp = mean(Life_expectancy))
avgLifeExpByContinent

boxplot(df2$Life_expectancy~df2$Continent, main='Comparing life expectancy to Continent',
        xlab='Continent', col="red", ylab = "Life Expectancy",)

#Null hypothesis: No  differences between the group means
oneway_anova<-aov(Life_expectancy~as.factor(Continent),data=df2)
summary(oneway_anova)
#Inference- F-value and the p-value is less than 0.05, hence we reject H0 at the 5% significance level.

cat("Bonferroni post-hoc test","\n")
pairwise.t.test(df2$Life_expectancy, df2$Continent, p.adj = "bonferroni")
cat("\n","Tukey post-hoc test","\n")
tukey.lifeexpectancy<-TukeyHSD(oneway_anova)
plot(tukey.lifeexpectancy)

df2$residuals<-oneway_anova$residuals
par(mfrow=c(1,2))
hist(df2$residuals, main="Standardised residuals-histogram",xlab="Standardised residuals")
qqnorm(df2$residuals,pch=19)
qqline(df2$residuals1)
shapiro.test(df2$residuals)

