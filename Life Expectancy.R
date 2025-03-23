# import data set from Excel
dataset <- read.csv(file.choose(), header = T)

# packages and libraries
install.packages("summarytools")
library(summarytools)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tseries")
library(tseries)
install.packages("lmtest")
library(lmtest)
install.packages("strucchange")
library(strucchange)
install.packages("sandwich")
library(sandwich)

#pearson test for considering which variables to remove from model
cor.test(dataset$Outdoor.air.pollution...., dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$HIV...Estimated.number.of.people.that.have.been.infected, dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$malaria...Estimated.number.of.people.that.have.been.infected, dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$Average.income.per.person...., dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$Alcohol.consumption.per.person..liters..year., dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$density.per.square..km., dataset$Life_expectancy, method = c("pearson"))
cor.test(dataset$Cigarette.consumption...., dataset$Life_expectancy, method = c("pearson"))

# unite between Europe with North America to the category Developed Continents
newDataset <- dataset[,c(2,3,4,5,6,9,10,11)] #sub table without the relevant variables
for(i in 1:101){
  if(newDataset[i, 7] == "5")
    newDataset[i, 7] = "4" 
}

# define Continent and Member of OECD as categorical variables and therefore are adequate to become the dummy variables
Continent_dummy <- factor(newDataset$Continent)
Continent_dummy <- relevel(Continent_dummy, ref=c(1))
OECD_dummy <- factor(newDataset$Member.of.OECD)
OECD_dummy <- relevel(OECD_dummy, ref=c(1))

#-Intraction Continentions

# ---------Intraction Continent - Outdoor.air.pollution
max(newDataset$Outdoor.air.pollution....) #max 
min(newDataset$Outdoor.air.pollution....) #min 

check1 <- lm(formula = Life_expectancy ~ Outdoor.air.pollution.... * factor(newDataset$Continent=='1'), data = newDataset)
check2 <- lm(formula = Life_expectancy ~ Outdoor.air.pollution.... * factor(newDataset$Continent=='2'), data = newDataset)
check3 <- lm(formula = Life_expectancy ~ Outdoor.air.pollution.... * factor(newDataset$Continent=='3'), data = newDataset)
check4 <- lm(formula = Life_expectancy ~ Outdoor.air.pollution.... * factor(newDataset$Continent=='4'), data = newDataset)

plot(newDataset$Outdoor.air.pollution....[newDataset$Continent=='1'],newDataset$Life_expectancy[newDataset$Continent=='1'],
     col="blue",xlab="Air Pollution",ylab="Life Expectancy",main="Air Pollution vs Life Expectancy",xlim=c(11.00,100.00),ylim=c(50.90,81.20))

points(newDataset$Outdoor.air.pollution[newDataset$Continent=='2'],newDataset$Life_expectancy[newDataset$Continent=='2'],
       col="green")
points(newDataset$Outdoor.air.pollution[newDataset$Continent=='3'],newDataset$Life_expectancy[newDataset$Continent=='3'],
       col="PURPLE")
points(newDataset$Outdoor.air.pollution[newDataset$Continent=='4'],newDataset$Life_expectancy[newDataset$Continent=='4'],
       col="darkturquoise")
legend(90,83,legend=c("1","2","3","4"),col=c("blue","green","PURPLE","darkturquoise"),pch=c(0.5,0.5,0.5,0.5),bty="n")

abline(check1,col="blue", lwd=2)
abline(check2,col="green", lwd=2)
abline(check3,col="PURPLE", lwd=2)
abline(check4,col="darkturquoise", lwd=2)


# ---------Intraction Continent - Average.income.per.person- not good
max(newDataset$Average.income.per.person....) #max 
min(newDataset$Average.income.per.person....) #min 

check1 <- lm(formula = Life_expectancy ~ Average.income.per.person.... * factor(newDataset$Continent=='1'), data = newDataset)
check2 <- lm(formula = Life_expectancy ~ Average.income.per.person.... * factor(newDataset$Continent=='2'), data = newDataset)
check3 <- lm(formula = Life_expectancy ~ Average.income.per.person.... * factor(newDataset$Continent=='3'), data = newDataset)
check4 <- lm(formula = Life_expectancy ~ Average.income.per.person.... * factor(newDataset$Continent=='4'), data = newDataset)

plot(newDataset$Average.income.per.person....[newDataset$Continent=='1'],newDataset$Life_expectancy[newDataset$Continent=='1'],
     col="blue",xlab="Average Income Per Person",ylab="Life Expectancy",main="Average Income vs Life Expectancy", xlim=c(627,67700),ylim=c(50.90,81.20))

points(newDataset$Average.income.per.person....[newDataset$Continent=='2'],newDataset$Life_expectancy[newDataset$Continent=='2'],
       col="green")
points(newDataset$Average.income.per.person....[newDataset$Continent=='3'],newDataset$Life_expectancy[newDataset$Continent=='3'],
       col="PURPLE")
points(newDataset$Average.income.per.person....[newDataset$Continent=='4'],newDataset$Life_expectancy[newDataset$Continent=='4'],
       col="darkturquoise")
legend(60000,81.20,legend=c("1","2","3","4"),col=c("blue","green","PURPLE","darkturquoise"),pch=c(0.5,0.5,0.5,0.5),bty="n")

abline(check1,col="blue", lwd=2)
abline(check2,col="green", lwd=2)
abline(check3,col="PURPLE", lwd=2)
abline(check4,col="darkturquoise", lwd=2)

# ---------Intraction Continent - malaria
max(newDataset$malaria...Estimated.number.of.people.that.have.been.infected) #max 
min(newDataset$malaria...Estimated.number.of.people.that.have.been.infected) #min 

check1 <- lm(formula = Life_expectancy ~ malaria...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='1'), data = newDataset)
check2 <- lm(formula = Life_expectancy ~ malaria...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='2'), data = newDataset)
check3 <- lm(formula = Life_expectancy ~ malaria...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='3'), data = newDataset)
check4 <- lm(formula = Life_expectancy ~ malaria...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='4'), data = newDataset)

plot(newDataset$malaria...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='1'],newDataset$Life_expectancy[newDataset$Continent=='1'],
     col="blue",xlab="malaria",ylab="Life Expectancy",main="malaria vs Life Expectancy", xlim=c(0,81640),ylim=c(50.90,81.20))

points(newDataset$malaria...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='2'],newDataset$Life_expectancy[newDataset$Continent=='2'],
       col="green")
points(newDataset$malaria...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='3'],newDataset$Life_expectancy[newDataset$Continent=='3'],
       col="PURPLE")
points(newDataset$malaria...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='4'],newDataset$Life_expectancy[newDataset$Continent=='4'],
       col="darkturquoise")
legend(70000,81.20,legend=c("1","2","3","4"),col=c("blue","green","PURPLE","darkturquoise"),pch=c(0.5,0.5,0.5,0.5),bty="n")

abline(check1,col="blue", lwd=2)
abline(check2,col="green", lwd=2)
abline(check3,col="PURPLE", lwd=2)
abline(check4,col="darkturquoise", lwd=2)


# ---------Intraction Continent - HIV
max(newDataset$HIV...Estimated.number.of.people.that.have.been.infected) #max 
min(newDataset$HIV...Estimated.number.of.people.that.have.been.infected) #min 

check1 <- lm(formula = Life_expectancy ~ HIV...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='1'), data = newDataset)
check2 <- lm(formula = Life_expectancy ~ HIV...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='2'), data = newDataset)
check3 <- lm(formula = Life_expectancy ~ HIV...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='3'), data = newDataset)
check4 <- lm(formula = Life_expectancy ~ HIV...Estimated.number.of.people.that.have.been.infected * factor(newDataset$Continent=='4'), data = newDataset)

plot(newDataset$HIV...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='1'],newDataset$Life_expectancy[newDataset$Continent=='1'],
     col="blue",xlab="HIV",ylab="Life Expectancy",main="HIV vs Life Expectancy", xlim=c(200,7700000),ylim=c(50.90,81.20))

points(newDataset$HIV...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='2'],newDataset$Life_expectancy[newDataset$Continent=='2'],
       col="green")
points(newDataset$HIV...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='3'],newDataset$Life_expectancy[newDataset$Continent=='3'],
       col="PURPLE")
points(newDataset$HIV...Estimated.number.of.people.that.have.been.infected[newDataset$Continent=='4'],newDataset$Life_expectancy[newDataset$Continent=='4'],
       col="darkturquoise")
legend(7700000,81.20,legend=c("1","2","3","4"),col=c("blue","green","PURPLE","darkturquoise"),pch=c(0.5,0.5,0.5,0.5),bty="n")

abline(check1,col="blue", lwd=2)
abline(check2,col="green", lwd=2)
abline(check3,col="PURPLE", lwd=2)
abline(check4,col="darkturquoise", lwd=2)

# ---------Intraction Continent - Cigarette Consumption
max(newDataset$Cigarette.consumption....) #max 
min(newDataset$Cigarette.consumption....) #min 

check1 <- lm(formula = Life_expectancy ~ Cigarette.consumption.... * factor(newDataset$Continent=='1'), data = newDataset)
check2 <- lm(formula = Life_expectancy ~ Cigarette.consumption.... * factor(newDataset$Continent=='2'), data = newDataset)
check3 <- lm(formula = Life_expectancy ~ Cigarette.consumption.... * factor(newDataset$Continent=='3'), data = newDataset)
check4 <- lm(formula = Life_expectancy ~ Cigarette.consumption.... * factor(newDataset$Continent=='4'), data = newDataset)

plot(newDataset$Cigarette.consumption....[newDataset$Continent=='1'],newDataset$Life_expectancy[newDataset$Continent=='1'],
     col="blue",xlab="Cigarette Consumption",ylab="Life Expectancy",main="Cigarette Consumption vs Life Expectancy", xlim=c(1.3,39.9),ylim=c(50.90,81.20))

points(newDataset$Cigarette.consumption....[newDataset$Continent=='2'],newDataset$Life_expectancy[newDataset$Continent=='2'],
       col="green")
points(newDataset$Cigarette.consumption....[newDataset$Continent=='3'],newDataset$Life_expectancy[newDataset$Continent=='3'],
       col="PURPLE")
points(newDataset$Cigarette.consumption....[newDataset$Continent=='4'],newDataset$Life_expectancy[newDataset$Continent=='4'],
       col="darkturquoise")
legend(35,83,legend=c("1","2","3","4"),col=c("blue","green","PURPLE","darkturquoise"),pch=c(0.5,0.5,0.5,0.5),bty="n")

abline(check1,col="blue", lwd=2)
abline(check2,col="green", lwd=2)
abline(check3,col="PURPLE", lwd=2)
abline(check4,col="darkturquoise", lwd=2)

# Regression in steps
  # Empty Model
Emp <- lm(newDataset$Life_expectancy ~ 1, data = newDataset)
summary.lm(Emp)
  # Full Model
Full <- lm(formula = newDataset$Life_expectancy  ~ newDataset$Outdoor.air.pollution.... +  newDataset$HIV...Estimated.number.of.people.that.have.been.infected + newDataset$malaria...Estimated.number.of.people.that.have.been.infected + newDataset$Average.income.per.person.... + newDataset$Cigarette.consumption.... + Continent_dummy + OECD_dummy + newDataset$Outdoor.air.pollution....* Continent_dummy + newDataset$malaria...Estimated.number.of.people.that.have.been.infected * Continent_dummy + newDataset$Cigarette.consumption.... * Continent_dummy, data = newDataset)
summary.lm(Full)
  # FORWARD
fwd.model <- step(Emp, direction = 'forward', scope = formula(Full))
  # BACKWARD
bw.model <- step(Full, direction = 'backward', scope = ~ 1)
  # STEPWISE
sw.model <- step(Emp, direction = 'both', scope = formula(Full))


#comparing the chosen model from AIC to the full 
AIC.model <- lm(formula = newDataset$Life_expectancy ~ Continent_dummy + newDataset$Average.income.per.person.... , data = newDataset)
summary.lm(AIC.model)
summary.lm(Full)

#comparing the chosen model from AIC to the full 
BIC(Full)
BIC(AIC.model)

# best model 
bestModel <- AIC.model
coefficients(bestModel)

# Errors & Linear
summary(bestModel)
newDataset$fitted <- fitted(bestModel) #predicted values
newDataset$residuals <- residuals(bestModel) #residuals
s.r_res <- sqrt(var(newDataset$residuals)) #calculating the standard deviation of the errors
newDataset$standardResiduals <- (residuals(bestModel)/s.r_res) #saving the data set after the calculations
plot(newDataset$fitted, newDataset$standardResiduals, xlab = "Predicted Value", ylab = "Normalized Error") 
abline(0, 0)

# Normal 
qqnorm(newDataset$standardResiduals)
abline(a = 0, b = 1)
hist(newDataset$standardResiduals, prob = TRUE, xlab = "Normalized Error", main = "Histogram of Normalized Error")
lines(density(newDataset$standardResiduals), col = "blue", lwd = 2)

#KS Test
ks.test(x = newDataset$standardResiduals, y ="pnorm", alternative = "two.sided", exact = NULL)

#SW Test
shapiro.test(newDataset$standardResiduals)

#GQ Test- full or fwd.model, we need to put hear the final model
gqtest(bestModel)

#Chow Test- full or fwd.model, we need to put hear the final model
sctest(bestModel)

# F test for equal vars
Life_expectancy_vec <- newDataset$Life_expectancy
Life_expectancy_vec_sort <- sort(Life_expectancy_vec) 
third_vec_length <- round (length(Life_expectancy_vec_sort)/3)
third23_vec_length <- round (length(Life_expectancy_vec_sort)*2/3)
third_vec_values <- Life_expectancy_vec_sort[1:third_vec_length] #third first vaues
third23_vec_values <-  Life_expectancy_vec_sort[third23_vec_length:length(Life_expectancy_vec_sort)]#third last values
var.test(x = third_vec_values, y = third23_vec_values, ratio = 1, 
         alternative = c("two.sided"), conf.level = 0.95)

#transformation
library(MASS)
bc <- boxcox(newDataset$Life_expectancy ~ Continent_dummy + newDataset$Average.income.per.person.... , data = newDataset)
lambda <- bc$x[which.max(bc$y)] # Exact lambda
lambda
newForm <- (newDataset$Life_expectancy ^ lambda - 1) / lambda #transformation

# Regression in steps for the model after the transformation
  # Empty Model
Emp <- lm(newForm ~ 1, data = newDataset)
summary.lm(Emp)
  # Full Model
Full <- lm(formula = newForm  ~ newDataset$Outdoor.air.pollution.... +  newDataset$HIV...Estimated.number.of.people.that.have.been.infected + newDataset$malaria...Estimated.number.of.people.that.have.been.infected + newDataset$Average.income.per.person.... + newDataset$Cigarette.consumption.... + Continent_dummy + OECD_dummy + newDataset$Outdoor.air.pollution....* Continent_dummy + newDataset$malaria...Estimated.number.of.people.that.have.been.infected * Continent_dummy + newDataset$Cigarette.consumption.... * Continent_dummy, data = newDataset)
summary.lm(Full)
  # FORWARD
fwd.model <- step(Emp, direction = 'forward', scope = formula(Full))
  # BACKWARD
bw.model <- step(Full, direction = 'backward', scope = ~ 1)
  # STEPWISE
sw.model <- step(Emp, direction = 'both', scope = formula(Full))

#defining the final moodel (after the transformation)
finalModel <- lm(formula = newForm ~ newDataset$Average.income.per.person.... + Continent_dummy, data = newDataset)
summary.lm(finalModel)
coefficients(finalModel)