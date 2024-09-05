install.packages("haven")
install.packages("plm")
install.packages("lmtest")
install.packages("ggplot2")

library(haven)
library(plm)
library(lmtest)
library("ggplot2")

# Read the Stata file using 'read_dta' function
df <- haven::read_dta("Downloads/prodfn_data_1_1.dta")



# Summarizing the dataset
summary(df)

# Checking the completeness of the data
sum(is.na(df)) #0 missing data

# Data exploration

# Histogram of output(the dependent variable)
hist(df$y, main = "Histogram of ln(output)")

# Density plots of labor and capital
plot(density(df$l), main = "Density Plot of ln(labor)")
plot(density(df$k), main = "Density Plot of ln(capital)")

# Scatter plot of output with respect to labor and capital
ggplot(mapping=aes(x = df$k, y = df$y)) + geom_point() + stat_smooth(method="lm")
ggplot(mapping=aes(x = df$l, y = df$y)) + geom_point() + stat_smooth(method="lm")

# The Fixed Effects (FE) model
pdata = pdata.frame(df, index = c("ivar", "tvar"))


model_fe <- plm(y ~ l + k, data = df, model = "within")
summary(model_fe)

# The Random Effects (RE) model
model_re <- plm(y ~ l + k, data = df, model = "random")
summary(model_re)

# The Hausman test
phtest(model_fe, model_re)
# H0 is not rejected, which indicates that the RE estimator is efficient and consistent.

#-Breusch-Pagan LM test for heteroscedasticity
bptest(model_re, data = pdata)

# Wooldridge test for serial correlation
pbgtest(model_re, order = 1)