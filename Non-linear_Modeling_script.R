# Packages contain our data
library(ISLR2)
attach(Wage)

# Polynomial Regression

# Let's fit our model using a 4th degree polynomial using the poly() command. 
#In our output, each row corresponds to a linear contribution of age^x.
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# We can also use poly() to obtain age^x directly by using raw=TRUE

fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit2))

# There is no significant difference between these two models

# There are several equivalent ways of fitting the model

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(wage^4),
            data = Wage)
coef(fit2a)

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4),
            data = Wage)
coef(fit2b)

# We now create a grid of values for age at which we want predictions, and then 
# call the predict() function, specifying that we also want std

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid),
                 se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit,
                  preds$fit - 2 * preds$se.fit)
# Finally, we plot the data and add the fit from the degree-4 polynomial

# These control the margins of the plot
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age,wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lyt = 3)

# The fitted values obtained using the orthogonal functions does not affect the model meaningfully

preds2 <- predict(fit2, newdata = list(age = age.grid),
                  se = TRUE)
max(abs(preds$fit - preds2$fit))

# When performing polynomial regression we must decide on the degree of the polynomial to use.
# We can do this by finding the simplest factor which significantly explains our model

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Here, it looks like either a cubic or quartic polynomial provies the most reasonable fit

# We can also obtain these p-values using coef()

coef(summary(fit.5))

# Notice that the p-values are the same, and in fact the square of the t-statistics 
# are equal to the F-statistics from the anova() function

# We can use the anova() function to compare 3 models
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# Step functions

# To fit a step function, we use cut()
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# cut() auto picked the cutpoints at 33.5, 49, and 64,5 ~ years of age
# We could also have manually specified our cutpoints using the breaks argument
# cut() returns an ordered categorical variable, then lm() creates dummy variables
# for use in the regression. The age < 33.5 category is left out, as it corresponds 
# to our intercept

# Splines

library(splines)

# the bs() function generates the entire matrix of functions for splines with the specified set of knots
# By default, cubic splines are fitted

fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

# Here we have prespecified knots at ages 25, 40, 60. This produces a spline with six basic functions

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# In this case R chooses knots at ages 33.8,42.0, and 51.0, which correspond to the 25th, 50th, and 75th percentiles of age
# In order to fit a natural spline, we use the ns() function

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
                 se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, col = "red", lwd = 2)

# As with the bs() function, we could instead specify the knots directly using the knots option
# In order to fit a smoothing spline, we use the smooth.spline() function.

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Generalised Additive Models

# We now fit a GAM to predict wage using natural spline functions on lyear and age,
# treating education as a qualitative predictor (7.16)
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education,
           data = Wage)

# We now fit the model using smoothing splines rather than natural splines

library(gam)

# The s() function is used to indicate that we would like to use a smoothing spline
# The function of lyear should have 4 df, and age will have 5 df.
# We use the gam() function to fit our gam model using these components

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
              data = Wage)

par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

# We have to use plotGam()
plot.Gam(gam1, se = TRUE, col = "red")

# We can perform anova() to determine which model is best

gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education,
              data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# It looks like there is compelling evidence that a GAM with a linear function of lyear
# is better than a GAM model that does not include lyear at all. Model 2 is preferred

summary(gam.m3)

# The Anova for Parametric Effects p-values clearly demonstrate that year, age, and education 
# are all highly statistically significant, even when only assuming a linear relationship.

# We can make predictions using the predict() method for the class Gam. 
preds <- predict(gam.m2, newdata = Wage)
