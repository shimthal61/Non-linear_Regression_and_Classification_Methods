# The stock market data

# We'll use the Smarket data from ISLR2
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

# The cor() function produces a matrix that contrain all the pairwise comparisons
cor(Smarket)
# We get an error because direction is qualitative

cor(Smarket[, -9])

# The only substantial correlation is between Year and volume

attach(Smarket)
plot(Volume)

# Logistic Regression

# We can fit a logistic regression using the glm() function

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial)
summary(glm.fits)

# The smallest p-value here is Lag1, although it is still fairly large
# The negative predictor suggests that if the market had a positive return, is it less likely
# to go up today (although there is no clear relationship)

# We use the coef() function in order to access just the coefficients for this fitted model

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

# The predict function can be used to predict the probability that the market will go up

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)

# In order to predict whether the market will go up or down, we must convert these predicted
# probabilites into class labels, up or down. 

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)

(507 + 145) / 1250

mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
