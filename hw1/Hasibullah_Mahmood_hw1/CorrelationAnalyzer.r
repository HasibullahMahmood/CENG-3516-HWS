# import necessary libraries
library(MASS)
library(plyr)
library(ggplot2)

# preprocess data
mydata <- birthwt
colnames(mydata) <-
  c(
    "birthwt.below.2500",
    "mother.age",
    "mother.weight",
    "race",
    "mother.smokes",
    "previous.prem.labor",
    "hypertension",
    "uterine.irr",
    "physician.visits",
    "birthwt.grams"
  )
mydata <- transform(
  mydata,
  race = as.factor(mapvalues(
    race, c(1, 2, 3), c("white", "black", "other")
  )),
  mother.smokes = as.factor(mapvalues(mother.smokes, c(0, 1), c("no", "yes"))),
  hypertension = as.factor(mapvalues(hypertension, c(0, 1), c("no", "yes"))),
  uterine.irr = as.factor(mapvalues(uterine.irr, c(0, 1), c("no", "yes"))),
  birthwt.below.2500 = as.factor(mapvalues(
    birthwt.below.2500, c(0, 1), c("no", "yes")
  ))
)

# define function
correlation.analyzer <- function(data, x, y) {
  print(
    ggplot(data = data, aes(x = x, y = y)) +
      geom_point(color = 'blue') +
      geom_smooth(
        method = lm,
        se
        = FALSE,
        fullrange = TRUE,
        col = "#C42126"
      )
  )
  
  cor.val <- cor(x, y, method = 'pearson')
  if (cor.val > 0.7) {
    result <- "Strong Positive Relationship"
  } else if (cor.val > 0.3) {
    result <- "Moderate Positive Relationship"
  } else if (cor.val > 0) {
    result <- "Weak Positive Relationship"
  } else if (cor.val > -0.3) {
    result <- "Weak Negative Relationship"
  } else if (cor.val > -0.7) {
    result <- "Moderate Negative Relationship"
  } else if (cor.val >= -1) {
    result <- "Strong Negative Relationship"
  }
  return(cat(
    paste(
      " The Correlation Coefficient: ",
      cor.val,
      "\n",
      "Evaluation Result: ",
      result,
      "\n",
      sep = " "
    )
  ))
}


correlation.analyzer(mydata, mydata$mother.age, mydata$mother.weight)

# second example
# data("cars")
# correlation.analyzer(cars, cars$speed, cars$dist)
