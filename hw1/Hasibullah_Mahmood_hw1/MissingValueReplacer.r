# import data
mydata <- read.csv("data.csv", header = T)

# print data
mydata

# Implement function1
replace.na.with.mean <- function(data){
  for(i in 1:ncol(data)){
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  return(data)
}

# Implement function2
replace.na.with.mean2 <- function(data){
  ifelse(is.na(data), ave(data, FUN = function(x) mean(data, na.rm = T)), data)
}

# run function1
replace.na.with.mean(mydata)

# run function2
replace.na.with.mean2(mydata$age)
