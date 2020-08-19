library(MASS)
library(plyr)
library(ggplot2)

students <- read.csv("students.csv")

# Transform variables to factors with descriptive levels
students <- transform(students,
                     online.tutorial = as.factor(mapvalues(online.tutorial,
                                                         c(0,1), c("no", "yes"))),
                     graduated = as.factor(mapvalues(graduated,
                                                        c(0,1), c("no", "yes")))
                     )

males <- subset(students, gender=="Male")
females <- subset(students, gender=="Female")

x <- t.test(males$nc.score)$conf.int
students.nc.score <- data.frame(gender="Male",
                                scoreType="nc.score", 
                                mean=mean(males$nc.score), 
                                lower=x[1], 
                                upper=x[2])

y <- t.test(females$nc.score)$conf.int
students.nc.score <- rbind(students.nc.score, data.frame(gender="Female",
                                scoreType="nc.score", 
                                mean=mean(females$nc.score), 
                                lower=y[1], 
                                upper=y[2]))


ggplot(students.nc.score, aes(x=scoreType, y=mean, fill=gender)) +
  geom_bar(position="dodge", stat="identity", width = 0.3) +
  scale_y_discrete(limit=c(seq(1, 2.5))) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.1, # Width of the error bars
                position=position_dodge(0.3))



aggregate(salary ~ gender,
          data = sdata,
          FUN = function(x) {c(mean = mean(x), sd = sd(x))})







# Implement function
missingValueReplacer <- function(vector, trim=0.10){
  temp <- subset(vector, !is.na(vector))
  temp <- sort(temp)
  len <- length(temp)
  start <- round(len * trim + 1)
  end <- len - (start - 1)
  s <- 0
  for (i in start:end) {
    s <- s + temp[i]
  }
  average = s/(end-start + 1)
  
  for(i in 1:length(vector)) {
    if(is.na(vector[i])){
      vector[i] <- average
    }
  }
  
  vector
}



temp <- students$score1
boxplot(temp)

temp <- missingValueReplacer(temp)
boxplot(temp)


count(is.na(students$score2))
