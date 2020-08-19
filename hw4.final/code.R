df <- read.csv("data.csv")
summary(df$Score)
df$Dogum.Tarihi <- as.Date(df$Dogum.Tarihi, "%m/%d/%Y")
df$age <- format(df$Dogum.Tarihi, "%Y")
