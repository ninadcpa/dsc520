celcius <- c(10,-5,15,-20,25,35,-40)
farenheit <- c(50,23,59,-4,77,95,-40)
df <- data.frame(celcius, farenheit)
head(df)

temp_lm <-  lm(df$farenheit ~ df$celcius)
summary(temp_lm)
str(temp_lm)