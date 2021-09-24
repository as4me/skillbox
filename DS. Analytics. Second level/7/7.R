parf <- read.csv(file.choose())
dat <- parf[parf$vote != 2,]
test <- dat[dat$group == 'test','vote']
control <- dat[dat$group == 'control','vote']

barplot(table(dat$vote, dat$group), 
        beside=TRUE, 
        legend.text=TRUE, 
        col=c('green', 'blue'))

p_test <- sum(test) / length(test)
p_control <- sum(control) / length(control)
mean_number <- p_test - p_control



N <- 1000
set.seed(123)
differences <- rep(NA, N)
for(i in 1:N){
  s1 <- sample(control,replace=TRUE) 
  s2 <- sample(test,replace=TRUE) 
  p1 <- sum(s1)/length(s1) 
  p2 <- sum(s2)/length(s2)
  p_diff <- p2 - p1 
  differences[i] <- p_diff 
}
differences_cent = differences - mean(differences)
alpha <- 0.05
p_value <- sum(differences_cent>mean_number) / N

