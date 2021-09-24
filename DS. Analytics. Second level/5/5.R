install.packages("DescTools")

library(DescTools)
tit <- read.csv("Documents/GitHub/skillbox/DS. Analytics. Second level/5/experiment.csv")
table(tit)
test <- tit$test
control <- tit$control
MeanCI(test)
MeanCI(control)
# вывод
# разница между новым и старым дизайном есть. Интервалы не пересекаются. Новый дизайн лучше