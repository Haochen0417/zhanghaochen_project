library("mplot")
library("tidyverse")

abalone = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                   col.names = c("Sex", "Length", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight", "Rings"))
abalone = abalone %>% mutate(
    Sex = case_when(
        Sex == "M" ~ "Male",
        Sex == "F"~ "Female",
        Sex == "I" ~ "Infant"
    ),
    Age = Rings + 1.5
)
abalone1 = abalone %>% filter(WholeWeight > ShuckedWeight, WholeWeight > ShellWeight, Height > 0) %>% select(-Rings, -Sex, -WholeWeight)
abalone2 = abalone %>% filter(WholeWeight > ShuckedWeight, WholeWeight > ShellWeight, Height > 0) %>% select(-Rings, -Sex, -ShuckedWeight, -VisceraWeight, -ShellWeight)

abalone.fullallweight.lm = lm(Age ~ ., data = abalone1)
abalone.fullwholeweight.lm = lm(Age ~ ., data = abalone2)
abalone.best1.lm = lm(Age ~ Diameter	+ Height	+ ShuckedWeight+	ShellWeight, data = abalone1)
abalone.best2.lm = lm(Age ~ Diameter + Height , data = abalone2)

abalone.full.vis = vis(abalone.full.lm, B = 100)

abalone.best1.af = af(abalone.best1.lm, B = 80, n.c = 100, c.max = 100)
abalone.best2.af = af(abalone.best2.lm, B = 80, n.c = 100, c.max = 100)
abalone.full1.af = af(abalone.fullallweight.lm, B = 80, n.c = 100, c.max = 100)
abalone.full2.af = af(abalone.fullwholeweight.lm, B = 80, n.c = 100, c.max = 100)

save(abalone.full.vis, abalone.best1.af, abalone.best2.af, abalone.full1.af,abalone.full2.af, file="abaloneStability.rda")
