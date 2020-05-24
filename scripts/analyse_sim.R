# analyze simulation results
res <- read.table(file = "scripts/simulate_1_result.txt",
                  header = TRUE, sep = "\t")
res

fit1 <- lm(result ~ ndraw + par_skip, data = res)
summary(fit1)

fit2 <- lm(result ~ ndraw + par_skip + I(ndraw - 100) * I(par_skip - 10), data = res)
summary(fit2)

fit3 <- lm(result ~ ndraw + par_skip + I(ndraw * par_skip), data = res)
summary(fit3)


res <- read.table(file = "scripts/simulate_2_result.txt",
                  header = TRUE, sep = "\t")
res
