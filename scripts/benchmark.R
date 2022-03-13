library("microbenchmark")
library("brokenstick")

lm1 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = 0:3), times = 10)
lm2 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.5)), times = 10)
lm3 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25)), times = 10)
fit_lm <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25))

control <- control_brokenstick(method = "kr")
kr1 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = 0:3, control = control), times = 10)
kr2 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.5), control = control), times = 10)
kr3 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control), times = 10)
kr4 <- microbenchmark(brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.1), control = control), times = 10)

control <-  control_brokenstick(method = "kr", kr = list(runin = 100L, ndraw = 50L, par_skip = 10L))
Rprof("test.out")
fit <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)
Rprof(NULL)
summaryRprof("test.out")


# better matrix inverse?
library("microbenchmark")
data <- matrix(c(1, 0, 0, 0,
                 0.5, 0.5, 0, 0,
                 0, 0.5, 0.5, 0,
                 0, 0, 0.5, 0.5,
                 0, 0, 0, 1),
               nrow = 5, byrow = TRUE)
vv <- crossprod(data)

i1 <- solve(vv)
i2 <- chol2inv(chol(vv))
all.equal(i1, i2)

microbenchmark(solve(vv), times = 1000)
microbenchmark(solve.default(vv), times = 1000)
microbenchmark(chol2inv(chol(vv)), times = 1000)
microbenchmark(chol2inv(chol.default(vv)), times = 1000)
# chol2inv(chol(vv) is about twice as fast as solve()

# Using sparseMatrix
library("Matrix")
# NOTE: side effect: loading Matrix made chol2inv(chol(vv)) slower
microbenchmark(solve(vv), times = 1000)
microbenchmark(chol2inv(chol(vv)), times = 1000)
microbenchmark(chol2inv(chol.default(vv)), times = 1000)

sp <- as(band(vv, -1, 1), "sparseMatrix")

i1 <- solve(sp, sparse = TRUE)
i2 <- chol2inv(chol(sp))
all.equal(as.matrix(i1), as.matrix(i2))

microbenchmark(solve(sp), times = 1000)
microbenchmark(chol2inv(chol.default(sp)), times = 1000)
# now solve() is faster, but both are much slower than before

# How about a larger sparse matrix?
large <- splines::bs(1:100, degree = 1, knots = seq(0, 99, 3))
vv <- crossprod(large)

# conclusion: sparseMatrix has no speed benefits
# fastest solution: cholinv(chol.default(vv))
