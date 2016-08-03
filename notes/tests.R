

predict(exp, x = c(NA,1), y = c(10, 10))
#Error in solve.default(Z %*% export$omega %*% t(Z) + 
# diag(export$sigma2,  :  system is computation#ally singular: reciprocal condition number = 0

predict(exp, x = c(NA,1), y = c(10, 12), filter_na = TRUE)
#Error in solve.default(Z %*% export$omega %*% t(Z) + 
# diag(export$sigma2,  :  system is computation#ally singular: reciprocal condition number = 0


predict(exp, x = NA, y = 10)
#Error in splineDesign(Aknots, x, ord) : 
#  length of 'derivs' is larger than length of 'x'

predict(exp, x = NA, y = 10, filter_na = TRUE)
