vimCox.oneprime <-
function (oneprime, cl, oob, inbagg) 
  {
    x <- numeric(length(oob))
    coef <- 1
    mat.model <- data.frame(cl = cl[oob], x)
    ll.null <- predPL(coef, mat.model)
    x <- oneprime
    coef <- coxph(cl[inbagg] ~ x[inbagg], ties = "breslow")$coefficients
    mat.model <- data.frame(cl = cl[oob], x[oob])
    ll.prime <- predPL(coef, mat.model)
    -2 * (ll.null - ll.prime)
  }
