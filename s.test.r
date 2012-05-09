# source(s.test.r")

pboot <- function(lmer.input.null ,lmer.input.alt) {
  l.sim <- simulate(lmer.input.null)
  lmer.null.temp <- refit(lmer.input.null, l.sim)
  lmer.alt.temp <- refit(lmer.input.alt, l.sim)
  d.lr.stats <- as.numeric(2*(logLik(lmer.alt.temp)-logLik(lmer.null.temp)))
  d.fixef.stats <- coef(summary(lmer.alt.temp))[2, 1]
  d.t.stats <- coef(summary(lmer.alt.temp))[2, 3]
  df.output <- data.frame(d.lr.stats, d.fixef.stats, d.t.stats)
}

sleepstudy_PB <- replicate(2,pboot(fm2,fm1))
rownames(sleepstudy_PB) <- c("lr", "fixef", "tval")
print(sleepstudy_PB)