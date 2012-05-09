#### function to compute Pearson r and CI using bootstrap
# function to compute Pearson correlation
corr <- function(dat, ind) {
  if (!(is.matrix( dat) &&
  ncol(dat) == 2 &&
  length(ind)== nrow(dat) ))
  {
    stop("invalid arguments")
  }
  cor.test(dat[ind, 1], dat[ind, 2], method="pearson")$estimate
}

boot.corr <- function (sample, r, replication = 2000, alpha = 0.05) {
  boot.out <- boot(as.matrix(sample), corr, replication)
  boot.ci.out <- boot.ci(boot.out, conf=1-alpha, type = "perc")
  r_unbiased <- 2 * boot.out$t0 - mean(boot.out$t)
  r_ci_width <- boot.ci.out$p[1,5] - boot.ci.out$p[1,4]
  if(r < boot.ci.out$p[1,4] || r > boot.ci.out$p[1,5])
  r_coverage <- 0
  else r_coverage <- 1
  list(r = r_unbiased, width = r_ci_width, coverage = r_coverage)
}
