# source("f.lmer.multi.variable.model.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.multi.variable.model <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  # Subsets data and removes potential outliers. For this subset to affect the output, the other subset command below needs to be commented out.
#  df.temp <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & (i.sample.number.2 == 2 | i.sample.number.2 == 3)
#    | (c.id == "mp" & i.sample.number.2 == 4))
 
#  df.temp <- subset(df.temp, c.two.adult.samples == "yes" | c.three.adult.samples == "yes")
#  df.temp <- subset(df.temp, !(c.id == "wnd"))
#  This gets all adult females, including the potential outlier MP. WND sample 1 is also included even though there is no rank for that sample
#  df.temp <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
#  df.temp <- subset(df.data.flat, c.sex == "f")
  df.temp <- df.data.flat
#  df.temp <- subset(df.data.flat, c.sex == "f")
#  df.temp <- subset(df.data.flat, c.age == "adult")
#  c.data.set.name <- "full.set"
  c.data.set.name <- "full.set"
#  c.data.set.name <- "gol.mp.removed"
#  c.data.set.name <- "full.female.male.cr.gol.mp.removed"

#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp <- subset(df.temp, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp <- subset(df.temp, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp$d.age.months <- log(df.temp$d.age.months)  # Use log transformed d.age.months

  print(length(df.temp$c.id))
  print(length(unique(df.temp$c.id)))
  print(length(unique(df.temp$i.rank)))  

  c.independent <- "c.sex"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
#  cv.independent.variables.short <- c("i.rank")                # comment this line out to run the full set of variables.
  cv.immune.short <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm", 
    "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm")
  cv.colnames <- c("c.data.set.name", "c.dependent", "c.independent", "c.boot.type", 
    "chi.square.pval", "cftest.pval", 
    "d.lr.pval", "d.fixef.pval", "d.t.test.pval", 
    "d.lr.test.stats", "d.fixef.test.stats", "d.t.test.stats", 
    "d.lr.std.err", "d.fixef.std.err", "d.t.test.std.err",
    "r", "r.squared", "corr.of.fixed.effects[2,1]")

  i.fixed.effect.levels <- 1          # use this constant to set the number of rows needed in the output data frame
  m.output.temp <- matrix(nrow = (3 * i.fixed.effect.levels), ncol = length(cv.colnames))
  colnames(m.output.temp) <- cv.colnames
  df.output.temp <- m.output.temp       # create the data frame for storing results while looping through dependend and independent variables

  i.replicates <- 1
  i.counter <- 1
  i.counter.stop <- length(cv.immune.short) 
  for(i.counter in i.counter:i.counter.stop)  {

    d.independent <- df.temp[[c.independent]]   
    c.dependent <- cv.immune.short[i.counter]
    d.dependent <- df.temp[[c.dependent]]

    print(paste(" **************** c.independent: ", c.independent, "**********************"))   
    print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
  
    lmer.null <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, REML = FALSE)
    lmer.alt.1 <- lmer(d.dependent ~ c.sex + (1 | c.id), data = df.temp, REML = FALSE)
    lmer.alt.2 <- lmer(d.dependent ~ c.age + (1 | c.id), data = df.temp, REML = FALSE)
    lmer.alt.3 <- lmer(d.dependent ~ c.sex * c.age + (1 | c.id), data = df.temp, REML = FALSE)

  
    print("summary(lmer.null))") 
    print(summary(lmer.null))
    print("summary(lmer.alt.1))") 
    print(summary(lmer.alt.1))
    print("summary(lmer.alt.2))") 
    print(summary(lmer.alt.2))
    print("summary(lmer.alt.3))") 
    print(summary(lmer.alt.3))

    # Computing a pval for the summary lmer.alt.1 model -> results are the same as cftest.  cftest reports at z value and z test p value because we are using a normal distribution to obtain the p value
#    tval <- attributes(summary(lmer.alt.1))$coef[, 3]
#    pval <- 2 * pnorm(abs(tval), lower = FALSE)
#    print(cbind(attributes(summary(lmer.alt.1))$coef, `p value` = signif(pval, 5)))

    print("cftest(lmer.alt.1)")
    print(cftest(lmer.alt.1))     # computes p-vals for the lmer
    print("cftest(lmer.alt.2)")
    print(cftest(lmer.alt.2))     # computes p-vals for the lmer
    print("cftest(lmer.alt.3)")
    print(cftest(lmer.alt.3))     # computes p-vals for the lmer

  #      print(confint(cftest(lmer.alt.1)))     # computes p-vals for the lmer
    # ANOVA of models
    print("anova(lmer.null, lmer.alt.1, lmer.alt.2, lmer.alt.3, REML = FALSE)")
    print(anova(lmer.null, lmer.alt.1, lmer.alt.2, lmer.alt.3, REML = FALSE))
    
    # Compute the LR test stat for comparing to the bootstrap results
    d.lr.test.stats <- signif(as.numeric(2*(logLik(lmer.alt.1, REML = FALSE)-logLik(lmer.null, REML = FALSE)), digits = 5))
    # Store the independent variable effect sizes for comparing to the bootstrap results
    d.fixef.test.stats <- signif(coef(summary(lmer.alt.1))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
    # Store the t-stat for the independent for comparing to the bootstrap results
    d.t.test.stats <- signif(coef(summary(lmer.alt.1))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
    print(paste("number of rows in fixef: ", length(coef(summary(lmer.alt.1))[,1])))
    print(paste("number of columns in fixef: ", length(coef(summary(lmer.alt.1))[1,])))

# print(str(lmer.alt.1))
# print(attr(terms(lmer.alt.1), "term.labels")[1])
# print(attr(terms(lmer.alt.1), "variables"))
# print(attr(terms(lmer.alt.1), "variables")[[1]])     # prints "list"
# print(attr(terms(lmer.alt.1), "variables")[[2]])     # prints "d.dependent"
# print(attr(terms(lmer.alt.1), "variables")[[3]])     # prints "c.sex" or whatever the first independent variable is
# print(attr(terms(lmer.alt.1), "dimnames"))           # prints NULL
# print(attr(terms(lmer.alt.1), "factors"))            # prints a matrix
# print(attr(attr(terms(lmer.alt.1), "dataClasses"), "names")[1])            # prints "d.dependent"
# print(attr(attr(terms(lmer.alt.1), "dataClasses"), "names")[2])            # prints "c.sex" or whatever the first independent variable is

# print(dimnames(lmer.alt.1@frame[1])[[2]])                          # prints the name of the dependent variable
# print(dimnames(lmer.alt.1@frame[2])[[2]])                          # prints the name of independent variable 1
# print(dimnames(lmer.alt.1@frame[3])[[2]])                          # prints the name of independent variable 2 or the random variable depending on how many independent variables are in the model

    par(mfrow = c(3,7))

    # plotting lmer model 2
    plot(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[,2], 
      main = paste((dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]]), "\ncftest pval: ", 
        signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
        "\nanova pval: ", 
        signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 4), 
        sep = ""), 
      xlab = c.independent, ylab = c.dependent)
    text(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[,2], labels = paste(df.temp$c.id, df.temp$i.sample.number.2), pos = 4, cex = 1)
#    abline(a = (fixef(lmer.alt.1))[[1]], b = (fixef(lmer.alt.1))[[2]])    
#    abline(a = (fixef(lmer.alt.1))[["(Intercept)"]], b = (fixef(lmer.alt.1))[["d.independent"]])    
    hist(resid(lmer.alt.1), main = paste("Histogram of residuals - ", (dimnames(lmer.alt.1@frame[1])[[2]]), 
      "\nr.squared: ", signif(sqrt((vcov(lmer.alt.2)@factors$Cholesky)[1,1]), digits = 5))) 
    lines(density(resid(lmer.alt.1)))                                     # 
    qqnorm(resid(lmer.alt.1), main = paste("qqnorm - ", (dimnames(lmer.alt.1@frame[2])[[2]])))
    qqline(resid(lmer.alt.1))
    plot(fitted(lmer.alt.1), resid(lmer.alt.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")
    text(fitted(lmer.alt.1), resid(lmer.alt.1), labels = df.temp$c.id)      # use this to label the points in the plot
    abline(0,0)
    plot(resid(lmer.alt.1) ~ lmer.alt.1@frame[,2], main = paste((dimnames(lmer.alt.1@frame[2])[[2]]), ", Values x Residuals"))  
    abline(0,0)
    plot(1,1)
    plot(1,1)

    # plotting lmer model 2
    plot(lmer.alt.2@frame[,1] ~ lmer.alt.2@frame[,2], 
      main = paste((dimnames(lmer.alt.2@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.2@frame[2])[[2]]), "\ncftest pval: ", 
        signif(((cftest(lmer.alt.2))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
        "\nanova pval: ", 
        signif((anova(lmer.null, lmer.alt.2))$"Pr(>Chisq)"[2], digits = 4), 
        sep = ""), 
      xlab = c.independent, ylab = c.dependent)
    text(lmer.alt.2@frame[,1] ~ lmer.alt.2@frame[,2], labels = paste(df.temp$c.id, df.temp$i.sample.number.2), pos = 4, cex = 1)
#    abline(a = (fixef(lmer.alt.2))[[1]], b = (fixef(lmer.alt.2))[[2]])    
#    abline(a = (fixef(lmer.alt.2))[["(Intercept)"]], b = (fixef(lmer.alt.2))[["d.independent"]])    
    hist(resid(lmer.alt.2), main = paste("Histogram of residuals - ", (dimnames(lmer.alt.2@frame[1])[[2]]), 
      "\nr.squared: ", signif(sqrt((vcov(lmer.alt.2)@factors$Cholesky)[1,1]), digits = 5))) 
    lines(density(resid(lmer.alt.2)))                                     # 
    qqnorm(resid(lmer.alt.2), main = paste("qqnorm - ", (dimnames(lmer.alt.2@frame[2])[[2]])))
    qqline(resid(lmer.alt.2))
    plot(fitted(lmer.alt.2), resid(lmer.alt.2), main = "fitted x resid", xlab = "fitted", ylab = "resid")
    text(fitted(lmer.alt.2), resid(lmer.alt.2), labels = df.temp$c.id)      # use this to label the points in the plot
    abline(0,0)
    plot(resid(lmer.alt.2) ~ lmer.alt.2@frame[,2], main = paste((dimnames(lmer.alt.2@frame[2])[[2]]), ", Values x Residuals"))  
    abline(0,0)
    plot(1,1)
    plot(1,1)

print(dimnames(lmer.alt.3@frame))

    # plotting lmer model 3
    plot(lmer.alt.3@frame[,1] ~ lmer.alt.3@frame[,2] * lmer.alt.3@frame[,3], 
#      main = paste((dimnames(lmer.alt.3@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.3@frame[2])[[2]]), "\ncftest pval: ", 
#        signif(((cftest(lmer.alt.3))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
#        "\nanova pval: ", 
#        signif((anova(lmer.null, lmer.alt.3))$"Pr(>Chisq)"[2], digits = 4), 
#        sep = ""), 
#      xlab = c.independent, ylab = c.dependent)
)
#    text(lmer.alt.3@frame[,1] ~ lmer.alt.3@frame[,2], labels = paste(df.temp$c.id, df.temp$i.sample.number.2), pos = 4, cex = 1)
#    abline(a = (fixef(lmer.alt.3))[[1]], b = (fixef(lmer.alt.3))[[2]])    
#    abline(a = (fixef(lmer.alt.3))[["(Intercept)"]], b = (fixef(lmer.alt.3))[["d.independent"]])    
    hist(resid(lmer.alt.3), main = paste("Histogram of residuals - ", (dimnames(lmer.alt.3@frame[1])[[2]]), 
      "\nr.squared: ", signif(sqrt((vcov(lmer.alt.2)@factors$Cholesky)[1,1]), digits = 5))) 
    lines(density(resid(lmer.alt.3)))                                     # 
    qqnorm(resid(lmer.alt.3), 
#       main = paste("qqnorm, ", (dimnames(lmer.alt.3@frame[1])[[2]])), " ~ ", 
#      (dimnames(lmer.alt.3@frame[2])[[2]]), " * ", (dimnames(lmer.alt.3@frame[3])[[2]])
)
    qqline(resid(lmer.alt.3))
    plot(fitted(lmer.alt.3), resid(lmer.alt.3), main = "fitted x resid", xlab = "fitted", ylab = "resid")
    text(fitted(lmer.alt.3), resid(lmer.alt.3), labels = df.temp$c.id)      # use this to label the points in the plot
    abline(0,0)
    plot(resid(lmer.alt.3) ~ lmer.alt.3@frame[,2] * lmer.alt.3@frame[,3], main = "Values x Residuals")  
    abline(0,0)

      ################### parametric bootstrapping using simulation based on null model ###################################
  ######################################################################################
    par(mfrow = c(3,3))
  
    c.boot.type <- "boot.parametric"
    i.output.row <- 1
    d.lr.stats <- numeric(i.replicates)
    d.fixef.stats  <- numeric(i.replicates)
    d.t.stats <- numeric(i.replicates)
    for(i in 1:i.replicates)  {
      d.simulated.response <- unlist(simulate(lmer.null))
  #        d.simulated.response <- sample(d.dependent, size = length(d.dependent), replace = TRUE)
      lmer.null.temp <- lmer(d.simulated.response ~ 1 + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      lmer.alt.1.temp <- lmer(d.simulated.response ~ d.independent + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      d.lr.stats[i] <- as.numeric(2*(logLik(lmer.alt.1.temp)-logLik(lmer.null.temp)))
      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 1]
      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 3]
#      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "Estimate"]
#      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "t value"]
    }
  
    d.lr.proportion.greater.than.zero <- mean(d.lr.stats < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
#    print(paste("d.lr.proportion.greater.than.zero: ", d.lr.proportion.greater.than.zero)) 
    d.lr.pval <- signif(mean(d.lr.stats > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
#    print(paste("d.lr.pval: ", d.lr.pval)) 
    d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
#    print(paste("d.lr.std.err: ", d.lr.std.err))
    hist(d.lr.stats, xlim = c(-2*max(abs(d.lr.stats)), 2*abs(max(d.lr.stats))), 
      main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
        "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
        "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
        sep = ""))
    points(1 ~ d.lr.test.stats, pch = 8)
  
    d.fixef.proportion.greater.than.zero <- mean(abs(d.fixef.stats) < 0.00001)  # compute the proportion of regression coefficients greater than zero
#    print(paste("d.fixef.proportion.greater.than.zero: ", d.fixef.proportion.greater.than.zero)) 
    d.fixef.pval <- signif(mean(abs(d.fixef.stats) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
#    print(paste("d.fixef.pval: ", d.fixef.pval)) 
    d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.fixef.std.err: ", d.fixef.std.err))
    hist(d.fixef.stats, xlim = c(-2*max(abs(d.fixef.stats)), 2*max(abs(d.fixef.stats))), 
      main = paste(c.data.set.name, "\nd.fixef.pval: ", 
        signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
        signif(d.fixef.test.stats, digits = 5)))
    points(1 ~ d.fixef.test.stats, pch = 8)
  
    d.t.test.proportion.greater.than.zero <- mean(abs(d.t.stats) < 0.00001)  # compute the proportion of t stats greater than zero
#    print(paste("d.t.test.proportion.greater.than.zero: ", d.t.test.proportion.greater.than.zero)) 
    d.t.test.pval <- signif(mean(abs(d.t.stats) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
#    print(paste("d.t.test.pval: ", d.t.test.pval)) 
    d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.t.test.std.err: ", d.t.test.std.err))
    hist(d.t.stats, xlim = c(-2*max(abs(d.t.stats)), 2*max(abs(d.t.stats))), 
      main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
    points(1 ~ d.t.test.stats, pch = 8)

    df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
      signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
      signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),         # returns the pval from the second enrty in the pval matrix, aka the first level of the first independent variable   
      d.lr.pval,  
      d.fixef.pval,  
      d.t.test.pval,  
      d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
      d.lr.std.err, d.fixef.std.err, d.t.test.std.err,
      signif((vcov(lmer.alt.1)@factors$Cholesky)[1,1], digits = 5), 
      signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5),
      signif((vcov(lmer.alt.1)@factors$correlation)[2,1], digits = 5))
       
  ################### parametric bootstrapping using simulation based on null model ###################################
  ######################################################################################
  
  ################### permutation tests (no resampling) ###################################
  ######################################################################################

    c.boot.type <- "boot.permutation"  
    i.output.row <- 2
    d.lr.stats <- numeric(i.replicates)
    d.fixef.stats  <- numeric(i.replicates)
    d.t.stats <- numeric(i.replicates)
    for(i in 1:i.replicates)  {
  #    d.simulated.response <- unlist(simulate(lmer.null))
      d.simulated.response <- sample(d.dependent, size = length(d.dependent), replace = FALSE)
      lmer.null.temp <- lmer(d.simulated.response ~ 1 + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      lmer.alt.1.temp <- lmer(d.simulated.response ~ d.independent + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      d.lr.stats[i] <- as.numeric(2*(logLik(lmer.alt.1.temp)-logLik(lmer.null.temp)))
      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 1]
      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 3]
#      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "Estimate"]
#      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "t value"]
    }

    d.lr.proportion.greater.than.zero <- mean(d.lr.stats < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
#    print(paste("d.lr.proportion.greater.than.zero: ", d.lr.proportion.greater.than.zero)) 
    d.lr.pval <- signif(mean(d.lr.stats > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
#    print(paste("d.lr.pval: ", d.lr.pval)) 
    d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
#    print(paste("d.lr.std.err: ", d.lr.std.err))
    hist(d.lr.stats, xlim = c(-2*max(abs(d.lr.stats)), 2*abs(max(d.lr.stats))), 
      main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
        "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
        "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
        sep = ""))
    points(1 ~ d.lr.test.stats, pch = 8)
  
    d.fixef.proportion.greater.than.zero <- mean(abs(d.fixef.stats) < 0.00001)  # compute the proportion of regression coefficients greater than zero
#    print(paste("d.fixef.proportion.greater.than.zero: ", d.fixef.proportion.greater.than.zero)) 
    d.fixef.pval <- signif(mean(abs(d.fixef.stats) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
#    print(paste("d.fixef.pval: ", d.fixef.pval)) 
    d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.fixef.std.err: ", d.fixef.std.err))
    hist(d.fixef.stats, xlim = c(-2*max(abs(d.fixef.stats)), 2*max(abs(d.fixef.stats))), 
      main = paste(c.data.set.name, "\nd.fixef.pval: ", 
        signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
        signif(d.fixef.test.stats, digits = 5)))
    points(1 ~ d.fixef.test.stats, pch = 8)
  
    d.t.test.proportion.greater.than.zero <- mean(abs(d.t.stats) < 0.00001)  # compute the proportion of t stats greater than zero
#    print(paste("d.t.test.proportion.greater.than.zero: ", d.t.test.proportion.greater.than.zero)) 
    d.t.test.pval <- signif(mean(abs(d.t.stats) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
#    print(paste("d.t.test.pval: ", d.t.test.pval)) 
    d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.t.test.std.err: ", d.t.test.std.err))
    hist(d.t.stats, xlim = c(-2*max(abs(d.t.stats)), 2*max(abs(d.t.stats))), 
      main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
    points(1 ~ d.t.test.stats, pch = 8)
  
    df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
      signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
      signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),       
      d.lr.pval,  
      d.fixef.pval,  
      d.t.test.pval,  
      d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
      d.lr.std.err, d.fixef.std.err, d.t.test.std.err,
      signif((vcov(lmer.alt.1)@factors$Cholesky)[1,1], digits = 5), 
      signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5),
      signif((vcov(lmer.alt.1)@factors$correlation)[2,1], digits = 5))
     
  ################### end of permutation tests (no resampling) ###################################
  ######################################################################################
  
  ################### non-parametric bootstrap tests (resampling) ###################################
  ######################################################################################
  
    c.boot.type <- "boot.replace" 
    i.output.row <- 3
    d.lr.stats <- numeric(i.replicates)
    d.fixef.stats  <- numeric(i.replicates)
    d.t.stats <- numeric(i.replicates)
    for(i in 1:i.replicates)  {
  #    d.simulated.response <- unlist(simulate(lmer.null))
      d.simulated.response <- sample(d.dependent, size = length(d.dependent), replace = TRUE)
      lmer.null.temp <- lmer(d.simulated.response ~ 1 + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      lmer.alt.1.temp <- lmer(d.simulated.response ~ d.independent + (1 | c.id), data = df.temp, REML = FALSE) # verbose = TRUE, REML = FALSE)
      d.lr.stats[i] <- as.numeric(2*(logLik(lmer.alt.1.temp)-logLik(lmer.null.temp)))
      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 1]
      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))[2, 3]
#      d.fixef.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "Estimate"]
#      d.t.stats[i] <- coef(summary(lmer.alt.1.temp))["d.independent", "t value"]
    }

    d.lr.proportion.greater.than.zero <- mean(d.lr.stats < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
#    print(paste("d.lr.proportion.greater.than.zero: ", d.lr.proportion.greater.than.zero)) 
    d.lr.pval <- signif(mean(d.lr.stats > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
#    print(paste("d.lr.pval: ", d.lr.pval)) 
    d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
#    print(paste("d.lr.std.err: ", d.lr.std.err))
    hist(d.lr.stats, xlim = c(-2*max(abs(d.lr.stats)), 2*abs(max(d.lr.stats))), 
      main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
        "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
        "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
        sep = ""))
    points(1 ~ d.lr.test.stats, pch = 8)
  
    d.fixef.proportion.greater.than.zero <- mean(abs(d.fixef.stats) < 0.00001)  # compute the proportion of regression coefficients greater than zero
#    print(paste("d.fixef.proportion.greater.than.zero: ", d.fixef.proportion.greater.than.zero)) 
    d.fixef.pval <- signif(mean(abs(d.fixef.stats) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
#    print(paste("d.fixef.pval: ", d.fixef.pval)) 
    d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.fixef.std.err: ", d.fixef.std.err))
    hist(d.fixef.stats, xlim = c(-2*max(abs(d.fixef.stats)), 2*max(abs(d.fixef.stats))), 
      main = paste(c.data.set.name, "\nd.fixef.pval: ", 
        signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
        signif(d.fixef.test.stats, digits = 5)))
    points(1 ~ d.fixef.test.stats, pch = 8)

    d.t.test.proportion.greater.than.zero <- mean(abs(d.t.stats) < 0.00001)  # compute the proportion of t stats greater than zero
#    print(paste("d.t.test.proportion.greater.than.zero: ", d.t.test.proportion.greater.than.zero)) 
    d.t.test.pval <- signif(mean(abs(d.t.stats) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
#    print(paste("d.t.test.pval: ", d.t.test.pval)) 
    d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
#    print(paste("d.t.test.std.err: ", d.t.test.std.err))
    hist(d.t.stats, xlim = c(-2*max(abs(d.t.stats)), 2*max(abs(d.t.stats))), 
      main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
    points(1 ~ d.t.test.stats, pch = 8)
  
    df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
      signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
      signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),       
      d.lr.pval,  
      d.fixef.pval,  
      d.t.test.pval,  
      d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
      d.lr.std.err, d.fixef.std.err, d.t.test.std.err,
      signif((vcov(lmer.alt.1)@factors$Cholesky)[1,1], digits = 5), 
      signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5),
      signif((vcov(lmer.alt.1)@factors$correlation)[2,1], digits = 5))

################### end of non-parametric bootstrap tests (resampling) ###################################
######################################################################################

    if(i.counter == 1)  {
      df.output.final <- df.output.temp
    }
    else if (i.counter > 1) {
      df.output.final <- rbind(df.output.final, df.output.temp)
    }
  }

  plot(1,1, main = paste("end of ", c.dependent, " ~ ", c.independent))

#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".", c.data.set.name, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
}  



