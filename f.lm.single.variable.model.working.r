# source("f.lm.single.variable.model.working.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lm.single.variable.model <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  # Subsets data and removes potential outliers. For this subset to affect the output, the other subset command below needs to be commented out.
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & (i.sample.number.2 == 2 | i.sample.number.2 == 3)
#    | (c.id == "mp" & i.sample.number.2 == 4))
 
#  df.temp.full <- subset(df.temp, c.two.adult.samples == "yes" | c.three.adult.samples == "yes")
#  This gets all adult females, including the potential outlier MP. WND sample 1 is also included even though there is no rank for that sample
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
#  df.temp.full <- df.data.flat
#  df.temp.full <- subset(df.data.flat, c.sex == "f")
#  df.temp.full <- subset(df.data.flat, c.age == "adult")
#  c.data.set.name <- "full.set"
  c.data.set.name <- "female"  # Inside the loop we use !is.na to remove any records that have na values
#  c.data.set.name <- "gol.mp.removed"
#  c.data.set.name <- "full.female.male.cr.gol.mp.removed"

#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp$d.age.months <- log(df.temp$d.age.months)  # Use log transformed d.age.months

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  
#  df.dummy <- data.frame(df.temp.full$c.id)
#  print(cbind(df.dummy, df.temp.full$i.rank))

  c.independent <- "i.rank" # "c.reproductive.status"  #"c.age"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  cv.independent.variables <- c("i.rank", "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females
#  cv.independent.variables <- c("c.age", "c.reproductive.status") # c.sex can only be run with males and females

#  c.dependent <- "d.mic.90.level.bka.ec"
#  cv.independent.variables.short <- c("i.rank")                # comment this line out to run the full set of variables.
  cv.dependent.variables <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm", 
    "i.cubs.weaned", "d.repro.lifespan", "d.ars", "d.cub.surv", "d.littersize", "d.litters.per.year")
#    "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm")

# cv.dependent.variables <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg", 
#  "d.blank.abs.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm", 
#  "i.glucose.green", "i.glucose.red", "d.pcv", "d.total.solids",
#  "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
#  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm", "d.serology.index.igg", 
#  "d.serology.index.rank.igg", "d.serology.index.igm", "d.serology.index.rank.igm", "d.eosinophils", "d.lymphocytes", 
#  "d.monocytes", "d.neutrophils", "d.total.wbc", "d.relative.eosinophils", "d.relative.lymphocytes", 
#  "d.relative.monocytes", "d.relative.neutrophils", "d.t", "d.p", "d.c", "d.e", "d.lh", 
#  "d.igf06", "i.cubs.weaned", "d.repro.lifespan", "d.ars", "d.cub.surv", "d.littersize", "d.litters.per.year")
#  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer")

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
  i.counter.stop <- length(cv.dependent.variables) 
  for(i.counter in i.counter:i.counter.stop)  {

    c.dependent <- cv.dependent.variables[i.counter]

    print(paste(" **************** c.independent: ", c.independent, "**********************"))   
    print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
 
    print("Before subsetting within the loop.")
    i.full.c.id <- length(df.temp.full$c.id)
    i.full.unique.c.id <- length(unique(df.temp.full$c.id))
    i.full.i.rank <- length(df.temp.full$i.rank)
    i.full.unique.i.rank <- length(unique(df.temp.full$i.rank))
    print(i.full.c.id)
    print(i.full.unique.c.id)
    print(i.full.i.rank)
    print(i.full.unique.i.rank)

    # We can only include records in the lm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
    #  If the number of records in the lm.null and lm.alt models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
#    df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.independent]]))
#    df.temp <- subset(df.temp, !is.na(df.temp[[c.dependent]]))
    df.temp <- df.temp.full
    if(c.independent == "c.reproductive.status")  {
      df.temp <- subset(df.temp, c.reproductive.status != "bado" & !is.na(c.reproductive.status))
      df.temp$c.reproductive.status <- factor(as.character(df.temp$c.reproductive.status))
    }

    print("After subsetting within loop.")
    i.subset.c.id <- length(df.temp$c.id)
    i.subset.unique.c.id <- length(unique(df.temp$c.id))
    i.subset.i.rank <- length(df.temp$i.rank)
    i.subset.unique.i.rank <- length(unique(df.temp$i.rank))

print(length(df.temp[[c.dependent]]))
print(length(df.temp[[c.independent]]))
print(length(df.temp$c.id))

#      df.temp <- aggregate(cbind("i.rank", "d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg", 
#        "d.blank.abs.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm" 
    df.temp <- aggregate(cbind(i.rank, d.age.months, d.mic.90.level.bka.ec, d.mic.90.level.bka.pm, d.blank.abs.total.igg,	d.blank.abs.total.igm, 
      d.blank.abs.ec.igg, d.blank.abs.ec.igm, d.blank.abs.pm.igg, d.blank.abs.pm.igm,
      i.cubs.weaned, d.repro.lifespan, d.ars, d.cub.surv, d.littersize, d.litters.per.year
      ) 
#        "i.glucose.green", "i.glucose.red", "d.pcv", "d.total.solids",
#        "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
#        "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm", "d.serology.index.igg", 
#        "d.serology.index.rank.igg", "d.serology.index.igm", "d.serology.index.rank.igm", "d.eosinophils", "d.lymphocytes", 
#        "d.monocytes", "d.neutrophils", "d.total.wbc", "d.relative.eosinophils", "d.relative.lymphocytes", 
#        "d.relative.monocytes", "d.relative.neutrophils", "d.t", "d.p", "d.c", "d.e", "d.lh", 
#        "d.igf06", "i.cubs.weaned", "d.repro.lifespan", "d.ars", "d.cub.surv", "d.littersize", "d.litters.per.year")
      ~ c.id 
      + c.age
      + c.sex
      + c.reproductive.status
      , 
      data = df.temp, mean, na.action = na.pass)

#    print(df.temp)
    print("After aggregating  measures")
    print(df.temp)
    i.subset.c.id <- length(df.temp$c.id)
    i.subset.unique.c.id <- length(unique(df.temp$c.id))
    i.subset.i.rank <- length(df.temp$i.rank)
    i.subset.unique.i.rank <- length(unique(df.temp$i.rank))
 
    d.independent <- df.temp[[c.independent]]   
    d.dependent <- df.temp[[c.dependent]]

    # temporary code for converting the dependent variable into an ordinal, rank varible. Can change rank method for "random" or "average" for ties
#    d.independent <- df.temp[[c.independent]]  
#    d.independent.rank <- rank(d.independent, ties.method = "random")  # random assignment of tied variables
#    d.dependent <- df.temp[[c.dependent]]
#    d.dependent.rank <- rank(d.dependent, ties.method = "average")                                 # na.last = NA,
#    print(data.frame(cbind(as.character(df.temp$c.id), df.temp$i.rank, as.character(df.temp$c.sex), 
#        d.independent, d.independent.rank, d.dependent, d.dependent.rank)))
#    d.dependent <- d.dependent.rank        # re-assigning the rank of the depedent variables based on rank. This allow a non-parametric test in which ties are randomly assigned a rank


    lm.null <- lm(d.dependent ~ 1, data = df.temp)
    lm.alt.1 <- lm(d.dependent ~ d.independent, data = df.temp)
    # non-parametric wilcox test
#    wt.alt.1 <- wilcox_test(d.dependent ~ d.independent, data = df.temp, distribution = "exact", conf.int = TRUE)
#    print(wt.alt.1)
    rcorr.alt.1 <- rcorrcens(d.dependent ~ d.independent, outx = TRUE)
    print(rcorr.alt.1)
    plot(rcorr.alt.1)  

    print("summary(lm.null))") 
    print(summary(lm.null))
    print("summary(lm.alt.1))") 
    print(summary(lm.alt.1))
    print("cftest(lm.alt.1)")
    print(cftest(lm.alt.1))     # computes p-vals for the lm
    # ANOVA of models
#    print("anova(lm.null, lm.alt.1)")
#    print(anova(lm.null, lm.alt.1))
    
    # Compute the LR test stat for comparing to the bootstrap results
    d.lr.test.stats <- signif(as.numeric(2*(logLik(lm.alt.1, REML = FALSE)-logLik(lm.null, REML = FALSE)), digits = 5))
    # Store the independent variable effect sizes for comparing to the bootstrap results
    d.fixef.test.stats <- signif(coef(summary(lm.alt.1))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
    # Store the t-stat for the independent for comparing to the bootstrap results
    d.t.test.stats <- signif(coef(summary(lm.alt.1))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
    dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
    print(dv.test.stats)
    print(paste("number of rows in fixef: ", length(coef(summary(lm.alt.1))[,1])))
    print(paste("number of columns in fixef: ", length(coef(summary(lm.alt.1))[1,])))



    par(mfrow = c(2,3))

    # plotting lm model 1
    plot(lm.alt.1$model[,1] ~ lm.alt.1$model[,2], 
      main = paste((attr(attr(lm.alt.1$terms, "factors"), "dimnames")[[1]][1]), " ~ ", 
        (attr(attr(lm.alt.1$terms, "factors"), "dimnames")[[1]][1]), 
        "\ncftest pval: ", signif(((cftest(lm.alt.1))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
#        "\nanova pval: ", signif((anova(lm.null, lm.alt.1))$"Pr(>Chisq)"[2], digits = 4), 
        sep = ""), 
      xlab = c.independent, ylab = c.dependent)
    text(lm.alt.1$model[,1] ~ lm.alt.1$model[,2], labels = paste(df.temp$c.id, df.temp$i.sample.number.2), pos = 4, cex = 1)
    if(is.double(d.independent)== TRUE)  {
      abline(a = (coef(lm.alt.1))[1], b = (coef(lm.alt.1))[2])    # a = intercept, b = slope (coefficient)
    }  
#    abline(a = (fixef(lm.alt.1))[["(Intercept)"]], b = (fixef(lm.alt.1))[["d.independent"]])    
    hist(resid(lm.alt.1), main = paste("Histogram of residuals - ", c.independent)) 
#      "\nr.squared: ", signif(sqrt((vcov(lm.alt.1)@factors$Cholesky)[1,1]), digits = 5)

    lines(density(resid(lm.alt.1)))                                     # 
    qqnorm(resid(lm.alt.1), main = paste("qqnorm\n", (attr(attr(lm.alt.1$terms, "factors"), "dimnames")[[1]][1]), " ~ ", 
      (attr(attr(lm.alt.1$terms, "factors"), "dimnames")[[1]][2])))
    qqline(resid(lm.alt.1))
    plot(fitted(lm.alt.1), resid(lm.alt.1), main = paste("fitted x resid\n", c.dependent, " ~ ", c.independent), xlab = "fitted", ylab = "resid")
    text(fitted(lm.alt.1), resid(lm.alt.1), labels = paste(df.temp$c.id, df.temp$i.sample.number.2))      # use this to label the points in the plot
    abline(0,0)
    plot(resid(lm.alt.1) ~ lm.alt.1$model[,2], 
      main = paste(c.independent, ", Values x Residuals\n",
        "i.subset.c.id: ", i.subset.c.id, ", i.subset.unique.c.id: ", i.subset.unique.c.id))  
    abline(0,0)


      ################### parametric bootstrapping using simulation based on null model ###################################
  ######################################################################################
#    par(mfrow = c(3,3))
#  
#    c.boot.type <- "boot.parametric"
#    i.output.row <- 1

#    print(" ********************  staring f.boot.parametric ********************") 
#    df.test.stats <- replicate(i.replicates, f.boot.parametric(lm.null, lm.alt.1))
#    rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")

#    d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
#    d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
#    d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
#    hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
#      main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
#        "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
#        "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
#        sep = ""))
#    points(1 ~ d.lr.test.stats, pch = 8)
  
#    d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
#    d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
#    d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
#    hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
#      main = paste(c.data.set.name, "\nd.fixef.pval: ", 
#        signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
#        signif(d.fixef.test.stats, digits = 5)))
#    points(1 ~ d.fixef.test.stats, pch = 8)
  
#    d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
#    d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
#    d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
#    hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
#      main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
#    points(1 ~ d.t.test.stats, pch = 8)

#    df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
#      signif((anova(lm.null, lm.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
#      signif(((cftest(lm.alt.1))$test$pvalues)[2], digits = 5),         # returns the pval from the second enrty in the pval matrix, aka the first level of the first independent variable   
#      d.lr.pval,  
#      d.fixef.pval,  
#      d.t.test.pval,  
#      d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
#      d.lr.std.err, d.fixef.std.err, d.t.test.std.err,
#      signif((vcov(lm.alt.1)@factors$Cholesky)[1,1], digits = 5), 
#      signif(sqrt((vcov(lm.alt.1)@factors$Cholesky)[1,1]), digits = 5),
#      signif((vcov(lm.alt.1)@factors$correlation)[2,1], digits = 5))
       
  ################### parametric bootstrapping using simulation based on null model ###################################
  ######################################################################################
  
################### end of non-parametric bootstrap tests (resampling) ###################################
######################################################################################

    if(i.counter == 1)  {
      df.output.final <- df.output.temp
    }
    else if (i.counter > 1) {
      df.output.final <- rbind(df.output.final, df.output.temp)
    }
  }

  par(mfrow = c(1, 1))
  plot(1,1, main = paste("end of ", c.dependent, " ~ ", c.independent))
#  df.output.final <- cbind(df.output.final, i.replicates)

#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".", c.data.set.name, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
}  



