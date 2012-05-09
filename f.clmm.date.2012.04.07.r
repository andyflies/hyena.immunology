# source("f.clmm.date.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.clmm.date <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- df.data.flat
#  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
#  print(sort(df.temp.full$d.sample.date))
#  df.temp.full <- subset(df.temp.full, !(c.id == "mrph" & i.sample.number.2 == 3))  
#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp.full <- subset(df.temp.full, !(c.id == "mrph" & i.sample.number.2 == 3))  



  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  
#  print(cbind(df.temp.full$c.id, df.temp.full$d.mic.90.bka.ec, log2(df.temp.full$d.mic.90.bka.ec), df.temp.full$d.mic.90.level.bka.ec,  
#    df.temp.full$d.mic.90.bka.pm, log2(df.temp.full$d.mic.90.bka.pm), df.temp.full$d.mic.90.level.bka.pm)) 

#  df.temp.full$d.sample.date <- (df.temp.full$d.sample.date - min(df.temp.full$d.sample.date))

#  c.independent <- "i.rank"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
  cv.independent.variables <- c("d.sample.date") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females
                                                                                   #  "c.age", 
  df.temp.full$c.mic.median.level.ec <- df.temp.full$d.mic.90.median.bka.ec
  df.temp.full$c.mic.median.level.ec[df.temp.full$d.mic.90.median.bka.ec < 160] <- 1
  df.temp.full$c.mic.median.level.ec[df.temp.full$d.mic.90.median.bka.ec == 160] <- 2
  df.temp.full$c.mic.median.level.ec[df.temp.full$d.mic.90.median.bka.ec > 160] <- 3
  df.temp.full$c.mic.median.level.pm <- df.temp.full$d.mic.90.median.bka.pm
  df.temp.full$c.mic.median.level.pm[df.temp.full$d.mic.90.median.bka.pm < 20] <- 1
  df.temp.full$c.mic.median.level.pm[df.temp.full$d.mic.90.median.bka.pm == 20] <- 2
  df.temp.full$c.mic.median.level.pm[df.temp.full$d.mic.90.median.bka.pm > 20] <- 3

  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "c.mic.median.level.ec", "d.mic.90.median.bka.pm", "c.mic.median.level.pm") 
    
  par(mfrow = c(2,3))

  i.fixed.effect.levels <- 1          # use this constant to set the number of rows needed in the output data frame

  i.replicates <- 1
  i.counter.1 <- 1
  i.counter.1.stop <- length(cv.independent.variables) 
  for(i.counter.1 in i.counter.1:i.counter.1.stop)  {

    c.independent <- cv.independent.variables[i.counter.1]

    i.counter.2 <- 1
    i.counter.2.stop <- length(cv.dependent.variables) 
    for(i.counter.2 in i.counter.2:i.counter.2.stop)  {
  
      c.dependent <- cv.dependent.variables[i.counter.2]
  
      print(paste(" **************** c.independent: ", c.independent, "**********************"))   
      print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
   
      print("Before subsetting within the loop.")
      i.full.c.id <- length(df.temp.full$c.id)
      i.full.unique.c.id <- length(unique(df.temp.full$c.id))
      i.full.i.rank <- length(df.temp.full$i.rank)
      i.full.unique.i.rank <- length(unique(df.temp.full$i.rank))
  
      # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.independent]]))
      df.temp <- subset(df.temp, !is.na(df.temp[[c.dependent]]))
  
      i.subset.c.id <- length(df.temp$c.id)
      i.subset.unique.c.id <- length(unique(df.temp$c.id))
      i.subset.i.rank <- length(df.temp$i.rank)
      i.subset.unique.i.rank <- length(unique(df.temp$i.rank))

      i.female <- length(df.temp[df.temp$c.sex == "f", 1])  # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.male <- length(df.temp[df.temp$c.sex == "m", 1])  # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.adult.female <- length(df.temp[df.temp$c.sex == "f" & df.temp$c.age == "adult", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.adult.male <- length(df.temp[df.temp$c.sex == "m" & df.temp$c.age == "adult", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.subadult.female <- length(df.temp[df.temp$c.sex == "f" & df.temp$c.age == "subadult", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.subadult.male <- length(df.temp[df.temp$c.sex == "m" & df.temp$c.age == "subadult", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.bado <- length(df.temp[df.temp$c.sex == "f" & df.temp$c.reproductive.status == "bado", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.lactating <- length(df.temp[df.temp$c.sex == "f" & df.temp$c.reproductive.status == "lactating", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    
      i.pregnant <- length(df.temp[df.temp$c.sex == "f" & df.temp$c.reproductive.status == "pregnant", 1])      # extract the matching variables and count the length of column 1 in the data frame, store this number to add to the output data frame    

      df.temp$d.independent <- df.temp[[c.independent]]  
      df.temp$d.dependent <- df.temp[[c.dependent]]
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      df.temp$d.dependent <- factor(df.temp[[c.dependent]])      
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      print("summary(df.temp$d.independent)")
      print(summary(df.temp$d.independent))

      print("clmm.0 <- clmm(d.dependent ~ 1 + (1 | c.id), data = df.temp)")
      print("clmm.1 <- clmm(d.dependent ~ d.independent + (1 | c.id), data = df.temp)")
      clmm.0 <- clmm(d.dependent ~ 1 + (1 | c.id), data = df.temp, Hess = TRUE) #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #,trace = 1, innerCtrl = "noWarn")
      clmm.1 <- clmm(d.dependent ~ d.independent + (1 | c.id), data = df.temp, Hess = TRUE) #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
      clmm.2 <- clmm(d.dependent ~ d.independent + i.rank + (1 | c.id), data = df.temp, Hess = TRUE) #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")

      par(mfrow = c(3,3))
      print("summary(clmm.0))") 
      print(summary(clmm.0))
      print("summary(clmm.1))") 
      print(summary(clmm.1))
      print("summary(clmm.2))") 
      print(summary(clmm.2))

      print("lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp)")
      print("lmer.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp)")
      lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, Hess = TRUE) #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #,trace = 1, innerCtrl = "noWarn")
      lmer.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp, Hess = TRUE) #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
      lmer.2 <- lmer(d.dependent ~ d.independent + i.rank + (1 | c.id), data = df.temp, Hess = TRUE) #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")

#      par(mfrow = c(3,3))
      print("cftest(lmer.0))") 
      print(cftest(lmer.0))

      print("cftest(lmer.1))") 
      print(cftest(lmer.1))

      print("cftest(lmer.2))") 
      print(cftest(lmer.2))

# pr.clmm.1 <- profile(clmm.1)
# confint(pr.clmm.1)
# par(mfrow = c(2,2))
# plot(pr.clmm.1)
# plot(pr.clmm.1, Log=TRUE, relative = TRUE)
# plot(pr.clmm.1, Log=TRUE, relative = FALSE)
# par(mfrow=c(1,1))
# plot(1,1)

# print(str(clmm.2))
print(clmm.2$coefficients["d.independent"])
      plot(jitter(as.double(d.dependent)) ~ d.independent, data = df.temp, main = paste(c.dependent, " ~ ", c.independent))
# abline(a = (fixef(clmm.1))[[1]], b = (fixef(clmm.1))[[2]])    # a = intercept, b = slope (coefficient)  
      # ANOVA of models
#      print("anova(clmm.0, clmm.1)")
#      print(anova(clmm.0, clmm.1))
      
      # Compute the LR test stat for comparing to the bootstrap results
#      d.lr.test.stats <- signif(as.numeric(2*(logLik(clmm.1, REML = FALSE)-logLik(clmm.0, REML = FALSE)), digits = 5))
      # Store the independent variable effect sizes for comparing to the bootstrap results
#      d.fixef.test.stats <- signif(coef(summary(clmm.1))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
      # Store the t-stat for the independent for comparing to the bootstrap results
#      d.t.test.stats <- signif(coef(summary(clmm.1))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
#      dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
#      print(dv.test.stats)
#      print(paste("number of rows in fixef: ", length(coef(summary(clmm.1))[,1])))
#      print(paste("number of columns in fixef: ", length(coef(summary(clmm.1))[1,])))
  
   
        ################### parametric bootstrapping using simulation based on null model ###################################
    ######################################################################################
#      par(mfrow = c(3,3))
    
      c.boot.type <- "boot.parametric"
      i.output.row <- 1
  
#  print(" ********************  staring f.boot.parametric ********************") 
#      df.test.stats <- replicate(i.replicates, f.boot.parametric(clmm.0, clmm.1))
#      rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")
  
#      d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
#      d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
#      d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 

#      hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
#        main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
#          "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
#          "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
#          sep = ""))
#      points(1 ~ d.lr.test.stats, pch = 8)
    
#      d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
#      d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
#      d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 

#      hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
#        main = paste("\nd.fixef.pval: ", 
#          signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
#          signif(d.fixef.test.stats, digits = 5)))
#      points(1 ~ d.fixef.test.stats, pch = 8)
    
#      d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
#      d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
#      d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 

#      hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
#        main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
#      points(1 ~ d.t.test.stats, pch = 8)
           
    ################### parametric bootstrapping using simulation based on null model ###################################
    ######################################################################################
    
  
    }                        # end of inner loop for dependent variables
  }                        # end of outer loop for independent variables

  par(mfrow = c(1,1))
  plot(1,1, main = paste("space filler"))
#  df.output.final <- cbind(df.output.final, i.replicates)

#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  



