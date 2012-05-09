#### Load Andy's functions
                             
print("Need to add na.rm=TRUE to most of these functions")
par(mfrow=c(2,2))

### List of functions
f.list.functions <- function()
{
  # Creates boxplots for a single hyena for a single isotype target. Does not use negative absorbance and concentration values.
  print("sortframe<-function(df,...) df[do.call(order,list(...)),]")

}

f.sort.frame <- function(df.input, ...)   # Heirarchical sorting of a date frame.  Need to input the data frame to sort and the variables to sort by
{
  df.input[do.call(order,list(...)),]  
  # Use this format to call the function
  # with(name of data frame, sortframe(name of data frame, name of
  #   first variable to sort by, name of second variable to sort by))
  # e.g. df.master.wbc <- with(df.master.wbc, f.sort.frame(df.master.wbc, c.id, i.sample.number))
}

# source("f.calculate.percent.inhibition.r")

f.calculate.percent.inhibition <- function()   # Heirarchical sorting of a date frame.  Need to input the data frame to sort and the variables to sort by
{

  # Need to calculate the mean and sd of replicates before doing any other modications.
  #  Use the mean and sd to calculate within sample CV usign the across dilution individual means
  df.bka.temp <- with(df.bka.raw, f.sort.frame(df.bka.raw, c.id, i.replicate.number, c.strain, i.sample.number))
  # print(subset(df.bka.temp, select = c(c.id, i.sample.dilution, i.replicate.number, i.sample.number, d.percent.control.abs, d.percent.inhibition, d.mic.90, c.strain)))
  
  df.bka.mean.across.dilutions <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.replicate.number
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.raw, mean, na.action = na.pass
  )
  
  df.bka.mean.within.sample <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.mean.across.dilutions, mean, na.action = na.pass
  )
  
  df.bka.mean.within.sample.sd <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.mean.across.dilutions, sd, na.action = na.pass
  )
  
  df.bka.mean.within.sample$d.percent.inhibition.sd <- df.bka.mean.within.sample.sd$d.percent.inhibition
  d.percent.inhibition.within.sample.cv <- (df.bka.mean.within.sample$d.percent.inhibition.sd / df.bka.mean.within.sample$d.percent.inhibition)
  df.bka.mean.within.sample$d.percent.inhibition.within.sample.cv <- d.percent.inhibition.within.sample.cv
  
  df.bka.mean.across.dilutions <- with(df.bka.mean.across.dilutions, f.sort.frame(df.bka.mean.across.dilutions, c.id, i.replicate.number, c.strain, i.sample.number))
  df.bka.mean.within.sample <- with(df.bka.mean.within.sample, f.sort.frame(df.bka.mean.within.sample, c.id, c.strain, i.sample.number))
  
  # Need to rename variables so they are split by bacterial strain
  df.ec.temp <- df.bka.mean.within.sample[df.bka.mean.within.sample$c.strain == "ec",]
  df.pm.temp <- df.bka.mean.within.sample[df.bka.mean.within.sample$c.strain == "pm",]
  df.ec.temp <- subset(df.ec.temp, select = -c(c.strain))  # do not need c.strain in the merged data frame, so get rid of it by negative subsetting
  df.pm.temp <- subset(df.pm.temp, select = -c(c.strain))  # do not need c.strain in the merged data frame, so get rid of it by negative subsetting

  # merge the data frames and add the strain to the end of the non-merged variable names
  df.bka.mean.within.sample <- merge(df.ec.temp, df.pm.temp,  # merge the two data frames. Keep all rows in the first df.
    by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  #  all.x = TRUE,
    suffixes = c(".ec",".pm"), all = TRUE)                                  # rename the non-key columns when there are conflicts
  df.bka.mean.within.sample
}


# this function can only be used when the exact data frame to be manipulated is entered
f.convert.to.ranks.backup <- function(df.input)   # Create a new df that has the within column ranks of the input df
{
  i.stop.value <- length(colnames(df.input))
  i.counter <- 1
  for(i.counter in i.counter:i.stop.value)
  {
    df.input[, i.counter] <- rank(df.input[, i.counter], na.last = "keep") 
  }
  df.input
}

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.convert.to.ranks <- function(df.input, ...)   
{
  l.names <- as.list(...)             # need to convert the input to a list.  Use list() does not work here
  cv.names <- as.character(l.names)   # convert to a character vector
  df.temp <- df.input[, cv.names]
  i.stop.value <- length(cv.names)
  i.counter <- 1
  for(i.counter in i.counter:i.stop.value)
  {
    df.temp[, cv.names[i.counter]] <- rank(df.temp[, cv.names[i.counter]], na.last = "keep")
#    print(paste("i.counter: ", i.counter))
#    i.counter <- i.counter + 1 
  }
  colnames(df.temp) <- paste(cv.names, ".rank", sep="")   # Add the suffix ".rank" to the input column names, then change the names of the new df
  df.temp <- cbind(df.input, df.temp)                     # Bind the new df and the input df together and return
}

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.make.serology.index <- function(df.input, ...)   
{
  l.names <- as.list(...)             # need to convert the input to a list.  Use list() does not work here
  cv.names <- as.character(l.names)   # convert to a character vector
  cv.rank.names <- paste(cv.names, ".rank", sep="")
  print(cv.rank.names[1])
  df.temp <- df.input[, cv.rank.names]
  if(grepl("igg", cv.rank.names[1]))      
  {  
    d.serology.index.igg <- rowMeans(df.temp)  
    d.serology.index.rank.igg <- rank(d.serology.index.igg, na.last = "keep")       
    df.temp <- cbind(df.input, d.serology.index.igg, d.serology.index.rank.igg) # Bind the new df and the input df together and return
  }
  else if(grepl("igm", cv.rank.names[1])) 
  {  
    d.serology.index.igm <- rowMeans(df.temp)    
    d.serology.index.rank.igm <- rank(d.serology.index.igm, na.last = "keep")       
    df.temp <- cbind(df.input, d.serology.index.igm, d.serology.index.rank.igm) # Bind the new df and the input df together and return
  }
#  name(d.serology.index) <- paste(d.serology.index, c.suffix, sep="")   # Add the suffix of ".igg" or ".igM" to the new column, then bind it to the input df and return
  df.temp
}


merge.rec <- function(.list, ...){
    if(length(.list)==1) return(.list[[1]])
    Recall(c(list(merge(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
}

# source("f.lmer.univariate.categorical.models.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.univariate.categorical.models <- function(df.input, ...)   
{
  # Subsets data and removes potential outliers. For this subset to affect the output, the other subset command below needs to be commented out.
  df.temp <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & (i.sample.number.2 == 2 | i.sample.number.2 == 3)
    | (c.id == "mp" & i.sample.number.2 == 4))
 
  df.temp <- subset(df.temp, c.two.adult.samples == "yes" | c.three.adult.samples == "yes")
  df.temp <- subset(df.temp, !(c.id == "wnd"))
#  This gets all adult females, including the potential outlier MP. WND sample 1 is also included even though there is no rank for that sample
#  df.temp <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
  df.temp <- subset(df.data.flat, c.sex == "f")
#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp <- subset(df.temp, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp <- subset(df.temp, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp$d.age.months <- log(df.temp$d.age.months)  # Use log transformed d.age.months

#  print(df.temp[,1:11])
  print(length(df.temp$c.id))
  print(length(unique(df.temp$c.id)))
  print(length(unique(df.temp$i.rank)))  

  c.independent <- "i.rank"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
  i.counter.1 <- 1
  i.stop.count.1 <- length(cv.independent.variables.short)
  i.stop.count.1 <- length(cv.independent.variables.categorical)
  # loops through all of the independent variables
  for(i.counter.1 in i.counter.1:i.stop.count.1)
  {
    c.independent <- cv.independent.variables.short[i.counter.1]     
    c.independent <- cv.independent.variables.categorical[i.counter.1]
    d.independent <- df.temp[[c.independent]]
    print("************************ Model creation ************************")
    print(paste(" **************** c.independent: ", c.independent, "**********************"))   
  
    i.counter.2 <- 1
    i.stop.count.2 <- length(cv.immune.short)
  #  i.stop.count.2 <- 2   # This is the temporary stop number while developing this function
    # loops through all of the dependent variables
    for(i.counter.2 in i.counter.2:i.stop.count.2)  {
      c.dependent <- cv.immune.short[i.counter.2]     
      d.dependent <- df.temp[[c.dependent]]
      print("************************ Model creation ************************")
      print(paste(" **************** c.dependent: ", c.dependent, "**********************"))   

      lm.null <- lm(d.dependent ~ 1, data = df.temp)
      lmer.null <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, verbose = FALSE, REML = FALSE)
      lmer.alt <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp, verbose = FALSE, REML = FALSE)
      ml.result <- as.numeric(2*logLik(lmer.null) - logLik(lm.null))
      print(paste("lm.null and lmer.null pchisq: ", pchisq(ml.result, 1))) #, lower = FALSE)))
#      print(summary(lm.null))
#      print(summary(lmer.null))
#      print(ranef(lmer.null)$c.id)
#      print(summary(lmer.alt))
      print(cftest(lmer.alt))     # computes p-vals for the lmer
#      print(confint(cftest(lmer.alt)))     # computes p-vals for the lmer
      ml.result.2 <- as.numeric(2*logLik(lmer.alt) - logLik(lmer.null))
      print(paste("lmer.null and lmer.alt pchisq: , pchisq(ml.result.2, 1, lower.tail. = TRUE")) #, lower = FALSE)))
      print(paste("lmer.null and lmer.alt pchisq: ", pchisq(ml.result.2, 1, lower.tail = TRUE))) #, lower = FALSE)))
#      print(paste("lmer.null and lmer.alt pchisq: , pchisq(ml.result.2, 1, lower.tail = FALSE")) #, lower = FALSE)))
#      print(paste("lmer.null and lmer.alt pchisq: ", pchisq(ml.result.2, 1, lower.tail = FALSE))) #, lower = FALSE)))
      print("anova(lmer.null, lmer.alt")
      print(anova(lmer.null, lmer.alt))
      print("anova(lmer.alt")
      print(anova(lmer.alt))
    
      par(mfrow = c(3,3))
      print("((cftest(lmer.alt))$test$pvalues)")
      print(((cftest(lmer.alt))$test$pvalues))
      plot(d.dependent ~ d.independent, 
#        main = paste(#c.dependent, " ~ ", c.independent, 
#          "\ncftest pval: ", 
#          signif(((cftest(lmer.alt))$test$pvalues), digits = 4), 
#          signif(((cftest(lmer.alt))$test$pvalues)["d.independent"], digits = 4), 
#          "\nanova pval: ", 
#          signif((anova(lmer.null, lmer.alt))$"Pr(>Chisq)"[2], digits = 4), 
#          sep = ""), 
        xlab = c.independent, ylab = c.dependent)
      axis(3, at = 1:length((cftest(lmer.alt))$test$pvalues), labels = paste("p: ", signif(((cftest(lmer.alt))$test$pvalues), digits = 4)))

#      points(d.dependent ~ d.independent, cex = 1.2, col = "blue")
      text(d.dependent ~ d.independent, labels = df.temp$c.id, pos = 4, cex = 1)
#      text(x = max(d.independent, na.rm = TRUE) * 0.9, y = max(d.dependent, na.rm = TRUE), "text here", cex = 1, pos=4, col="red")
#      abline(a = (fixef(lmer.alt))[["(Intercept)"]], b = (fixef(lmer.alt))[["d.independent"]])    
      hist(resid(lmer.alt), main = paste("Histogram of residuals - ", c.dependent)) 
      lines(density(resid(lmer.alt)))                                     # 
      qqnorm(resid(lmer.alt), main = paste("qqnorm - ", c.independent))
      qqline(resid(lmer.alt))
#      text(x = Bodywt, y = Brainwt, labels=row.names(primates), pos=4)
#      qqmath(ranef(lmer.alt, postVar = TRUE), strip = FALSE)$c.id
      plot(fitted(lmer.alt), resid(lmer.alt), main = "fitted x resid", xlab = "fitted", ylab = "resid")
      text(fitted(lmer.alt), resid(lmer.alt), labels = df.temp$c.id)      # use this to label the points in the plot
      abline(0,0)
      plot(resid(lmer.alt) ~ lmer.alt@frame$d.independent, main = " Values x Residuals")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
      abline(0,0)
    #  print(data.frame(lmer.alt@frame$c.id, lmer.alt@frame$i.rank))
      par(mfrow = c(1,1))
   
      # Compute the LR test stat for comparing to the bootstrap results
      d.lr.test.stat <- as.numeric(2*(logLik(lmer.alt)-logLik(lmer.null)))
      print(paste("d.lr.test.stat: ", d.lr.test.stat))
          
  #    d.lr.stat <- numeric(1000)
  #    for(i in 1:1000)  {
  #      d.simulated.response <- unlist(simulate(lmer.null))
  #       # d.simulated.response <- sample(d.simulated.response, replace=T) 
  #      lmer.null <- lmer(d.simulated.response ~ 1 + (1 | c.id), data = df.temp, verbose = TRUE, REML = FALSE)
  #      lmer.alt <- lmer(d.simulated.response ~ d.independent + (1 | c.id), data = df.temp, verbose = TRUE, REML = FALSE)
  #      d.lr.stat[i] <- as.numeric(2*(logLik(lmer.alt)-logLik(lmer.null)))
  #    }
    
  #    print(" **********************************  bootstrap results ***********************************************")
  #    d.lr.proportion.greater.than.zero <- mean(d.lr.stat < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
  #    print(paste("d.lr.proportion.greater.than.zero: ", d.lr.proportion.greater.than.zero)) 
  #    d.lr.pval <- mean(d.lr.stat > d.lr.test.stat)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
  #    print(paste("d.lr.pval: ", d.lr.pval)) 
  #    d.lr.std.err <- sqrt(d.lr.pval * 0.98/1000) 
  #    print(paste("d.lr.std.err: ", d.lr.std.err))
  
    } 
  }    
  plot(1,1)
    
}  


f.r2.corr.lmer <- function(lmer.object)  {
  summary(lm(attr(lmer.object, "y") ~ fitted (lmer.object)))$r.squared
}

f.boot.parametric <- function(lmer.input.null, lmer.input.alt) {
  l.sim <- simulate(lmer.input.null)
  lmer.null.temp <- refit(lmer.input.null, l.sim)
  lmer.alt.temp <- refit(lmer.input.alt, l.sim)
  d.lr.stats <- as.numeric(2*(logLik(lmer.alt.temp)-logLik(lmer.null.temp)))
  d.fixef.stats <- coef(summary(lmer.alt.temp))[2, 1]
  d.t.stats <- coef(summary(lmer.alt.temp))[2, 3]
  dv.output <- c(d.lr.stats, d.fixef.stats, d.t.stats)
}

f.boot.permutation <- function(lmer.input.null, lmer.input.alt) {
  l.sim <- sample(lmer.input.null@frame[,1], size = length(lmer.input.null@frame[,1]), replace = FALSE)
  lmer.null.temp <- refit(lmer.input.null, l.sim)
  lmer.alt.temp <- refit(lmer.input.alt, l.sim)
  d.lr.stats <- as.numeric(2*(logLik(lmer.alt.temp)-logLik(lmer.null.temp)))
  d.fixef.stats <- coef(summary(lmer.alt.temp))[2, 1]
  d.t.stats <- coef(summary(lmer.alt.temp))[2, 3]
  dv.output <- c(d.lr.stats, d.fixef.stats, d.t.stats)
}

f.boot.replace <- function(lmer.input.null, lmer.input.alt) {
  l.sim <- sample(lmer.input.null@frame[,1], size = length(lmer.input.null@frame[,1]), replace = TRUE)
  lmer.null.temp <- refit(lmer.input.null, l.sim)
  lmer.alt.temp <- refit(lmer.input.alt, l.sim)
  d.lr.stats <- as.numeric(2*(logLik(lmer.alt.temp)-logLik(lmer.null.temp)))
  d.fixef.stats <- coef(summary(lmer.alt.temp))[2, 1]
  d.t.stats <- coef(summary(lmer.alt.temp))[2, 3]
  dv.output <- c(d.lr.stats, d.fixef.stats, d.t.stats)
}



# source("f.lmer.single.variable.model.working.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.single.variable.model <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  # Subsets data and removes potential outliers. For this subset to affect the output, the other subset command below needs to be commented out.
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & (i.sample.number.2 == 2 | i.sample.number.2 == 3)
#    | (c.id == "mp" & i.sample.number.2 == 4))
 
#  df.temp.full <- subset(df.temp, c.two.adult.samples == "yes" | c.three.adult.samples == "yes")
#  This gets all adult females, including the potential outlier MP. WND sample 1 is also included even though there is no rank for that sample
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
#  df.temp.full <- subset(df.data.flat, c.sex == "f") # & c.age == "adult")
  df.temp.full <- df.data.flat
  df.temp.full <- subset(df.data.flat, c.sex == "f")
#  df.temp.full <- subset(df.data.flat, c.age == "adult")
#  c.data.set.name <- "full.set"
  c.data.set.name <- "full.female"  # Inside the loop we use !is.na to remove any records that have na values
#  c.data.set.name <- "gol.mp.removed"
#  c.data.set.name <- "full.female.male.cr.gol.mp.removed"

#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp$d.age.months <- log(df.temp$d.age.months)  # Use log transformed d.age.months

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  
#  df.dummy <- data.frame(df.temp.full$c.id)
#  print(cbind(df.dummy, df.temp.full$i.rank))

#  c.independent <- "i.rank"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
  cv.independent.variables <- c("i.rank", "d.age.months", "c.age", "c.reproductive.status") # c.sex can only be run with males and females

  cv.dependent.variables <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm", 
    "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm")

#  cv.dependent.variables <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg", 
#    "d.blank.abs.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm", 
#    "i.glucose.green", "i.glucose.red", "d.pcv", "d.total.solids",
#    "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
#    "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm", "d.serology.index.igg", 
#    "d.serology.index.rank.igg", "d.serology.index.igm", "d.serology.index.rank.igm", "d.eosinophils", "d.lymphocytes", 
#    "d.monocytes", "d.neutrophils", "d.total.wbc", 
#    "d.relative.eosinophils", "d.relative.lymphocytes", "d.relative.monocytes", "d.relative.neutrophils", 
#    "d.c", "d.t", "d.igf06")
  #  "d.e", "i.cubs.weaned", "d.repro.lifespan", "d.ars", "d.cub.surv", "d.littersize", "d.litters.per.year")
  #  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer")

  cv.colnames <- c("c.data.set.name", "c.dependent", "c.independent", "c.boot.type", 
    "chi.square.pval", "cftest.pval", 
    "d.lr.pval", "d.fixef.pval", "d.t.test.pval", 
    "d.lr.test.stats", "d.fixef.test.stats", "d.t.test.stats", 
    "i.subset.c.id", "i.subset.unique.c.id",
    "i.subset.i.rank", "i.subset.unique.i.rank",
    "i.female", "i.male",
    "i.adult.female", "i.adult.male",
    "i.subadult.female", "i.subadult.male",
    "i.bado", "i.lactating",
    "i.pregnant", "d.lr.std.err", "d.fixef.std.err", "d.t.test.std.err",
    "r", "r.squared", "corr.of.fixed.effects[2,1]")

  i.fixed.effect.levels <- 1          # use this constant to set the number of rows needed in the output data frame
  m.output.temp <- matrix(nrow = (3 * i.fixed.effect.levels), ncol = length(cv.colnames))
  colnames(m.output.temp) <- cv.colnames
  df.output.temp <- m.output.temp       # create the data frame for storing results while looping through dependend and independent variables

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
  
      # We can only include records in the lmer models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the lmer.null and lmer.alt models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.independent]]))
      df.temp <- subset(df.temp, !is.na(df.temp[[c.dependent]]))

      if(c.independent == "c.reproductive.status")  {
        df.temp <- subset(df.temp, c.reproductive.status != "bado")
      }
  
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

      d.independent <- df.temp[[c.independent]]   
      d.dependent <- df.temp[[c.dependent]]
  
      lmer.null <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, REML = FALSE)
      lmer.alt.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp, REML = FALSE)
      
      print("summary(lmer.null))") 
      print(summary(lmer.null))
      print("summary(lmer.alt.1))") 
      print(summary(lmer.alt.1))
  
      # Computing a pval for the summary lmer.alt.1 model -> results are the same as cftest.  cftest reports at z value and z test p value because we are using a normal distribution to obtain the p value
  #    tval <- attributes(summary(lmer.alt.1))$coef[, 3]
  #    pval <- 2 * pnorm(abs(tval), lower = FALSE)
  #    print(cbind(attributes(summary(lmer.alt.1))$coef, `p value` = signif(pval, 5)))
  
      print("cftest(lmer.alt.1)")
      print(cftest(lmer.alt.1))     # computes p-vals for the lmer
  
      # ANOVA of models
      print("anova(lmer.null, lmer.alt.1, REML = FALSE)")
      print(anova(lmer.null, lmer.alt.1, REML = FALSE))
      
      # Compute the LR test stat for comparing to the bootstrap results
      d.lr.test.stats <- signif(as.numeric(2*(logLik(lmer.alt.1, REML = FALSE)-logLik(lmer.null, REML = FALSE)), digits = 5))
      # Store the independent variable effect sizes for comparing to the bootstrap results
      d.fixef.test.stats <- signif(coef(summary(lmer.alt.1))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
      # Store the t-stat for the independent for comparing to the bootstrap results
      d.t.test.stats <- signif(coef(summary(lmer.alt.1))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
      dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
      print(dv.test.stats)
      print(paste("number of rows in fixef: ", length(coef(summary(lmer.alt.1))[,1])))
      print(paste("number of columns in fixef: ", length(coef(summary(lmer.alt.1))[1,])))
  
      par(mfrow = c(2,3))
  
      # plotting lmer model 1
      plot(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[,2], 
        main = paste((dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]]), 
          "\ncftest pval: ", signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
          "\nanova pval: ", signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 4), 
          sep = ""), 
        xlab = c.independent, ylab = c.dependent)
      text(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[,2], labels = paste(df.temp$c.id, df.temp$i.sample.number.2), pos = 4, cex = 1)
      if(is.double(d.independent)== TRUE)  {
        abline(a = (fixef(lmer.alt.1))[[1]], b = (fixef(lmer.alt.1))[[2]])    # a = intercept, b = slope (coefficient)
      }  
  #    abline(a = (fixef(lmer.alt.1))[["(Intercept)"]], b = (fixef(lmer.alt.1))[["d.independent"]])    
      hist(resid(lmer.alt.1), main = paste("Histogram of residuals - ", (dimnames(lmer.alt.1@frame[1])[[2]]), 
        "\nr.squared: ", signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5))) 
      lines(density(resid(lmer.alt.1)))                                     # 
      qqnorm(resid(lmer.alt.1), main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
      qqline(resid(lmer.alt.1))
      plot(fitted(lmer.alt.1), resid(lmer.alt.1), main = paste("fitted x resid\n", c.dependent, " ~ ", c.independent), xlab = "fitted", ylab = "resid")
      text(fitted(lmer.alt.1), resid(lmer.alt.1), labels = paste(df.temp$c.id, df.temp$i.sample.number.2))      # use this to label the points in the plot
      abline(0,0)
      plot(resid(lmer.alt.1) ~ lmer.alt.1@frame[,2], 
        main = paste((dimnames(lmer.alt.1@frame[2])[[2]]), ", Values x Residuals\n",
          "i.subset.c.id: ", i.subset.c.id, ", i.subset.unique.c.id: ", i.subset.unique.c.id))  
      abline(0,0)
  
  
        ################### parametric bootstrapping using simulation based on null model ###################################
    ######################################################################################
      par(mfrow = c(3,3))
    
      c.boot.type <- "boot.parametric"
      i.output.row <- 1
  
  print(" ********************  staring f.boot.parametric ********************") 
      df.test.stats <- replicate(i.replicates, f.boot.parametric(lmer.null, lmer.alt.1))
      rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")
  
      d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
      d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
      d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
        main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
          "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
          "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
          sep = ""))
      points(1 ~ d.lr.test.stats, pch = 8)
    
      d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
      d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
      d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
        main = paste(c.data.set.name, "\nd.fixef.pval: ", 
          signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
          signif(d.fixef.test.stats, digits = 5)))
      points(1 ~ d.fixef.test.stats, pch = 8)
    
      d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
      d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
      d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
        main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
      points(1 ~ d.t.test.stats, pch = 8)
  
      df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
        signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
        signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),         # returns the pval from the second enrty in the pval matrix, aka the first level of the first independent variable   
        d.lr.pval,  
        d.fixef.pval,  
        d.t.test.pval,  
        d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
        i.subset.c.id, i.subset.unique.c.id,
        i.subset.i.rank, i.subset.unique.i.rank,
        i.female, i.male,
        i.adult.female, i.adult.male,
        i.subadult.female, i.subadult.male,
        i.bado, i.lactating, i.pregnant,
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
  
      print(" ********************  staring f.boot.permutation ********************") 
      df.test.stats <- replicate(i.replicates, f.boot.permutation(lmer.null, lmer.alt.1))
      rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")
  
      d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
      d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
      d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
        main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
          "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
          "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
          sep = ""))
      points(1 ~ d.lr.test.stats, pch = 8)
    
      d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
      d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
      d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
        main = paste(c.data.set.name, "\nd.fixef.pval: ", 
          signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
          signif(d.fixef.test.stats, digits = 5)))
      points(1 ~ d.fixef.test.stats, pch = 8)
    
      d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
      d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
      d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
        main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
      points(1 ~ d.t.test.stats, pch = 8)
  
      df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
        signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
        signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),         # returns the pval from the second enrty in the pval matrix, aka the first level of the first independent variable   
        d.lr.pval,  
        d.fixef.pval,  
        d.t.test.pval,  
        d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
        i.subset.c.id, i.subset.unique.c.id,
        i.subset.i.rank, i.subset.unique.i.rank,
        i.female, i.male,
        i.adult.female, i.adult.male,
        i.subadult.female, i.subadult.male,
        i.bado, i.lactating, i.pregnant,
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
  
      print(" ********************  staring f.boot.replace ********************") 
      df.test.stats <- replicate(i.replicates, f.boot.replace(lmer.null, lmer.alt.1))
      rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")
  
      d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
      d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
      d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
        main = paste(c.boot.type, " of ", c.dependent, " ~ ", c.independent, 
          "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
          "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
          sep = ""))
      points(1 ~ d.lr.test.stats, pch = 8)
    
      d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
      d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
      d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
        main = paste(c.data.set.name, "\nd.fixef.pval: ", 
          signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
          signif(d.fixef.test.stats, digits = 5)))
      points(1 ~ d.fixef.test.stats, pch = 8)
    
      d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
      d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
      d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
      hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
        main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
      points(1 ~ d.t.test.stats, pch = 8)
  
      df.output.temp[i.output.row,] <- cbind(c.data.set.name, c.dependent, c.independent, c.boot.type, 
        signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 5), 
        signif(((cftest(lmer.alt.1))$test$pvalues)[2], digits = 5),         # returns the pval from the second enrty in the pval matrix, aka the first level of the first independent variable   
        d.lr.pval,  
        d.fixef.pval,  
        d.t.test.pval,  
        d.lr.test.stats, d.fixef.test.stats, d.t.test.stats, 
        i.subset.c.id, i.subset.unique.c.id,
        i.subset.i.rank, i.subset.unique.i.rank,
        i.female, i.male,
        i.adult.female, i.adult.male,
        i.subadult.female, i.subadult.male,
        i.bado, i.lactating, i.pregnant,
        d.lr.std.err, d.fixef.std.err, d.t.test.std.err,
        signif((vcov(lmer.alt.1)@factors$Cholesky)[1,1], digits = 5), 
        signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5),
        signif((vcov(lmer.alt.1)@factors$correlation)[2,1], digits = 5))
   
  ################### end of non-parametric bootstrap tests (resampling) ###################################
  ######################################################################################
  
      if(i.counter.1 == 1 & i.counter.2 == 1)  {
        df.output.final <- df.output.temp
      }
      else  {
        df.output.final <- rbind(df.output.final, df.output.temp)
      }
    }                        # end of inner loop for dependent variables
  }                        # end of outer loop for independent variables
  plot(1,1, main = paste("end of ", c.dependent, " ~ ", c.independent))
  df.output.final <- cbind(df.output.final, i.replicates)

  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".", c.data.set.name, ".csv", sep = ""))
  print("df.output.final.csv is written", sep = "")
}  



# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.multi.variable.model <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
  # Subsets data and removes potential outliers. For this subset to affect the output, the other subset command below needs to be commented out.
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & (i.sample.number.2 == 2 | i.sample.number.2 == 3)
#    | (c.id == "mp" & i.sample.number.2 == 4))
 
#  df.temp.full <- subset(df.temp, c.two.adult.samples == "yes" | c.three.adult.samples == "yes")
#  This gets all adult females, including the potential outlier MP. WND sample 1 is also included even though there is no rank for that sample
#  df.temp.full <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
#  df.temp.full <- subset(df.data.flat, c.sex == "f") # & c.age == "adult")
#  df.temp.full <- df.data.flat
  df.temp.full <- subset(df.data.flat)
  df.temp.full <- subset(df.data.flat, c.sex == "f")
#  c.data.set.name <- "full.set"
  c.data.set.name <- "full.female"  # Inside the loop we use !is.na to remove any records that have na values
#  c.data.set.name <- "gol.mp.removed"
#  c.data.set.name <- "full.female.male.cr.gol.mp.removed"

#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  

  # testing to see how subsetting out the tied values (e.g. bka.pm = 3) to see how the results change.
#  df.temp.full <- subset(df.temp.full, !(d.mic.90.level.bka.pm == 3))  
#  df.temp.full <- subset(df.temp.full, !(d.mic.90.level.bka.ec == 7))  

#  df.temp$d.age.months <- log(df.temp$d.age.months)  # Use log transformed d.age.months

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  
#  df.dummy <- data.frame(df.temp.full$c.id)
#  print(cbind(df.dummy, df.temp.full$i.rank))

#  c.independent <- "i.rank"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
  cv.independent.variables <- c("i.rank", "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females
#  cv.independent.variables <- c("c.sex") # c.sex can only be run with males and females
                                                                                   #  "c.age", 
  cv.dependent.variables <- c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm", 
    "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm")


  cv.colnames <- c("c.data.set.name", "c.dependent", "c.independent", "c.boot.type", 
    "chi.square.pval", "cftest.pval", 
    "d.lr.pval", "d.fixef.pval", "d.t.test.pval", 
    "d.lr.test.stats", "d.fixef.test.stats", "d.t.test.stats", 
    "i.subset.c.id", "i.subset.unique.c.id",
    "i.subset.i.rank", "i.subset.unique.i.rank",
    "i.female", "i.male",
    "i.adult.female", "i.adult.male",
    "i.subadult.female", "i.subadult.male",
    "i.bado", "i.lactating",
    "i.pregnant", "d.lr.std.err", "d.fixef.std.err", "d.t.test.std.err",
    "r", "r.squared", "corr.of.fixed.effects[2,1]")

  i.fixed.effect.levels <- 1          # use this constant to set the number of rows needed in the output data frame
  m.output.temp <- matrix(nrow = (3 * i.fixed.effect.levels), ncol = length(cv.colnames))
  colnames(m.output.temp) <- cv.colnames
  df.output.temp <- m.output.temp       # create the data frame for storing results while looping through dependend and independent variables

  i.replicates <- 1
  i.counter.1 <- 1
  i.counter.1.stop <- length(cv.independent.variables) 
  i.counter.1.stop <- 1 
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
  
      # We can only include records in the lmer models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the lmer.null and lmer.alt models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.independent]]))
      df.temp <- subset(df.temp, !is.na(df.temp[[c.dependent]]))

      c.independent <- "c.reproductive.status"
      if(c.independent == "c.reproductive.status")  {
        df.temp <- subset(df.temp, c.reproductive.status != "bado")
      }
  
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

      d.independent <- df.temp[[c.independent]]  
      d.dependent <- df.temp[[c.dependent]]
#      d.independent.rank <- rank(d.independent, ties.method = "random")  # random assignment of tied variables
#      d.dependent.rank <- rank(d.dependent, ties.method = "average")                                 # na.last = NA,
#      print(data.frame(cbind(as.character(df.temp$c.id), df.temp$i.rank, as.character(df.temp$c.sex), 
#          d.independent, d.independent.rank, d.dependent, d.dependent.rank)))
#      d.dependent <- d.dependent.rank        # re-assigning the rank of the depedent variables based on rank. This allow a non-parametric test in which ties are randomly assigned a rank

# x <- data.frame(cbind(df.temp$c.id, d.independent, d.dependent))
# z <- with(x, f.sort.frame(x, x[,2], x[,3], x[,1]))  
# print(z)

      lmer.null <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, REML = FALSE)
      lmer.alt.1 <- lmer(d.dependent ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.temp, REML = FALSE)
      
      print("summary(lmer.null))") 
      print(summary(lmer.null))
      print("summary(lmer.alt.1))") 
      print(summary(lmer.alt.1))
  
      # Computing a pval for the summary lmer.alt.1 model -> results are the same as cftest.  cftest reports at z value and z test p value because we are using a normal distribution to obtain the p value
  #    tval <- attributes(summary(lmer.alt.1))$coef[, 3]
  #    pval <- 2 * pnorm(abs(tval), lower = FALSE)
  #    print(cbind(attributes(summary(lmer.alt.1))$coef, `p value` = signif(pval, 5)))
  
      print("cftest(lmer.alt.1)")
      print(cftest(lmer.alt.1))     # computes p-vals for the lmer
  
      # ANOVA of models
      print("anova(lmer.null, lmer.alt.1, REML = FALSE)")
      print(anova(lmer.null, lmer.alt.1, REML = FALSE))
      
      # Compute the LR test stat for comparing to the bootstrap results
#      d.lr.test.stats <- signif(as.numeric(2*(logLik(lmer.alt.1, REML = FALSE)-logLik(lmer.null, REML = FALSE)), digits = 5))
#      # Store the independent variable effect sizes for comparing to the bootstrap results
#      d.fixef.test.stats <- signif(coef(summary(lmer.alt.1))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
#      # Store the t-stat for the independent for comparing to the bootstrap results
#      d.t.test.stats <- signif(coef(summary(lmer.alt.1))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
#      dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
#      print(dv.test.stats)
#      print(paste("number of rows in fixef: ", length(coef(summary(lmer.alt.1))[,1])))
#      print(paste("number of columns in fixef: ", length(coef(summary(lmer.alt.1))[1,])))
  
  
      # plotting lmer model 1
      i.plot.counter <- 1
      i.ranef.index <- length(lmer.alt.1@frame[1,])
      i.plot.counter.stop <- length(lmer.alt.1@frame[1,]) - 2  # subtract 2 because index 1 is dependent variable and last index is random effect

      par(mfrow = c(2, 3))
      hist(resid(lmer.alt.1), main = paste("Histogram of residuals - ", c.dependent))
#          "\nr.squared: ", signif(sqrt((vcov(lmer.alt.1)@factors$Cholesky)[1,1]), digits = 5))) 
      lines(density(resid(lmer.alt.1)))                                     # 
      qqnorm(resid(lmer.alt.1), main = paste("qqnorm\n", "\nanova pval: ", signif((anova(lmer.null, lmer.alt.1))$"Pr(>Chisq)"[2], digits = 4)))
      qqline(resid(lmer.alt.1))
      plot(fitted(lmer.alt.1), resid(lmer.alt.1), main = paste("fitted x resid\n", c.dependent), xlab = "fitted", ylab = "resid")
      text(fitted(lmer.alt.1), resid(lmer.alt.1), labels = lmer.alt.1@frame[, i.ranef.index])      # use this to label the points in the plot
      abline(0,0)
      
      # using i.counter.stop to index the random effects in the model output
      plot(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[, i.ranef.index], 
        main = paste((dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[i.ranef.index])[[2]]), 
          "\ncftest pval: ", signif(((cftest(lmer.alt.1))$test$pvalues)[i.ranef.index], digits = 4),   # ["d.independent"], digits = 4), 
          sep = ""), 
        xlab = dimnames(lmer.alt.1@frame[i.ranef.index])[[2]], ylab = dimnames(lmer.alt.1@frame[1])[[2]])
#          xlim = c(0, 1.1 * max(as.numeric(lmer.alt.1@frame[, i.index]))))
      text(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[, i.ranef.index], labels = lmer.alt.1@frame[, i.ranef.index], pos = 4, cex = 1)
      if(is.double(lmer.alt.1@frame[, i.ranef.index])== TRUE)  {
        abline(a = (fixef(lmer.alt.1))[[1]], b = (fixef(lmer.alt.1))[[i.ranef.index]])    # a = intercept, b = slope (coefficient)
      }  

      plot(resid(lmer.alt.1) ~ lmer.alt.1@frame[, i.ranef.index], 
        main = paste((dimnames(lmer.alt.1@frame[i.ranef.index])[[2]]), ", Values x Residuals\n",
          "i.subset.c.id: ", i.subset.c.id, ", i.subset.unique.c.id: ", i.subset.unique.c.id),
        xlab = dimnames(lmer.alt.1@frame[i.ranef.index])[[2]])  
      abline(0,0)

      par(mfcol = c(2, 4))
      ###################  loop through the independent variables and make diagnostic plots ################333
      for(i.plot.counter in i.plot.counter:i.plot.counter.stop)  {
        i.index <- i.plot.counter + 1   # We want to plot each independent variable, so we skip the first index, because that contains the dependent variable    

        plot(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[, i.index], 
          main = paste((dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[i.index])[[2]]), 
            "\ncftest pval: ", signif(((cftest(lmer.alt.1))$test$pvalues)[i.index], digits = 4),   # ["d.independent"], digits = 4), 
            sep = ""), 
          xlab = dimnames(lmer.alt.1@frame[i.index])[[2]], ylab = dimnames(lmer.alt.1@frame[1])[[2]])
#          xlim = c(0, 1.1 * max(as.numeric(lmer.alt.1@frame[, i.index]))))
        text(lmer.alt.1@frame[,1] ~ lmer.alt.1@frame[, i.index], labels = lmer.alt.1@frame[, i.ranef.index], pos = 4, cex = 1)
        if(is.double(lmer.alt.1@frame[, i.index])== TRUE)  {
          abline(a = (fixef(lmer.alt.1))[[1]], b = (fixef(lmer.alt.1))[[i.index]])    # a = intercept, b = slope (coefficient)
        }  

        plot(resid(lmer.alt.1) ~ lmer.alt.1@frame[, i.index], 
          main = paste((dimnames(lmer.alt.1@frame[i.index])[[2]]), ", Values x Residuals\n",
            "i.subset.c.id: ", i.subset.c.id, ", i.subset.unique.c.id: ", i.subset.unique.c.id),
          xlab = dimnames(lmer.alt.1@frame[i.index])[[2]])  
        text(resid(lmer.alt.1) ~ lmer.alt.1@frame[, i.index], labels = lmer.alt.1@frame[, i.ranef.index],  pos = 4)      # use this to label the points in the plot
        abline(0,0)
      }
    }
  }                        # end of inner loop for dependent variables
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  



# source("f.ggplot.single.variable.bka.rankr")

f.ggplot.single.variable.bka <- function()
{
  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1.75, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0, face = "bold"),
					axis.text.y =       theme_text(size = base_size * 1.75, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5, face = "bold"),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 2.25, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 2.25, angle = 90, vjust = 0, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 0.8),
					legend.title =      theme_text(size = base_size * 1, hjust = 0, face = "bold"),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = NA, colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size * 2.5, face = "bold"),
					strip.text.y =      theme_text(size = base_size * 2.5, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size * 2, face = "bold"), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(1, 10, 1, 10), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }
  
  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  

  df.ec <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", "c.strain.bka.ec", "d.mic.90.bka.ec")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.mic.90 <- log(df.ec$d.mic.90.bka.ec)
  df.ec <- subset(df.ec, select = c("c.id", "i.rank", "d.sample.date", "c.strain", "d.mic.90"))

  df.pm <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", "c.strain.bka.pm", "d.mic.90.bka.pm")]
  df.pm$c.strain <- "Proteus mirabilis"
  df.pm$d.mic.90 <- log(df.pm$d.mic.90.bka.pm)
  df.pm <- subset(df.pm, select = c("c.id", "i.rank", "d.sample.date", "c.strain", "d.mic.90"))

  lmer.null.ec <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.alt.1.ec <- lmer(d.mic.90 ~ i.rank + (1 | c.id), data = df.ec, REML = FALSE)
  print(cftest(lmer.alt.1.ec)) 
  print("summary(lmer.alt.1.ec))") 
  print(summary(lmer.alt.1.ec))

  lmer.null.pm <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.alt.1.pm <- lmer(d.mic.90 ~ i.rank + (1 | c.id), data = df.pm, REML = FALSE)
  print(cftest(lmer.alt.1.pm)) 
  print("summary(lmer.alt.1.pm))") 
  print(summary(lmer.alt.1.pm))

  par(mfrow = c(3, 3))
  plot(lmer.alt.1.ec@frame[,1] ~ lmer.alt.1.ec@frame[,2], 
    main = paste((dimnames(lmer.alt.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.alt.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.null.ec, lmer.alt.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.alt.1.ec@frame[,1] ~ lmer.alt.1.ec@frame[,2], labels = paste(df.ec$c.id, df.ec$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.alt.1.ec))[[1]], b = (fixef(lmer.alt.1.ec))[[2]])    # a = intercept, b = slope (coefficient)

  df.ec$d.intercept <- fixef(lmer.alt.1.ec)[[1]]
  df.ec$d.slope <- fixef(lmer.alt.1.ec)[[2]]
  df.pm$d.intercept <- fixef(lmer.alt.1.pm)[[1]]
  df.pm$d.slope <- fixef(lmer.alt.1.pm)[[2]]

  df.temp <- rbind(df.ec, df.pm)

  gg.rank <- ggplot(data = df.temp, 
    aes(x = i.rank, 
        y = d.mic.90)
    )
     
  print(gg.rank
    + geom_point(aes(x = i.rank))
    + geom_abline(aes(intercept = d.intercept, slope = d.slope), size = 1.5) #, data = df.temp)
    + xlab("Rank")
    + ylab("log ( MIC )")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  
   
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}



# source("f.calc.inhibition.area.under.curve.r")
# This function uses "sintegral" to calculate the area under the dose-reponse curve for the control and an individual sample
#  The area under the sample curve is then subtracted from the control curve to yeild the proportion inhibition for the sample and this values is returned
f.calc.inhibition.area.under.curve <- function(df.input, ...)
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()

#  if(is.null(df.input[1,1])) { return(NA)  }
#  if(is.na(df.input[1,1])) { return(NA)  }
#  df.sample <- subset(df.input, c.id == c.name & i.sample.number == i.sample & c.strain == c.bacteria)
#  print(df.sample)

# This code is used to get the means of the replicates; it is not needed because the output of the function "sintegral" is the same using the raw values and the means of the replicates
#  df.sample.means <- aggregate(d.percent.control.abs
#  ~ i.sample.dilution, FUN = mean, data = df.sample)
#  print(df.sample.means)

#  d.area.control <- sintegral(x = log2(df.sample.means$i.sample.dilution), fx = rep(1, 8), n.pts = 1000)
#  d.area.1000 <- sintegral(x = log2(df.sample.means$i.sample.dilution), fx = df.sample.means$d.percent.control.abs, n.pts = 1000)
#  print(d.area.control$value)
#  print(d.area.1000$value)
   
  d.area.control <- sintegral(x = log2(df.input$i.sample.dilution), fx = rep(1, length(df.input$d.percent.control.abs), ret = FALSE), n.pts = 1000)$value
  d.area.sample <- sintegral(x = log2(df.input$i.sample.dilution), fx = (1 - df.input$d.percent.control.abs), n.pts = 1000, ret = FALSE)$value
  d.sintegral.mic <- (d.area.control - d.area.sample)       # value is the estimated area under the curve
  d.sintegral.mic <- d.area.sample       # value is the estimated area under the curve
  d.prop.inhibition <- (d.sintegral.mic / d.area.control)       # calculate the proportion of growth inhibited

#  d.area.control <- sintegral(x = log2(df.input$i.sample.dilution), fx = rep(1, length(df.input$d.percent.control.abs)), n.pts = 1000)
#  d.area.sample <- sintegral(x = log2(df.input$i.sample.dilution), fx = df.input$d.percent.control.abs, n.pts = 1000)
#  d.inhibition <- (d.area.control$value - d.area.sample$value)       # value is the estimated area under the curve
#  d.prop.inhibition <- (d.inhibition / d.area.control$value)       # calculate the proportion of growth inhibited
  if(d.sintegral.mic > 7) {        d.prop.inhibition <- 7      }
  if(d.sintegral.mic < 0) {        d.prop.inhibition <- 0      }

  print(d.sintegral.mic)
  d.sintegral.mic
}


# source("f.run.calc.inhibition.area.under.curve.r")
# Calls the function f.run.calc.inhibition.area.under.curve to find the area under the curve of i.sample.dilution x d.percent.control.abs
# df.input.list should be the data frame that contains all the  samples (i.e. c.id = 104, i.sample.number = 104
# df.input.raw is the full bka data
# *** df.input.list needs to contain the data from only one bacterial strain, so this function needs to be run twice to add the new variable to the main data frame for each strain
f.run.calc.inhibition.area.under.curve <- function(df.input.list, df.input.raw, ...)
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()

  i.number.samples <- length(df.input.list[,1])  # Get the number of records in the dataframe
  print(i.number.samples)
  d.sintegral.mic <- rep(NA, i.number.samples)
  i.counter <- 1
  for(i.counter in i.counter:i.number.samples)
  {
    print(i.counter)
    print(as.character(df.input.list[i.counter, "c.id"]))
    df.sample <- subset(df.input.raw, c.id == as.character(df.input.list[i.counter, "c.id"])
      & i.sample.number == as.character(df.input.list[i.counter, "i.sample.number"]))
    d.sintegral.mic[i.counter] <- f.calc.inhibition.area.under.curve(df.sample)
  }
  d.sintegral.mic
}


# source("romr.fnc.r")
# This functions replaces the romr.fnc function in the LMERconveniencefunctions package. 
# The original function returns an empty data frame when none of the rstands exceed the trim value

romr.fnc <- function (model, data, trim = 2.5)
{
    data$rstand = as.vector(scale(resid(model)))
    row.names(data) = 1:nrow(data)
    outliers = as.numeric(row.names(data[abs(data$rstand) > trim, ]))
    data0 = data
    if(length(outliers) > 0)
    {
      data = data[-outliers, , drop = TRUE]
    }
    cat("n.removed =", (nrow(data0) - nrow(data)), "\n")
    cat("percent.removed =", (nrow(data0) - nrow(data))/nrow(data0) *
        100, "\n")
    return(list(data = data, data0 = data0, n.removed = nrow(data0) -
        nrow(data), percent.removed = (nrow(data0) - nrow(data))/nrow(data0) *
        100))
}

f.remove.outliers <- function (df.data, c.variable, c.transform = NULL, trim = 3)
{
  if(!is.null(c.transform))
  {
    df.data[[c.variable]] = eval(call(c.transform, df.data[[c.variable]]))
  }
  df.data$z.score = as.vector(scale(df.data[[c.variable]]))
  row.names(df.data$model) = 1:nrow(df.data$model)
  outliers = as.numeric(row.names(df.data$model[abs(df.data$z.score) > trim, ]))
  df.data0 = df.data
  if(length(outliers) > 0)
  {
    df.data$model = df.data$model[-outliers, ] #, drop = TRUE]
  }
  cat("n.removed =", (nrow(df.data0$model) - nrow(df.data$model)), "\n")
  cat("percent.removed =", (nrow(df.data0$model) - nrow(df.data$model))/nrow(df.data0$model) * 100, "\n")
  return(list(df.data = df.data$model, df.data0 = df.data0$model, n.removed = nrow(df.data0$model) -
      nrow(df.data$model), percent.removed = (nrow(df.data0$model) - nrow(df.data$model))/nrow(df.data0$model) * 100))
}



vif.mer <- function (fit) {
    ## adapted from rms::vif

    v <- vcov(fit)
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
    X <- fit@X
    nam <- names(fixef(fit))

    ## exclude intercepts
    nrp <- sum(1 * (nam == "(Intercept)"))
    if (nrp > 0) {
        X <- X[, -(1:nrp), drop = FALSE]
        nam <- nam[-(1:nrp)]
    }

    if (add.intercept) {
        X <- cbind(rep(1), scale(X, scale = scale, center = center))
        kappa(X, exact = exact)
    } else {
        kappa(scale(X, scale = scale, center = scale), exact = exact)
    }
}

colldiag.mer <- function (fit,
                          scale = TRUE, center = FALSE,
                          add.intercept = TRUE) {
    ## adapted from perturb::colldiag, method in Belsley, Kuh, and
    ## Welsch (1980).  look for a high condition index (> 30) with
    ## more than one high variance propotion.  see ?colldiag for more
    ## tips.
    result <- NULL
    if (center)
        add.intercept <- FALSE
    if (is.matrix(fit) || is.data.frame(fit)) {
        X <- as.matrix(fit)
        nms <- colnames(fit)
    }
    else if (class(fit) == "mer") {
        nms <- names(fixef(fit))
        X <- fit@X
        if (any(grepl("(Intercept)", nms))) {
            add.intercept <- FALSE
        }
    }
    X <- X[!is.na(apply(X, 1, all)), ]

    if (add.intercept) {
        X <- cbind(1, X)
        colnames(X)[1] <- "(Intercept)"
    }
    X <- scale(X, scale = scale, center = center)

    svdX <- svd(X)
    svdX$d
    condindx <- max(svdX$d)/svdX$d
    dim(condindx) <- c(length(condindx), 1)

    Phi = svdX$v %*% diag(1/svdX$d)
    Phi <- t(Phi^2)
    pi <- prop.table(Phi, 2)
    colnames(condindx) <- "cond.index"
    if (!is.null(nms)) {
        rownames(condindx) <- nms
        colnames(pi) <- nms
        rownames(pi) <- nms
    } else {
        rownames(condindx) <- 1:length(condindx)
        colnames(pi) <- 1:ncol(pi)
        rownames(pi) <- 1:nrow(pi)
    }

    result <- data.frame(cbind(condindx, pi))
    zapsmall(result)
}

maxcorr.mer <- function (fit,
                         exclude.intercept = TRUE) {
    so <- summary(fit)
    corF <- so@vcov@factors$correlation
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0 & exclude.intercept) {
        corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }
    corF[!lower.tri(corF)] <- 0
    maxCor <- max(corF)
    minCor <- min(corF)
    if (abs(maxCor) > abs(minCor)) {
        zapsmall(maxCor)
    } else {
        zapsmall(minCor)
    }
}

