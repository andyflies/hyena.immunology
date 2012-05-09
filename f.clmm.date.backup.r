# source("f.clmm.date.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.clmm.date <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status", 
    "c.prey.density.2.level", "c.prey.density.3.level",
    "i.rank", "d.age.months",  
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.prey.density",
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.c", "d.t", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
  df.temp.full$i.sample.year <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
  print(summary(df.temp.full$i.sample.year))
  df.temp.full$i.sample.year <- df.temp.full$i.sample.year - min(df.temp.full$i.sample.year) + 1
  print(summary(df.temp.full$i.sample.year))
#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  

  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
  print(summary(df.temp.full$i.sample.days))
  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)-min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
  print(summary(df.temp.full$i.sample.days))

  df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero
  print((df.temp.full$i.sample.months))

#  df.temp.full$d.prey.density <- log((df.temp.full$d.prey.low.density.prior + df.temp.full$d.prey.low.density.current) / 2) 
#  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.3.level
  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.2.level
  df.temp.full$d.prey.density <- log10(df.temp.full$d.prey.density) 
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  

  cv.independent.variables <- c("d.sample.date") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females

  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.bka.ec") 
#  cv.dependent.variables <- c("d.mic.90.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "c.mic.median.level.ec", "d.mic.90.median.bka.pm", "c.mic.median.level.pm") 


#  c.links <- c("probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "Aranda-Ordaz", "log-gamma")
#  c.links <- c("probit", "cloglog", "logistic") #, "log-gamma") #, "loglog", )
  c.links <- c("logistic") #, "log-gamma") #, "loglog", )

  par(mfrow = c(2,3))

  i.fixed.effect.levels <- 1          # use this constant to set the number of rows needed in the output data frame

  i.replicates <- 1
  i.counter.1 <- 1
  i.counter.1.stop <- length(cv.independent.variables) 
  for(i.counter.1 in i.counter.1:i.counter.1.stop)  {

    c.independent <- cv.independent.variables[i.counter.1]

    i.counter.2 <- 1
    i.counter.2.stop <- length(cv.dependent.variables) 
#    for(i.counter.2 in 1:cv.dependent.variables)  {
    for(i.counter.2 in i.counter.2:i.counter.2.stop)  {
  
      c.dependent <- cv.dependent.variables[i.counter.2]
  
      print(paste(" **************** c.independent: ", c.independent, "**********************"))   
      print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
   
      print("Before subsetting within the loop.")
      i.full.c.id <- length(df.temp.full$c.id)
      i.full.unique.c.id <- length(unique(df.temp.full$c.id))
  
      # We can only include records in the clmm2 models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm2.0 and clmm2. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.independent]]))
      df.temp <- subset(df.temp, !is.na(df.temp[[c.dependent]]))
  
      df.temp$d.independent <- df.temp[[c.independent]]  
      df.temp$d.dependent <- log2(df.temp[[c.dependent]]) # log transform the response for the lmer models
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      df.temp$d.dependent.factor <- ordered(df.temp[[c.dependent]])    # clm and clmm models need to have factors for response variables  
#     df.temp$d.dependent.factor <- factor(log2(df.temp[[c.dependent]]))    # clm and clmm models need to have factors for response variables  
      print("summary(df.temp$d.dependent.factor)")
      print(summary(df.temp$d.dependent.factor))
      print("summary(df.temp$d.independent)")
      print(summary(df.temp$d.independent))
            
      print("")
      print(" ******* starting clmm2 models **************************")
# m2 <- update(m1, link = "probit")
      for(i in 1:length(c.links))
      {
        print(paste("*****************************", c.links[i], "************************************"))      
        clm2.months <- clm2(d.dependent.factor ~ log10(i.sample.months), data = df.temp, Hess = TRUE, link = c.links[i], method = "ucminf",
          threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
        clmm2.months <- clmm2(d.dependent.factor ~ log10(i.sample.months), random = c.id, data = df.temp, Hess = TRUE, link = c.links[i], method = "ucminf", 
          threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
        print("clm2.months$convergence")
        print(clm2.months$convergence)
        print("clmm2.months$convergence")
        print(clmm2.months$convergence)
        print(summary(clm2.months))
        print(summary(clmm2.months))
        print(anova(clm2.months, clmm2.months))
      }

      for(i in 1:length(c.links))
      {
        print(paste("*****************************", c.links[i], "************************************"))      
        clm2.days <- clm2(d.dependent.factor ~ log10(i.sample.days), data = df.temp, Hess = TRUE, link = c.links[i], 
          threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
        clmm2.days <- clmm2(d.dependent.factor ~ log10(i.sample.days), random = c.id, data = df.temp, Hess = TRUE, link = c.links[i], 
          threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
        print("clm2.days$convergence")
        print(clm2.days$convergence)
        print("clmm2.days$convergence")
        print(clmm2.days$convergence)
        print(summary(clm2.days))
        print(summary(clmm2.days))
        print(anova(clm2.days, clmm2.days))
      }
      
#      for(i in 1:length(c.links))
#      {
#        print(paste("*****************************", c.links[i], "************************************"))      
#        clm2.year <- clm2(d.dependent.factor ~ (i.sample.year), data = df.temp, Hess = TRUE, link = c.links[i], 
#          threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
#        clmm2.year <- clmm2(d.dependent.factor ~ (i.sample.year), random = c.id, data = df.temp, Hess = TRUE, link = c.links[i], 
#          threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
#        print("clm2.year$convergence")
#        print(clm2.year$convergence)
#        print("clmm2.year$convergence")
#        print(clmm2.year$convergence)
#        print(summary(clm2.year))
#        print(summary(clmm2.year))
#        print(anova(clm2.year, clmm2.year))
#      }

#      clm2.date <- clm2(d.dependent.factor ~ d.independent, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.date <- clmm2(d.dependent.factor ~ d.independent, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.date))
#      print(summary(clmm2.date))
#      print(anova(clm2.date, clmm2.date))

#      clm2.date <- clm2(d.dependent.factor ~ log10(as.double(d.independent)), data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.date <- clmm2(d.dependent.factor ~ log10(as.double(d.independent)), random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.date))
#      print(summary(clmm2.date))
#      print(anova(clm2.date, clmm2.date))

#      clm2.no.random <- clm2(d.dependent.factor ~ 1, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.random <- clmm2(d.dependent.factor ~ 1, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.no.random))
#      print(summary(clmm2.random))
#      print(anova(clm2.no.random, clmm2.random))

#      clm2.age <- clm2(d.dependent.factor ~ d.age.months, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.age <- clmm2(d.dependent.factor ~ d.age.months, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.age))
#      print(summary(clmm2.age))
#      print(anova(clm2.age, clmm2.age))
      
#      clm2.prey <- clm2(d.dependent.factor ~ d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.prey <- clmm2(d.dependent.factor ~ d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.prey))
#      print(summary(clmm2.prey))
#      print(anova(clm2.prey, clmm2.prey))

#      clm2.date.prey.1 <- clm2(d.dependent.factor ~ d.independent + d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible")
#      clmm2.date.prey.1 <- clmm2(d.dependent.factor ~ d.independent + d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      print(summary(clm2.date.prey.1))
#      print(summary(clmm2.date.prey.1))
#      print(anova(clm2.date.prey.1, clmm2.date.prey.1))

      print("")
      print(" ******* starting lmer models **************************")
      lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp)
      lmer.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp)
      lmer.age <- lmer(d.dependent ~ d.age.months + (1 | c.id), data = df.temp)
#      lmer.prey <- lmer(d.dependent ~ d.prey.density + (1 | c.id), data = df.temp)
#      lmer.date.prey.1 <- lmer(d.dependent ~ d.independent + d.prey.density + (1 | c.id), data = df.temp)

#      print(anova(clmm2.random, clmm2.date, clmm2.age))         

         
#    print(anova(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, 
#      lmer.date.rank.1, lmer.date.prey.1, lmer.date.repro.1, 
#      lmer.rank.prey.1, lmer.rank.prey.2,
#      lmer.rank.age.1, lmer.rank.repro.1, lmer.rank.repro.2, 
#      lmer.date.rank.prey.1, lmer.date.rank.prey.2, lmer.date.rank.age.1, lmer.date.rank.repro.1, lmer.date.rank.repro.2,
#      lmer.rank.prey.repro.1, lmer.rank.prey.repro.2,
#      lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1,
#      lmer.date.rank.prey.repro.1, lmer.date.rank.prey.repro.2, lmer.date.rank.prey.repro.12, lmer.date.rank.prey.repro.22, lmer.date.rank.age.repro.1
#    ))         

#  t.aicc <- (aictab(cand.set = list(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, lmer.date.prey.1 
#    ), 
#    modnames=c("lmer.0", "lmer.1", "lmer.age", "lmer.prey", "lmer.date.prey.1"), 
#    sort = TRUE))

#    print("t.aicc")
#    print(t.aicc)
#    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))
      
      par(mfrow = c(3,3))
         

# pr.clmm2.date <- profile(clmm2.date)
# confint(pr.clmm2.date)
# par(mfrow = c(2,2))
# plot(pr.clmm2.date)
# plot(pr.clmm2.date, Log=TRUE, relative = TRUE)
# plot(pr.clmm2.date, Log=TRUE, relative = FALSE)
# par(mfrow=c(1,1))
# plot(1,1)

      plot(jitter(as.double(d.dependent)) ~ jitter(as.double(i.sample.days)), data = df.temp, main = paste(c.dependent, " ~ ", "i.sample.year"))
      plot(jitter(as.double(d.dependent)) ~ jitter(as.double(i.sample.year)), data = df.temp, main = paste(c.dependent, " ~ ", "i.sample.year"))
      plot(jitter(as.double(d.dependent)) ~ d.independent, data = df.temp, main = paste(c.dependent, " ~ ", c.independent))
      plot(d.dependent ~ d.sample.date, data = df.temp, main = "d.dependent ~ d.sample.date") 
      plot(d.dependent ~ log10(as.double(d.sample.date)), data = df.temp, main = "d.dependent ~ log10(d.sample.date)") 
      plot(d.dependent ~ d.prey.density, data = df.temp, main = "d.dependent ~ d.prey.density") 
      plot(d.dependent ~ d.age.months, data = df.temp, main = "d.dependent ~ d.age.months") 
      plot(d.dependent ~ d.total.igg, data = df.temp, main = "d.dependent ~ d.total.igg")
      plot(d.dependent ~ d.total.igm, data = df.temp, main = "d.dependent ~ d.total.igm")
      plot(d.dependent ~ d.ec.igg, data = df.temp, main = "d.dependent ~ d.ec.igg")
      plot(d.dependent ~ d.pm.igg, data = df.temp, main = "d.dependent ~ d.pm.igg")
      plot(d.dependent ~ d.ec.igm, data = df.temp, main = "d.dependent ~ d.ec.igm")
      plot(d.dependent ~ d.pm.igm, data = df.temp, main = "d.dependent ~ d.pm.igm")
    }
  }
#  m.cor <- cor(df.temp[, 8:24], use = "pairwise.complete.obs", method = c("spearman"))
#  write.csv(m.cor, file = "temp.cor.csv")
  

  par(mfrow = c(1,1))
  plot(1,1, main = paste("space filler"))
#  df.output.final <- cbind(df.output.final, i.replicates)
#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  
