# source("f.clmm.prey.density.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.clmm.prey.density <- function(df.input, ...)   
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
#  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.2.level
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

  cv.independent.variables <- c("d.prey.density") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females

  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm") 


#  c.links <- c("probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "Aranda-Ordaz", "log-gamma")
#  c.links <- c("probit", "cloglog", "logit") #, "log-gamma") #, "loglog", )
  c.links <- c("logit") #, "log-gamma") #, "loglog", )

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
  
      # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
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

      ##############################  The data used in the clm and clmm calls need to be a global varialbe or the accessory function  ###############      
      ############################################################################################################################################
      ############################################################################################################################################
      ############################################################################################################################################
      df.temp <<- df.temp
            
      print("")
      print(" ******* starting clmm models **************************")
 
      for(i in 1:length(c.links))
      {
        print(paste("*****************************", c.links[i], "************************************"))      
        clm.prey.1 <- clm(d.dependent.factor ~ log10(d.prey.density), data = df.temp, Hess = TRUE, link = c.links[i], 
          clm.control = list(method = "ucminf", maxIter = 200, maxLineIter = 200, gradTol = 1e-6),
          threshold = "flexible", nAGQ = 10)
        clmm.prey.1 <- clmm(d.dependent.factor ~ log10(d.prey.density) + (1 | c.id), data = df.temp, Hess = TRUE, link = c.links[i], 
#          clmm.control = list(method = "ucminf", maxIter = 200, maxLineIter = 200, gradTol = 1e-6),
          threshold = "flexible", nAGQ = 10) 
        print(summary(clm.prey.1))
        print(summary(clmm.prey.1))
        print(anova(clm.prey.1, clmm.prey.1))
      }

      for(i in 1:length(c.links))
      {
        print(paste("*****************************", c.links[i], "************************************"))      
        clm.prey.1 <- clm(d.dependent.factor ~ c.prey.density.2.level, data = df.temp, Hess = TRUE, #link = c.links[i], 
          threshold = "flexible", nAGQ = 10)
        clmm.prey.1 <- clmm(d.dependent.factor ~ c.prey.density.2.level + (1 | c.id), data = df.temp, Hess = TRUE, # link = c.links[i], 
          threshold = "flexible", nAGQ = 10)

        print("coef(clm.prey.1)")
        print(coef(clm.prey.1))
        print("vcov(clm.prey.1)")
        print(vcov(clm.prey.1))
        print("extractAIC(clm.prey.1)")
        print(extractAIC(clm.prey.1))
        print("logLik(clm.prey.1)")
        print(logLik(clm.prey.1))

        print("confint(clm.prey.1)") ## type = "profile"
        print(confint(clm.prey.1)) ## type = "profile"
        print("confint(clm.prey.1, type = Wald)")
        print(confint(clm.prey.1, type = "Wald"))
        print("profile generation")
        pr.clm.prey.1 <- profile(clm.prey.1)
        plot(pr.clm.prey.1, root = TRUE) ## check for linearity 
        print("confint(pr.clm.prey.1)")
        print(confint(pr.clm.prey.1))
        pr.clm.prey.1 <- profile(clm.prey.1)
        plot(pr.clm.prey.1, approx = TRUE) ## check for linearity 
        print("confint(pr.clm.prey.1)")
        print(confint(pr.clm.prey.1))
        
        print(summary(clm.prey.1))
        print(summary(clmm.prey.1))
        print(anova(clm.prey.1, clmm.prey.1))
                
        par(mfrow = c(3, 3))
        slice.clm.prey.1 <- slice(clm.prey.1)
        plot(slice.clm.prey.1)
#        par(mfrow = c(3, 3))
#        slice.clmm.prey.1 <- slice(clmm.prey.1)
#        plot(slice.clmm.prey.1)
        
      }
      

      print("")
      print(" ******* starting lmer models **************************")
      lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp)
      lmer.1 <- lmer(d.dependent ~ d.prey.density + (1 | c.id), data = df.temp)
      lmer.2 <- lmer(d.dependent ~ c.prey.density.2.level + (1 | c.id), data = df.temp)

#  t.aicc <- (aictab(cand.set = list(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, lmer.date.prey.1 
#    ), 
#    modnames=c("lmer.0", "lmer.1", "lmer.age", "lmer.prey", "lmer.date.prey.1"), 
#    sort = TRUE))

#    print("t.aicc")
#    print(t.aicc)
#    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))
      
      par(mfrow = c(3,3))


      plot(jitter(as.double(d.dependent)) ~ jitter(as.double(d.prey.density)), data = df.temp, main = paste(c.dependent, " ~ ", "d.prey.density"))
      plot(d.dependent ~ log10(as.double(d.prey.density)), data = df.temp, main = "d.dependent ~ log10(d.prey.density)") 
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
