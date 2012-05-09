# source("f.clmm.models.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.clmm.models <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "c.reproductive.status", "i.rank", "d.age.months",  
    "d.mic.90.bka.ec", "d.mic.90.bka.pm",
    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.c", "d.t", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
#  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
#  print(sort(df.temp.full$d.sample.date))
#  df.temp.full <- subset(df.temp.full, !(c.id == "mrph" & i.sample.number.2 == 3))  
#  Remove MP sample 1 to see how this affects the results.  It is a possible outlier
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.full <- subset(df.temp.full, c.sex == "f" & d.age.months >= 24)
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  


  df.temp.full$d.prey.density <- log((df.temp.full$d.prey.low.density.prior + df.temp.full$d.prey.low.density.current) / 2) 
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  
#  print(cbind(df.temp.full$c.id, df.temp.full$d.mic.90.bka.ec, log2(df.temp.full$d.mic.90.bka.ec), df.temp.full$d.mic.90.level.bka.ec,  
#    df.temp.full$d.mic.90.bka.pm, log2(df.temp.full$d.mic.90.bka.pm), df.temp.full$d.mic.90.level.bka.pm)) 

#  df.temp.full$d.sample.date <- (df.temp.full$d.sample.date - min(df.temp.full$d.sample.date))

#  c.independent <- "i.rank"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
  cv.independent.variables <- c("d.sample.date") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females
#  cv.independent.variables <- c("i.rank") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females

#  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
  cv.dependent.variables <- c("d.mic.90.bka.ec") #, "d.mic.90.bka.pm") 
# cv.dependent.variables <- c("d.mic.90.bka.pm") 
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
#    for(i.counter.2 in 1:cv.dependent.variables)  {
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
  
      df.temp$d.independent <- df.temp[[c.independent]]  
      df.temp$d.dependent <- log2(df.temp[[c.dependent]])
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      df.temp$d.dependent.factor <- factor(df.temp[[c.dependent]])      
      print("summary(df.temp$d.dependent.factor)")
      print(summary(df.temp$d.dependent.factor))
      print("summary(df.temp$d.independent)")
      print(summary(df.temp$d.independent))

      print("")
      print(" ******* starting clmm models **************************")
#      clmm.0 <- clmm(d.dependent.factor ~ 1 + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #,trace = 1, innerCtrl = "noWarn")
print("test1")
      clmm.1 <- clmm(d.dependent.factor ~ d.independent + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
print("test2")
      clmm.rank <- clmm(d.dependent.factor ~ i.rank + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
print("test3")
      clmm.repro <- clmm(d.dependent.factor ~ c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
print("test5")
      clmm.prey <- clmm(d.dependent.factor ~ d.prey.density + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") #, link = "logit") #, nAGQ = 10, maxIter = 200, maxLineIter = 200) #, trace = 1, innerCtrl = "noWarn")
print("test6")
      clmm.date.rank.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
print("test8")
      clmm.date.prey.1 <- clmm(d.dependent.factor ~ d.independent + d.prey.density + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
print("test10")
      clmm.date.repro.1 <- clmm(d.dependent.factor ~ d.independent + c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
print("test11")

      clmm.rank.prey.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + d.prey.density + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.prey.2 <- clmm(d.dependent.factor ~ d.independent + i.rank * d.prey.density + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.age.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + d.age.months + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.repro.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.repro.2 <- clmm(d.dependent.factor ~ d.independent + i.rank * c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
print("test13")
      clmm.rank.prey.repro.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + d.prey.density + c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.prey.repro.2 <- clmm(d.dependent.factor ~ d.independent + i.rank * d.prey.density +  c.reproductive.status +  (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.prey.repro.12 <- clmm(d.dependent.factor ~ d.independent + i.rank + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.prey.repro.22 <- clmm(d.dependent.factor ~ d.independent + i.rank * d.prey.density + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 
      clmm.rank.age.repro.1 <- clmm(d.dependent.factor ~ d.independent + i.rank + d.age.months +  c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", threshold = "flexible") 

      print("")
      print(" ******* starting lmer models **************************")
      lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp)
      lmer.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp)
      lmer.rank <- lmer(d.dependent ~ i.rank + (1 | c.id), data = df.temp)
      lmer.repro <- lmer(d.dependent ~ c.reproductive.status + (1 | c.id), data = df.temp)
      lmer.age <- lmer(d.dependent ~ d.age.months + (1 | c.id), data = df.temp)
      lmer.prey <- lmer(d.dependent ~ d.prey.density + (1 | c.id), data = df.temp)
print("test20")

      lmer.date.rank.1 <- lmer(d.dependent ~ d.independent + i.rank + (1 | c.id), data = df.temp)
      lmer.date.prey.1 <- lmer(d.dependent ~ d.independent + d.prey.density + (1 | c.id), data = df.temp)
      lmer.date.repro.1 <- lmer(d.dependent ~ d.independent + c.reproductive.status + (1 | c.id), data = df.temp)
print("test25")

      lmer.rank.prey.1 <- lmer(d.dependent ~ d.independent + i.rank + d.prey.density + (1 | c.id), data = df.temp)
      lmer.rank.prey.2 <- lmer(d.dependent ~ d.independent + i.rank * d.prey.density + (1 | c.id), data = df.temp)
      lmer.rank.age.1 <- lmer(d.dependent ~ d.independent + i.rank + d.age.months + (1 | c.id), data = df.temp)
      lmer.rank.repro.1 <- lmer(d.dependent ~ d.independent + i.rank + c.reproductive.status + (1 | c.id), data = df.temp)
      lmer.rank.repro.2 <- lmer(d.dependent ~ d.independent + i.rank * c.reproductive.status + (1 | c.id), data = df.temp)
print("test30")

      lmer.rank.prey.repro.1 <- lmer(d.dependent ~ d.independent + i.rank + d.prey.density + c.reproductive.status + (1 | c.id), data = df.temp)
print("test31")
      lmer.rank.prey.repro.2 <- lmer(d.dependent ~ d.independent + i.rank * d.prey.density +  c.reproductive.status +  (1 | c.id), data = df.temp)
print("test32")
      lmer.rank.prey.repro.12 <- lmer(d.dependent ~ d.independent + i.rank + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
print("test33")
      lmer.rank.prey.repro.22 <- lmer(d.dependent ~ d.independent + i.rank * d.prey.density + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
print("test34")
      lmer.rank.age.repro.1 <- lmer(d.dependent ~ d.independent + i.rank + d.age.months +  c.reproductive.status + (1 | c.id), data = df.temp)
print("test35")

  l.models <- list(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, lmer.date.rank.1, lmer.date.prey.1, 
    lmer.date.repro.1, lmer.rank.prey.1, lmer.rank.prey.2, lmer.rank.age.1, lmer.rank.repro.1, lmer.rank.repro.2,
    lmer.rank.prey.repro.1, lmer.rank.prey.repro.2, lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1
  )
  
  print("AICc(lmer.1)")
  print(AICc(lmer.1))
  t.aicc <- (aictab(cand.set = list(lmer.0, 
    lmer.1,
    lmer.rank, 
    lmer.repro, 
    lmer.age,
    lmer.prey, lmer.date.rank.1, lmer.date.prey.1, lmer.date.repro.1, lmer.rank.prey.1, lmer.rank.prey.2,
    lmer.rank.age.1,
    lmer.rank.repro.1, 
    lmer.rank.repro.2,
    lmer.rank.prey.repro.1, 
    lmer.rank.prey.repro.2,
    lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1
    ), 
    modnames=c("lmer.0", 
    "lmer.1",
    "lmer.rank",
    "lmer.repro", 
    "lmer.age",
    "lmer.prey", "lmer.date.rank.1", "lmer.date.prey.1", "lmer.date.repro.1", "lmer.rank.prey.1", "lmer.rank.prey.2", 
    "lmer.rank.age.1",
    "lmer.rank.repro.1", 
    "lmer.rank.repro.2",
    "lmer.rank.prey.repro.1",
    "lmer.rank.prey.repro.2",
    "lmer.rank.prey.repro.12", "lmer.rank.prey.repro.22", "lmer.rank.age.repro.1"
    ), 
    sort = TRUE))

      print(anova(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age,
      lmer.prey, lmer.date.rank.1, lmer.date.prey.1, lmer.date.repro.1, lmer.rank.prey.1, lmer.rank.prey.2, 
      lmer.rank.age.1, lmer.rank.repro.1, lmer.rank.repro.2, lmer.rank.prey.repro.1, lmer.rank.prey.repro.2,
      lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1))         

      print("t.aicc")
      print(t.aicc)
      write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))
      
      par(mfrow = c(3,3))
      print("summary(clmm.1))") 
      print(summary(clmm.1))
      print("cftest(lmer.1))") 
      print(cftest(lmer.1))
      print("ad.test(resid(lmer.1))")                
      print(ad.test(resid(lmer.1)))  
print("")
print("")
print("")
print("")

#      print("summary(clmm.rank))") 
#      print(summary(clmm.rank))
      print("cftest(lmer.rank))") 
      print(cftest(lmer.rank))
      print("ad.test(resid(lmer.rank))")                
      print(ad.test(resid(lmer.rank)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.prey))") 
      print(summary(clmm.prey))
      print("cftest(lmer.prey))") 
      print(cftest(lmer.prey))
      print("ad.test(resid(lmer.prey))")                
      print(ad.test(resid(lmer.prey)))  
print("")
print("")
print("")
print("")
      
#      print("summary(clmm.repro))") 
#      print(summary(clmm.repro))
      print("cftest(lmer.repro))") 
      print(cftest(lmer.repro))
      print("ad.test(resid(lmer.repro))")                
      print(ad.test(resid(lmer.repro)))  
print("")
print("")
print("")
print("")
 
       print("summary(clmm.date.rank.1))") 
      print(summary(clmm.date.rank.1))
      print("cftest(lmer.date.rank.1))") 
      print(cftest(lmer.date.rank.1))
      print("ad.test(resid(lmer.date.rank.1))")                
      print(ad.test(resid(lmer.date.rank.1)))  
print("")
print("")
print("")
print("")
            
      print("summary(clmm.date.prey.1))") 
      print(summary(clmm.date.prey.1))
      print("cftest(lmer.date.prey.1))") 
      print(cftest(lmer.date.prey.1))
      print("ad.test(resid(lmer.date.prey.1))")                
      print(ad.test(resid(lmer.date.prey.1)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.date.repro.1))") 
      print(summary(clmm.date.repro.1))
      print("cftest(lmer.date.repro.1))") 
      print(cftest(lmer.date.repro.1))
      print("ad.test(resid(lmer.date.repro.1))")                
      print(ad.test(resid(lmer.date.repro.1)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.prey.1))") 
      print(summary(clmm.rank.prey.1))
      print("cftest(lmer.rank.prey.1))") 
      print(cftest(lmer.rank.prey.1))
      print("ad.test(resid(lmer.rank.prey.1))")                
      print(ad.test(resid(lmer.rank.prey.1)))  
print("")
print("")
print("")
print("")

      print("summary(clmm.rank.prey.2))") 
      print(summary(clmm.rank.prey.2))
      print("cftest(lmer.rank.prey.2))") 
      print(cftest(lmer.rank.prey.2))
      print("ad.test(resid(lmer.rank.prey.2))")                
      print(ad.test(resid(lmer.rank.prey.2)))  
print("")
print("")
print("")
print("")
       
      print("summary(clmm.rank.repro.1))") 
      print(summary(clmm.rank.repro.1))
      print("cftest(lmer.rank.repro.1))") 
      print(cftest(lmer.rank.repro.1))
      print("ad.test(resid(lmer.rank.repro.1))")                
      print(ad.test(resid(lmer.rank.repro.1)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.repro.2))") 
      print(summary(clmm.rank.repro.2))
      print("cftest(lmer.rank.repr.2))") 
      print(cftest(lmer.rank.repro.2))
      print("ad.test(resid(lmer.rank.repro.2))")                
      print(ad.test(resid(lmer.rank.repro.2)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.age.1))") 
      print(summary(clmm.rank.age.1))
      print("cftest(lmer.rank.age.1))") 
      print(cftest(lmer.rank.age.1))
      print("ad.test(resid(lmer.rank.age.1))")                
      print(ad.test(resid(lmer.rank.age.1)))  
print("")
print("")
print("")
print("")

#      print("summary(clmm.rank.age.2))") 
#      print(summary(clmm.rank.age.2))
#      print("cftest(lmer.rank.age.2))") 
#      print(cftest(lmer.rank.age.2))
#      print("ad.test(resid(lmer.rank.age.2))")                
#      print(ad.test(resid(lmer.rank.age.2)))  
print("")
print("")
print("")
print("")

      print("summary(clmm.rank.prey.repro.1))") 
      print(summary(clmm.rank.prey.repro.1))
      print("cftest(lmer.rank.prey.repro.1))") 
      print(cftest(lmer.rank.prey.repro.1))
      print("ad.test(resid(lmer.rank.prey.repro.1))")                
      print(ad.test(resid(lmer.rank.prey.repro.1)))  
print("")
print("")
print("")
print("")
             
      print("summary(clmm.rank.prey.repro.2))") 
      print(summary(clmm.rank.prey.repro.2))
      print("cftest(lmer.rank.prey.repro.2))") 
      print(cftest(lmer.rank.prey.repro.2))
      print("ad.test(resid(lmer.rank.prey.repro.2))")                
      print(ad.test(resid(lmer.rank.prey.repro.2)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.prey.repro.12))") 
      print(summary(clmm.rank.prey.repro.12))
      print("cftest(lmer.rank.prey.repro.12))") 
      print(cftest(lmer.rank.prey.repro.12))
      print("ad.test(resid(lmer.rank.prey.repro.12))")                
      print(ad.test(resid(lmer.rank.prey.repro.12)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.prey.repro.22))") 
      print(summary(clmm.rank.prey.repro.22))
      print("cftest(lmer.rank.prey.repro.22))") 
      print(cftest(lmer.rank.prey.repro.22))
      print("ad.test(resid(lmer.rank.prey.repro.22))")                
      print(ad.test(resid(lmer.rank.prey.repro.22)))  
print("")
print("")
print("")
print("")
      
      print("summary(clmm.rank.age.repro.1))") 
      print(summary(clmm.rank.age.repro.1))
      print("cftest(lmer.rank.age.repro.1))") 
      print(cftest(lmer.rank.age.repro.1))
      print("ad.test(resid(lmer.rank.age.repro.1))")                
      print(ad.test(resid(lmer.rank.age.repro.1)))  
print("")
print("")
print("")
print("")
            

# pr.clmm.1 <- profile(clmm.1)
# confint(pr.clmm.1)
# par(mfrow = c(2,2))
# plot(pr.clmm.1)
# plot(pr.clmm.1, Log=TRUE, relative = TRUE)
# plot(pr.clmm.1, Log=TRUE, relative = FALSE)
# par(mfrow=c(1,1))
# plot(1,1)

# print(str(clmm.2))
print(clmm.1$coefficients["d.independent"])
      plot(jitter(as.double(d.dependent)) ~ d.independent, data = df.temp, main = paste(c.dependent, " ~ ", c.independent))

      plot(d.dependent ~ d.sample.date, data = df.temp, main = "d.dependent ~ d.sample.date") 
      plot(d.dependent ~ log(as.double(d.sample.date)), data = df.temp, main = "d.dependent ~ log(d.sample.date)") 
      plot(d.dependent ~ i.rank, data = df.temp, main = "d.dependent ~ i.rank") 
      plot(d.dependent ~ c.reproductive.status, data = df.temp, main = "d.dependent ~ c.reproductive.status") 
      plot(d.dependent ~ d.prey.density, data = df.temp, main = "d.dependent ~ d.prey.density") 
      plot(d.dependent ~ d.age.months, data = df.temp, main = "d.dependent ~ d.age.months") 
      plot(d.dependent ~ d.total.igg, data = df.temp, main = "d.dependent ~ d.total.igg")
      plot(d.dependent ~ d.total.igm, data = df.temp, main = "d.dependent ~ d.total.igm")
      plot(d.dependent ~ d.ec.igg, data = df.temp, main = "d.dependent ~ d.ec.igg")
      plot(d.dependent ~ d.pm.igg, data = df.temp, main = "d.dependent ~ d.pm.igg")
      plot(d.dependent ~ d.ec.igm, data = df.temp, main = "d.dependent ~ d.ec.igm")
      plot(d.dependent ~ d.pm.igm, data = df.temp, main = "d.dependent ~ d.pm.igm")


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

  print("length(colnames(df.temp))")
  print(length(colnames(df.temp)))
  print("(colnames(df.temp))")
  print((colnames(df.temp)))
  
  m.cor <- cor(df.temp[, 6:25], use = "pairwise.complete.obs", method = c("spearman"))
  write.csv(m.cor, file = "temp.cor.csv")
  

  par(mfrow = c(1,1))
  plot(1,1, main = paste("space filler"))
#  df.output.final <- cbind(df.output.final, i.replicates)

#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  



