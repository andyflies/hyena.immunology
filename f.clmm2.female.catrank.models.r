# source("f.clmm2.female.catrank.models.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.clmm2.female.catrank.models <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "c.reproductive.status", "i.sample.number.2",
    "c.prey.density.2.level", "c.prey.density.3.level",
    "i.rank", "c.rank.2.level", "c.rank.3.level", "d.age.months",  
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm", "d.prey.density",
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

  df.temp.full <- subset(df.temp.full, c.sex == "f" & d.age.months >= 24)
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  


#  df.temp.full$d.prey.density <- log((df.temp.full$d.prey.low.density.prior + df.temp.full$d.prey.low.density.current) / 2) 
#  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.3.level
#  df.temp.full$d.prey.density <- factor(df.temp.full$c.prey.density.3.level)
  df.temp.full$d.prey.density <- log10(df.temp.full$d.prey.density) 
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$c.rank.3.level)))  
#  print(cbind(df.temp.full$c.id, df.temp.full$d.mic.90.bka.ec, log2(df.temp.full$d.mic.90.bka.ec), df.temp.full$d.mic.90.level.bka.ec,  
#    df.temp.full$d.mic.90.bka.pm, log2(df.temp.full$d.mic.90.bka.pm), df.temp.full$d.mic.90.level.bka.pm)) 

#  df.temp.full$d.sample.date <- (df.temp.full$d.sample.date - min(df.temp.full$d.sample.date))

#  c.independent <- "c.rank.3.level"   # Sets the default c.independent to rank.  The loops below will actually use a vector of variable names.
#  c.dependent <- "d.mic.90.level.bka.ec"
  cv.independent.variables <- c("d.sample.date") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females
#  cv.independent.variables <- c("c.rank.3.level") #, "d.age.months", "c.reproductive.status") # c.sex can only be run with males and females

  cv.dependent.variables <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm") 
#  cv.dependent.variables <- c("d.mic.90.bka.pm") 
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
      i.full.c.rank.3.level <- length(df.temp.full$c.rank.3.level)
      i.full.unique.c.rank.3.level <- length(unique(df.temp.full$c.rank.3.level))
  
      # We can only include records in the clmm2 models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm2.0 and clmm2. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
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
      
      print(df.temp)
      df.temp <- df.temp[!is.na(df.temp$c.rank.3.level),]
#      df.temp <- df.temp[!is.na(df.temp$d.prey.density),]
#      df.temp <- df.temp[!is.na(df.temp$c.reproductive.status),]
      print(df.temp)

      par(mfrow=c(3,3))
      plot(1 ~ 1, main = paste(" **************** c.dependent: ", c.dependent, "**********************"))
      plot(jitter(as.double(d.dependent), factor = 10) ~ d.independent, data = df.temp, main = paste(c.dependent, " ~ ", c.independent))
      plot(d.dependent ~ d.sample.date, data = df.temp, main = "d.dependent ~ d.sample.date") 
      plot(d.dependent ~ log(as.double(d.sample.date)), data = df.temp, main = "d.dependent ~ log(d.sample.date)") 
      plot(d.dependent ~ c.reproductive.status, data = df.temp, main = "d.dependent ~ c.reproductive.status") 
      plot(d.dependent ~ i.rank, data = df.temp, main = "d.dependent ~ i.rank") 
      plot(d.dependent ~ c.rank.2.level, data = df.temp, main = "d.dependent ~ c.rank.2.level") 
      plot(d.dependent ~ c.rank.3.level, data = df.temp, main = "d.dependent ~ c.rank.3.level") 
      coplot(jitter(as.double(d.dependent), factor = 10) ~ i.rank | d.prey.density, data = df.temp) #, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ i.rank | c.prey.density.2.level, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ i.rank | c.prey.density.3.level, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.2.level | d.prey.density, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.2.level | c.prey.density.2.level, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.2.level | c.prey.density.3.level, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.3.level | d.prey.density, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.3.level | c.prey.density.2.level, data = df.temp, panel = panel.smooth)
      coplot(jitter(as.double(d.dependent), factor = 10) ~ c.rank.3.level | c.prey.density.3.level, data = df.temp, panel = panel.smooth)

      par(mfrow=c(3,4))
      plot(d.dependent ~ d.prey.density, data = df.temp, main = "d.dependent ~ d.prey.density") 
      plot(d.dependent ~ c.prey.density.2.level, data = df.temp, main = "d.dependent ~ c.prey.density.2.level") 
      plot(jitter(as.double(d.dependent)) ~ c.prey.density.2.level, data = df.temp, main = "d.dependent ~ jitter(c.prey.density.2.level)") 
      par(mfrow=c(3,4))
#      plot(d.dependent ~ c.rank.3.level + c.prey.density.3.level, data = df.temp, main = "d.dependent ~ c.rank.3.level + c.prey.density.3.level") 
       par(mfrow=c(3,4))
#      plot(d.dependent ~ c.rank.3.level * c.prey.density.3.level, data = df.temp, main = "d.dependent ~ c.rank.3.level * c.prey.density.3.level") 
#      plot(d.dependent ~ c.prey.density.3.level, data = df.temp, main = "d.dependent ~ c.prey.density.3.level") 
#      plot(d.dependent ~ c.rank.2.level, data = subset(df.temp, c.prey.density.2.level == 1), main = "d.dependent ~ c.prey.density.2.level == 1") 
#      plot(d.dependent ~ c.rank.2.level, data = subset(df.temp, c.prey.density.2.level == 2), main = "d.dependent ~ c.prey.density.2.level == 2") 
#      plot(d.dependent ~ c.rank.3.level, data = subset(df.temp, c.prey.density.2.level == 1), main = "d.dependent ~ c.prey.density.2.level == 1") 
#      plot(d.dependent ~ c.rank.3.level, data = subset(df.temp, c.prey.density.2.level == 2), main = "d.dependent ~ c.prey.density.2.level == 2") 
      par(mfrow=c(3,3))
#      plot(d.dependent ~ c.rank.2.level, data = subset(df.temp, c.prey.density.3.level == 1), main = "d.dependent ~ c.prey.density.3.level == 1") 
#      plot(d.dependent ~ c.rank.2.level, data = subset(df.temp, c.prey.density.3.level == 2), main = "d.dependent ~ c.prey.density.3.level == 2") 
#      plot(d.dependent ~ c.rank.2.level, data = subset(df.temp, c.prey.density.3.level == 3), main = "d.dependent ~ c.prey.density.3.level == 3") 
#      plot(d.dependent ~ c.rank.3.level, data = subset(df.temp, c.prey.density.3.level == 1), main = "d.dependent ~ c.prey.density.3.level == 1") 
#      plot(d.dependent ~ c.rank.3.level, data = subset(df.temp, c.prey.density.3.level == 2), main = "d.dependent ~ c.prey.density.3.level == 2") 
#      plot(d.dependent ~ c.rank.3.level, data = subset(df.temp, c.prey.density.3.level == 3), main = "d.dependent ~ c.prey.density.3.level == 3") 

      par(mfrow=c(3,3))
      plot(d.dependent ~ d.age.months, data = df.temp, main = "d.dependent ~ d.age.months") 
      plot(d.dependent ~ d.total.igg, data = df.temp, main = "d.dependent ~ d.total.igg")
      plot(d.dependent ~ d.total.igm, data = df.temp, main = "d.dependent ~ d.total.igm")
      plot(d.dependent ~ d.ec.igg, data = df.temp, main = "d.dependent ~ d.ec.igg")
      plot(d.dependent ~ d.pm.igg, data = df.temp, main = "d.dependent ~ d.pm.igg")
      plot(d.dependent ~ d.ec.igm, data = df.temp, main = "d.dependent ~ d.ec.igm")
      plot(d.dependent ~ d.pm.igm, data = df.temp, main = "d.dependent ~ d.pm.igm")


      
      print("")
      print(" ******* starting clmm2 models **************************")
      clm2.rank <- clm2(d.dependent.factor ~ i.rank, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.rank <- clmm2(d.dependent.factor ~ i.rank, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
          threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank))
      print(summary(clmm2.rank))
      print(anova(clm2.rank, clmm2.rank))

      clm2.rank <- clm2(d.dependent.factor ~ c.rank.3.level, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank <- clmm2(d.dependent.factor ~ c.rank.3.level, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank))
      print(summary(clmm2.rank))
      print(anova(clm2.rank, clmm2.rank))


# print(anova(clmn.1, clmm2.2))
      clm2.0 <- clm2(d.dependent.factor ~ 1, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.0 <- clmm2(d.dependent.factor ~ 1, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.0))
      print(summary(clmm2.0))
      print(anova(clm2.0, clmm2.0))

      clm2.repro <- clm2(d.dependent.factor ~ c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.repro <- clmm2(d.dependent.factor ~ c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.repro))
      print(summary(clmm2.repro))
      print(anova(clm2.repro, clmm2.repro))

      clm2.age <- clm2(d.dependent.factor ~ log10(d.age.months), data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.age <- clmm2(d.dependent.factor ~ d.age.months, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.age))
      print(summary(clmm2.age))
      print(anova(clm2.age, clmm2.age))
      
      clm2.prey <- clm2(d.dependent.factor ~ d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.prey <- clmm2(d.dependent.factor ~ d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.prey))
      print(summary(clmm2.prey))
      print(anova(clm2.prey, clmm2.prey))


      clm2.rank.prey.1 <- clm2(d.dependent.factor ~ i.rank + d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.prey.1 <- clmm2(d.dependent.factor ~ i.rank + d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.1))
      print(summary(clmm2.rank.prey.1))
      print(anova(clm2.rank.prey.1, clmm2.rank.prey.1))

      clm2.rank.prey.1 <- clm2(d.dependent.factor ~ c.rank.3.level + d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.prey.1 <- clmm2(d.dependent.factor ~ c.rank.3.level + d.prey.density, random = c.id, data = df.temp, Hess = TRUE, 
        link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.1))
      print(summary(clmm2.rank.prey.1))
      print(anova(clm2.rank.prey.1, clmm2.rank.prey.1))

      clm2.rank.prey.2 <- clm2(d.dependent.factor ~ i.rank * d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.rank.prey.2 <- clmm2(d.dependent.factor ~ i.rank * d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.2))
      print(summary(clmm2.rank.prey.2))
      print(anova(clm2.rank.prey.2, clmm2.rank.prey.2))

      clm2.rank.prey.2 <- clm2(d.dependent.factor ~ i.rank * factor(c.prey.density.2.level), data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "flexible", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.rank.prey.2 <- clmm2(d.dependent.factor ~ i.rank * factor(c.prey.density.2.level), random = c.id, data = df.temp, Hess = TRUE, 
        link = "logistic", method = "ucminf", 
        threshold = "flexible", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.2))
      print(summary(clmm2.rank.prey.2))
      print(anova(clm2.rank.prey.2, clmm2.rank.prey.2))

      clm2.rank.prey.2 <- clm2(d.dependent.factor ~ i.rank * factor(c.prey.density.2.level), data = df.temp, Hess = TRUE, link = "logistic", 
        method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200, gradTol = 1e-6))
      clmm2.rank.prey.2 <- clmm2(d.dependent.factor ~ i.rank * factor(c.prey.density.2.level), random = c.id, data = df.temp, Hess = TRUE, 
        link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.2))
      print(summary(clmm2.rank.prey.2))
      print(anova(clm2.rank.prey.2, clmm2.rank.prey.2))

#      clm2.rank.prey.2 <- clm2(d.dependent.factor ~ c.rank.3.level * d.prey.density, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.rank.prey.2 <- clmm2(d.dependent.factor ~ c.rank.3.level * d.prey.density, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
#      print(summary(clm2.rank.prey.2))
#      print(summary(clmm2.rank.prey.2))
#      print(anova(clm2.rank.prey.2, clmm2.rank.prey.2))

      clm2.rank.repro.1 <- clm2(d.dependent.factor ~ c.rank.3.level + c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.repro.1 <- clmm2(d.dependent.factor ~ c.rank.3.level + c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.repro.1))
      print(summary(clmm2.rank.repro.1))
      print(anova(clm2.rank.repro.1, clmm2.rank.repro.1))

#      clm2.rank.repro.2 <- clm2(d.dependent.factor ~ c.rank.3.level * c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.rank.repro.2 <- clmm2(d.dependent.factor ~ c.rank.3.level * c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
#      print(summary(clm2.rank.repro.2))
#      print(summary(clmm2.rank.repro.2))
#      print(anova(clm2.rank.repro.2, clmm2.rank.repro.2))

#      clmm2.date.rank.repro.2 <- clmm2(d.dependent.factor ~ d.independent + c.rank.3.level * c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", threshold = "flexible", nAGQ = 10)
#      clm2.rank.prey.repro.1 <- clm2(d.dependent.factor ~ c.rank.3.level + d.prey.density + c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.rank.prey.repro.1 <- clmm2(d.dependent.factor ~ c.rank.3.level + d.prey.density + c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200)) #, gradTol = 1e-6))
#      print(summary(clm2.rank.prey.repro.1))
#      print(summary(clmm2.rank.prey.repro.1))
#      print(anova(clm2.rank.prey.repro.1, clmm2.rank.prey.repro.1))

#      clm2.rank.prey.repro.2 <- clm2(d.dependent.factor ~ c.rank.3.level * d.prey.density +  c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.rank.prey.repro.2 <- clmm2(d.dependent.factor ~ c.rank.3.level * d.prey.density +  c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200)) #, gradTol = 1e-6))
#      print(summary(clm2.rank.prey.repro.2))
#      print(summary(clmm2.rank.prey.repro.2))
#      print(anova(clm2.rank.prey.repro.2, clmm2.rank.prey.repro.2))

#      clm2.rank.prey.repro.12 <- clm2(d.dependent.factor ~ c.rank.3.level + d.prey.density * c.reproductive.status, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.rank.prey.repro.12 <- clmm2(d.dependent.factor ~ c.rank.3.level + d.prey.density * c.reproductive.status, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200)) #, gradTol = 1e-6))
#      print(summary(clm2.rank.prey.repro.12))
#      print(summary(clmm2.rank.prey.repro.12))
#      print(anova(clm2.rank.prey.repro.12, clmm2.rank.prey.repro.12))

      clm2.rank.prey.repro.22 <- clm2(d.dependent.factor ~ c.rank.3.level * factor(c.prey.density.2.level) + factor(c.prey.density.2.level) * c.reproductive.status, 
        data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.prey.repro.22 <- clmm2(d.dependent.factor ~ c.rank.3.level * factor(c.prey.density.2.level) + factor(c.prey.density.2.level) * c.reproductive.status, 
        random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.repro.22))
      print(summary(clmm2.rank.prey.repro.22))
      print(anova(clm2.rank.prey.repro.22, clmm2.rank.prey.repro.22))

      clm2.rank.prey.repro.222 <- clm2(d.dependent.factor ~ c.rank.3.level * factor(c.prey.density.2.level) 
        + c.rank.3.level * c.reproductive.status + factor(c.prey.density.2.level) * c.reproductive.status, 
        data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.prey.repro.222 <- clmm2(d.dependent.factor ~ c.rank.3.level * factor(c.prey.density.2.level) 
        + c.rank.3.level * c.reproductive.status + factor(c.prey.density.2.level) * c.reproductive.status, random = c.id, 
        data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.prey.repro.222))
      print(summary(clmm2.rank.prey.repro.222))
      print(anova(clm2.rank.prey.repro.222, clmm2.rank.prey.repro.222))

      clm2.rank.age.repro.1 <- clm2(d.dependent.factor ~ c.rank.3.level + d.age.months +  c.reproductive.status, 
        data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
      clmm2.rank.age.repro.1 <- clmm2(d.dependent.factor ~ c.rank.3.level + d.age.months +  c.reproductive.status, random = c.id, 
        data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
        threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200, gradTol = 1e-6))
      print(summary(clm2.rank.age.repro.1))
      print(summary(clmm2.rank.age.repro.1))
      print(anova(clm2.rank.age.repro.1, clmm2.rank.age.repro.1))

#      clm2.date.rank.1 <- clm2(d.dependent.factor ~ d.independent + c.rank.3.level, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", threshold = "symmetric", control = clm2.control(maxIter = 200, maxLineIter = 200)) #, gradTol = 1e-6))
#      clmm2.date.rank.1 <- clmm2(d.dependent.factor ~ d.independent + c.rank.3.level, random = c.id, data = df.temp, Hess = TRUE, link = "logistic", method = "ucminf", 
#          threshold = "symmetric", nAGQ = 10, control = clmm2.control(maxIter = 200,  maxLineIter = 200)) #, gradTol = 1e-6))
#      print(summary(clm2.date.rank.1))
#      print(summary(clmm2.date.rank.1))
#      print(anova(clm2.date.rank.1, clmm2.date.rank.1))

      print("")
      print(" ******* starting lmer models **************************")
      lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp)
      lmer.1 <- lmer(d.dependent ~ d.independent + (1 | c.id), data = df.temp)
      lmer.rank <- lmer(d.dependent ~ c.rank.3.level + (1 | c.id), data = df.temp)
      lmer.repro <- lmer(d.dependent ~ c.reproductive.status + (1 | c.id), data = df.temp)
      lmer.age <- lmer(d.dependent ~ d.age.months + (1 | c.id), data = df.temp)
      lmer.prey <- lmer(d.dependent ~ d.prey.density + (1 | c.id), data = df.temp)
print("test20")

#      lmer.date.rank.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + (1 | c.id), data = df.temp)
#      lmer.date.prey.1 <- lmer(d.dependent ~ d.independent + d.prey.density + (1 | c.id), data = df.temp)
#      lmer.date.repro.1 <- lmer(d.dependent ~ d.independent + c.reproductive.status + (1 | c.id), data = df.temp)

#      lmer.rank.prey.1 <- lmer(d.dependent ~ c.rank.3.level + d.prey.density + (1 | c.id), data = df.temp)
#      lmer.rank.prey.2 <- lmer(d.dependent ~ c.rank.3.level * d.prey.density + (1 | c.id), data = df.temp)
#      lmer.rank.age.1 <- lmer(d.dependent ~ c.rank.3.level + d.age.months + (1 | c.id), data = df.temp)
#      lmer.rank.repro.1 <- lmer(d.dependent ~ c.rank.3.level + c.reproductive.status + (1 | c.id), data = df.temp)
#      lmer.rank.repro.2 <- lmer(d.dependent ~ c.rank.3.level * c.reproductive.status + (1 | c.id), data = df.temp)

#      lmer.date.rank.prey.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + d.prey.density + (1 | c.id), data = df.temp)
#     lmer.date.rank.prey.2 <- lmer(d.dependent ~ d.independent + c.rank.3.level * d.prey.density + (1 | c.id), data = df.temp)
#      lmer.date.rank.age.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + d.age.months + (1 | c.id), data = df.temp)
#      lmer.date.rank.repro.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + c.reproductive.status + (1 | c.id), data = df.temp)
#      lmer.date.rank.repro.2 <- lmer(d.dependent ~ d.independent + c.rank.3.level * c.reproductive.status + (1 | c.id), data = df.temp)
print("test30")
#      lmer.rank.prey.repro.1 <- lmer(d.dependent ~ c.rank.3.level + d.prey.density + c.reproductive.status + (1 | c.id), data = df.temp)
#      lmer.rank.prey.repro.2 <- lmer(d.dependent ~ c.rank.3.level * d.prey.density +  c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.rank.prey.repro.12 <- lmer(d.dependent ~ c.rank.3.level + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.rank.prey.repro.22 <- lmer(d.dependent ~ c.rank.3.level * d.prey.density + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.rank.age.repro.1 <- lmer(d.dependent ~ c.rank.3.level + d.age.months +  c.reproductive.status + (1 | c.id), data = df.temp)

#      lmer.date.rank.prey.repro.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + d.prey.density + c.reproductive.status + (1 | c.id), data = df.temp)
#      lmer.date.rank.prey.repro.2 <- lmer(d.dependent ~ d.independent + c.rank.3.level * d.prey.density +  c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.date.rank.prey.repro.12 <- lmer(d.dependent ~ d.independent + c.rank.3.level + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.date.rank.prey.repro.22 <- lmer(d.dependent ~ d.independent + c.rank.3.level * d.prey.density + d.prey.density * c.reproductive.status +  (1 | c.id), data = df.temp)
#      lmer.date.rank.age.repro.1 <- lmer(d.dependent ~ d.independent + c.rank.3.level + d.age.months +  c.reproductive.status + (1 | c.id), data = df.temp)
print("test35")

#    l.models <- list(clmm2.0, lmer.0, lmer.1, clmm2.rank, lmer.rank, clmm2.repro, lmer.repro, clmm2.age, lmer.age, clmm2.prey, lmer.prey,
#      clmm2.date.rank.1, lmer.date.rank.1, clmm2.date.prey.1, lmer.date.prey.1, clmm2.date.repro.1, lmer.date.repro.1, 
#      clmm2.rank.prey.1, 
#      lmer.rank.prey.1, 
#      clmm2.rank.prey.2,
#      lmer.rank.prey.2,
#      clmm2.rank.age.1, 
#      lmer.rank.age.1, 
#      clmm2.rank.repro.1, 
#      lmer.rank.repro.1, 
#      clmm2.rank.repro.2, 
#      lmer.rank.repro.2, 
#      clmm2.date.rank.prey.1, lmer.date.rank.prey.1, clmm2.date.rank.prey.2, lmer.date.rank.prey.2, clmm2.date.rank.age.1, lmer.date.rank.age.1, 
#      clmm2.date.rank.repro.1, lmer.date.rank.repro.1, 
#      clmm2.date.rank.repro.2,
#      lmer.date.rank.repro.2,
#      clmm2.rank.prey.repro.1, lmer.rank.prey.repro.1, clmm2.rank.prey.repro.2,lmer.rank.prey.repro.2,
#      clmm2.rank.prey.repro.12, lmer.rank.prey.repro.12, clmm2.rank.prey.repro.22, lmer.rank.prey.repro.22, 
#      clmm2.rank.age.repro.1, 
#      lmer.rank.age.repro.1
#      clmm2.date.rank.prey.repro.1, lmer.date.rank.prey.repro.1, clmm2.date.rank.prey.repro.2, lmer.date.rank.prey.repro.2, 
#      clmm2.date.rank.prey.repro.12, lmer.date.rank.prey.repro.12, clmm2.date.rank.prey.repro.22, lmer.date.rank.prey.repro.22, 
#      clmm2.date.rank.age.repro.1, lmer.date.rank.age.repro.1
#    )
    
#    print(length(l.models))
#    for(i in 1:length(l.models))
#    {
#      print(paste(" ****************** model", i, "    ***********************************************************************", sep = ""))
#      if(as.character(typeof(l.models[[i]])) == "list")
#      {
#        print(paste("summary(l.models[[", i, "]]))", sep = ""))
#        print(summary(l.models[[i]]))
#      }
#      else
#      {
#        print(paste("cftest(l.models[[", i, "]]))", sep = ""))
#        print(cftest(l.models[[i]]))
#        print("ad.test(resid(l.models[[i]]))")                
#        print(ad.test(resid(l.models[[i]])))  
#      }
#      print("", quote = FALSE)
#      print("", quote = FALSE)      
#    }

#    print(anova(clmm2.0, clmm2.date, clmm2.rank, clmm2.repro, clmm2.age, clmm2.prey, 
#      clmm2.date.rank.1, clmm2.date.prey.1, clmm2.date.repro.1, 
#      clmm2.rank.prey.1, clmm2.rank.prey.2,
#      clmm2.rank.age.1, clmm2.rank.repro.1, clmm2.rank.repro.2, 
#      clmm2.date.rank.prey.1, clmm2.date.rank.prey.2, clmm2.date.rank.age.1, clmm2.date.rank.repro.1, # clmm2.date.rank.repro.2,
#      clmm2.rank.prey.repro.1, clmm2.rank.prey.repro.2,
#      clmm2.rank.prey.repro.12, clmm2.rank.prey.repro.22, clmm2.rank.age.repro.1
#      clmm2.date.rank.prey.repro.1, clmm2.date.rank.prey.repro.2, clmm2.date.rank.prey.repro.12, clmm2.date.rank.prey.repro.22, clmm2.date.rank.age.repro.1
#    ))         

#    print(anova(clmm2.rank, clmm2.rank.prey.2))         
#    print(anova(clmm2.rank, clmm2.rank.repro.1))         
#    print(anova(clmm2.rank, clmm2.rank.prey.repro.1))         
#    print(anova(clmm2.rank, clmm2.rank.prey.repro.2))         
#    print(anova(clmm2.rank, clmm2.rank.prey.repro.12))         
#    print(anova(clmm2.rank, clmm2.rank.prey.repro.22))         

#    print(anova(clmm2.rank.prey.2, clmm2.rank.prey.repro.1))         
#    print(anova(clmm2.rank.prey.2, clmm2.rank.prey.repro.2))         
#    print(anova(clmm2.rank.prey.2, clmm2.rank.repro.1))         
#    print(anova(clmm2.rank.prey.2, clmm2.rank.prey.repro.12))         
#    print(anova(clmm2.rank.prey.2, clmm2.rank.prey.repro.22))         

#    print(anova(clmm2.rank.repro.1, clmm2.rank.prey.repro.1))         
#    print(anova(clmm2.rank.repro.1, clmm2.rank.prey.repro.2))         
#    print(anova(clmm2.rank.repro.1, clmm2.rank.prey.repro.12))         
#    print(anova(clmm2.rank.repro.1, clmm2.rank.prey.repro.22))         
         
#    print(anova(clmm2.rank,
#      clmm2.rank.prey.2, 
#      clmm2.rank.repro.1, 
#      clmm2.rank.prey.repro.1, 
#      clmm2.rank.prey.repro.2,
#      clmm2.rank.prey.repro.12, 
#      clmm2.rank.prey.repro.22
#      clmm2.rank.age.repro.1
#    ))         
         
#    print(anova(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, 
#      lmer.date.rank.1, lmer.date.prey.1, lmer.date.repro.1, 
#      lmer.rank.prey.1, lmer.rank.prey.2,
#      lmer.rank.age.1, lmer.rank.repro.1, lmer.rank.repro.2, 
#      lmer.date.rank.prey.1, lmer.date.rank.prey.2, lmer.date.rank.age.1, lmer.date.rank.repro.1, lmer.date.rank.repro.2,
#      lmer.rank.prey.repro.1, lmer.rank.prey.repro.2,
#      lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1
 #     lmer.date.rank.prey.repro.1, lmer.date.rank.prey.repro.2, lmer.date.rank.prey.repro.12, lmer.date.rank.prey.repro.22, lmer.date.rank.age.repro.1
#    ))         

#  t.aicc <- (aictab(cand.set = list(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, 
#    lmer.date.rank.1, lmer.date.prey.1, lmer.date.repro.1, 
#    lmer.rank.prey.1, lmer.rank.prey.2,
#    lmer.rank.age.1, lmer.rank.repro.1, lmer.rank.repro.2, 
#    lmer.date.rank.prey.1, lmer.date.rank.prey.2, lmer.date.rank.age.1, lmer.date.rank.repro.1, lmer.date.rank.repro.2,
#    lmer.rank.prey.repro.1, lmer.rank.prey.repro.2,
#    lmer.rank.prey.repro.12, lmer.rank.prey.repro.22, lmer.rank.age.repro.1
#    lmer.date.rank.prey.repro.1, lmer.date.rank.prey.repro.2, lmer.date.rank.prey.repro.12, lmer.date.rank.prey.repro.22, lmer.date.rank.age.repro.1
#    ), 
#    modnames=c("lmer.0", "lmer.1", "lmer.rank", "lmer.repro", "lmer.age", "lmer.prey", 
#    "lmer.date.rank.1", "lmer.date.prey.1", "lmer.date.repro.1", 
#    "lmer.rank.prey.1", "lmer.rank.prey.2", 
#    "lmer.rank.age.1", "lmer.rank.repro.1", "lmer.rank.repro.2", 
#    "lmer.date.rank.prey.1", "lmer.date.rank.prey.2", "lmer.date.rank.age.1", "lmer.date.rank.repro.1", "lmer.date.rank.repro.2",
#    "lmer.rank.prey.repro.1", "lmer.rank.prey.repro.2",
#    "lmer.rank.prey.repro.12", "lmer.rank.prey.repro.22", "lmer.rank.age.repro.1"
#    "lmer.date.rank.prey.repro.1", "lmer.date.rank.prey.repro.2", "lmer.date.rank.prey.repro.12", "lmer.date.rank.prey.repro.22", "lmer.date.rank.age.repro.1"
#    ), 
#    sort = TRUE))

#    print("t.aicc")
#    print(t.aicc)
#    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))

print("test40")


#      print("t.aicc")
#      print(t.aicc)
#      write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))
      
      par(mfrow = c(3,3))
         

# pr.clmm2.date <- profile(clmm2.date)
# confint(pr.clmm2.date)
# par(mfrow = c(2,2))
# plot(pr.clmm2.date)
# plot(pr.clmm2.date, Log=TRUE, relative = TRUE)
# plot(pr.clmm2.date, Log=TRUE, relative = FALSE)
# par(mfrow=c(1,1))
# plot(1,1)

# print(str(clmm2.2))


      # Compute the LR test stat for comparing to the bootstrap results
#      d.lr.test.stats <- signif(as.numeric(2*(logLik(clmm2.date, REML = FALSE)-logLik(clmm2.0, REML = FALSE)), digits = 5))
      # Store the independent variable effect sizes for comparing to the bootstrap results
#      d.fixef.test.stats <- signif(coef(summary(clmm2.date))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
      # Store the t-stat for the independent for comparing to the bootstrap results
#      d.t.test.stats <- signif(coef(summary(clmm2.date))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
#      dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
#      print(dv.test.stats)
#      print(paste("number of rows in fixef: ", length(coef(summary(clmm2.date))[,1])))
#      print(paste("number of columns in fixef: ", length(coef(summary(clmm2.date))[1,])))
  
   
        ################### parametric bootstrapping using simulation based on null model ###################################
    ######################################################################################
#      par(mfrow = c(3,3))
    
      c.boot.type <- "boot.parametric"
      i.output.row <- 1
  
#  print(" ********************  staring f.boot.parametric ********************") 
#      df.test.stats <- replicate(i.replicates, f.boot.parametric(clmm2.0, clmm2.date))
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



