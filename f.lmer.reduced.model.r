# source("f.lmer.reduced.model.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.reduced.model <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status", 
    "c.prey.density.2.level", "c.prey.density.3.level",
    "c.rank.2.level", "c.rank.3.level", "d.age.months",
    "d.mass", "d.minutes.blood", "d.pcv", "i.glucose.green", "d.total.solids", "d.serology.index.rank.igg", "d.serology.index.rank.igm",
    "i.rank", "d.rank.proportion",   
    "d.age.months", "d.prey.density",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.cortisol", "d.testosterone", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
#  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
  df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
  df.temp.full$i.sample.years <- df.temp.full$i.sample.year - min(df.temp.full$i.sample.year) + 1

  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
  df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero

#  df.temp.full <- subset(df.temp.full, c.sex == "f" & i.sample.number.2 == 2)
  df.temp.full <- subset(df.temp.full, c.sex == "f" & d.age.months >= 24)
  df.temp.full <- subset(df.temp.full, !is.na(i.rank))  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  
  df.temp.full <- subset(df.temp.full, !is.na(c.reproductive.status))  
  df.temp.full <- subset(df.temp.full, !is.na(d.prey.density))  
  
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  

#  df.temp.full$d.prey.density <- log((df.temp.full$d.prey.low.density.prior + df.temp.full$d.prey.low.density.current) / 2) 
#  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.3.level
#  df.temp.full$c.prey.density <- df.temp.full$c.prey.density.2.level
  df.temp.full$i.rank <- (df.temp.full$i.rank) 
  df.temp.full$d.prey.density <- log(df.temp.full$d.prey.density) 
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  

  cv.dependent.factors <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
  cv.dependent <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm") 
#  cv.dependent <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 


#  c.links <- c("probit", "cloglog", "loglog", "cauchit", "Aranda-Ordaz", "Aranda-Ordaz", "log-gamma")
  c.links <- c("logit") 

  par(mfrow = c(2,3))
    i.counter.2 <- 1
    i.counter.2.stop <- length(cv.dependent)
      
    for(i.counter.2 in i.counter.2:i.counter.2.stop)  {
  
      c.dependent.factor <- cv.dependent.factors[i.counter.2]
      c.dependent <- cv.dependent[i.counter.2]
      
  
#      print(paste(" **************** c.independent: ", c.independent, "**********************"))   
      print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
   
      print("Before subsetting within the loop.")
      i.full.c.id <- length(df.temp.full$c.id)
      i.full.unique.c.id <- length(unique(df.temp.full$c.id))
  
      # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.dependent]]))
  
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      df.temp$d.dependent <- (df.temp[[c.dependent]])    # lm and lmer model response variables 
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
#      df.temp$d.dependent <- (df.temp[[c.dependent]])    # lm and lmer model response variables 
      df.temp$d.dependent.factor <- ordered(df.temp[[c.dependent.factor]])    # clm and clmm models need to have factors for response variables  
      df.temp$d.date <- (df.temp$i.sample.months)    # lm and lmer model response variables 
      print("summary(df.temp$d.date)")
      print(summary(df.temp$d.date))
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

#        clm.rank.1 <- clm(d.dependent.factor ~ d.age.months + d.rank.proportion * d.prey.density
#          + d.rank.proportion * c.reproductive.status + d.prey.density * c.reproductive.status, data = df.temp, Hess = TRUE, link = "logit", 
#          clm.control = list(method = "ucminf", maxIter = 500, maxLineIter = 500, gradTol = 1e-6),
#          threshold = "flexible", nAGQ = 10)
#        print(summary(clm.rank.1))
#        clmm.rank.1 <- clmm(d.dependent.factor ~ d.rank.proportion * d.prey.density
#          + d.rank.proportion * c.reproductive.status + d.prey.density * c.reproductive.status + (1 | c.id), data = df.temp, Hess = TRUE, link = "logit", 
#          method = "ucminf", gradTol = 1e-6, maxIter = 500, maxLineIter = 500, nAGQ = 10,
#          threshold = "flexible") 
#        print(summary(clmm.rank.1))
#        print(anova(clm.rank.1, clmm.rank.1))

        
#        df.count <- count(df.temp, "d.dependent.factor")
#        df.count$d.initial.prob <- df.count[["freq"]] / length(df.temp[["d.dependent.factor"]])
#        print(df.count)
#        clmm.rank.1$d.initial.prob <- 1:length(df.temp[["d.dependent.factor"]])
#        for(i in 1:length(df.count[["d.dependent.factor"]]))
#        {
#          clmm.rank.1$d.initial.prob[as.character(clmm.rank.1$model[[1]]) == as.character(df.count[["d.dependent.factor"]][i])] <- df.count$d.initial.prob[i]
#        }

#        clmm.rank.1$d.residuals <- (clmm.rank.1$d.initial.prob - fitted(clmm.rank.1))
        par(mfrow = c(3,3))
#        plot(jitter(clmm.rank.1$d.residuals) ~ jitter(fitted(clmm.rank.1)), main = clmm.rank.1$terms)
#        hist(clmm.rank.1$d.residuals) #, breaks = sort(unique(clmm.rank.1$d.residuals))))
#        qqnorm(clmm.rank.1$d.residuals)
#        qqline(clmm.rank.1$d.residuals)
#        d.resid.mean <- mean(clmm.rank.1$d.residuals)
#        d.resid.var <- var(clmm.rank.1$d.residuals)
#        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
#        print(lillie.test(clmm.rank.1$d.residuals))
#        print(ad.test(clmm.rank.1$d.residuals))
#        print(cvm.test(clmm.rank.1$d.residuals))

        # lmer models of the data. compare model fits
        par(mfrow = c(3,3))
#        lm.rank.1 <- lm(d.dependent ~ d.rank.proportion * d.prey.density
#          + d.rank.proportion * c.reproductive.status + d.prey.density * c.reproductive.status, 
#          data = df.temp, REML = FALSE) 
#        print(summary(lm.rank.1))
#        plot(jitter(resid(lm.rank.1)) ~ jitter(fitted(lm.rank.1)), main = terms(lm.rank.1))
#        hist(resid(lm.rank.1)) #, breaks = sort(unique(lm.rank.1$d.residuals))))
#        qqnorm(resid(lm.rank.1))
#        qqline(resid(lm.rank.1))
#        d.resid.mean <- mean(resid(lm.rank.1))
#        d.resid.var <- var(resid(lm.rank.1))
#        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
#        print(lillie.test(resid(lm.rank.1)))
#        print(ad.test(resid(lm.rank.1)))
#        print(cvm.test(resid(lm.rank.1)))

        lmer.1 <- lmer((d.dependent) ~ (d.date) 
          + i.rank
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.1)))
        print(cftest(lmer.1))
        plot(jitter(resid(lmer.1)) ~ jitter(fitted(lmer.1)), main = terms(lmer.1))
        hist(resid(lmer.1)) #, breaks = sort(unique(lmer.1$d.residuals))))
        qqnorm(resid(lmer.1))
        qqline(resid(lmer.1))
        d.resid.mean <- mean(resid(lmer.1))
        d.resid.var <- var(resid(lmer.1))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.1)))
        print(ad.test(resid(lmer.1)))
        print(cvm.test(resid(lmer.1)))

        lmer.2 <- lmer((d.dependent) ~ (d.date) 
         + d.prey.density 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.2)))
        print(cftest(lmer.2))
        plot(jitter(resid(lmer.2)) ~ jitter(fitted(lmer.2)), main = terms(lmer.2))
        hist(resid(lmer.2)) #, breaks = sort(unique(lmer.2$d.residuals))))
        qqnorm(resid(lmer.2))
        qqline(resid(lmer.2))
        d.resid.mean <- mean(resid(lmer.2))
        d.resid.var <- var(resid(lmer.2))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.2)))
        print(ad.test(resid(lmer.2)))
        print(cvm.test(resid(lmer.2)))

        lmer.3 <- lmer((d.dependent) ~ (d.date) 
          + c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.3)))
        print(cftest(lmer.3))
        plot(jitter(resid(lmer.3)) ~ jitter(fitted(lmer.3)), main = terms(lmer.3))
        hist(resid(lmer.3)) #, breaks = sort(unique(lmer.3$d.residuals))))
        qqnorm(resid(lmer.3))
        qqline(resid(lmer.3))
        d.resid.mean <- mean(resid(lmer.3))
        d.resid.var <- var(resid(lmer.3))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.3)))
        print(ad.test(resid(lmer.3)))
        print(cvm.test(resid(lmer.3)))

        lmer.4 <- lmer((d.dependent) ~ (d.date) 
          + i.rank * d.prey.density
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.4)))
        print(cftest(lmer.4))
        plot(jitter(resid(lmer.4)) ~ jitter(fitted(lmer.4)), main = terms(lmer.4))
        hist(resid(lmer.4)) #, breaks = sort(unique(lmer.4$d.residuals))))
        qqnorm(resid(lmer.4))
        qqline(resid(lmer.4))
        d.resid.mean <- mean(resid(lmer.4))
        d.resid.var <- var(resid(lmer.4))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.4)))
        print(ad.test(resid(lmer.4)))
        print(cvm.test(resid(lmer.4)))
        
        lmer.5 <- lmer((d.dependent) ~ (d.date) 
          + i.rank * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.5)))
        print(cftest(lmer.5))
        plot(jitter(resid(lmer.5)) ~ jitter(fitted(lmer.5)), main = terms(lmer.5))
        hist(resid(lmer.5)) #, breaks = sort(unique(lmer.5$d.residuals))))
        qqnorm(resid(lmer.5))
        qqline(resid(lmer.5))
        d.resid.mean <- mean(resid(lmer.5))
        d.resid.var <- var(resid(lmer.5))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.5)))
        print(ad.test(resid(lmer.5)))
        print(cvm.test(resid(lmer.5)))
        
        lmer.6 <- lmer((d.dependent) ~ (d.date) 
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.6)))
        print(cftest(lmer.6))
        plot(jitter(resid(lmer.6)) ~ jitter(fitted(lmer.6)), main = terms(lmer.6))
        hist(resid(lmer.6)) #, breaks = sort(unique(lmer.6$d.residuals))))
        qqnorm(resid(lmer.6))
        qqline(resid(lmer.6))
        d.resid.mean <- mean(resid(lmer.6))
        d.resid.var <- var(resid(lmer.6))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.6)))
        print(ad.test(resid(lmer.6)))
        print(cvm.test(resid(lmer.6)))
        
        lmer.7 <- lmer((d.dependent) ~ (d.date) 
          + i.rank * d.prey.density
          + i.rank * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.7)))
        print(cftest(lmer.7))
        plot(jitter(resid(lmer.7)) ~ jitter(fitted(lmer.7)), main = terms(lmer.7))
        hist(resid(lmer.7)) #, breaks = sort(unique(lmer.7$d.residuals))))
        qqnorm(resid(lmer.7))
        qqline(resid(lmer.7))
        d.resid.mean <- mean(resid(lmer.7))
        d.resid.var <- var(resid(lmer.7))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.7)))
        print(ad.test(resid(lmer.7)))
        print(cvm.test(resid(lmer.7)))
        
        lmer.8 <- lmer((d.dependent) ~ (d.date) 
          + i.rank * d.prey.density
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.8)))
        print(cftest(lmer.8))
        plot(jitter(resid(lmer.8)) ~ jitter(fitted(lmer.8)), main = terms(lmer.8))
        hist(resid(lmer.8)) #, breaks = sort(unique(lmer.8$d.residuals))))
        qqnorm(resid(lmer.8))
        qqline(resid(lmer.8))
        d.resid.mean <- mean(resid(lmer.8))
        d.resid.var <- var(resid(lmer.8))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.8)))
        print(ad.test(resid(lmer.8)))
        print(cvm.test(resid(lmer.8)))
        
        lmer.9 <- lmer((d.dependent) ~ (d.date) 
        + d.serology.index.rank.igg + d.serology.index.rank.igm
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.9)))
        print(cftest(lmer.9))
        plot(jitter(resid(lmer.9)) ~ jitter(fitted(lmer.9)), main = terms(lmer.9))
        hist(resid(lmer.9)) #, breaks = sort(unique(lmer.9$d.residuals))))
        qqnorm(resid(lmer.9))
        qqline(resid(lmer.9))
        d.resid.mean <- mean(resid(lmer.9))
        d.resid.var <- var(resid(lmer.9))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.9)))
        print(ad.test(resid(lmer.9)))
        print(cvm.test(resid(lmer.9)))

        lmer.10 <- lmer((d.dependent) ~ (d.date) 
          + i.rank * d.prey.density
          + i.rank * c.reproductive.status 
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.10)))
        print(cftest(lmer.10))
        plot(jitter(resid(lmer.10)) ~ jitter(fitted(lmer.10)), main = terms(lmer.10))
        hist(resid(lmer.10)) #, breaks = sort(unique(lmer.10$d.residuals))))
        qqnorm(resid(lmer.10))
        qqline(resid(lmer.10))
        d.resid.mean <- mean(resid(lmer.10))
        d.resid.var <- var(resid(lmer.10))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.10)))
        print(ad.test(resid(lmer.10)))
        print(cvm.test(resid(lmer.10)))

        lmer.11 <- lmer((d.dependent) ~ (d.date) 
          + d.age.months # + d.minutes.blood # + d.total.solids + d.mass + 
          + i.rank * d.prey.density
          + i.rank * c.reproductive.status 
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.11)))
        print(cftest(lmer.11))
        plot(jitter(resid(lmer.11)) ~ jitter(fitted(lmer.11)), main = terms(lmer.11))
        hist(resid(lmer.11)) #, breaks = sort(unique(lmer.11$d.residuals))))
        qqnorm(resid(lmer.11))
        qqline(resid(lmer.11))
        d.resid.mean <- mean(resid(lmer.11))
        d.resid.var <- var(resid(lmer.11))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.11)))
        print(ad.test(resid(lmer.11)))
        print(cvm.test(resid(lmer.11)))

        lmer.12 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
        + d.serology.index.rank.igg # + d.serology.index.rank.igm
#          + d.total.igg + d.total.igm 
          + i.rank * d.prey.density
          + i.rank * c.reproductive.status 
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.12)))
        print(cftest(lmer.12))
        plot(jitter(resid(lmer.12)) ~ jitter(fitted(lmer.12)), main = terms(lmer.12))
        hist(resid(lmer.12)) #, breaks = sort(unique(lmer.12$d.residuals))))
        qqnorm(resid(lmer.12))
        qqline(resid(lmer.12))
        d.resid.mean <- mean(resid(lmer.12))
        d.resid.var <- var(resid(lmer.12))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.12)))
        print(ad.test(resid(lmer.12)))
        print(cvm.test(resid(lmer.12)))

        lmer.13 <- lmer((d.dependent) ~ (d.date) 
         + d.minutes.blood 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.13)))
        print(cftest(lmer.13))
        plot(jitter(resid(lmer.13)) ~ jitter(fitted(lmer.13)), main = terms(lmer.13))
        hist(resid(lmer.13)) #, breaks = sort(unique(lmer.13$d.residuals))))
        qqnorm(resid(lmer.13))
        qqline(resid(lmer.13))
        d.resid.mean <- mean(resid(lmer.13))
        d.resid.var <- var(resid(lmer.13))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.13)))
        print(ad.test(resid(lmer.13)))
        print(cvm.test(resid(lmer.13)))

        lmer.14 <- lmer((d.dependent) ~ (d.date) 
          + d.age.months * c.reproductive.status # + d.minutes.blood # + d.total.solids + d.mass + 
          + i.rank * d.prey.density
          + i.rank * c.reproductive.status 
          + d.prey.density * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.14)))
        print(cftest(lmer.14))
        plot(jitter(resid(lmer.14)) ~ jitter(fitted(lmer.14)), main = terms(lmer.14))
        hist(resid(lmer.14)) #, breaks = sort(unique(lmer.14$d.residuals))))
        qqnorm(resid(lmer.14))
        qqline(resid(lmer.14))
        d.resid.mean <- mean(resid(lmer.14))
        d.resid.var <- var(resid(lmer.14))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.14)))
        print(ad.test(resid(lmer.14)))
        print(cvm.test(resid(lmer.14)))
        
        lmer.15 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
        + d.serology.index.rank.igg # + d.serology.index.rank.igm
#          + d.total.igg + d.total.igm 
          + i.rank # * d.prey.density
          + c.reproductive.status 
          + d.prey.density # * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.15)))
        print(cftest(lmer.15))
        plot(jitter(resid(lmer.15)) ~ jitter(fitted(lmer.15)), main = terms(lmer.15))
        hist(resid(lmer.15)) #, breaks = sort(unique(lmer.15$d.residuals))))
        qqnorm(resid(lmer.15))
        qqline(resid(lmer.15))
        d.resid.mean <- mean(resid(lmer.15))
        d.resid.var <- var(resid(lmer.15))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.15)))
        print(ad.test(resid(lmer.15)))
        print(cvm.test(resid(lmer.15)))
        
        lmer.16 <- lmer((d.dependent) ~ (d.date) 
          + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
          + i.rank # * d.prey.density
          + c.reproductive.status 
          + d.prey.density # * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.16)))
        print(cftest(lmer.16))
        plot(jitter(resid(lmer.16)) ~ jitter(fitted(lmer.16)), main = terms(lmer.16))
        hist(resid(lmer.16)) #, breaks = sort(unique(lmer.16$d.residuals))))
        qqnorm(resid(lmer.16))
        qqline(resid(lmer.16))
        d.resid.mean <- mean(resid(lmer.16))
        d.resid.var <- var(resid(lmer.16))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.16)))
        print(ad.test(resid(lmer.16)))
        print(cvm.test(resid(lmer.16)))

        lmer.17 <- lmer((d.dependent) ~ (d.date) 
          + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
          + d.cortisol
          + d.testosterone
          + d.mass
          + i.rank # * d.prey.density
          + c.reproductive.status 
          + d.prey.density # * c.reproductive.status 
          + (1 | c.id), 
          data = df.temp, REML = FALSE) 
        print(length(resid(lmer.17)))
        print(cftest(lmer.17))
        plot(jitter(resid(lmer.17)) ~ jitter(fitted(lmer.17)), main = terms(lmer.17))
        hist(resid(lmer.17)) #, breaks = sort(unique(lmer.17$d.residuals))))
        qqnorm(resid(lmer.17))
        qqline(resid(lmer.17))
        d.resid.mean <- mean(resid(lmer.17))
        d.resid.var <- var(resid(lmer.17))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(lmer.17)))
        print(ad.test(resid(lmer.17)))
        print(cvm.test(resid(lmer.17)))

#  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
#  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
#  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
#  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
#  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
#  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

      print(aictab(cand.set = list(lmer.1, lmer.2, lmer.3, lmer.4, lmer.5, lmer.6, lmer.7, lmer.8, lmer.9, lmer.10, lmer.11, 
        lmer.12, lmer.13, lmer.14, lmer.15, lmer.16, lmer.17), 
        modnames = c("lmer.1", "lmer.2", "lmer.3", "lmer.4", "lmer.5", "lmer.6", "lmer.7", "lmer.8", "lmer.9", "lmer.10", 
        "lmer.11", "lmer.12", "lmer.13", "lmer.14", "lmer.15", "lmer.16", "lmer.17"), 
        sort = TRUE))
      }

      
      print("")

#  t.aicc <- (aictab(cand.set = list(lmer.0, lmer.1, lmer.rank, lmer.repro, lmer.age, lmer.prey, lmer.date.prey.1 
#    ), 
#    modnames=c("lmer.0", "lmer.1", "lmer.age", "lmer.prey", "lmer.date.prey.1"), 
#    sort = TRUE))

#    print("t.aicc")
#    print(t.aicc)
#    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent.variables[i.counter.2], ".csv", sep = ""))
      
      par(mfrow = c(3,3))
    }
  m.cor <- cor(df.temp[, 12:38], use = "pairwise.complete.obs", method = c("spearman"))
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
