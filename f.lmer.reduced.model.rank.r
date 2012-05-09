# source("f.lmer.reduced.model.rank.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.reduced.model.rank <- function(df.input, ...)   
{
  options(warn = 1) 
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status", 
    "c.prey.density.2.level", "c.prey.density.3.level",
    "c.rank.2.level", "c.rank.3.level", "d.age.months",
    "d.mass", "d.minutes.blood", "d.pcv", "i.glucose.green", "d.total.solids", "d.serology.index.rank.igg", "d.serology.index.rank.igm",
    "i.rank", "d.rank.proportion",   
    "d.age.months", "d.prey.density", "d.precipitation",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.cortisol", "d.testosterone", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
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
  df.temp.full$d.prey.density <- log10(df.temp.full$d.prey.density) 
  df.temp.full$d.precipitation <- log10(df.temp.full$d.precipitation) 
  print("summary(df.temp.full$d.precipitation)")
  print(summary(df.temp.full$d.precipitation))
  df.temp.full$d.age.months <- (df.temp.full$d.age.months) 
  df.temp.full$d.date <- (df.temp.full$i.sample.months)    # lm and lmer model response variables 
  print("summary(df.temp.full$d.date)")
  print(summary(df.temp.full$d.date))

  df.temp.full$d.mic.90.bka.ec <- log2(df.temp.full$d.mic.90.bka.ec)
  df.temp.full$d.mic.90.bka.pm <- log2(df.temp.full$d.mic.90.bka.pm)
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$d.rank.proportion)))  

  cv.dependent.factors <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm", "d.total.igg", "d.total.igm") 
  cv.dependent <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.total.igg", "d.total.igm") 
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
      warning()  # this command should flush the warning messages, which should allow me to locate where any future warnings are being generated
   
      print("Before subsetting within the loop.")
      i.full.c.id <- length(df.temp.full$c.id)
      i.full.unique.c.id <- length(unique(df.temp.full$c.id))
  
      # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
      #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
      df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.dependent]]))
  
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
      df.temp$d.dependent <- df.temp[[c.dependent]]    # lm and lmer model response variables 
      print("summary(df.temp$d.dependent)")
      print(summary(df.temp$d.dependent))
#      df.temp$d.dependent <- (df.temp[[c.dependent]])    # lm and lmer model response variables 
      df.temp$d.dependent.factor <- ordered(df.temp[[c.dependent.factor]])    # clm and clmm models need to have factors for response variables  
      ##############################  The data used in the clm and clmm calls need to be a global varialbe or the accessory function  ###############      
      ############################################################################################################################################
      ############################################################################################################################################
      ############################################################################################################################################
#      df.temp <<- df.temp
            
      print("")
      print(" ******* starting lmer models **************************")

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

      l.models <- list()

      par(mfrow = c(3,3))
      lmer.1 <- lmer((d.dependent) ~ (d.date) 
        + i.rank
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[1] <- lmer.1
print("a")
      par(mfrow = c(3,3))
      lmer.2 <- lmer((d.dependent) ~ (d.date) 
       + d.prey.density 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[2] <- lmer.2

      par(mfrow = c(3,3))
      lmer.3 <- lmer((d.dependent) ~ (d.date) 
        + c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[3] <- lmer.3

      par(mfrow = c(3,3))
      lmer.4 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.prey.density
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[4] <- lmer.4
      
      par(mfrow = c(3,3))
      lmer.5 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[5] <- lmer.5
      
      par(mfrow = c(3,3))
      lmer.6 <- lmer((d.dependent) ~ (d.date) 
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[6] <- lmer.6
      
      par(mfrow = c(3,3))
      lmer.7 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[7] <- lmer.7
      
      par(mfrow = c(3,3))
      lmer.8 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.prey.density
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[8] <- lmer.8
      
      par(mfrow = c(3,3))
      lmer.9 <- lmer((d.dependent) ~ (d.date) 
      + d.serology.index.rank.igg + d.serology.index.rank.igm
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[9] <- lmer.9

      par(mfrow = c(3,3))
      lmer.10 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[10] <- lmer.10


      par(mfrow = c(3,3))
      lmer.11 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.minutes.blood # + d.total.solids + d.mass + 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[11] <- lmer.11
#      plotLMER3d.fnc(lmer.11, pred = "i.rank", intr = "d.prey.density", plot.type = "contour")
#      plotLMER3d.fnc(lmer.11, pred = "i.rank", intr = "c.reproductive.status", plot.type = "persp")
#      plotLMER3d.fnc(lmer.11, pred = "d.prey.density", intr = "c.reproductive.status", plot.type = "persp")
#        plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "persp3d")
#        plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
#        plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "persp")
print("b")

      par(mfrow = c(3,3))
      lmer.12 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
        + d.serology.index.rank.igg # + d.serology.index.rank.igm
#          + d.total.igg + d.total.igm 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[12] <- lmer.12
#      plotLMER3d.fnc(lmer.12, pred = "i.rank", intr = "d.prey.density", plot.type = "contour")
#      plotLMER3d.fnc(lmer.12, pred = "i.rank", intr = "c.reproductive.status", plot.type = "persp")
#      plotLMER3d.fnc(lmer.12, pred = "d.prey.density", intr = "c.reproductive.status", plot.type = "persp")

print("c")

      par(mfrow = c(3,3))
      lmer.13 <- lmer((d.dependent) ~ (d.date) 
       + d.minutes.blood 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[13] <- lmer.13

      par(mfrow = c(3,3))
      lmer.14 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months * c.reproductive.status # + d.minutes.blood # + d.total.solids + d.mass + 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
        + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[14] <- lmer.14
      
      par(mfrow = c(3,3))
      lmer.15 <- lmer((d.dependent) ~ (d.date) 
      + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
      + d.serology.index.rank.igg # + d.serology.index.rank.igm
#          + d.total.igg + d.total.igm 
        + i.rank # * d.prey.density
        + c.reproductive.status 
        + d.prey.density # * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[15] <- lmer.15
      
      par(mfrow = c(3,3))
      lmer.16 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
        + i.rank # * d.prey.density
        + c.reproductive.status 
        + d.prey.density # * c.reproductive.status 
        + d.precipitation
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[16] <- lmer.16

      par(mfrow = c(3,3))
      lmer.17 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
        + d.cortisol
        + d.testosterone
        + d.mass
        + i.rank # * d.prey.density
        + c.reproductive.status 
        + d.prey.density # * c.reproductive.status 
        + d.precipitation
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[17] <- lmer.17

      par(mfrow = c(3,3))
      lmer.18 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months # + d.mass + d.minutes.blood + d.total.solids 
#          + d.cortisol
        + d.testosterone
        + d.mass
        + i.rank # * d.prey.density
        + c.reproductive.status * d.cortisol
        + d.prey.density # * c.reproductive.status 
        + d.precipitation
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[18] <- lmer.18

      par(mfrow = c(3,3))
      lmer.19 <- lmer((d.dependent) ~ (d.date) 
        + d.precipitation # * d.prey.density
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[19] <- lmer.19

      par(mfrow = c(3,3))
      lmer.20 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.prey.density
        + i.rank * d.precipitation
        + i.rank * c.reproductive.status
        + d.prey.density * d.precipitation
        + d.prey.density * c.reproductive.status
        + d.precipitation * c.reproductive.status
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[20] <- lmer.20

      par(mfrow = c(3,3))
      lmer.21 <- lmer((d.dependent) ~ (d.date) 
        + d.age.months
        + i.rank * d.prey.density
        + i.rank * d.precipitation
        + i.rank * c.reproductive.status
        + d.prey.density * d.precipitation
        + d.prey.density * c.reproductive.status
        + d.precipitation * c.reproductive.status
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[21] <- lmer.21

      par(mfrow = c(3,3))
      lmer.22 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.precipitation
        + d.precipitation * c.reproductive.status 
        + d.prey.density * d.precipitation 
        + i.rank * d.prey.density
        + i.rank * c.reproductive.status 
#          + d.prey.density * c.reproductive.status 
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[22] <- lmer.22

      par(mfrow = c(3,3))
      lmer.23 <- lmer((d.dependent) ~ (d.date) 
        + i.rank * d.precipitation
        + i.rank * d.age.months
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
      l.models[23] <- lmer.23
      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "persp")
#        plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "persp3d")
      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "persp")

      lmer.24 <- lmer((d.dependent) ~ (d.date) 
#          + i.rank
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 

      lmer.25 <- lmer((d.dependent) ~ (d.date) 
          + d.cortisol
        + (1 | c.id), 
        data = df.temp, REML = FALSE) 
        
      print(cftest(lmer.25))
      print(pamer.fnc(lmer.25))
      plot(resid(lmer.25) ~ fitted(lmer.25), main = terms(lmer.25))
      hist(resid(lmer.25), main = paste(c.dependent, ", lmer.25", sep = "")) 
      qqnorm(resid(lmer.25))
      qqline(resid(lmer.25))
      mcp.fnc(lmer.25)

print("d")
     
      i.number.of.models <- 23
      i <- 1
      for(i in i:i.number.of.models)
      {
        warning()
        print(" ********************************************************************************************" )
        print(paste(c.dependent, ",    loop: ", i))
        par(mfrow = c(2,2))
        print("l.models[[i]]")
        print(l.models[[i]])
        print(length(resid(l.models[[i]])))
        print(cftest(l.models[[i]]))
        if(i != 17 & i != 20)
        {
          print(pamer.fnc(l.models[[i]]))
        }
        plot(resid(l.models[[i]]) ~ fitted(l.models[[i]]), main = terms(l.models[[i]]))
        hist(resid(l.models[[i]]), main = paste(c.dependent, ", lmer.", i, sep = "")) 
        qqnorm(resid(l.models[[i]]))
        qqline(resid(l.models[[i]]))
        mcp.fnc(l.models[[i]])
#          plotRaw3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
#          plotLMER3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
        d.resid.mean <- mean(resid(l.models[[i]]))
        d.resid.var <- var(resid(l.models[[i]]))
        print(paste("resid.mean, var: ", d.resid.mean, d.resid.var))
        print(lillie.test(resid(l.models[[i]])))
        print(ad.test(resid(l.models[[i]])))
        print(cvm.test(resid(l.models[[i]])))
      }
      

#  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
#  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm
#  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
#  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
#  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
#  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

    t.aicc <- aictab(cand.set = list(lmer.1, lmer.2, lmer.3, lmer.4, lmer.5, lmer.6, lmer.7, lmer.8, lmer.9, lmer.10, lmer.11, 
      lmer.12, lmer.13, lmer.14, lmer.15, lmer.16, lmer.17, lmer.18, lmer.19, lmer.20, lmer.21, lmer.22, lmer.23, lmer.24), 
      modnames = c("lmer.1", "lmer.2", "lmer.3", "lmer.4", "lmer.5", "lmer.6", "lmer.7", "lmer.8", "lmer.9", "lmer.10", 
      "lmer.11", "lmer.12", "lmer.13", "lmer.14", "lmer.15", "lmer.16", "lmer.17", "lmer.18", "lmer.19", "lmer.20", "lmer.21", "lmer.22", 
      "lmer.23", "lmer.24"), 
      sort = TRUE)

    print("t.aicc")
    print(t.aicc)
    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent[i.counter.2], ".csv", sep = ""))

      print("")      
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
