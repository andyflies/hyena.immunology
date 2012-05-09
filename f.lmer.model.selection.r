# source("f.lmer.model.selection.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.model.selection <- function(df.input, ...)   
{
  options(warn = 1) 
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c(
    "d.age.months", "i.number.adult.female.by.year", 
    "d.mass", "d.minutes.blood", "d.pcv", "i.glucose.green", "d.total.solids",
    "i.rank", "d.rank.proportion",   
    "d.prey.density", "d.precipitation",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.mic.90.level.median.bka.ec", "d.mic.90.level.median.bka.pm",
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", 
    "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm",
    "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm", "i.cmi.bsa.igg.rank", "i.cmi.cav2.igg.rank",
    "i.cmi.ccv.igg.rank", "i.cmi.cdv.igg.rank", "i.cmi.dirofilaria.igg.rank", "i.cmi.fiv.p24.igg.rank",
    "i.cmi.bsa.igm.rank", "i.cmi.cav2.igm.rank", "i.cmi.ccv.igm.rank", "i.cmi.cdv.igm.rank", "i.cmi.dirofilaria.igm.rank",
    "i.cmi.fiv.p24.igm.rank", "d.serology.index.igg", "d.serology.index.rank.igg", "d.serology.index.igm", "d.serology.index.rank.igm",
    "d.neutrophil.lymphocyte.ratio", "d.relative.eosinophils", "d.relative.lymphocytes", "d.relative.monocytes", "d.relative.neutrophils", "d.total.wbc",
    "d.cortisol", "d.testosterone", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month",
    "c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status",
    "c.prey.density.2.level", "c.prey.density.3.level", # "i.weights",
    "c.rank.2.level", "c.rank.3.level"
    ))
    
  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
  df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
#  df.temp.full$i.sample.years <- df.temp.full$i.sample.year - min(df.temp.full$i.sample.year) + 1

  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
  df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero

#  df.temp.full <- subset(df.temp.full, c.sex == "f" & i.sample.number.2 == 2)
  df.temp.full <- subset(df.temp.full, c.sex == "f" & d.age.months >= 24)
#  df.temp.full <- subset(df.temp.full, !is.na(i.rank))  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  
#  df.temp.full <- subset(df.temp.full, !is.na(c.reproductive.status))  
#  df.temp.full <- subset(df.temp.full, !is.na(d.prey.density))  
  
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  

#  df.temp.full$i.rank <- scale(df.temp.full$i.rank, scale = TRUE) 
#  df.temp.full$d.prey.density <- scale((df.temp.full$d.prey.density), scale = TRUE) 
#  df.temp.full$d.precipitation <- scale((df.temp.full$d.precipitation), scale = TRUE) 
#  df.temp.full$d.age.months <- scale((df.temp.full$d.age.months), scale = TRUE)
#  df.temp.full$d.cortisol <- scale((df.temp.full$d.cortisol), scale = TRUE) 
#  df.temp.full$d.testosterone <- scale(log(df.temp.full$d.testosterone), scale = TRUE) 
#  df.temp.full$d.mass <- scale(df.temp.full$d.mass, scale = TRUE) 
#  df.temp.full$d.minutes.blood <- scale(df.temp.full$d.minutes.blood, scale = TRUE) 
#  df.temp.full$d.pcv <- scale(df.temp.full$d.pcv, scale = TRUE) 

#  df.temp.full$d.date <- time(df.temp.full$i.sample.days)
  df.temp.full$d.date <- scale(df.temp.full$i.sample.days)
  df.temp.full$i.rank <- scale(df.temp.full$i.rank) 
  df.temp.full$d.prey.density <- scale(log(df.temp.full$d.prey.density)) 
  df.temp.full$d.precipitation <- scale(log(df.temp.full$d.precipitation)) 
  df.temp.full$d.age.months <- scale(log(df.temp.full$d.age.months))
  df.temp.full$d.cortisol <- scale(log(df.temp.full$d.cortisol)) 
  df.temp.full$d.testosterone <- scale(log(df.temp.full$d.testosterone)) 
  df.temp.full$d.mass <- scale(df.temp.full$d.mass)
  df.temp.full$d.minutes.blood <- scale(df.temp.full$d.minutes.blood) 
  df.temp.full$d.pcv <- scale(df.temp.full$d.pcv)

  df.temp.full$d.mic.90.level.median.bka.ec <- (df.temp.full$d.mic.90.level.median.bka.ec / 8)
#  df.temp.full$d.mic.90.bka.ec <- (log2(df.temp.full$d.mic.90.bka.ec) / log2(640))
  df.temp.full$d.mic.90.bka.ec <- log2(df.temp.full$d.mic.90.bka.ec)
  df.temp.full$d.mic.90.bka.pm <- log2(df.temp.full$d.mic.90.bka.pm)
  df.temp.full$d.total.igg <- (df.temp.full$d.blank.abs.corrected.total.igg)
  df.temp.full$d.total.igm <- (df.temp.full$d.blank.abs.corrected.total.igm)
#  df.temp.full$d.total.igg <- log(df.temp.full$d.blank.abs.corrected.total.igg)
#  df.temp.full$d.total.igm <- sqrt(df.temp.full$d.blank.abs.corrected.total.igm)
  df.temp.full$d.ec.igg <- df.temp.full$d.blank.abs.ec.igg
  df.temp.full$d.pm.igg <- df.temp.full$d.blank.abs.pm.igg
  df.temp.full$d.ec.igm <- df.temp.full$d.blank.abs.ec.igm
  df.temp.full$d.pm.igm <- df.temp.full$d.blank.abs.pm.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$d.rank.proportion)))  

  cv.dependent.factors <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm", "d.total.igg", "d.total.igm") 
  cv.dependent <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.total.igg", "d.total.igm") 
#  cv.dependent <- c("d.mic.90.level.median.bka.ec") 
#  cv.dependent <- c("d.mic.90.bka.ec") 
#  cv.dependent <- c("d.mic.90.bka.pm") 
#  cv.dependent <- c("d.total.igg") 
#  cv.dependent <- c("d.total.igm") 

  m.cor <- cor(df.temp.full[, 1:63], use = "pairwise.complete.obs", method = c("spearman"))
  write.csv(m.cor, file = "temp.cor.csv")

  par(mfrow = c(2,3))
  i.counter.2 <- 1
  i.counter.2.stop <- length(cv.dependent)
    
  for(i.counter.2 in i.counter.2:i.counter.2.stop)  
  {
    c.dependent.factor <- cv.dependent.factors[i.counter.2]
    c.dependent <- cv.dependent[i.counter.2]
    
    print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
 
    print("Before subsetting within the loop.")
    i.full.c.id <- length(df.temp.full$c.id)
    i.full.unique.c.id <- length(unique(df.temp.full$c.id))

    # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
    #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and cftest pval will not match. Same goes for bootstrap values
    df.temp <- subset(df.temp.full, !is.na(df.temp.full[[c.dependent]]))

    df.temp$d.dependent <- df.temp[[c.dependent]]    # lm and lmer model response variables 
    print("summary(df.temp$d.dependent)")
    print(summary(df.temp$d.dependent))
    df.temp$d.dependent.factor <- ordered(df.temp[[c.dependent.factor]])    # clm and clmm models need to have factors for response variables  
    ##############################  The data used in the clm and clmm calls need to be a global varialbe or the accessory function  ###############      
    ############################################################################################################################################
    ############################################################################################################################################
    ############################################################################################################################################
#      df.temp <<- df.temp

    print("")
    print(" ******* starting lmer models **************************")

    l.models <- list()
    l.romr.models <- list()

    print("1")
    par(mfrow = c(3,3))
    lmer.1 <- lmer(d.dependent ~ 
      d.date 
      + d.rank.proportion
      + c.reproductive.status
      + d.prey.density
      + d.age.months
#      + d.cortisol
#      + d.testosterone
      + d.rank.proportion * d.prey.density      
#      + d.rank.proportion * c.reproductive.status      
#      + d.age.months * d.rank.proportion
#      + d.age.months * d.rank.proportion
#      + d.rank.proportion * d.prey.density
#      + d.rank.proportion * c.reproductive.status
#      + d.rank.proportion * d.cortisol
#      + d.rank.proportion * d.testosterone      
#      + d.prey.density * c.reproductive.status
#      + d.prey.density * d.cortisol
#      + d.prey.density * d.testosterone      
#      + c.reproductive.status * d.cortisol
#      + c.reproductive.status * d.testosterone      
#      + d.cortisol * d.testosterone      
      + (1 | c.id), 
      data = df.temp, REML = FALSE, na.action = na.omit, control = list(maxIter = 1000), verbose = TRUE) #, family = binomial (link = "logit")) 
#      data = df.temp, REML = FALSE, na.action = na.omit, control = list(maxIter = 1000), verbose = TRUE, family = binomial (link = "logit")) 
 
    l.models[1] <- lmer.1        
#      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
#      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "persp")
#      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
#      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "persp")

    save(l.models, file = "test.RData")
   
    i.number.of.models <- 1
    i <- 1
    for(i in i:i.number.of.models)
    {
      print(" ********************************************************************************************" )
      print(paste(c.dependent, ",    loop: ", i))
      print("length(resid(l.models[[i]]))")
      print(length(resid(l.models[[i]])))
      print("cftest(l.models[[i]])")
      print(cftest(l.models[[i]]))
      print(lillie.test(resid(l.models[[i]])))
      print(ad.test(resid(l.models[[i]])))
      print(" mcmcsamp original model  ********************************************************************************************" )
      print("lmer.samp = mcmcsamp(l.models[[i]], n = 10000")
      lmer.samp = mcmcsamp(l.models[[i]], n = 10000)
      print(" HPDinterval original model  ********************************************************************************************" )
      print("HPDinterval(lmer.samp, prob = 0.95)") 
      print(HPDinterval(lmer.samp, prob = 0.95)) 
      print(pamer.fnc(l.models[[i]]), ndigits = 8)
      print("pamer.fnc(l.models[[i]]), ndigits = 8")
 
      print(" removing outliers above 2.5 removed ***** ***** *** *** ***** ***** *** *** ***** ***** *** ***  ")  
      l.romr.data <- romr.fnc(l.models[[i]], data = lmer.1@frame, trim = 2.5)
      print(length(df.temp[,1]))

      print(" Updating models ********************************************************************************************" )
      l.romr.models[i] <- update(l.models[[i]], data = l.romr.data$data)        
      print("cftest(l.romr.models[[i]]")
      print(cftest(l.romr.models[[i]]))
      print(lillie.test(resid(l.romr.models[[i]])))
      print(ad.test(resid(l.romr.models[[i]])))
      
      print(" mcmcsamp updated model  ********************************************************************************************" )
      lmer.romr.samp = mcmcsamp(l.romr.models[[i]], n = 10000)
      print(" HPDinterval updated model  ********************************************************************************************" )
      print("HPDinterval(lmer.romr.samp, prob = 0.95)") 
      print(HPDinterval(lmer.romr.samp, prob = 0.95)) 
      print("pamer.fnc(l.romr.models[[i]]), ndigits = 8")
      print(pamer.fnc(l.romr.models[[i]]), ndigits = 8)
      
      par(mfrow = c(2,4))
      boxplot(model.frame(l.models[[i]])[[1]] ~ model.frame(l.models[[i]])[[2]], main = terms(l.models[[i]]))
      plot(model.frame(l.models[[i]])[[1]] ~ model.frame(l.models[[i]])[[2]], main = terms(l.models[[i]]))
      plot(resid(l.models[[i]]) ~ model.frame(l.models[[i]])[[2]], main = terms(l.models[[i]]))
      plot(scale(resid(l.models[[i]])) ~ fitted(l.models[[i]]), main = terms(l.models[[i]]))
      print(cor.test(resid(l.models[[i]]), fitted(l.models[[i]]), method = "pearson"))
      hist(resid(l.models[[i]]), main = paste(c.dependent, ", lmer.", i, sep = "")) 
      qqnorm(resid(l.models[[i]]))
      qqline(resid(l.models[[i]]))
      mcp.fnc(l.models[[i]])
      print(densityplot(lmer.samp))
      print(qqmath(lmer.samp))
      print(xyplot(lmer.samp))

      par(mfrow = c(2,4))
      boxplot(model.frame(l.models[[i]])[[1]] ~ model.frame(l.models[[i]])[[2]], main = terms(l.models[[i]]))
      plot(model.frame(l.romr.models[[i]])[[1]] ~ model.frame(l.romr.models[[i]])[[2]], main = terms(l.romr.models[[i]]))
      plot(resid(l.romr.models[[i]]) ~ model.frame(l.romr.models[[i]])[[2]], main = terms(l.romr.models[[i]]))
      plot(scale(resid(l.romr.models[[i]])) ~ fitted(l.romr.models[[i]]), main = terms(l.romr.models[[i]]))
      hist(resid(l.romr.models[[i]]), main = paste(c.dependent, ", lmer.", i, sep = "")) 
      qqnorm(resid(l.romr.models[[i]]))
      qqline(resid(l.romr.models[[i]]))
      mcp.fnc(l.romr.models[[i]])
      print(densityplot(lmer.samp))
      print(qqmath(lmer.samp))
      print(xyplot(lmer.samp))
#          plotRaw3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
#          plotLMER3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
    }
    
#    t.aicc <- aictab(cand.set = list(lmer.1, lmer.2, lmer.3, lmer.4, lmer.5, lmer.6, lmer.7, lmer.8, lmer.9, lmer.10, lmer.11, 
#      lmer.12, lmer.13, lmer.14, lmer.15, lmer.16, lmer.17, lmer.18, lmer.19, lmer.20, lmer.21, lmer.22, lmer.23, lmer.24), 
#      modnames = c("lmer.1", "lmer.2", "lmer.3", "lmer.4", "lmer.5", "lmer.6", "lmer.7", "lmer.8", "lmer.9", "lmer.10", 
#      "lmer.11", "lmer.12", "lmer.13", "lmer.14", "lmer.15", "lmer.16", "lmer.17", "lmer.18", "lmer.19", "lmer.20", "lmer.21", "lmer.22", 
#      "lmer.23", "lmer.24"), 
#      sort = TRUE)

#    print("t.aicc")
#    print(t.aicc)
#    write.csv(t.aicc, file = paste("t.aicc.", cv.dependent[i.counter.2], ".csv", sep = ""))

    print("")      
    par(mfrow = c(3,3))
  }
  

  par(mfrow = c(1,1))
  plot(1,1, main = paste("space filler"))
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  
