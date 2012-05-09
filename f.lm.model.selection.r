# source("f.lm.model.selection.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lm.model.selection <- function(df.input, ...)   
{
  options(warn = 1) 
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status",
    "c.prey.density.2.level", "c.prey.density.3.level",
    "c.rank.2.level", "c.rank.3.level",
    "d.age.months", "i.number.adult.female.by.year", 
    "d.mass", "d.minutes.blood", "d.pcv", "i.glucose.green", "d.total.solids",
    "i.rank", "d.rank.proportion",   
    "d.prey.density", "d.precipitation",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
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
    "d.cortisol", "d.testosterone", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1999", "%d%b%Y"))
  df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
  df.temp.full$i.sample.years <- df.temp.full$i.sample.year - min(df.temp.full$i.sample.year) + 1

  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
  df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero

#  df.temp.full <- subset(df.temp.full, c.sex == "f" & i.sample.number.2 == 2)
#  df.temp.full <- subset(df.temp.full, c.sex == "f" & d.age.months >= 24)
#  df.temp.full <- subset(df.temp.full, !is.na(i.rank))  
#  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
#  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  
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

  df.temp.full$d.date <- (df.temp.full$i.sample.years)
  df.temp.full$i.rank <- df.temp.full$i.rank 
  df.temp.full$d.prey.density <- log(df.temp.full$d.prey.density) 
  df.temp.full$d.precipitation <- log(df.temp.full$d.precipitation) 
  df.temp.full$d.age.months <- (df.temp.full$d.age.months)
  df.temp.full$d.cortisol <- log(df.temp.full$d.cortisol) 
  df.temp.full$d.testosterone <- log(df.temp.full$d.testosterone) 
  df.temp.full$d.mass <- df.temp.full$d.mass 
  df.temp.full$d.minutes.blood <- df.temp.full$d.minutes.blood 
  df.temp.full$d.pcv <- df.temp.full$d.pcv 

  df.temp.full$d.mic.90.bka.ec <- log2(df.temp.full$d.mic.90.bka.ec)
  df.temp.full$d.mic.90.bka.pm <- log2(df.temp.full$d.mic.90.bka.pm)
  df.temp.full$d.total.igg <- log(df.temp.full$d.blank.abs.corrected.total.igg)
  df.temp.full$d.total.igm <- sqrt(df.temp.full$d.blank.abs.corrected.total.igm)
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
  cv.dependent <- c("d.total.igm") 

  m.cor <- cor(df.temp.full[, 11:73], use = "pairwise.complete.obs", method = c("pearson"))
  write.csv(m.cor, file = "temp.cor.csv")

  par(mfrow = c(2,3))
  i.counter.2 <- 1
  i.counter.2.stop <- length(cv.dependent)
    
  for(i.counter.2 in i.counter.2:i.counter.2.stop)  
  {
    c.dependent.factor <- cv.dependent.factors[i.counter.2]
    c.dependent <- cv.dependent[i.counter.2]
    
    print(paste(" **************** c.dependent: ", c.dependent, "**********************")) 
#    warning()  # this command should flush the warning messages, which should allow me to locate where any future warnings are being generated
 
    print("Before subsetting within the loop.")
    i.full.c.id <- length(df.temp.full$c.id)
    i.full.unique.c.id <- length(unique(df.temp.full$c.id))

    # We can only include records in the clmm models that do not include NAs, so we subset the df.temp.full to df.temp each time throug the loop.
    #  If the number of records in the clmm.0 and clmm. models are not the same, then the anova pval and summary pval will not match. Same goes for bootstrap values
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
    print(" ******* starting lm models **************************")

    l.models <- list()
    l.romr.models <- list()

    print("1")
    par(mfrow = c(3,3))
    c.formula <- formula(d.dependent ~ 
      d.date 
#      + d.age.months * i.rank
#      + d.age.months * i.rank
#      + i.rank * d.prey.density
#      + i.rank * c.reproductive.status
#      + i.rank * d.cortisol
#      + i.rank * d.testosterone      
#      + d.prey.density * c.reproductive.status
#      + d.prey.density * d.cortisol
#      + d.prey.density * d.testosterone      
#      + c.reproductive.status * d.cortisol
#      + c.reproductive.status * d.testosterone      
#      + d.cortisol * d.testosterone      
#      + (1 | c.id)
    )
    lm.1 <- lm(c.formula, data = df.temp, na.action = na.omit) #, family = gaussian(link = "identity")) 
    
#    lm.1 <- lm(d.dependent ~ 
#      d.date 
#      + d.age.months * i.rank
#      + d.age.months * i.rank
#      + i.rank * d.prey.density
#      + i.rank * c.reproductive.status
#      + i.rank * d.cortisol
#      + i.rank * d.testosterone      
#      + d.prey.density * c.reproductive.status
#      + d.prey.density * d.cortisol
#      + d.prey.density * d.testosterone      
#      + c.reproductive.status * d.cortisol
#      + c.reproductive.status * d.testosterone      
#      + d.cortisol * d.testosterone      
#      + (1 | c.id)
#      , 
#      data = df.temp, na.action = na.omit) #, family = gaussian(link = "identity")) 
 
    l.models[[1]] <- lm.1
#    print(str(lm.1))
#    print(str(l.models))
#    print(l.models[[1]]$model)
        
#      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
#      plotLMER3d.fnc(lmer.23, pred = "i.rank", intr = "d.precipitation", plot.type = "persp")
#      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "contour")
#      plotRaw3d.fnc(data = df.temp, response = "d.dependent", pred = "i.rank", intr = "d.precipitation", plot.type = "persp")
   
    i.number.of.models <- 1
    i <- 1
    for(i in i:i.number.of.models)
    {
#      warning()
      print(" ********************************************************************************************" )
      print(paste(c.dependent, ",    loop: ", i))
      print("length(resid(l.models[[i]]))")
      print(length(resid(l.models[[i]])))
      print("summary(l.models[[i]])")
      print(summary(l.models[[i]]))
      print(lillie.test(resid(l.models[[i]])))
      print(ad.test(resid(l.models[[i]])))
#      print(" mcmcsamp original model  ********************************************************************************************" )
#      lm.samp = mcmcsamp(l.models[[i]], n = 500)
#      lm.samp = mcmcsamp(l.models[[i]], n = 500)
#      print(" HPDinterval original model  ********************************************************************************************" )
#      print("HPDinterval(lm.samp, prob = 0.95)") 
#      print(HPDinterval(lm.samp, prob = 0.95)) 
 
      print(" removing outliers above 2.5 removed ***** ***** *** *** ***** ***** *** *** ***** ***** *** ***  ")  
#      l.romr.data <- romr.fnc(l.models[[i]], data = lm.1$model, trim = 2.5)
      l.romr.data <- f.remove.outliers(df.data = l.models[[i]], c.variable = "residuals", c.transform = NULL, trim = 2.5)


      print(" Updating models ********************************************************************************************" )
#      l.romr.models[i] <- update(l.models[[i]], data = l.romr.data$df.data)        
      lm.romr.1 <- lm(c.formula, data = l.romr.data$df.data, na.action = na.omit) #, family = gaussian(link = "identity")) 
      l.romr.models[[1]] <- lm.romr.1

      print("summary(l.romr.models[[i]]")
      print(summary(l.romr.models[[i]]))
      print(lillie.test(resid(l.romr.models[[i]])))
      print(ad.test(resid(l.romr.models[[i]])))
      
#      print(" mcmcsamp updated model  ********************************************************************************************" )
#      lm.romr.samp = mcmcsamp(l.romr.models[[i]], n = 500)
#      lm.romr.samp = mcmcsamp(l.romr.models[[i]], n = 500)
#      print(" HPDinterval updated model  ********************************************************************************************" )
#      print("HPDinterval(lm.romr.samp, prob = 0.95)") 
#      print(HPDinterval(lm.romr.samp, prob = 0.95)) 
#      print(pamer.fnc(l.models[[i]]), ndigits = 8)
#        mcposthoc.fnc(lm.samp, two.tailed = TRUE, mcmc = TRUE, nsim = 100, ndigits = 8)  # var, 
#        mcposthoc.fnc(lm.romr.samp, two.tailed = TRUE, mcmc = TRUE, nsim = 100, ndigits = 8)  # var, 
      par(mfrow = c(2,4))
      plot(l.models[[i]]$model[[1]] ~ l.models[[i]]$model[[2]], main = terms(l.models[[i]]))
      plot(resid(l.models[[i]]) ~ fitted(l.models[[i]]), main = terms(l.models[[i]]))
      hist(resid(l.models[[i]]), main = paste(c.dependent, ", lm.", i, sep = "")) 
      qqnorm(resid(l.models[[i]]))
      qqline(resid(l.models[[i]]))
#      mcp.fnc(l.models[[i]])
#      print(densityplot(lm.samp))
#      print(qqmath(lm.samp))
#      print(xyplot(lm.samp))

      par(mfrow = c(2,4))
      plot(l.romr.models[[i]]$model[[1]] ~ l.romr.models[[i]]$model[[2]], main = terms(l.romr.models[[i]]))
      plot(resid(l.romr.models[[i]]) ~ fitted(l.romr.models[[i]]), main = terms(l.romr.models[[i]]))
      hist(resid(l.romr.models[[i]]), main = paste(c.dependent, ", lm.", i, sep = "")) 
      qqnorm(resid(l.romr.models[[i]]))
      qqline(resid(l.romr.models[[i]]))
#      mcp.fnc(l.romr.models[[i]])
 #     print(densityplot(lm.samp))
#      print(qqmath(lm.samp))
#      print(xyplot(lm.samp))
#          plotRaw3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
#          plotLMER3d.fnc(l.models[[i]], pred = "i.rank", intr = "d.precipitation")
    }
    
#    t.aicc <- aictab(cand.set = list(lm.1, lm.2, lm.3, lm.4, lm.5, lm.6, lm.7, lm.8, lm.9, lm.10, lm.11, 
#      lm.12, lm.13, lm.14, lm.15, lm.16, lm.17, lm.18, lm.19, lm.20, lm.21, lm.22, lm.23, lm.24), 
#      modnames = c("lm.1", "lm.2", "lm.3", "lm.4", "lm.5", "lm.6", "lm.7", "lm.8", "lm.9", "lm.10", 
#      "lm.11", "lm.12", "lm.13", "lm.14", "lm.15", "lm.16", "lm.17", "lm.18", "lm.19", "lm.20", "lm.21", "lm.22", 
#      "lm.23", "lm.24"), 
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
