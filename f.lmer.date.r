# source("f.lmer.date.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lmer.date <- function(df.input, ...)   
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
#  print()
  df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status", 
    "c.prey.density.2.level", "c.prey.density.3.level",
    "i.rank", "d.rank.proportion", "c.rank.2.level", "c.rank.3.level", "d.age.months",  
    "d.age.months", "d.prey.density",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.c", "d.t", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    
#  df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
  df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
  df.temp.full$i.sample.years <- df.temp.full$i.sample.years - min(df.temp.full$i.sample.years) + 1

  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
  df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
  df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero
#  df.temp.full$d.date <- sqrt(df.temp.full$i.sample.years)
  df.temp.full$d.date <- (df.temp.full$i.sample.years)
  print("summary(df.temp.full$d.date)")
  print(summary(df.temp.full$d.date))
  
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

  df.temp.full$d.mic.90.bka.ec <- log2(df.temp.full$d.mic.90.bka.ec)
  df.temp.full$d.mic.90.bka.pm <- log2(df.temp.full$d.mic.90.bka.pm)
  df.temp.full$d.total.igg <- df.temp.full$d.blank.abs.corrected.total.igg
  df.temp.full$d.total.igm <- df.temp.full$d.blank.abs.corrected.total.igm

  print(length(df.temp.full$c.id))
  print(length(unique(df.temp.full$c.id)))
  print(length(unique(df.temp.full$i.rank)))  

  cv.dependent.factors <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 
  cv.dependent <- c("d.mic.90.bka.ec", "d.mic.90.bka.pm") 
#  cv.dependent <- c("d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm") 

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
#      df.temp$d.dependent <- (df.temp[[c.dependent]])    # lm and lmer model response variables 
      df.temp$d.dependent.factor <- ordered(df.temp[[c.dependent.factor]])    # clm and clmm models need to have factors for response variables  

      lmer.1 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp, REML = FALSE) 
      print(summary(lmer.1))
      print(cftest(lmer.1))
      l.lmer.romr.1 <- romr.fnc(lmer.1, data = df.temp, trim = 2.5)
      lmer.romr.1 <- update(lmer.1, data = l.lmer.romr.1$data)        
      print(summary(lmer.romr.1))
      print(cftest(lmer.romr.1))

      lmer.2 <- lmer(d.dependent ~ d.date + (1 | c.id), data = df.temp, REML = FALSE) 
      print(summary(lmer.2))
      print(cftest(lmer.2))
      l.lmer.romr.2 <- romr.fnc(lmer.2, data = df.temp, trim = 2.5)
      lmer.romr.2 <- update(lmer.2, data = l.lmer.romr.2$data)        
      print(summary(lmer.romr.2))
      print(cftest(lmer.romr.2))
      
      print(anova(lmer.romr.1, lmer.romr.2))

      par(mfrow = c(2,4))
      plot(d.dependent ~ d.date, data = df.temp)
      plot(jitter(resid(lmer.1)) ~ jitter(fitted(lmer.1)), main = terms(lmer.1))
      hist(resid(lmer.1)) #, breaks = sort(unique(lmer.1$d.residuals))))
      qqnorm(resid(lmer.1))
      qqline(resid(lmer.1))
      plot(d.dependent ~ d.date, data = df.temp)
      plot(jitter(resid(lmer.romr.1)) ~ jitter(fitted(lmer.romr.1)), main = terms(lmer.1))
      hist(resid(lmer.romr.1)) #, breaks = sort(unique(lmer.1$d.residuals))))
      qqnorm(resid(lmer.romr.1))
      qqline(resid(lmer.romr.1))
      mcp.fnc(lmer.1)
      mcp.fnc(lmer.romr.1)
      
      par(mfrow = c(2,4))
      plot(d.dependent ~ d.date, data = df.temp)
      plot(jitter(resid(lmer.2)) ~ jitter(fitted(lmer.2)), main = terms(lmer.2))
      hist(resid(lmer.2)) #, breaks = sort(unique(lmer.2$d.residuals))))
      qqnorm(resid(lmer.2))
      qqline(resid(lmer.2))
      plot(d.dependent ~ d.date, data = df.temp)
      plot(jitter(resid(lmer.romr.2)) ~ jitter(fitted(lmer.romr.2)), main = terms(lmer.2))
      hist(resid(lmer.romr.2)) #, breaks = sort(unique(lmer.2$d.residuals))))
      qqnorm(resid(lmer.romr.2))
      qqline(resid(lmer.romr.2))
      mcp.fnc(lmer.2)
      mcp.fnc(lmer.romr.2)

      print(lillie.test(resid(lmer.1)))
      print(ad.test(resid(lmer.1)))
      print(lillie.test(resid(lmer.romr.1)))
      print(ad.test(resid(lmer.romr.1)))
      print(lillie.test(resid(lmer.2)))
      print(ad.test(resid(lmer.2)))
      print(lillie.test(resid(lmer.romr.2)))
      print(ad.test(resid(lmer.romr.2)))


  }      
      print("")

  par(mfrow = c(1,1))
  plot(1,1, main = paste("space filler"))
#  df.output.final <- cbind(df.output.final, i.replicates)
#  write.csv(x = df.output.final, file = paste("df.ouput.final.", c.independent, ".csv", sep = ""))
#  print("df.output.final.csv is written", sep = "")
  d.end.time <- Sys.time()
  print(d.start.time)
  print(d.end.time)
}  
