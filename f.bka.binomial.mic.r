# source("f.bka.binomial.mic.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.bka.binomial.mic <- function()
{
  d.start.time <- Sys.time()

  df.temp.full <- df.bka.full.dilution
  df.temp.full$c.90.percent.inhibit <- as.integer(df.temp.full$c.90.percent.inhibit)
   
#  df.temp.full <- subset(df.bka.full.dilution, c.sex == "f" & i.replicate.number == 1)
  df.temp.full <- subset(df.temp.full, c.sex == "f")
  df.temp.full <- subset(df.temp.full, d.age.months >= 24 )
#  df.temp.full <- subset(df.temp.full, !is.na(d.age.months))
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  
#  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  
#  df.temp.full <- subset(df.temp.full, !(is.na(d.prey.low.density.prior)))  
#  df.temp.full <- subset(df.temp.full, !(is.na(d.prey.low.density.current)))  
  df.temp.full.ec <- subset(df.temp.full, c.strain == "ec")
  df.temp.full.pm <- subset(df.temp.full, c.strain == "pm")

  # Keep only the dilutions in which there is at least one difference (i.e. get rid of the first 4 dilutions for EC, because all samples inhibited at these dilutions
  df.temp.full.ec <- subset(df.temp.full.ec, i.sample.dilution >= 80)
  df.temp.full.pm <- subset(df.temp.full.pm, i.sample.dilution <= 40)

#  print(xtabs(~ c.id + i.sample.dilution + i.replicate.number, df.temp.full.ec))
#  print(xtabs(~ c.id + i.sample.dilution + i.replicate.number, df.temp.full.ec))

  print("length(df.temp.full$c.id)")
  print(length(df.temp.full$c.id))

#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "ger" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "jj" & i.sample.number.2 == 1))


  df.ec <- df.temp.full.ec[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "c.90.percent.inhibit", "i.sample.dilution", "i.replicate.number", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.prey.density <- log((df.ec$d.prey.low.density.prior + df.ec$d.prey.low.density.current) / 2) 
  
  df.pm <- df.temp.full.pm[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "c.90.percent.inhibit", "i.sample.dilution", "i.replicate.number", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.pm$d.prey.density <- log((df.pm$d.prey.low.density.prior + df.pm$d.prey.low.density.current) / 2) 

  # remove the replicates by taking the median of the 3 replicates
  df.ec <- aggregate(c.90.percent.inhibit ~
    c.id + i.rank + d.age.months + c.reproductive.status + d.sample.date + c.strain + 
    c.90.percent.inhibit + i.sample.dilution + d.prey.density, data = df.ec, FUN = median) #, na.action = na.pass)
  df.pm <- aggregate(c.90.percent.inhibit ~
    c.id + i.rank + d.age.months + c.reproductive.status + d.sample.date + c.strain + 
    c.90.percent.inhibit + i.sample.dilution + d.prey.density, data = df.pm, FUN = median) #, na.action = na.pass)
    
#  print(df.ec$i.sample.dilution)  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 5])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 10])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 20])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 40])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 80])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 160])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 320])  
  print(df.ec$c.90.percent.inhibit[df.ec$i.sample.dilution == 640])  
print("")
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 5])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 10])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 20])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 40])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 80])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 160])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 320])  
  print(df.pm$c.90.percent.inhibit[df.pm$i.sample.dilution == 640])  

  print(df.ec[df.ec$i.sample.dilution == 80,])  
  print(df.ec[df.ec$i.sample.dilution == 160,])  
  print(df.ec[df.ec$i.sample.dilution == 320,])  
  print(df.ec[df.ec$i.sample.dilution == 640,])  

  print(df.pm[df.pm$i.sample.dilution == 5,])  
  print(df.pm[df.pm$i.sample.dilution == 10,])  
  print(df.pm[df.pm$i.sample.dilution == 20,])  
  print(df.pm[df.pm$i.sample.dilution == 40,])  
  print(df.pm[df.pm$i.sample.dilution == 80,])  
    
#  df.ec$d.dependent.1 <- log2(df.ec$d.mic.90)
#  df.pm$d.dependent.1 <- log2(df.pm$d.mic.90)
  df.ec$d.dependent.1 <- df.ec$c.90.percent.inhibit
  df.pm$d.dependent.1 <- df.pm$c.90.percent.inhibit
   
#  df.ec$d.independent.1 <- log(df.ec$d.age.months)
#  df.pm$d.independent.1 <- log(df.pm$d.age.months)
  df.ec$d.independent.1 <- df.ec$i.rank
  df.pm$d.independent.1 <- df.pm$i.rank
  df.ec$d.independent.2 <- df.ec$c.reproductive.status
  df.pm$d.independent.2 <- df.pm$c.reproductive.status
#  df.ec$d.independent.3 <- df.ec$d.age.months
#  df.pm$d.independent.3 <- df.pm$d.age.months
  df.ec$d.independent.3 <- df.ec$d.prey.density
  df.pm$d.independent.3 <- df.pm$d.prey.density
  
  df.ec <- subset(df.ec, !is.na(d.independent.1))
  df.ec <- subset(df.ec, !is.na(d.independent.2))
  df.ec <- subset(df.ec, !is.na(d.independent.3))
  df.pm <- subset(df.pm, !is.na(d.independent.1))
  df.pm <- subset(df.pm, !is.na(d.independent.2))
  df.pm <- subset(df.pm, !is.na(d.independent.3))

  print(summary(df.ec)) 
  print(summary(df.pm)) 
  par(mfrow = c(3,4))
 
  print("******************************** EC *******************************")
  print("******************************** EC *******************************")
  print("******************************** model 1 *******************************")

  lmer.0.ec <- lmer((d.dependent.1) ~ 1 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.1.ec <- lmer((d.dependent.1) ~ d.independent.1 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.2.ec <- lmer((d.dependent.1) ~ d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.3.ec <- lmer((d.dependent.1) ~ d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.4.ec <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.5.ec <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial)
  lmer.6.ec <- lmer(d.dependent.1 ~ d.independent.2 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.7.ec <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.2 + d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.8.ec <- lmer(d.dependent.1 ~ d.independent.1 * d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.9.ec <- lmer(d.dependent.1 ~ d.independent.1 * d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.10.ec <- lmer(d.dependent.1 ~ d.independent.2 * d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.11.ec <- lmer(d.dependent.1 ~ d.independent.1*d.independent.2 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.12.ec <- lmer(d.dependent.1 ~ d.independent.1*d.independent.3 +  d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.13.ec <- lmer(d.dependent.1 ~ d.independent.1 + d.independent.2*d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.14.ec <- lmer(d.dependent.1 ~ d.independent.1*d.independent.2 + d.independent.1*d.independent.3 + d.independent.2*d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.ec, REML = FALSE, family = binomial, nAGQ = 1)
  print(anova(lmer.0.ec, lmer.1.ec, lmer.2.ec, lmer.3.ec, lmer.4.ec, lmer.5.ec, lmer.6.ec, lmer.7.ec, lmer.8.ec, 
    lmer.9.ec, lmer.10.ec, lmer.11.ec, lmer.12.ec, lmer.13.ec, lmer.14.ec))
  print("")
  print("")
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  print("lillie.test(resid(lmer.1.ec))") 
  print(lillie.test(resid(lmer.1.ec))) 
  print("hist(resid(lmer.1.ec))") 
  hist(resid(lmer.1.ec))
  print("qqnorm(resid(lmer.1.ec)") 
  qqnorm(resid(lmer.1.ec))
  print("qqline(resid(lmer.1.ec)") 
  qqline(resid(lmer.1.ec))
  print("")
  print("summary(lmer.2.ec)") 
  print(summary(lmer.2.ec)) 
  print("lillie.test(resid(lmer.2.ec)") 
  print(lillie.test(resid(lmer.2.ec))) 
  print("hist(resid(lmer.2.ec)") 
  hist(resid(lmer.2.ec))
  print("qqnorm(resid(lmer.2.ec)") 
  qqnorm(resid(lmer.2.ec)) 
  print("qqline(resid(lmer.2.ec)") 
  qqline(resid(lmer.2.ec))
  print("")
  print("summary(lmer.3.ec)") 
  print(summary(lmer.3.ec)) 
  print("lillie.test(resid(lmer.3.ec))") 
  print(lillie.test(resid(lmer.3.ec))) 
  print("hist(resid(lmer.3.ec)") 
  hist(resid(lmer.3.ec)) 
  print("qqnorm(resid(lmer.3.ec)") 
  qqnorm(resid(lmer.3.ec))
  print("qqline(resid(lmer.3.ec)") 
  qqline(resid(lmer.3.ec))
  print("")
  print("summary(lmer.4.ec)") 
  print(summary(lmer.4.ec)) 
  print("lillie.test(resid(lmer.4.ec))") 
  print(lillie.test(resid(lmer.4.ec))) 
  print("hist(resid(lmer.4.ec)") 
  hist(resid(lmer.4.ec))
  print("qqnorm(resid(lmer.4.ec)") 
  qqnorm(resid(lmer.4.ec))
  print("qqline(resid(lmer.4.ec)") 
  qqline(resid(lmer.4.ec))
  print("")
  print("")
  print("summary(lmer.5.ec)") 
  print(summary(lmer.5.ec)) 
  print("lillie.test(resid(lmer.5.ec))") 
  print(lillie.test(resid(lmer.5.ec))) 
  print("hist(resid(lmer.5.ec)") 
  hist(resid(lmer.5.ec))
  print("qqnorm(resid(lmer.5.ec)") 
  qqnorm(resid(lmer.5.ec))
  print("qqline(resid(lmer.5.ec)") 
  qqline(resid(lmer.5.ec))
  print("")
  print("summary(lmer.6.ec)") 
  print(summary(lmer.6.ec)) 
  print("lillie.test(resid(lmer.6.ec))") 
  print(lillie.test(resid(lmer.6.ec))) 
  print("hist(resid(lmer.6.ec)") 
  hist(resid(lmer.6.ec))
  print("qqnorm(resid(lmer.6.ec)") 
  qqnorm(resid(lmer.6.ec))
  print("qqline(resid(lmer.6.ec)") 
  qqline(resid(lmer.6.ec))
  print("")
  print("summary(lmer.7.ec)") 
  print(summary(lmer.7.ec)) 
  print("lillie.test(resid(lmer.7.ec))") 
  print(lillie.test(resid(lmer.7.ec))) 
  print("hist(resid(lmer.7.ec)") 
  hist(resid(lmer.7.ec))
  print("qqnorm(resid(lmer.7.ec)") 
  qqnorm(resid(lmer.7.ec))
  print("qqline(resid(lmer.7.ec)") 
  qqline(resid(lmer.7.ec))
  print("")
  print("summary(lmer.8.ec)") 
  print(summary(lmer.8.ec)) 
  print("lillie.test(resid(lmer.8.ec))") 
  print(lillie.test(resid(lmer.8.ec))) 
  print("hist(resid(lmer.8.ec)") 
  hist(resid(lmer.8.ec))
  print("qqnorm(resid(lmer.8.ec)") 
  qqnorm(resid(lmer.8.ec))
  print("qqline(resid(lmer.8.ec)") 
  qqline(resid(lmer.8.ec))
  print("")
  print("")
  print("summary(lmer.9.ec)") 
  print(summary(lmer.9.ec)) 
  print("lillie.test(resid(lmer.9.ec))") 
  print(lillie.test(resid(lmer.9.ec))) 
  print("hist(resid(lmer.9.ec)") 
  hist(resid(lmer.9.ec))
  print("qqnorm(resid(lmer.9.ec)") 
  qqnorm(resid(lmer.9.ec))
  print("qqline(resid(lmer.9.ec)") 
  qqline(resid(lmer.9.ec))
  print("")
  print("summary(lmer.10.ec)") 
  print(summary(lmer.10.ec)) 
  print("lillie.test(resid(lmer.10.ec))") 
  print(lillie.test(resid(lmer.10.ec))) 
  print("hist(resid(lmer.10.ec)") 
  hist(resid(lmer.10.ec))
  print("qqnorm(resid(lmer.10.ec)") 
  qqnorm(resid(lmer.10.ec))
  print("qqline(resid(lmer.10.ec)") 
  qqline(resid(lmer.10.ec))
  print("")
  print("summary(lmer.11.ec)") 
  print(summary(lmer.11.ec)) 
  print("lillie.test(resid(lmer.11.ec))") 
  print(lillie.test(resid(lmer.11.ec))) 
  print("hist(resid(lmer.11.ec)") 
  hist(resid(lmer.11.ec))
  print("qqnorm(resid(lmer.11.ec)") 
  qqnorm(resid(lmer.11.ec))
  print("qqline(resid(lmer.11.ec)") 
  qqline(resid(lmer.11.ec))
  print("")
  print("summary(lmer.12.ec)") 
  print(summary(lmer.12.ec)) 
  print("lillie.test(resid(lmer.12.ec))") 
  print(lillie.test(resid(lmer.12.ec))) 
  print("hist(resid(lmer.12.ec)") 
  hist(resid(lmer.12.ec)) 
  print("qqnorm(resid(lmer.12.ec)") 
  qqnorm(resid(lmer.12.ec))
  print("qqline(resid(lmer.12.ec)") 
  qqline(resid(lmer.12.ec))
  print("")
  print("summary(lmer.13.ec)") 
  print(summary(lmer.13.ec)) 
  print("lillie.test(resid(lmer.13.ec))") 
  print(lillie.test(resid(lmer.13.ec))) 
  print("hist(resid(lmer.13.ec)") 
  hist(resid(lmer.13.ec))
  print("qqnorm(resid(lmer.13.ec)") 
  qqnorm(resid(lmer.13.ec))
  print("qqline(resid(lmer.13.ec)") 
  qqline(resid(lmer.13.ec))
  print("")
  print("summary(lmer.14.ec)") 
  print(summary(lmer.14.ec)) 
  print("lillie.test(resid(lmer.14.ec))") 
  print(lillie.test(resid(lmer.14.ec))) 
  print("hist(resid(lmer.14.ec)") 
  hist(resid(lmer.14.ec))
  print("qqnorm(resid(lmer.14.ec)") 
  qqnorm(resid(lmer.14.ec))
  print("qqline(resid(lmer.14.ec)") 
  qqline(resid(lmer.14.ec))
  print("")

#  mcp.fnc(model = lmer.1.ec)  
  
  print("******************************** PM *******************************")
  print("******************************** PM *******************************")
  print("******************************** model 1 *******************************")

  lmer.0.pm <- lmer((d.dependent.1) ~ 1 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.1.pm <- lmer((d.dependent.1) ~ d.independent.1 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.2.pm <- lmer((d.dependent.1) ~ d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.3.pm <- lmer((d.dependent.1) ~ d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.4.pm <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.5.pm <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.6.pm <- lmer(d.dependent.1 ~ d.independent.2 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.7.pm <- lmer(d.dependent.1 ~ d.independent.1 +  d.independent.2 + d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.8.pm <- lmer(d.dependent.1 ~ d.independent.1 * d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.9.pm <- lmer(d.dependent.1 ~ d.independent.1 * d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.10.pm <- lmer(d.dependent.1 ~ d.independent.2 * d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.11.pm <- lmer(d.dependent.1 ~ d.independent.1*d.independent.2 +  d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.12.pm <- lmer(d.dependent.1 ~ d.independent.1*d.independent.3 +  d.independent.2 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.13.pm <- lmer(d.dependent.1 ~ d.independent.1 + d.independent.2*d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  lmer.14.pm <- lmer(d.dependent.1 ~ d.independent.1*d.independent.2 + d.independent.1*d.independent.3 + d.independent.2*d.independent.3 + (1 | c.id) + (1 | i.sample.dilution), data = df.pm, REML = FALSE, family = binomial, nAGQ = 1)
  print(anova(lmer.0.pm, lmer.1.pm, lmer.2.pm, lmer.3.pm, lmer.4.pm, lmer.5.pm, lmer.6.pm, lmer.7.pm, lmer.8.pm, 
    lmer.9.pm, lmer.10.pm, lmer.11.pm, lmer.12.pm, lmer.13.pm, lmer.14.pm))
  print("")
  print("")
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 
  print("lillie.test(resid(lmer.1.pm))") 
  print(lillie.test(resid(lmer.1.pm))) 
  print("hist(resid(lmer.1.pm))") 
  hist(resid(lmer.1.pm))
  print("qqnorm(resid(lmer.1.pm)") 
  qqnorm(resid(lmer.1.pm))
  print("qqline(resid(lmer.1.pm)") 
  qqline(resid(lmer.1.pm))
  print("")
  print("summary(lmer.2.pm)") 
  print(summary(lmer.2.pm)) 
  print("lillie.test(resid(lmer.2.pm)") 
  print(lillie.test(resid(lmer.2.pm))) 
  print("hist(resid(lmer.2.pm)") 
  hist(resid(lmer.2.pm))
  print("qqnorm(resid(lmer.2.pm)") 
  qqnorm(resid(lmer.2.pm)) 
  print("qqline(resid(lmer.2.pm)") 
  qqline(resid(lmer.2.pm))
  print("")
  print("summary(lmer.3.pm)") 
  print(summary(lmer.3.pm)) 
  print("lillie.test(resid(lmer.3.pm))") 
  print(lillie.test(resid(lmer.3.pm))) 
  print("hist(resid(lmer.3.pm)") 
  hist(resid(lmer.3.pm)) 
  print("qqnorm(resid(lmer.3.pm)") 
  qqnorm(resid(lmer.3.pm))
  print("qqline(resid(lmer.3.pm)") 
  qqline(resid(lmer.3.pm))
  print("")
  print("summary(lmer.4.pm)") 
  print(summary(lmer.4.pm)) 
  print("lillie.test(resid(lmer.4.pm))") 
  print(lillie.test(resid(lmer.4.pm))) 
  print("hist(resid(lmer.4.pm)") 
  hist(resid(lmer.4.pm))
  print("qqnorm(resid(lmer.4.pm)") 
  qqnorm(resid(lmer.4.pm))
  print("qqline(resid(lmer.4.pm)") 
  qqline(resid(lmer.4.pm))
  print("")
  print("")
  print("summary(lmer.5.pm)") 
  print(summary(lmer.5.pm)) 
  print("lillie.test(resid(lmer.5.pm))") 
  print(lillie.test(resid(lmer.5.pm))) 
  print("hist(resid(lmer.5.pm)") 
  hist(resid(lmer.5.pm))
  print("qqnorm(resid(lmer.5.pm)") 
  qqnorm(resid(lmer.5.pm))
  print("qqline(resid(lmer.5.pm)") 
  qqline(resid(lmer.5.pm))
  print("")
  print("summary(lmer.6.pm)") 
  print(summary(lmer.6.pm)) 
  print("lillie.test(resid(lmer.6.pm))") 
  print(lillie.test(resid(lmer.6.pm))) 
  print("hist(resid(lmer.6.pm)") 
  hist(resid(lmer.6.pm))
  print("qqnorm(resid(lmer.6.pm)") 
  qqnorm(resid(lmer.6.pm))
  print("qqline(resid(lmer.6.pm)") 
  qqline(resid(lmer.6.pm))
  print("")
  print("summary(lmer.7.pm)") 
  print(summary(lmer.7.pm)) 
  print("lillie.test(resid(lmer.7.pm))") 
  print(lillie.test(resid(lmer.7.pm))) 
  print("hist(resid(lmer.7.pm)") 
  hist(resid(lmer.7.pm))
  print("qqnorm(resid(lmer.7.pm)") 
  qqnorm(resid(lmer.7.pm))
  print("qqline(resid(lmer.7.pm)") 
  qqline(resid(lmer.7.pm))
  print("")
  print("summary(lmer.8.pm)") 
  print(summary(lmer.8.pm)) 
  print("lillie.test(resid(lmer.8.pm))") 
  print(lillie.test(resid(lmer.8.pm))) 
  print("hist(resid(lmer.8.pm)") 
  hist(resid(lmer.8.pm))
  print("qqnorm(resid(lmer.8.pm)") 
  qqnorm(resid(lmer.8.pm))
  print("qqline(resid(lmer.8.pm)") 
  qqline(resid(lmer.8.pm))
  print("")
  print("")
  print("summary(lmer.9.pm)") 
  print(summary(lmer.9.pm)) 
  print("lillie.test(resid(lmer.9.pm))") 
  print(lillie.test(resid(lmer.9.pm))) 
  print("hist(resid(lmer.9.pm)") 
  hist(resid(lmer.9.pm))
  print("qqnorm(resid(lmer.9.pm)") 
  qqnorm(resid(lmer.9.pm))
  print("qqline(resid(lmer.9.pm)") 
  qqline(resid(lmer.9.pm))
  print("")
  print("summary(lmer.10.pm)") 
  print(summary(lmer.10.pm)) 
  print("lillie.test(resid(lmer.10.pm))") 
  print(lillie.test(resid(lmer.10.pm))) 
  print("hist(resid(lmer.10.pm)") 
  hist(resid(lmer.10.pm))
  print("qqnorm(resid(lmer.10.pm)") 
  qqnorm(resid(lmer.10.pm))
  print("qqline(resid(lmer.10.pm)") 
  qqline(resid(lmer.10.pm))
  print("")
  print("summary(lmer.11.pm)") 
  print(summary(lmer.11.pm)) 
  print("lillie.test(resid(lmer.11.pm))") 
  print(lillie.test(resid(lmer.11.pm))) 
  print("hist(resid(lmer.11.pm)") 
  hist(resid(lmer.11.pm))
  print("qqnorm(resid(lmer.11.pm)") 
  qqnorm(resid(lmer.11.pm))
  print("qqline(resid(lmer.11.pm)") 
  qqline(resid(lmer.11.pm))
  print("")
  print("summary(lmer.12.pm)") 
  print(summary(lmer.12.pm)) 
  print("lillie.test(resid(lmer.12.pm))") 
  print(lillie.test(resid(lmer.12.pm))) 
  print("hist(resid(lmer.12.pm)") 
  hist(resid(lmer.12.pm)) 
  print("qqnorm(resid(lmer.12.pm)") 
  qqnorm(resid(lmer.12.pm))
  print("qqline(resid(lmer.12.pm)") 
  qqline(resid(lmer.12.pm))
  print("")
  print("summary(lmer.13.pm)") 
  print(summary(lmer.13.pm)) 
  print("lillie.test(resid(lmer.13.pm))") 
  print(lillie.test(resid(lmer.13.pm))) 
  print("hist(resid(lmer.13.pm)") 
  hist(resid(lmer.13.pm))
  print("qqnorm(resid(lmer.13.pm)") 
  qqnorm(resid(lmer.13.pm))
  print("qqline(resid(lmer.13.pm)") 
  qqline(resid(lmer.13.pm))
  print("")
  print("summary(lmer.14.pm)") 
  print(summary(lmer.14.pm)) 
  print("lillie.test(resid(lmer.14.pm))") 
  print(lillie.test(resid(lmer.14.pm))) 
  print("hist(resid(lmer.14.pm)") 
  hist(resid(lmer.14.pm))
  print("qqnorm(resid(lmer.14.pm)") 
  qqnorm(resid(lmer.14.pm))
  print("qqline(resid(lmer.14.pm)") 
  qqline(resid(lmer.14.pm))
  print("")


# *********************** Plots ****************************************************
  par(mfrow = c(3,4))
  hist(resid(lmer.1.ec), main = paste("ec", "resid(lmer.1.ec)"), freq = FALSE)
  lines(density(resid(lmer.1.ec)))                                     # 
  qqnorm(ranef(lmer.1.ec)$c.id[[1]], main = "qqnorm(ranef(lmer.1.ec)$c.id[[1]]")
  qqline(ranef(lmer.1.ec)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.1.ec), main = "qqnorm(resid(lmer.1.ec)")
  qqline(resid(lmer.1.ec))
  # Tukey-Anscombe plot
  plot(resid(lmer.1.ec, type="p")~fitted(lmer.1.ec), main = "plot(resid(lmer.1.ec, type=p)~fitted(lmer.1.ec)")
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.1.ec, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE, main = "dotplot(ranef(lmer.1.ec, which = c.id, postVar = TRUE)")
  plot(fitted(lmer.1.ec), resid(lmer.1.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.ec), resid(lmer.1.ec), labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.ec) ~ lmer.1.ec@frame[,2], main = "resid(lmer.1.ec) ~ lmer.1.ec@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.1.ec) ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.2.ec), main = paste("ec", "resid(lmer.2.ec)"), freq = FALSE)
  lines(density(resid(lmer.2.ec)))                                     # 
  qqnorm(ranef(lmer.2.ec)$c.id[[1]])
  qqline(ranef(lmer.2.ec)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.2.ec))
  qqline(resid(lmer.2.ec))
  # Tukey-Anscombe plot
  plot(resid(lmer.2.ec, type="p")~fitted(lmer.2.ec))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.2.ec, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.2.ec), resid(lmer.2.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.2.ec), resid(lmer.2.ec), labels = paste(lmer.2.ec@frame$c.id, lmer.2.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.2.ec) ~ lmer.2.ec@frame[,2], main = "resid(lmer.2.ec) ~ lmer.2.ec@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.2.ec) ~ lmer.2.ec@frame[,2], labels = paste(lmer.2.ec@frame$c.id, lmer.2.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.3.ec), main = paste("ec", "resid(lmer.3.ec)"), freq = FALSE)
  lines(density(resid(lmer.3.ec)))                                     # 
  qqnorm(ranef(lmer.3.ec)$c.id[[1]])
  qqline(ranef(lmer.3.ec)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.3.ec))
  qqline(resid(lmer.3.ec))
  # Tukey-Anscombe plot
  plot(resid(lmer.3.ec, type="p")~fitted(lmer.3.ec))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.3.ec, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.3.ec), resid(lmer.3.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.3.ec), resid(lmer.3.ec), labels = paste(lmer.3.ec@frame$c.id, lmer.3.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.3.ec) ~ lmer.3.ec@frame[,2], main = "resid(lmer.3.ec) ~ lmer.3.ec@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.3.ec) ~ lmer.3.ec@frame[,2], labels = paste(lmer.3.ec@frame$c.id, lmer.3.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.4.ec), main = paste("ec", "resid(lmer.4.ec)"), freq = FALSE)
  lines(density(resid(lmer.4.ec)))                                     # 
  qqnorm(ranef(lmer.4.ec)$c.id[[1]])
  qqline(ranef(lmer.4.ec)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.4.ec))
  qqline(resid(lmer.4.ec))
  # Tukey-Anscombe plot
  plot(resid(lmer.4.ec, type="p")~fitted(lmer.4.ec))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.4.ec, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.4.ec), resid(lmer.4.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.4.ec), resid(lmer.4.ec), labels = paste(lmer.4.ec@frame$c.id, lmer.4.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.4.ec) ~ lmer.4.ec@frame[,2], main = "resid(lmer.4.ec) ~ lmer.4.ec@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.4.ec) ~ lmer.4.ec@frame[,2], labels = paste(lmer.4.ec@frame$c.id, lmer.4.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)


  par(mfrow = c(3,4))
  hist(resid(lmer.1.pm), main = paste("ec", "resid(lmer.1.pm)"), freq = FALSE)
  lines(density(resid(lmer.1.pm)))                                     # 
  qqnorm(ranef(lmer.1.pm)$c.id[[1]])
  qqline(ranef(lmer.1.pm)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.1.pm))
  qqline(resid(lmer.1.pm))
  # Tukey-Anscombe plot
  plot(resid(lmer.1.pm, type="p")~fitted(lmer.1.pm))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.1.pm, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.1.pm), resid(lmer.1.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.pm), resid(lmer.1.pm), labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.pm) ~ lmer.1.pm@frame[,2], main = "resid(lmer.1.pm) ~ lmer.1.pm@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.1.pm) ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.2.pm), main = paste("ec", "resid(lmer.2.pm)"), freq = FALSE)
  lines(density(resid(lmer.2.pm)))                                     # 
  qqnorm(ranef(lmer.2.pm)$c.id[[1]])
  qqline(ranef(lmer.2.pm)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.2.pm))
  qqline(resid(lmer.2.pm))
  # Tukey-Anscombe plot
  plot(resid(lmer.2.pm, type="p")~fitted(lmer.2.pm))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.2.pm, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.2.pm), resid(lmer.2.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.2.pm), resid(lmer.2.pm), labels = paste(lmer.2.pm@frame$c.id, lmer.2.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.2.pm) ~ lmer.2.pm@frame[,2], main = "resid(lmer.2.pm) ~ lmer.2.pm@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.2.pm) ~ lmer.2.pm@frame[,2], labels = paste(lmer.2.pm@frame$c.id, lmer.2.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.3.pm), main = paste("ec", "resid(lmer.3.pm)"), freq = FALSE)
  lines(density(resid(lmer.3.pm)))                                     # 
  qqnorm(ranef(lmer.3.pm)$c.id[[1]])
  qqline(ranef(lmer.3.pm)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.3.pm))
  qqline(resid(lmer.3.pm))
  # Tukey-Anscombe plot
  plot(resid(lmer.3.pm, type="p")~fitted(lmer.3.pm))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.3.pm, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.3.pm), resid(lmer.3.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.3.pm), resid(lmer.3.pm), labels = paste(lmer.3.pm@frame$c.id, lmer.3.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.3.pm) ~ lmer.3.pm@frame[,2], main = "resid(lmer.3.pm) ~ lmer.3.pm@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.3.pm) ~ lmer.3.pm@frame[,2], labels = paste(lmer.3.pm@frame$c.id, lmer.3.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

  par(mfrow = c(3,4))
  hist(resid(lmer.4.pm), main = paste("ec", "resid(lmer.4.pm)"), freq = FALSE)
  lines(density(resid(lmer.4.pm)))                                     # 
  qqnorm(ranef(lmer.4.pm)$c.id[[1]])
  qqline(ranef(lmer.4.pm)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.4.pm))
  qqline(resid(lmer.4.pm))
  # Tukey-Anscombe plot
  plot(resid(lmer.4.pm, type="p")~fitted(lmer.4.pm))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.4.pm, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE, main = "dotplot(ranef(lmer.4.pm, which = c.id, postVar = TRUE)")
  plot(fitted(lmer.4.pm), resid(lmer.4.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.4.pm), resid(lmer.4.pm), labels = paste(lmer.4.pm@frame$c.id, lmer.4.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.4.pm) ~ lmer.4.pm@frame[,2], main = "resid(lmer.4.pm) ~ lmer.4.pm@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.4.pm) ~ lmer.4.pm@frame[,2], labels = paste(lmer.4.pm@frame$c.id, lmer.4.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
    
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  d.end.time <- Sys.time()
  print(d.end.time - d.start.time)
}