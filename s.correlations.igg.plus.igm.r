# source("s.correlations.igg.plus.igm.r")

set.seed(123)

df.temp <- subset(df.data.flat, select = c("c.id", "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", 
  "i.cmi.cav2.igm", "i.corona.titer", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "c.cdv", "i.cdv.titer", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm"))

df.temp.2 <- subset(df.data.flat, select = c("i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", 
  "i.cmi.cav2.igm", "i.corona.titer", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cdv.titer", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm"))

df.temp$c.cdv <- as.character(df.temp$c.cdv)
df.temp$c.cdv[df.temp$c.cdv == "pos"] <- 1   # 1 = TRUE = positive
df.temp$c.cdv[df.temp$c.cdv == "neg"] <- 0   # 0 = FALSE = negative
df.temp$c.cdv <- as.integer(df.temp$c.cdv)

df.temp$c.cmi.cdv.igg <- df.temp$i.cmi.cdv.igg
df.temp$c.cmi.cdv.igg[df.temp$i.cmi.cdv.igg >= 200] <- 1   # 1 = TRUE = positive
df.temp$c.cmi.cdv.igg[df.temp$i.cmi.cdv.igg < 200] <- 0   # 0 = FALSE = negative
print(df.temp)

m.cor <- cor(df.temp.2, df.temp.2,  use = "pairwise.complete.obs")
print(m.cor)
write.csv(m.cor, file = "correlations.csv") 


par(mfrow = c(2,2))
print("**************** ccv ************************")
lm.1 <- lm(log(i.corona.titer) ~ (i.cmi.ccv.igg), data = df.temp)
print(summary(lm.1))
plot(lm.1)
hist(resid(lm.1))
print("***************** lillie.test *************************************")
print(lillie.test(resid(lm.1)))
print(ad.test(resid(lm.1)))
print(cvm.test(resid(lm.1)))

# lmer.0 <- lmer(log(i.corona.titer) ~ (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
# print(summary(lmer.0))
# print("cftest(lmer.0)")
# print(cftest(lmer.0))
# hist(resid(lmer.0))
# qqnorm(resid(lmer.0)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
# qqline(resid(lmer.0))

lmer.1 <- lmer(log(i.corona.titer) ~ (i.cmi.ccv.igg) + (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
print(summary(lmer.1))
print("cftest(lmer.1)")
print(cftest(lmer.1))
hist(resid(lmer.1))
qqnorm(resid(lmer.1)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
qqline(resid(lmer.1))

print(length(resid(lmer.1)))

# print(anova(lmer.0, lmer.1))

print("***************** lillie.test *************************************")
print(lillie.test(resid(lmer.1)))
print(ad.test(resid(lmer.1)))
print(cvm.test(resid(lmer.1)))

print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "pearson"))
print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "kendall"))
print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "spearman"))


par(mfrow = c(4,4))
print("**************** cdv ************************")
lm.1 <- lm(log(i.cdv.titer) ~ (i.cmi.cdv.igg), data = df.temp)
print(summary(lm.1))
plot(lm.1)
hist(resid(lm.1))
print("***************** lillie.test *************************************")
print(lillie.test(resid(lm.1)))
print(ad.test(resid(lm.1)))
print(cvm.test(resid(lm.1)))

# lmer.0 <- lmer(log(i.cdv.titer) ~ (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
# print(summary(lmer.0))
# print("cftest(lmer.0)")
# print(cftest(lmer.0))
# hist(resid(lmer.0))
# qqnorm(resid(lmer.0)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
# qqline(resid(lmer.0))

lmer.1 <- lmer(log(i.cdv.titer) ~ (i.cmi.cdv.igg) + (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
print(summary(lmer.1))
print("cftest(lmer.1)")
print(cftest(lmer.1))
hist(resid(lmer.1))
qqnorm(resid(lmer.1)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
qqline(resid(lmer.1))

# print(length(resid(lmer.0)))
print(length(resid(lmer.1)))

# print(anova(lmer.0, lmer.1))

print("***************** lillie.test *************************************")
print(lillie.test(resid(lmer.1)))
print(ad.test(resid(lmer.1)))
print(cvm.test(resid(lmer.1)))

# l.ct.cdv <- (cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson"))
# print(l.ct.cdv)
# l.ct.cdv <- (cor.test((df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson"))
# print(l.ct.cdv)
print(cor.test((df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson"))
print(cor.test((df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "kendall"))
print(cor.test((df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "spearman"))

f.boot.replace <- function(i.input.1, i.input.2) {
  df.input <- data.frame(i.input.1, i.input.2)
  df.input <- subset(df.input, !is.na(df.input[,1]) & !is.na(df.input[,2]))
  i.indices <- 1:length(df.input[,1])
  i.sample.indices <- sample(i.indices, replace = TRUE)
#  print(cbind(df.input[i.sample.indices,1], df.input[i.sample.indices,2]))
  l.cor.test <- cor.test(df.input[i.sample.indices,1], df.input[i.sample.indices,2], use = "pairwise.complete.obs", method = "pearson")
  d.cor.stat <- l.cor.test$estimate
}


l.ct <- cor.test(df.temp$i.cdv.titer, df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cdv.titer, df.temp$i.cmi.cdv.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cdv.titer and i.cmi.cdv.igg")
points(1 ~ l.ct.cdv$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.cdv.igg, df.temp$i.cmi.cdv.igm, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.cdv.igg, df.temp$i.cmi.cdv.igm))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cdv.titer and i.cmi.cdv.igg")
points(1 ~ l.ct.cdv$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.cdv.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.ccv.igg and i.cmi.cdv.igg")
points(1 ~ l.ct$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.ccv.igm, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.ccv.igm))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.ccv.igg and i.cmi.cdv.igg")
points(1 ~ l.ct$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.dirofilaria.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.ccv.igg, df.temp$i.cmi.dirofilaria.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.ccv.igg and i.cmi.dirofilaria.igg")
points(1 ~ l.ct$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.cdv.igg, df.temp$i.cmi.dirofilaria.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.cdv.igg, df.temp$i.cmi.dirofilaria.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.cdv.igg and i.cmi.dirofilaria.igg")
points(1 ~ l.ct$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.ccv.igm, df.temp$i.cmi.dirofilaria.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.ccv.igm, df.temp$i.cmi.dirofilaria.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.ccv.igm and i.cmi.dirofilaria.igg")
points(1 ~ l.ct$estimate, pch = 8)

l.ct <- cor.test(df.temp$i.cmi.dirofilaria.igg, df.temp$i.cmi.dirofilaria.igm, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(df.temp$i.cmi.ccv.igm, df.temp$i.cmi.dirofilaria.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cmi.ccv.igm and i.cmi.dirofilaria.igg")
points(1 ~ l.ct$estimate, pch = 8)



# write.csv(m.cor, file = "correlations.cdv.csv") 
# v.numeric.variables <- sapply(df.data.flat, is.numeric)
# df.temp <- df.data.flat[, v.numeric.variables]
    
# m.cov <- cov(df.temp, df.temp,  use = "pairwise.complete.obs")
# write.csv(m.cov, file = "covariance.csv") 

# rm(df.temp)
# rm(v.numeric.variables)
# rm(df.data.numeric)