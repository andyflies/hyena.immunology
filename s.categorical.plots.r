# source("s.categorical.plots.r")

df.temp.full <- subset(df.data.flat, select = c("c.id", "c.age", "c.sex", "d.sample.date", "i.sample.number.2", "c.reproductive.status",
    "c.prey.density.2.level", "c.prey.density.3.level",
    "c.rank.2.level", "c.rank.3.level",
    "d.age.months", "i.number.adult.female.by.year", 
    "d.mass", "d.minutes.blood", "d.pcv", "i.glucose.green", "d.total.solids", "d.serology.index.rank.igg", "d.serology.index.rank.igm",
    "i.rank", "d.rank.proportion",   
    "d.prey.density", "c.prey.density.2.level", "c.prey.density.3.level",
    "d.precipitation","c.precipitation.2.level", "c.precipitation.3.level",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
    "d.mic.90.median.bka.ec", "d.mic.90.median.bka.pm",
#    "d.prey.low.density.prior", "d.prey.low.density.current",
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.pm.igg",
    "d.neutrophil.lymphocyte.ratio", "d.relative.eosinophils", "d.relative.lymphocytes", "d.relative.monocytes", "d.relative.neutrophils",
    "d.blank.abs.ec.igm", "d.blank.abs.pm.igm", "d.cortisol", "d.testosterone", "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    ))
    

# Use this code for calculating time from the date of the first sample collected (i.e. 1993)
df.temp.full <- subset(df.temp.full, d.sample.date > as.Date("1jan1996", "%d%b%Y"))
df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
df.temp.full$i.sample.years <- df.temp.full$i.sample.year - min(df.temp.full$i.sample.year) + 1
df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
df.temp.full$i.sample.months <- round((julian(df.temp.full$d.sample.date) - min(julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero
######

df.temp.reduced <- subset(df.temp.full, select = c(
    "d.age.months", # "i.sample.years", "i.sample.months", "i.sample.days", 
    "i.rank", "d.rank.proportion",   
    "d.prey.density", 
#    "d.precipitation",
    "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
    "d.blank.abs.corrected.total.igg", "d.blank.abs.corrected.total.igm", 
#    "d.neutrophil.lymphocyte.ratio", "d.relative.eosinophils", "d.relative.lymphocytes", "d.relative.monocytes", "d.relative.neutrophils",
#    "d.ars.cubs", "d.ars.grad", "d.ars.wean", "d.ars.24.month"
    "d.cortisol", "d.testosterone"
    ))
    

# Use this code for calculating time in the freezer
# df.temp.full$i.sample.years <- as.integer(format(df.temp.full$d.sample.date, "%Y"))
# df.temp.full$i.sample.years <- max(df.temp.full$i.sample.year) - (df.temp.full$i.sample.year) + 1
# df.temp.full$i.sample.days <- julian(df.temp.full$d.sample.date)
# df.temp.full$i.sample.days <- max(julian(df.temp.full$d.sample.date)) - (julian(df.temp.full$d.sample.date)) + 1  # can't take log of zero, so start at day one instead of zero
# df.temp.full$i.sample.months <- round((max(julian(df.temp.full$d.sample.date)) - (julian(df.temp.full$d.sample.date))) / 30, 0) + 1  # can't take log of zero, so start at month one instead of zero
#####

# df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
# df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
# df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))

# df.temp.full <- subset(df.temp.full, c.sex == "f")
# df.temp.full <- subset(df.temp.full, d.age.months >= 24)
# df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")
# df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")
# df.temp.full <- subset(df.temp.full, !is.na(c.reproductive.status))
# df.temp.full <- subset(df.temp.full, !is.na(i.rank))
# df.temp.full <- subset(df.temp.full, !is.na(d.prey.density))
print("length(df.temp.full$c.sex)")
print(length(df.temp.full$c.sex))

f.reciprocal <- function(x) 1/x

par(mfrow=c(3,4))
c.var = "d.age.months"
hist(df.temp.full[[c.var]], main = c.var)
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
# hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(3,4))
c.var = "d.mic.90.bka.ec"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(3,4))
c.var = "d.mic.90.bka.pm"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])


par(mfrow=c(3,4))
c.var = "d.blank.abs.corrected.total.igg"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(2,3))
par(mfrow=c(3,4))
c.var = "d.blank.abs.corrected.total.igm"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat, c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(3,4))
c.var = "d.prey.density"
print(!is.na(df.data.flat[[c.var]]))
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(3,4))
c.var = "d.precipitation"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(3,4))
c.var = "d.cortisol"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])

par(mfrow=c(2,3))
par(mfrow=c(3,4))
c.var = "d.testosterone"
hist(df.temp.full[[c.var]], main = c.var)
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = NULL, trim = 3)[["df.data"]][[c.var]])
hist(log2(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log2", trim = 3)[["df.data"]][[c.var]])
hist(log(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log", trim = 3)[["df.data"]][[c.var]])
hist(log10(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "log10", trim = 3)[["df.data"]][[c.var]])
hist(sqrt(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "sqrt", trim = 3)[["df.data"]][[c.var]])
hist(1/(df.temp.full[[c.var]]))
hist(f.remove.outliers(df.data = df.data.flat[!is.na(df.data.flat[[c.var]]),], c.variable = c.var, c.transform = "f.reciprocal", trim = 3)[["df.data"]][[c.var]])


par(mfrow=c(3,4))

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ i.rank, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ i.rank, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ i.rank, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ i.rank, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

plot(log2(d.mic.90.bka.ec) ~ c.rank.2.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.rank.2.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.rank.2.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.rank.2.level, data = df.temp.full)

plot(log2(d.mic.90.bka.ec) ~ c.rank.3.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.rank.3.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.rank.3.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.rank.3.level, data = df.temp.full)

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ (d.prey.density), data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ (d.prey.density), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ (d.prey.density), data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ (d.prey.density), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ (d.prey.density), data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ (d.prey.density), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ (d.prey.density), data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ (d.prey.density), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

plot(log2(d.mic.90.bka.ec) ~ c.prey.density.2.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.prey.density.2.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.prey.density.2.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.prey.density.2.level, data = df.temp.full)

plot(log2(d.mic.90.bka.ec) ~ c.prey.density.3.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.prey.density.3.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.prey.density.3.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.prey.density.3.level, data = df.temp.full)

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ (d.precipitation), data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ (d.precipitation), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ (d.precipitation), data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ (d.precipitation), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ (d.precipitation), data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ (d.precipitation), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ (d.precipitation), data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ (d.precipitation), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

plot(log2(d.mic.90.bka.ec) ~ c.precipitation.2.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.precipitation.2.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.precipitation.2.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.precipitation.2.level, data = df.temp.full)

plot(log2(d.mic.90.bka.ec) ~ c.precipitation.3.level, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.precipitation.3.level, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.precipitation.3.level, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.precipitation.3.level, data = df.temp.full)

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ d.minutes.blood, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ d.minutes.blood, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ d.minutes.blood, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ d.minutes.blood, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ d.minutes.blood, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ d.minutes.blood, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ d.minutes.blood, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ d.minutes.blood, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ i.sample.months, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ i.sample.months, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ i.sample.months, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ i.sample.months, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ i.number.adult.female.by.year, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ i.number.adult.female.by.year, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ i.number.adult.female.by.year, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ i.number.adult.female.by.year, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ i.number.adult.female.by.year, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ i.number.adult.female.by.year, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ i.number.adult.female.by.year, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ i.number.adult.female.by.year, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ d.serology.index.rank.igg, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ d.serology.index.rank.igg, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ d.serology.index.rank.igg, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ d.serology.index.rank.igg, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ d.serology.index.rank.igg, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ d.serology.index.rank.igg, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ d.serology.index.rank.igg, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ d.serology.index.rank.igg, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ d.serology.index.rank.igm, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ d.serology.index.rank.igm, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ d.serology.index.rank.igm, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ d.serology.index.rank.igm, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ d.serology.index.rank.igm, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ d.serology.index.rank.igm, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ d.serology.index.rank.igm, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ d.serology.index.rank.igm, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

print("a")

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ (d.cortisol), data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ (d.cortisol), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ (d.cortisol), data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ (d.cortisol), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ (d.cortisol), data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ (d.cortisol), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ (d.cortisol), data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ (d.cortisol), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print("b")

lm.1 <- lm(log2(d.mic.90.bka.ec) ~ (d.testosterone), data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ (d.testosterone), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ (d.testosterone), data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ (d.testosterone), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ (d.testosterone), data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ (d.testosterone), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ (d.testosterone), data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ (d.testosterone), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print("c")


lm.1 <- lm(log10(d.prey.density) ~ i.sample.months, data = df.temp.full)
plot(log10(d.prey.density) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(log10(d.precipitation) ~ i.sample.months, data = df.temp.full)
plot(log10(d.precipitation) ~ i.sample.months, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

par(mfrow=c(2,3))
lm.1 <- lm(d.cortisol ~ i.rank, data = df.temp.full)
plot(d.cortisol ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.cortisol ~ c.rank.3.level, data = df.temp.full)
plot(d.cortisol ~ c.rank.3.level, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.testosterone ~ i.rank, data = df.temp.full)
plot(d.testosterone ~ i.rank, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.testosterone ~ c.rank.3.level, data = df.temp.full)
plot(d.testosterone ~ c.rank.3.level, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

lm.1 <- lm(d.cortisol ~ c.reproductive.status, data = df.temp.full)
plot(d.cortisol ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.testosterone ~ c.reproductive.status, data = df.temp.full)
plot(d.testosterone ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))

par(mfrow=c(2,3))
lm.1 <- lm(d.ars.24.month ~ log2(d.mic.90.bka.ec), data = df.temp.full)
plot(d.ars.24.month ~ log2(d.mic.90.bka.ec), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.ars.24.month ~ log2(d.mic.90.bka.pm), data = df.temp.full)
plot(d.ars.24.month ~ log2(d.mic.90.bka.pm), data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.ars.24.month ~ d.blank.abs.corrected.total.igg, data = df.temp.full)
plot(d.ars.24.month ~ d.blank.abs.corrected.total.igg, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))
lm.1 <- lm(d.ars.24.month ~ d.blank.abs.corrected.total.igm, data = df.temp.full)
plot(d.ars.24.month ~ d.blank.abs.corrected.total.igm, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
print(summary(lm.1))


lm.1 <- lm(log2(d.mic.90.bka.ec) ~ c.reproductive.status, data = df.temp.full)
plot(log2(d.mic.90.bka.ec) ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log2(d.mic.90.bka.pm) ~ c.reproductive.status, data = df.temp.full)
plot(log2(d.mic.90.bka.pm) ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(log(d.blank.abs.corrected.total.igg) ~ c.reproductive.status, data = df.temp.full)
plot(log(d.blank.abs.corrected.total.igg) ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])
lm.1 <- lm(sqrt(d.blank.abs.corrected.total.igm) ~ c.reproductive.status, data = df.temp.full)
plot(sqrt(d.blank.abs.corrected.total.igm) ~ c.reproductive.status, data = df.temp.full, main = paste("pval: ", coef(summary(lm.1))[2,"Pr(>|t|)"]))
abline(a = coef(lm.1)[1], b = coef(lm.1)[2])

par(mfrow=c(2,3))
hist(df.temp.full$i.sample.months)
hist(log2(df.temp.full$i.sample.months))
hist(log(df.temp.full$i.sample.months))
hist(log10(df.temp.full$i.sample.months))
hist(sqrt(df.temp.full$i.sample.months))
hist(1/(df.temp.full$i.sample.months))

hist(df.temp.full$i.sample.days)
hist(log2(df.temp.full$i.sample.days))
hist(log(df.temp.full$i.sample.days))
hist(log10(df.temp.full$i.sample.days))
hist(sqrt(df.temp.full$i.sample.days))
hist(1/(df.temp.full$i.sample.days))

hist(df.temp.full$i.sample.years)
hist(log2(df.temp.full$i.sample.years))
hist(log(df.temp.full$i.sample.years))
hist(log10(df.temp.full$i.sample.years))
hist(sqrt(df.temp.full$i.sample.years))
hist(1/(df.temp.full$i.sample.years))


par(mfrow=c(2,4))

plot(1 ~ 1, data = df.temp.full)





