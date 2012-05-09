# source("f.calculate.percent.inhibition.r")

f.calculate.percent.inhibition <- function()   # Heirarchical sorting of a date frame.  Need to input the data frame to sort and the variables to sort by
{

  # Need to calculate the mean and sd of replicates before doing any other modications.
  #  Use the mean and sd to calculate within sample CV usign the across dilution individual means
  df.bka.temp <- with(df.bka.raw, f.sort.frame(df.bka.raw, c.id, i.replicate.number, c.strain, i.sample.number))
  # print(subset(df.bka.temp, select = c(c.id, i.sample.dilution, i.replicate.number, i.sample.number, d.percent.control.abs, d.percent.inhibition, d.mic.90, c.strain)))
  
  df.bka.mean.across.dilutions <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.replicate.number
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.raw, mean, na.action = na.pass
  )
  
  df.bka.mean.within.sample <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.mean.across.dilutions, mean, na.action = na.pass
  )
  
  df.bka.mean.within.sample.sd <- aggregate(cbind(  # i.sample.dilution,
    d.percent.control.abs,
    d.percent.inhibition,
    d.mic.90
    )
    ~ c.id
    + i.kay.code
    + i.sample.number
    + c.strain
    + d.sample.date
    ,
    data = df.bka.mean.across.dilutions, sd, na.action = na.pass
  )
  
  df.bka.mean.within.sample$d.percent.inhibition.sd <- df.bka.mean.within.sample.sd$d.percent.inhibition
  d.percent.inhibition.within.sample.cv <- (df.bka.mean.within.sample$d.percent.inhibition.sd / df.bka.mean.within.sample$d.percent.inhibition)
  df.bka.mean.within.sample$d.percent.inhibition.within.sample.cv <- d.percent.inhibition.within.sample.cv
  
  df.bka.mean.across.dilutions <- with(df.bka.mean.across.dilutions, f.sort.frame(df.bka.mean.across.dilutions, c.id, i.replicate.number, c.strain, i.sample.number))
  df.bka.mean.within.sample <- with(df.bka.mean.within.sample, f.sort.frame(df.bka.mean.within.sample, c.id, c.strain, i.sample.number))
  
  # Need to rename variables so they are split by bacterial strain
  df.ec.temp <- df.bka.mean.within.sample[df.bka.mean.within.sample$c.strain == "ec",]
  df.pm.temp <- df.bka.mean.within.sample[df.bka.mean.within.sample$c.strain == "pm",]
  df.ec.temp <- subset(df.ec.temp, select = -c(c.strain))  # do not need c.strain in the merged data frame, so get rid of it by negative subsetting
  df.pm.temp <- subset(df.pm.temp, select = -c(c.strain))  # do not need c.strain in the merged data frame, so get rid of it by negative subsetting

  # merge the data frames and add the strain to the end of the non-merged variable names
  df.bka.mean.within.sample <- merge(df.ec.temp, df.pm.temp,  # merge the two data frames. Keep all rows in the first df.
    by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  #  all.x = TRUE,
    suffixes = c(".ec",".pm"), all = TRUE)                                  # rename the non-key columns when there are conflicts
  df.bka.mean.within.sample
}