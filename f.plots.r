# source("f.lm.models.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.plots <- function(df.input, ...)   
{
  df.adult.female.flat <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
  print(df.adult.female.flat[,1:8])
  print(length(df.adult.female.flat$c.id))
  print(length(unique(df.adult.female.flat$c.id)))
  print(length(unique(df.adult.female.flat$i.rank)))  
  df.adult.female.flat <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & i.sample.number.2 == 2)
  print(df.adult.female.flat[,1:8])
  print(length(df.adult.female.flat$c.id))
  print(length(unique(df.adult.female.flat$c.id)))
  print(length(unique(df.adult.female.flat$i.rank)))  

  c.independent.variable <- "d.sample.month"
  df.adult.female.flat$d.sample.year <- factor(df.adult.female.flat$d.sample.year)
  df.adult.female.flat$d.sample.month <- factor(df.adult.female.flat$d.sample.month)

#  if(is.null(as.list(...))) { print("null list")  }
#  else { print(as.list(...))  }
#  if(is.null(as.list(...))) {  c.independent.variable <- "i.rank" }
#  else { c.independent.variable <- as.list(...)     }
  
  par(mfrow = c(3,3))
  i.counter <- 1
  i.counter.stop <- length(cv.dependent.variables)
  for(i.counter in i.counter:i.counter.stop)
  {
    print(paste(" **************** ", cv.dependent.variables[i.counter], " **************** "))

    lm.rank <- lm(df.adult.female.flat[[cv.dependent.variables[i.counter]]] ~ df.adult.female.flat[[c.independent.variable]], 
      data = df.adult.female.flat)
    lm.rank.summary <- summary.lm(lm.rank)
    print(lm.rank.summary)
    
    plot(df.adult.female.flat[[cv.dependent.variables[i.counter]]] ~ factor(df.adult.female.flat[[c.independent.variable]]), df.adult.female.flat,  
      main = paste(cv.dependent.variables[i.counter], " by ", c.independent.variable), 
      xlab = c.independent.variable,
      ylab = cv.dependent.variables[i.counter])
    i.counter <- i.counter + 1
  }  
}  































