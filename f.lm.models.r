# source("f.lm.models.r")

# This function converts the variables passed in the function call to ranks based on titers, then renames the variables and adds them to the input df before returning
f.lm.models <- function(df.input, ...)   
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

  c.independent.variable <- "i.rank"

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

    c.dependent.variable <- cv.dependent.variables[i.counter]   
    print(c.dependent.variable)
    

    lm.rank <- lm(df.adult.female.flat[[cv.dependent.variables[i.counter]]] ~ df.adult.female.flat[[c.independent.variable]], 
      data = df.adult.female.flat)
    lm.rank.summary <- summary.lm(lm.rank)

print(lm.rank.summary$coefficients)
print(names(lm.rank.summary$coefficients))

# names(lm.rank.summary$coefficients["df.adult.female.flat[[c.independent.variable]]", "Pr(>|t|)"]
    cv.temp.names <- rownames(lm.rank.summary$coefficients)
    print(cv.temp.names)
    cv.temp.names <- gsub(cv.temp.names[2], c.independent.variable, cv.temp.names)
    print(cv.temp.names)
    rownames(lm.rank.summary$coefficients) <- c("a", c.independent.variable)
#    cv.temp.names <- gsub("df.adult.female.flat[[c.independent.variable]]", c.independent.variable, cv.temp.names)
    print(cv.temp.names)

    print(lm.rank.summary$coefficients)
    
    plot(df.adult.female.flat[[cv.dependent.variables[i.counter]]] ~ df.adult.female.flat[[c.independent.variable]], df.adult.female.flat,  
      main = paste(cv.dependent.variables[i.counter], " by rank"), 
      sub = paste("adj.r.squared: ", signif(lm.rank.summary$adj.r.squared, digits = 3),
        "   df: ", lm.rank.summary$fstatistic["numdf"], ",", lm.rank.summary$fstatistic["dendf"],
        "    Pr(>|t|): ", signif(lm.rank.summary$coefficients["df.adult.female.flat[[c.independent.variable]]", "Pr(>|t|)"], digits = 4), sep = ""),
      xlab = c.independent.variable,
      ylab = cv.dependent.variables[i.counter])
    abline(lm.rank)
    i.counter <- i.counter + 1
  }  
}  































