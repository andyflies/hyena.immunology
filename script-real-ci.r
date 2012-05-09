# This script is one modified from Smithson's (2001) script to get real CIs
# (some bugs have been fixed)
# SMITHSON, M. (2001). Correct confidence intervals for various regression 
# effect sizes and parameters: the importance of noncentral distributions in
# computing intervals. Educational and Psychological Measurement 61, 605-632.
# This codes for a  function conf.limits.nct(tval.1,df,conf)
# (this also codes for some other functions, but never mind these)
# e.g. conf.limits.nct(4.2,30,0.95) - t value, df and 95% CI
# It gets the confidence interval for a t value.
# Then, it can be converted to r or d using fomula in the text of our
# accompanying paper (e.g. Equations 3, 4, 10, 11, 22 & 24): Nakagawa, S. and Cuthill, I.C. 
# 2007. Effect size, confidence interval and statistical significance: 
# a practical guide for biologists. Biological Reviews, 82, pp. 591–605.
# Or you can present t values and their CIs.
# run everything below from here till the end to use conf.limits.nct!

#########################################################################
#This is Krause's noncentral t pdf calculator, taken from the URL
#http://www2.active.ch/~krause.a/doc/statistics-in-pharma/code/bock2/index.html
#
ptnoncent <- function(tx, df, nonc = 0, itrmax = 1000, errmax
     = 1E-6)
{
    if(min(df) <= 0)
        stop("All df must be > 0")
    lengths <- c(length(tx), length(df), length(nonc))
    if(any(lengths < (ltx <- max(lengths)))) {
        tx <- rep(tx, length.out = ltx)
        df <- rep(df, length.out = ltx)
        nonc <- rep(nonc, length.out = ltx)
    }
    tnc <- numeric(ltx)
    del <- nonc
    negdel <- (tx < 0)
    del <- ifelse(negdel,  - del, del)
    xx <- (tx * tx)/(tx * tx + df)
    lambda <- del * del
    p <- 0.5 * exp(-0.5 * lambda)
    q <- 0.79788456080286496 * p * del
    ss <- 0.5 - p
    a <- rep(0.5, ltx)
    b <- 0.5 * df
    rxb <- (1 - xx)^b
    albeta <- 0.57236494292469997 + lgamma(b) - lgamma(
        a + b)
    xodd <- pbeta(xx, a, b)
    godd <- 2 * rxb * exp(a * log(xx) - albeta)
    xeven <- 1 - rxb
    geven <- b * xx * rxb
    tnc <- p * xodd + q * xeven
    itr <- 0
    err <- rep(1, ltx)
    while((itr <- itr + 1) <= itrmax && max(err) > errmax
        ) {
        a <- a + 1
        xodd <- xodd - godd
        xeven <- xeven - geven
        godd <- (godd * xx * (a + b - 1))/a
        geven <- (geven * xx * (a + b - 0.5))/(a +
            0.5)
        p <- (p * lambda)/(2 * itr)
        q <- (q * lambda)/(2 * itr + 1)
        ss <- ss - p
        tnc <- tnc + p * xodd + q * xeven
        err <- 2 * ss * (xodd - godd)
    }
    if(itr > itrmax)
        warning("maximum number of iteration reached"
            )
    tnc <- tnc + 1 - pnorm(del)
    ifelse(negdel, 1 - tnc, tnc)
}
# Function for finding the upper and lower confidence limits for the noncentrality from noncentral t distributions.
# Especially helpful when forming confidence intervals around the standardized effect size, Cohen's d.
###################################################################################################################
# The following code was adapted from code written by Michael Smithson:
# Australian National University, sometime around the early part of October, 2001
# Adapted by Joe Rausch & Ken Kelley: University of Notre Dame, in January 2002.
# Available at: JRausch@nd.edu & KKelley@nd.edu
###################################################################################################################

conf.limits.nct <- 

function(tval.1,df,conf)

{       
# tval.1 is the observed t value, df is the degrees of freedom (group size need not be equal), and conf is simply 1 - alpha

        Result <- matrix(NA,1,4)
        tval <- abs(tval.1)


############################This part Finds the Lower bound for the confidence interval###########################
ulim <- 1 - (1-conf)/2

# This first part finds a lower value from which to start.
        lc <- c(-tval,tval/2,tval)
                while(ptnoncent(tval,df,lc[1])<ulim)    {
                                            lc <- c(lc[1]-tval,lc[1],lc[3])
                                     }

# This next part finds the lower limit for the ncp.
         diff <- 1
                while(diff > .00000001)      {
                                    if(ptnoncent(tval,df,lc[2])<ulim)
                                            lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2])
                                            else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
                                        diff <- abs(ptnoncent(tval,df,lc[2]) - ulim)
                                        ucdf <- ptnoncent(tval,df,lc[2])
                                 }
         res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])

############################This part Finds the Upper bound for the confidence interval###########################
llim <- (1-conf)/2

# This first part finds an upper value from which to start.
        uc <- c(tval,1.5*tval,2*tval)
                while(ptnoncent(tval,df,uc[3])>llim)   {
                                           uc <- c(uc[1],uc[3],uc[3]+tval)
                                }

# This next part finds the upper limit for the ncp.
        diff <- 1
                while(diff > .00000001)         {
                                        if(ptnoncent(tval,df,uc[2])<llim)
                                            uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2])
                                            else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
                                        diff <- abs(ptnoncent(tval,df,uc[2]) - llim)
                                        lcdf <- ptnoncent(tval,df,uc[2])
                                }
        res <- ifelse(tval.1 >= 0,uc[2],-uc[2])
        

#################################This part Compiles the results into a matrix#####################################
        Result[1,1] <- min(res,res.1)
        Result[1,2] <- lcdf
        Result[1,3] <- max(res,res.1)
        Result[1,4] <- ucdf
dimnames(Result) <- list("Values", c("Lower.Limit", "Prob.Low.Limit", "Upper.Limit", "Prob.Up.Limit"))
        Result
}
#the end##################################################################
# -  please email itchyshin@yahoo.co.nz
# if you find some bugs in the code - thanks
