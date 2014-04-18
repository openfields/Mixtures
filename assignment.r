assignment<-function(ssize, mean1, sd1, mean2, sd2, value){
# FUNCTION ASSIGNMENT: calculates probability that an observation belongs to one of two size classes, given mean/sd of two size class distributions
# Created by WRF; 2 Apr 2014
#
# Inputs- ssize: sample size for number of random draws
#         mean1: mean of first distribution
#         sd1: st. dev. of first distribution
#         mean2: mean of second distribution
#         sd2: st. dev. of second distribution
#         value: value to evaluate
#
# Processing - calculates random samples from distributions, determines number of observations in each sample that fit into a size bin,
#         calculates assignment probability
#
# Output - returns vector with assignment probability
#
# Note: could change input to vectors of means and sds, allow for more flexibility in handling >2 size classes


# 1. Generate random draws from distributions; currently set to use normal, but can add option for lognormal, etc.
sample1<-rnorm(ssize, mean=mean1, sd=sd1)
sample2<-rnorm(ssize, mean=mean2, sd=sd2)

# 2. Calculate number of observations for a particular SVL bin; rnorm will give decimals, so if we want things for a bin of SVL=31mm, we're looking to
# see how many observations are between 30.5 and 31.5mm
which(sample1>value-.5)->tmp1.greaters
length(which(sample1[tmp1.greaters]<value+.5))->sample1.nobs

which(sample2>value-.5)->tmp2.greaters
length(which(sample2[tmp2.greaters]<value+.5))->sample2.nobs
total.nobs <- sample1.nobs + sample2.nobs

# 3. Calculate proportion of observations from each sample as assignment probability for that sample
assign.probs<-c((sample1.nobs/total.nobs), (sample2.nobs/total.nobs))

return(assign.probs)
}

# Examples:
# prob1 <- assignment(ssize=10000, mean1=25, sd1=5.5, mean2=30, sd2=6.7, value=28)
# prob2 <- assignment(ssize=10000, mean1=25, sd1=5.5, mean2=30, sd2=6.7, value=20)
