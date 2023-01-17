## Sample size for equality of means
## H_0: mu1=mu2
## two normally distributed samples
## of equal size (two-sided test)

mu1 = 132.85
s1 = 15.34
mu2 = 127.44
s2 = 18.23
alpha = 0.05
power = 0.8


zalpha = qnorm(1-alpha/2,0,1)
zbeta = qnorm(power,0,1)
n = ((s1*s1 + s2*s2)*(zalpha+zbeta)^2)/(mu2-mu1)^2


## -----------------------------------------------
## Sample size needed to compare two binomial proportions
## using a two-sided test where one sample is k times
## as large as the other sample (independent samples)

p1 = 0.0015
p2 = 0.0012
alpha = 0.05
power = 0.8
k = 1

q1 = 1-p1
q2 = 1-p2
zalpha = qnorm(1-alpha/2,0,1)
zbeta = qnorm(power,0,1)
p = (p1+k*p2)/(1+k)
q = 1-p

num = sqrt(p*q*(1+1/k))*zalpha + sqrt(p1*q1+p2*q2/k)*zbeta
n = num^2 / (p1-p2)^2


## ------------------------------------------
## Sample size estimation for the comparison of survival curves between
## two groups under the Cox Proportional-Hazards Model
## the ratio of participants in the experimental group to control is k

hr = 0.7 ## hazards ratio
k = 1 
pE = 0.3707
pC = 0.4890
alpha = 0.05
power = 0.8

zalpha = qnorm(1-alpha/2,0,1)
zbeta = qnorm(power,0,1)
m = (1/k)*((k*hr+1)/(hr-1))^2*(zalpha+zbeta)^2
n = m/(k*pE + pC)
