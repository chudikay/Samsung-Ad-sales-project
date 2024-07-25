#We shall first load ggplot2 package and the dataset
library(ggplot2)

#We shall then view the data
SamsungAdAnalysis

#We shall get a summary statistics of the data
summary(SamsungAdAnalysis)

#Before we start our hypothesis testing, we need to verify the normality of the data by 
#plotting a histogram and checking if it is bell shaped and it’s symmetrical distribution.

#Plot Histogram of SalesAd2
x=SamsungAdAnalysis$SalesAd2
h<-hist(x, breaks=10, col="yellow", xlab="SalesAd2Records",
        main="Distribution of Ad2 Weekly Sales Records")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#CONDUCT HYPOTHESIS TESTING USING t-test ANALYSIS

#STEP 1: We shall write out our null and alternative hypotheses as follows – 
Null Hypothesis, Ho: mu = 55,000 units/week
Alternative Hypothesis, Ha: mu > 55,000 units/week

#STEP 2: Choose a significance level
#We choose a significance level, Alpha of 5%
Alpha = 0.05

#STEP 3: Carryout test analysis
mu = 55000

#One Sample t-test – greater than 55,000
t.test(SamsungAdAnalysis$SalesAd2, mu = 55000, alternative = "greater")
