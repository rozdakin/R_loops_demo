##################################
# how to loop, not loop, and speed loops up
##################################
# Roz Dakin
# May 27 2015

# packages you'll need:
library(gapminder)
library(dplyr)
library(microbenchmark) 


# the for loop is most used. for(counter) {action steps}

for(i in 1:10){
	print(i)
}

# 3 ways: loop over indices. loop over elements. or loop over names/levels of a factor.

# example of looping over elements of a vector
for(i in gapminder$country) {
	print(i)
}


i <- 1
while(i < 10){
	print(i)
} # a neverending while loop. use 'escape' to stop

i <- 1
while(i < 10){
	i <- i + 1
	print(i)
} # better

for(i in 1:1000){
	print(i)
	if(i > 100){
		stop('too big') # error message
	}
}

##############
# why loop? 

# 1. resampling/simulating/permuting data

# example: simulating the CLT
(a <- rpois(50, lambda=10))
hist(a)

results <- rep(NA, 10000)
head(results)
for(i in 1:10000){
	a <- rpois(50, lambda=10)
	results[i] <-mean(a)
}
hist(results) # the mean of samples drawn from a poisson distribution is normally distributed

# bootstrapping: must be done with replacement
gapminder
mean(gapminder$gdpPercap)
hist(gapminder$gdpPercap) # let's get CIs on the mean of this non-normal variable by bootstrpping

gdpresults <- rep(NA, 10000)
head(results)
for(i in 1:10000){
	resample <- sample(gapminder$gdpPercap, replace=T)
	gdpresults[i] <- mean(resample)
}
quantile(gdpresults, c(0.025,0.975)) # get 95% CI
head(resample) # note objects we create in he loop actually get created, and overwritten with each iteration


# 2. processing/analyzing/visualizing large and/or distributed datasets

# suppose you want to do the same analysis to different subsets of a dataframe, or list
results <- rep(NA, length(countries))
for(i in 1:length(countries)){
	mod <- lm(lifeExp ~ gdpPercap, subset(gapminder, country==countries[i]))
	results[i]<-summary(mod)$r.squared
}
hist(results) # more appropriate when there are lots of intermediate steps. write your own function or loop instead of copy/paste many times

# or if you want to apply the same set of analyses steps to different columns of a dataframe:
responses <- names(gapminder)[4:6]
for(i in 1:3){
	myformula <- paste(responses[i],'~year')
	mod <- lm(as.formula(myformula), data=gapminder)
	results[i]<-summary(mod)$r.squared
}
results[1:3]

# note the use of as.formula() here. super useful! turn a text string into a formula that works with statistical functions like lm, lme, lme4, plot, etc


# here's another example: making and saving lots of separate plots

# for this you will want to create an empty folder and make it your working directory. then use the loop below to generate a whole bunch of plots in that folder

for(i in 1:length(countries)){
	mypath <- file.path(getwd(), paste(countries[i], '_', paste('00',i,sep=''), ".jpg", sep = ""))
	jpeg(file=mypath, width=4, height=3.5, units='in', res=150)
	par(las=1, bty='n', family='Times', tck=0.02)
	plot(gdpPercap~year,subset(gapminder, country==countries[i]), xlab='year', ylab='gdp per capita (units)', type='l', ylim=c(1,10000))
	dev.off()
} 

# can us this to make multiple stills to animate a time series...

years <- as.numeric(levels(factor(gapminder$year)))
for(i in 1:length(years)){
	mypath <- file.path(getwd(), paste('myanimation', '_', paste('00',i,sep=''), ".jpg", sep = ""))
	jpeg(file=mypath, width=4, height=3.5, units='in', res=150)
	par(las=1, bty='n', family='Times', tck=0.02)
	plot(gdpPercap~year, subset(gapminder, country=='Canada' & year < years[i]+1), xlab='year', ylab='gdp per capita (1000s)', type='l', ylim=c(1,100000), xlim=c(1950, 2010), yaxt='n', col='red')
	points(gdpPercap~year, subset(gapminder, country=='United States' & year < years[i]+1), type='l', col='blue')
	points(gdpPercap~year, subset(gapminder, country=='United Kingdom' & year < years[i]+1), type='l', col='green')
	points(gdpPercap~year, subset(gapminder, country=='Saudi Arabia' & year < years[i]+1), type='l', col='orange')
	axis(2, at=c(0,50000,100000), labels=c(0,500,1000))
	dev.off()
}

# note this could be a pdf too for editing in illustrator. depends on what you want to do with it. jpeg() just saves space



##############
# what NOT to do

# learning how to loop can make you drunk with power. but often not the best choice

# for example, DON't use a loop to do this:
head(gapminder)
for(i in 1:length(gapminder$pop)){
	gapminder$popround[i] <- round(gapminder$pop[i], -3)
}
head(gapminder)

# DO this instead:
gapminder$popround <- round(gapminder$pop, -3)
# vectorized.

# DON'T do this to recode values:
iris
iris$sep.group <- 0
for (i in 1:length(iris$Sepal.Length)){
	if(iris$Sepal.Length[i] > 5 & iris$Sepal.Length[i] < 6){
		iris$sep.group[i] <- 1
	}
	if(iris$Sepal.Length[i] >= 6){
		iris$sep.group[i] <- 2
	}
}
iris

# DO use ifelse :
iris$sep.group2 <- ifelse(iris$Sepal.Length > 6, 1, 0)
# can nest ifelse statements. useful to recode missing values/typos etc

# DON'T do this to summarize:
countries <- levels(gapminder$country)
cmeans <- rep(NA, length(countries))
for(i in 1:length(countries)){
	data <- subset(gapminder, country==countries[i])
	cmeans[i] <- mean(data$pop)
}
cmeans

# recall dplyr, summarise() function to very conveniently summarize to data frames
(by.country <- group_by(gapminder, country))
summ.country <- summarise(by.country, meanpop = round(mean(pop)/1000,-3)) 
head(summ.country)
# can add any/many other summary items to the summarise() that we want
summ.country <- summarise(by.country, meanpop = round(mean(pop)/1000,-3), maxlongevity = round(max(lifeExp)), samplesize = n()) 
head(summ.country)

# other ways to avoid loops in base R... apply family... tapply, lapply etc.
apply(gapminder[,4:6], 2, 'mean') # dataframes and matrices
apply(iris[,1:4], 1, 'sum')

help(tapply) # good for dataframes or separate vectors. can be ragged (diff amounts)
tapply(gapminder$lifeExp, gapminder$country, FUN='mean')
tapply(gapminder$lifeExp, gapminder$country, FUN=function(x) round(mean(x),-1))
# can write your own functions to bue used with apply & co. can be done inside or outside the loop

(poppercent <- tapply(gapminder$pop, gapminder$country, function(x) x/max(x))) # this is an example of how to use apply to normalize (i.e., express as % of max for each category)
str(poppercent) # a list. what to do?
(poppercent.df <- do.call('rbind', poppercent)) # can turn it into a dataframe

# lapply for lists. we'll do below
lapply(poppercent, mean)
lapply(poppercent, function(x) sd(x)/sqrt(length(x)))

# apply family is based on loops internally. better than writing your own loops because less typing.

# you can store model results, details of fancy plots like visreg or hist objects etc, in lists when you're doing repeated processes:
myhist <- list() # create an empty list object
mylist <- list()
for(i in 1:100){
	mylist[[i]] <- subset(gapminder, country==levels(gapminder$country)[i])
	myhist[[i]] <- hist(mylist[[i]]$pop, plot=F)
}
myhist[[1]]
# weird example but you get the idea
# lots more possibiilities here not get into. see Wickham's advanced R on Functionals.


# here's another trick for loops:
# assign. turn string into object name. SUPER useful
assign(paste('Canada','data',sep='_'), subset(gapminder,country=='Canada'))
Canada_data
# now imagine put this in a loop where you do some complex analyses on lots of datasets

# another time when looping is in order: doing something contingent on previous row(s), rolling functions (not shown)

####################
# how to speed your loops up. 

# three principles: most important is this one:

# 1. don't grow things in your loops.

# e.g. append adds elements to a vector. generally slow
append(1:10, 11:20) # avoid in your loops!
c(1:10, 11:20) # what is the difference? the point is, don't use these in your loops! avoid rbind too for the same reason

# to see why, contrast these two examples that do the exact same thing.
hit <- NA
system.time (for(i in 1:100000){if(runif(1) < 0.3) hit[i] <- T})
head(hit)

hit2 <- rep(NA, 100000)
system.time (for(i in 1:100000){if(runif(1) < 0.3) hit2[i] <- T})

# 1st example grows the result, 2nd exmaple stores results in already-sized vector. huge time savings for option 2

######
######
# aside: ways to time your code.

# enclose it in system.time
system.time(mean(1:1000000))

# sometimes do this instead for big chunks:
start <- Sys.time()
mean(1:1000000)
Sys.time()-start
# adds a tiny bit of time I think, but sometimes more readable to have the timing things on their own lines

proc.time() # I think this measures time of session

microbenchmark(mean(1:1000000)) # very cool

help(microbenchmark) # more accurate, because sampling. but beware: this also means it does the action many times over = takes n times as long. probably not necessary for most things

# you can also pass it as many things as you want, and change the nreps:
rnormtime <- microbenchmark( rnorm(1000), rnorm(10000), rnorm(100000), rnorm(1000000), times=50)
boxplot(rnormtime)
require("ggplot2")
autoplot(rnormtime)

rnormtime$time # note by saving the microbenchmark you can retrieve the timing data
rnormtime # units microseconds here. don't see how to change it to seconds in the help file

######
######

# back to: speeding up loops. principle 1: if you're storing the results in a vector, initialize it outside of the loop at or greater than the length it will need to be. 


# 2. vectorize wherever possible. basically take as much as you can out of the loop.

head(gapminder) # suppose we want to denote every time the lifeExp for a country went down as special 'bad' year. easy to see how to do this with a loop
start <- Sys.time()
gapminder$badyear <- NA
for(i in 2:length(gapminder[,1])){
  if((gapminder$lifeExp[i] < gapminder$lifeExp[i-1]) & (gapminder$country[i]==gapminder$country[i-1])){
    gapminder$badyear[i] <- 'bad'
  }
}
Sys.time() - start
summary(factor(gapminder$badyear))

#  in this case, we can get rid of the loop period.
start <- Sys.time()
gapminder$lifeExpnext <- NA
gapminder$lifeExpnext[1:length(gapminder[,1])-1] <- gapminder$lifeExp[2:length(gapminder[,1])]
gapminder$countrynext <- NA
gapminder$countrynext[1:length(gapminder[,1])-1] <- as.character(gapminder$country[2:length(gapminder[,1])])
badyear <- gapminder$countrynext==gapminder$country & gapminder$lifeExpnext < gapminder$lifeExp
Sys.time() - start
# way more text. but huge time saving if we were working with a really big dataset
# can always stick this new vector badyear back into the dataframe at the end

# so wherever possible evaluate conditionals & do other work outside of loops

# 3. indexing vectors is faster than indexing dataframes 

# suppose we want to simulate a bunch of t-tests with n = 50, and store the t value and p value each time.

(grp <- rep(1:2, each=25))

# writing functions to store the t test results: 
Tstat <- function(x, grp){
  t.test(x[grp == 1], x[grp == 2])$stat
}
Pval <- function(x, grp){
  t.test(x[grp == 1], x[grp == 2])$p.value
}


# let's try two methods of storing results: in a data frame, vs in two vectors
results <- data.frame(t1=rep(NA,10000), pval=NA)
t1 <- rep(NA, 10000)
pval <- rep(NA, 10000)

# show with system.time() first, then do and microbenchmark
system.time(
for(i in 1:10000){
  a <- rnorm(50, 10)
  results$t1[i] <-Tstat(a, grp)
  results$pval[i] <-Pval(a, grp)
}
)

system.time(
  for(i in 1:10000){
    a <- rnorm(50, 10)
    t1[i] <-Tstat(a, grp)
    pval[i] <-Pval(a, grp)
  }
)

# only difference between these two was use of dataframe vs vectors
# there is a small but real advantage to indexing vectors not dataframes
# we think because the '$' is doing work?


# bootstrap example
m <- 1000
n <- 50
X <- matrix(rnorm(m * n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n / 2)
system.time( for(i in 1:m) t.test(X[i, ] ~ grp)$stat )
system.time( for(i in 1:m) t.test(X[i, grp == 1], X[i, grp == 2])$stat )
# surprising that this makes a considerable diffrence. is it because there is a hidden as.formula() call or some other hidden work?
# principle: do less work in your loops


# one topic we did not cover:
# if you run out of memory & R crashes during a session, you can save different objects using save(), kill them, and bring into the workspace separately, as needed.
# see R inferno?

# Resources (free online!)
####################
# Patrick Burns' R Inferno: http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# Hadley Wickham's Advanced R (esp. Functionals and Profiling): http://adv-r.had.co.nz/ 



rm(list=ls())
