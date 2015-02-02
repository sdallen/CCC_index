#This Rcode plots the data and applies models to standardize CPUE
#################################################################################
#################################################################################
# This script produces estimates of CA Coastal Chinook (CCC) indices of abundance
# by standardizing average catch of CCC per hour catching fish
#################################################################################
# To Do:
# Include a covariate of fishery type (e.g., sport, comm nonretention, comm retention)
#################################################################################

rm(list = ls(all = TRUE))

# ==============================================================================
# Directories.

Top.dir <-"C:\\Users\\shanae.allen\\Documents\\Courses\\Reproducible Research\\my_proj"
setwd( Top.dir )

# ==============================================================================
# functions

source( "datcleannew.r" )
# ==============================================================================
# Libraries/ Settings
#  library("stats")
library("lattice")
library(mgcv) #for GAMs

#  install.packages(c("ggplot2","lubridate", "plyr", "mosaic", "mosaicData"))

library(lubridate)  # for dealing with dates
library(ggplot2)
library(plyr)


# =================================================================================
# Cleaned up data


cpuedat <- datcleannew()$cpuedat
dat <- datcleannew()$dat


################################################################################
#################################################################################
#################################################################################


# ==============================================================================
# Plot density of Cperhr - average catch per hour ( catch per day standardized by hours fished)
# ==============================================================================\

plot( aggregate( formula =  Cperhr ~ yfac, data=cpuedat, FUN = mean)  )



d <- ggplot(data = cpuedat, aes(x = Cperhr)) + geom_histogram() #, #fill = "blue"  binwidth = .01
d

# by year
d + facet_wrap( ~ yfac)

# by month and year
d + facet_wrap( ~ myfac)


# by vessel
d + facet_wrap(~ vessel_id) # Need cpue by month, month and lat/lon, year



# plot log transformed
dlog <- ggplot(data = cpuedat, aes(x = log(Cperhr + .001))) + geom_histogram( ) #binwidth = 2, #fill = "blue"
dlog

# by month and year
dlog + facet_wrap( ~ myfac)


# by vessel
dlog + facet_wrap(~ vessel_id) # Need cpue by month, month and lat/lon, year



# ==============================================================================
# Plot Cperhr on x axis and latitude on y axis 
# ==============================================================================\


# all months
d <- ggplot(data = cpuedat, aes(y = avglatday, x=Cperhr)) + geom_point()

# add line of where eel river is

d + geom_abline(slope=0, intercept=40.636)


# per month and year

d + geom_abline(slope=0, intercept=40.636) + facet_wrap( ~ myfac)



# ==============================================================================
# Plot Cperhr on y axis and longitude on x axis 
# ==============================================================================\


# all months
d <- ggplot(data = cpuedat, aes(y = Cperhr, x=avglonday)) + geom_point()

# add line of where eel river is

d + geom_vline(xintercept = -124.31)

# per month

d + geom_vline(xintercept = -124.31) + facet_wrap( ~ myfac)



################################################################################
#################################################################################
#################################################################################
# effort 

# ==============================================================================
# Bar plot of number of days fished per year (given that at least one fish was caught?)
# ==============================================================================\

plot( cpuedat$yfac, ylab="number of days fished", xlab="year" )

# ==============================================================================
# Total number of hours spent EACH YEAR catching fish
# ==============================================================================

plot( aggregate( formula =  hrsfished ~ yfac, data=cpuedat, FUN = sum), ylab="total number of hrs fished", xlab="year"  )  


# ==============================================================================
# Bar plot of AVG number of hours spent EACH DAY catching fish (given that at least one fish was caught?)
# ==============================================================================\

# per year
plot( aggregate( formula =  hrsfished ~ yfac, data=cpuedat, FUN = mean), ylab="avg hours fished per day", xlab="year"  )

# ==============================================================================
# line plot of number of vessels fishing per year (given that at least one fish was caught?)
# ==============================================================================\


plot( aggregate( formula =  vessel_id ~ yfac, data=cpuedat, FUN = length), ylab="total number of vessels", xlab="year"   )


# ==============================================================================
# Distribution of fishing over lat/long per month and year
# ==============================================================================\


# all months
deffort <- ggplot(data = cpuedat, aes(x = avglonday, y=avglatday)) + geom_point()

# add latitude lines of where CA/OR border is, eel river, and point reyes
deffort + geom_hline(yintercept = c(42,40.636, 38.04))

#per month and year

deffort + geom_hline(yintercept = c(42,40.636, 38.04)) + facet_wrap( ~ myfac)



# ==============================================================================
# first/last fishing day of each year
# ==============================================================================\

aggregate( formula = dmyfac ~ yfac, data=cpuedat, FUN = function(x) x[1])

aggregate( formula = dmyfac ~ yfac, data=cpuedat, FUN = function(x) x[length(x)])

# ==============================================================================
# min/max lat/lon per year
# ==============================================================================\

aggregate( formula = avglonday ~ yfac, data=cpuedat, FUN = min)

aggregate( formula = avglonday ~ yfac, data=cpuedat, FUN = max)

aggregate( formula = avglatday ~ yfac, data=cpuedat, FUN = min)

aggregate( formula = avglatday ~ yfac, data=cpuedat, FUN = max)



################################################################################
#################################################################################
#################################################################################
# Model of presence/absense


# Pres abs response variable

cpuedat$presabs = 0

cpuedat$presabs[ cpuedat$Cperhr >0 ] <- 1
colnames(cpuedat)


# Pres abs GAM model

pres.gam <- gam( presabs ~ s(yfac, bs="re")+ myfac + s(avglatday) + s(avglonday) +  s(vessel_id, bs="re"), 
                 family="binomial", data=cpuedat)

summary(pres.gam)

plot(pres.gam)


# using month and year interaction
pres.gam <- gam( presabs ~ s(yfac, mfac, bs="re")+ mfac + s(avglatday) + s(avglonday) +  s(vessel_id, bs="re"), 
                 family="binomial", data=cpuedat)

summary(pres.gam)

plot(pres.gam)



# using month and year interaction, lat and month interaction
pres.gam <- gam( presabs ~ s(yfac, mfac, bs="re")+ mfac + s(avglatday, by=mfac) + s(avglonday) +  s(vessel_id, bs="re"), 
                 family="binomial", data=cpuedat)

summary(pres.gam)

plot(pres.gam)


# using month and year interaction, lat and month interaction, and lon and month interaction
pres.bgam <- gam( presabs ~ mfac + yfac + s(yfac, mfac, bs="re")+ s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                  family="binomial", data=cpuedat)

summary(pres.bgam)

plot(pres.bgam)



# using month and year interaction, lat and month interaction, and lon and month interaction
pres.qbgam <- gam( presabs ~ mfac + yfac + s(yfac, mfac, bs="re")+ s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                   family="quasibinomial", data=cpuedat)

summary(pres.qbgam)

plot(pres.qbgam)

#no select=TRUE
#R-sq.(adj) =  0.326   Deviance explained =   36%
#GCV = 0.66101  Scale est. = 0.80459   n = 5686

#select=TRUE
#R-sq.(adj) =  0.324   Deviance explained = 35.8%
#GCV = 0.66173  Scale est. = 0.80167   n = 5686



# using month and year interaction, lat and month interaction, and lon and month interaction, random year intercept
pres.qbgam <- gam( presabs ~ mfac + s(yfac, bs="re") + s(yfac, mfac, bs="re")+ s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                   family="quasibinomial", data=cpuedat)

summary(pres.qbgam)

plot(pres.qbgam)

#no select=TRUE
#R-sq.(adj) =  0.326   Deviance explained =   36%
#GCV = 0.66099  Scale est. = 0.80982   n = 5686




##############################################################################################
# diagnostic plot 

gam.check(pres.qbgam, pch=19)  #, cex=.3)





################################################################################
#################################################################################
#################################################################################
# Model of cpue given presence


# using month and year interaction, lat and month interaction, and lon and month interaction
cpue.lgam <- gam( log(Cperhr) ~ mfac + yfac + s(yfac, mfac, bs="re") + s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                  data=cpuedat[cpuedat$presabs ==1, ])

summary(cpue.lgam )

plot(cpue.lgam)

#try gamma distribution
# using month and year interaction, lat and month interaction, and lon and month interaction
cpue.ggam <- gam( Cperhr ~ mfac + yfac + s(yfac, mfac, bs="re") + s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                  data=cpuedat[cpuedat$presabs ==1, ], family = Gamma(link = "log")) #, select=TRUE

summary(cpue.ggam )
AIC(cpue.ggam)
gam.check(cpue.ggam, pch=19)  #, cex=.3)


plot(cpue.ggam)

################################################################################
#################################################################################
#################################################################################
# Predict CPUE


#predpres <- predict(pres.gam,  type="response" )



#predcpue.pres <- exp( predict(lcpue.gam,  type="response", newdata=cpuedat ))*exp(b/2)


#predpres*predcpue.pres


################################################################################
#################################################################################
#################################################################################
# extract year effect to obtain index of abundance



param.names.pres <- names(coef(pres.qbgam))	
param.ind.pres <- seq(1, length(param.names.pres))
yr.coef.pres <- coef(pres.qbgam)[param.ind.pres[ substr(param.names.pres, 1, 4) %in% "yfac"] ]


# annual estimates of relative abundance and standard errors - back transform year means with an infinite series lognormal bias correction as in Lo et al (1992)

param.names <- names(coef(cpue.ggam))	
param.ind <- seq(1, length(param.names))
yr.coef <- coef(cpue.ggam)[param.ind[ substr(param.names, 1, 4) %in% "yfac"] ]

# bias correction
b <- sum( (cpue.ggam$residuals)^2)/cpue.ggam$df.residual


# final index of abundance estimate

index <- c( 0, yr.coef.pres*yr.coef*b )

names(index) <- as.character( seq(2010, 2013) )



plot( names(index), index, type="b", pch=16, ylim=c(-.5, .5), xlab="Year", ylab="Index", xaxt="n" )
axis( 1, at = names(index), labels=names(index))
abline(h=0, lty=2)





