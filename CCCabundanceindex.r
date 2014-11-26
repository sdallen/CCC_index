#This Rcode reads in the data


rm(list = ls(all = TRUE))

# ==============================================================================
# Directories.

 Top.dir <-"C:\\Users\\shanae.allen\\Documents\\Courses\\Reproducible Research\\my_proj"
 setwd( Top.dir )

#Dat.dir <- "Data"


# ==============================================================================
# Libraries/ Settings
#  library("stats")
  library("lattice")

#  install.packages(c("ggplot2","lubridate", "plyr", "mosaic", "mosaicData"))

  library(lubridate)  # for dealing with dates
  library(ggplot2)
  library(plyr)


# =================================================================================
# WC GSI fine scale data

  dat2010.fn <- "2010_pfx_query.csv"
  dat2011.fn <- "2011_pfx_query.csv"
  dat2012.fn <- "2012_pfx_query.csv"
  dat2013.fn <- "2013_pfx_query.csv"

  datinter.fn <- "interview_pfx_query.csv"

# ==============================================================================
# Read in data
# ==============================================================================\

  dat10 <- read.csv(dat2010.fn,  sep=",", as.is=TRUE, strip.white = TRUE)


  dat11 <- read.csv(dat2011.fn,  sep=",", as.is=TRUE, strip.white = TRUE)


  head(dat11)

  dat12 <- read.csv(dat2012.fn,  sep=",", as.is=TRUE, strip.white = TRUE)

  dat13 <- read.csv(dat2013.fn,  sep=",", as.is=TRUE, strip.white = TRUE)


  datid <- read.csv(datinter.fn,  sep=",", as.is=TRUE, strip.white = TRUE)

  colnames(datid)
# [1] "id"             "agency_abbr"    "interviewdate"  "fishCount"      "port"           "vessel_id"      "interviewer"    "verifier"      
# [9] "fisherytype"    "fisherygroup"   "landingreceipt" "crewpresent"   
 

  summary(datid)

  colnames(dat10)
# [1] "interview_id"    "agency_abbr"     "barcode"         "date_str"       
# [5] "time_str"        "lat"             "lon"             "length"         
# [9] "capture_depth"   "recorded_mark"   "mature_verbose"  "sampled_verbose"
#[13] "finsampled"      "notes"           "age"             "bestestimate1"  
#[17] "probability1"    "bestestimate2"   "probability2"    "snoutid"     
  colnames(dat11)
  colnames(dat12)
  colnames(dat13)

  # all columns are the same (not type but same header)

# ==============================================================================
# Parse data
# ==============================================================================

  dat10 <- dat10[, c("barcode", "interview_id",    "date_str",            
             "time_str",    "lat",        "lon",        "capture_depth",          
             "bestestimate1" ,   "probability1")]

  dat11 <- dat11[, c("barcode", "interview_id",    "date_str",            
             "time_str",    "lat",        "lon",        "capture_depth",          
             "bestestimate1" ,   "probability1")]

  dat12 <- dat12[, c("barcode", "interview_id",    "date_str",            
             "time_str",    "lat",        "lon",        "capture_depth",          
             "bestestimate1" ,   "probability1")]

  dat13 <- dat13[, c("barcode", "interview_id",    "date_str",            
             "time_str",    "lat",        "lon",        "capture_depth",          
             "bestestimate1" ,   "probability1")]

  datid <- datid[, c( "id",    "interviewdate",            
             "fishCount",    "vessel_id")]

# ==============================================================================
# Make columns same data type
# ==============================================================================


  summary(dat11)
  summary(dat12)
  summary(dat13)



# change interview id, lat, lon, capture_depth, probability1 to numeric

  #2010
  summary(dat10)
 # unique(dat10$interview_id)
 # unique(dat10$lat)
 # unique(dat10$lon)
 # unique(dat10$capture_depth)

  # change "None" to NA
  dat10[ dat10 =="None"] <- NA

  dat10[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat10[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat10)

  head(dat10)

  #2011
  summary(dat11)
#  unique(dat11$interview_id)
#  unique(dat11$lat)
#  unique(dat11$lon)
#  unique(dat11$capture_depth)

  dat11[ dat11 =="None"] <- NA

  dat11[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat11[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat11)

  head(dat11)


  #2012
  summary(dat12)
#  unique(dat12$interview_id)
#  unique(dat12$lat)
#  unique(dat12$lon)
#  unique(dat12$capture_depth)

  dat12[ dat12 =="None"] <- NA

  dat12[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat12[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat12)

  head(dat12)


  #2013
  summary(dat13)
#  unique(dat13$interview_id)
#  unique(dat13$lat)
#  unique(dat13$lon)
#  unique(dat13$capture_depth)

  dat13[ dat13 =="None"] <- NA

  dat13[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat13[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat13)

  head(dat13)


  #change datid columns id, fishCount and vessel_id to numeric/remove "None" if present
  summary(datid)
#  unique(datid$id)
#  unique(datid$fishCount)
#  unique(datid$vessel_id)


  # change "None" to NA
  datid[ datid =="None"] <- NA

  datid[,c( "id",  "fishCount",  "vessel_id")] <- 
		as.data.frame(sapply( datid[,c( "id",  "fishCount",  "vessel_id")] , as.numeric)) 
  summary(datid)

  head(datid)




# ==============================================================================
# Combine years 
# ==============================================================================\
  
  dat <- rbind( dat10, dat11, dat12, dat13)



  
# ==============================================================================
# Parse data some more to Remove NA or crazy vals
# ==============================================================================\

 # NA assignments
  sum(is.na(dat$bestestimate1)) #~10,000 records out of 50721

  dat <- dat[ !is.na(dat$bestestimate1), ]


# REMOVE records with NA for dates OR times
  sum( is.na(dat$date_str)) #0
  sum( is.na(dat$time_str)) #8

  dat <- dat[ !is.na(dat$time_str), ]



# Remove record with crazy large lon or NAs

  summary( dat$lon) #188 records
  sum ( dat$lon < -1000, na.rm=TRUE)  #2
  sum ( is.na(dat$lon))  #12
  sum ( is.na(dat$lat))  #12



  dat <- dat[ !is.na(dat$lon), ]

  dat <- dat [ dat$lon > -1000, ] 



# ==============================================================================
# deal with dates and times
# ==============================================================================\

# concatenate date and time
  dat$datetime <- paste( dat$date_str, dat$time_str, sep=" ")

# change dates and times to date objects

  dat$datetime <- mdy_hms(dat$datetime)


  datid$interviewdate <- ymd(datid$interviewdate)


# remove date_str and time_str columns

  dat <- dat[ , !(colnames(dat) %in% c("date_str", "time_str"))]

# ==============================================================================
# Merge dat and datid to obtain vessel id for each fish then CHECK
# ==============================================================================\
  

  sum( is.na( match( dat$interview_id, datid$id) ))

  dat$vessel_id <- datid$vessel_id[ match( dat$interview_id, datid$id) ]


  # sort on date for year 2011 to check vessel_ids match with other file 
  datsort_d <- dat[ year(dat$datetime) == 2011,]

  datsort_d <- datsort_d[ order(datsort_d$datetime),]

  datsort_d[1:10, ]

  #looks right but GSI_2011 file doesn't contain records with NA for best estimate,
	# didn't check all vesselids


  #remove NA vessel_ids
  sum(is.na(dat$vessel_id)) #0
  dat <- dat[ !is.na(dat$vessel_id), ]


# ==============================================================================
# how does lubridate treat dates?


  dat$mfac <- factor( month(dat$datetime)) #treats months the same from different years 

  dat$myfac <- cut( dat$datetime, breaks="month") #treats months differently from different years 
  dat$dmyfac <- cut( dat$datetime, breaks="day")
  dat$hdmyfac <- cut( dat$datetime, breaks="hour")


# ==============================================================================
# Plot data
# ==============================================================================\
  


    
  #to determine effort time (hour, day), see how much lat\lon varies


  g <- ggplot(data=dat, mapping = aes( x=factor( myfac ), y=lat)) #per month over all boats, hours, years

  g + geom_boxplot()


  
  g2 <- ggplot(data=dat, mapping = aes( x=factor( vessel_id), y=lat)) #per boat over all hours, months and years

  g2 + geom_boxplot()





# LATTICE PLOTS 

# boxplots for latitude by vessel and month  (treating months the same over years)
  bwplot(lat~factor(vessel_id)|factor( month( datetime )),
   ylab="lat", xlab="vessel",
   main="lat by vessel and month", data=dat)  #,
   #layout=(c(1,3))


# boxplots for longitude by vessel and month  (treating months the same over years)

  bwplot(lon~factor(vessel_id)|factor( month( datetime )),
   ylab="lon", xlab="vessel",
   main="lon by vessel and month", data=dat)  #,
   #layout=(c(1,3))




# ==============================================================================
# Parse data some more to Remove NA or crazy vals
# ==============================================================================\

# Remove October because poor coverage in latitude

  sum( month(dat$datetime)==10) #188 records

  dat <- dat[ month(dat$datetime)!=10 , ]




# ==============================================================================
# plot variation of lat/long per vessel within a month/day/hour
# ==============================================================================\

  # sd of latitude per vessel and month  (months NOT the same over years)

  sd.lat.mo <- aggregate( formula = lat ~ factor(vessel_id) + factor( myfac  ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lat.mo$lat), sd.lat.mo$lat)
 


  # sd of latitude per vessel and day

  sd.lat.day <- aggregate( formula = lat ~ factor(vessel_id) + factor( dmyfac  ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lat.day$lat), sd.lat.day$lat)
 


  # sd of latitude per vessel and hour

  sd.lat.hr <- aggregate( formula = lat ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lat.hr$lat), sd.lat.hr$lat)
 
  mean(sd.lat.hr$lat, na.rm=TRUE)

 


  # sd of longitude per vessel and month

  sd.lon.mo <- aggregate( formula = lon ~ factor(vessel_id) + factor( myfac ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lon.mo$lon), sd.lon.mo$lon)
 


  # sd of longitude per vessel and day

  sd.lon.day <- aggregate( formula = lon ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lon.day$lon), sd.lon.day$lon)
 


  # sd of longitude per vessel and hour

  sd.lon.hr <- aggregate( formula = lon ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) sd(x))
  
  plot( 1:length(sd.lon.hr$lon), sd.lon.hr$lon)
 
  mean(sd.lon.hr$lon, na.rm=TRUE)

 



 
  # CV of latitude per vessel and month

  CV.lat.mo <- aggregate( formula = lat ~ factor(vessel_id) + factor( myfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lat.mo$lat), CV.lat.mo$lat)
 
  mean(CV.lat.mo$lat, na.rm=TRUE)

  # CV of latitude per vessel and day

  CV.lat.day <- aggregate( formula = lat ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lat.day$lat), CV.lat.day$lat)
 
  mean(CV.lat.day$lat, na.rm=TRUE)

  # CV of latitude per vessel and hour

  CV.lat.hr <- aggregate( formula = lat ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lat.hr$lat), CV.lat.hr$lat)
 
  mean(CV.lat.hr$lat, na.rm=TRUE)





 
  # CV of longitude per vessel and month

  CV.lon.mo <- aggregate( formula = lon ~ factor(vessel_id) + factor( myfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lon.mo$lon), CV.lon.mo$lon)

   
  mean(CV.lon.mo$lon, na.rm=TRUE)


  # CV of lonitude per vessel and day

  CV.lon.day <- aggregate( formula = lon ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lon.day$lon), CV.lon.day$lon)
 
  mean(CV.lon.day$lon, na.rm=TRUE)


  # CV of lonitude per vessel and hour

  CV.lon.hr <- aggregate( formula = lon ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) sd(x)/mean(x))
  
  plot( 1:length(CV.lon.hr$lon), CV.lon.hr$lon)
 
  mean(CV.lon.hr$lon, na.rm=TRUE)







  # max diff of latitude per vessel and month  (months NOT the same over years)

  diff.lat.mo <- aggregate( formula = lat ~ factor(vessel_id) + factor( myfac  ), data = dat, 
			FUN = function(x) max(x) - min(x) )
  
  plot( 1:length(diff.lat.mo$lat), diff.lat.mo$lat)
 
  mean( diff.lat.mo$lat, na.rm=TRUE)


  # diff of latitude per vessel and day

  diff.lat.day <- aggregate( formula = lat ~ factor(vessel_id) + factor( dmyfac  ), data = dat, 
			FUN = function(x) max(x) - min(x))
  
  plot( 1:length(diff.lat.day$lat), diff.lat.day$lat)
  mean( diff.lat.day$lat, na.rm=TRUE) 


  # diff of latitude per vessel and hour

  diff.lat.hr <- aggregate( formula = lat ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) max(x) - min(x))
  
  plot( 1:length(diff.lat.hr$lat), diff.lat.hr$lat)
 
  mean(diff.lat.hr$lat, na.rm=TRUE)

 


  # diff of longitude per vessel and month

  diff.lon.mo <- aggregate( formula = lon ~ factor(vessel_id) + factor( myfac ), data = dat, 
			FUN = function(x) max(x) - min(x))
  
  plot( 1:length(diff.lon.mo$lon), diff.lon.mo$lon)
   mean( diff.lon.mo$lon, na.rm=TRUE) 


  # diff of longitude per vessel and day

  diff.lon.day <- aggregate( formula = lon ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) max(x) - min(x))
  
  plot( 1:length(diff.lon.day$lon), diff.lon.day$lon)
   mean( diff.lon.day$lon, na.rm=TRUE) 


  # diff of longitude per vessel and hour

  diff.lon.hr <- aggregate( formula = lon ~ factor(vessel_id) + factor( hdmyfac ), data = dat, 
			FUN = function(x) max(x) - min(x))
  
  plot( 1:length(diff.lon.hr$lon), diff.lon.hr$lon)
 
  mean(diff.lon.hr$lon, na.rm=TRUE)





# ==============================================================================
# Start with CPUE, effort per DAY  ----> catch of CCC per day and vessel
# ==============================================================================\

 # total catch of CCC per vessel day month year
  CPUE <- aggregate( formula = bestestimate1 ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) sum( x=="California_Coast") )
  
  m <- format( as.Date(CPUE[ , "factor(dmyfac)"]),'%m') #month

  CPUE$mfac <- factor(m,levels=unique(m))   #,ordered=TRUE

  # make new data frame


  avglatday <- aggregate( formula = lat ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) mean(x) )

  avglonday <- aggregate( formula = lon ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = function(x) mean(x) )

  # this is hours spent per vessel and day catching fish (not total hours fished)
  hrsfished <- aggregate( formula = hdmyfac ~ factor(vessel_id) + factor( dmyfac ), data = dat, 
			FUN = length )


# need to bring in vessel_ids per day and month, year

  cpuedat <- data.frame(yfac = cut( ymd(CPUE[ , "factor(dmyfac)"]), breaks="year"), 
        mfac = CPUE$mfac,
				myfac = cut( ymd(CPUE[ , "factor(dmyfac)"]), breaks="month"), dmyfac = CPUE[ , "factor(dmyfac)"], 
				vessel_id = CPUE[ , "factor(vessel_id)"], hrsfished = hrsfished$hdmyfac, 
        avglatday=avglatday$lat, avglonday=avglonday$lon, 
					Cperday = CPUE$bestestimate1,
					Cperhr = CPUE$bestestimate1/hrsfished$hdmyfac)

################################################################################
#################################################################################
#################################################################################


# ==============================================================================
# Plot density of Cperhr
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

  d + geom_hline(yintercept=40.636)


# per month and year

  d + geom_hline(yintercept=40.636) + facet_wrap( ~ myfac)

 

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
# Read in SST


  #2010

  SST10 <- read.csv( "SST-04-30-2010to09-24-2010.csv",  sep=",", as.is=TRUE, strip.white = TRUE, header=TRUE)


  summary(SST10)
  head(SST10)

  #every 27th line contains date info
  SST10[1, ]
  SST10[ 1 + 27, ]
  SST10[ 1 + 27 + 27, ]

  # extract these dates

  SST10date <- SST10[ seq(1, nrow(SST10), by = 27), 1]
  

  # truncate stuff
  SST10date <- substr( SST10date, start=16, stop = 26)

  # change to date

  SST10date <- strptime(SST10date, format="%d-%b-%Y")


  # match index on days fished


  dat10b <- dat[ year( dat$datetime) ==2010, ]

  ind.match <- match( yday( dat10b$datetime ) , yday(SST10date) )
  #CHECK
  dat10b$datetime[10]

  SST10[1 + 27*(ind.match[10]-1), ]  


  # for each index find matching lat and lon - rounded to the nearest .5

  head(SST10)
  head(dat)

  # delete date rows from file
  SST10 <- SST10[ -seq(1, nrow(SST10), by = 27), ]
  # rename rows and columns

  colnames(SST10) <- c( "lat", as.character( seq(125, 119, by=-.5)) )
  SST10[, "lat"] <- rep(as.character( seq(34, 46.5, by=.5)), length(SST10date)) 




  lon.ind <- match( round_any(dat10b$lon[ind.match], accuracy=.5), as.numeric( colnames(SST10)[-1]) ) 

  lat.ind <- match( round_any(dat10b$lat[ind.match], accuracy=.5), as.numeric( SST10[, 1]) ) 



################################################################################
#################################################################################
#################################################################################
# Model of presence/absense


  library(mgcv) #for GAMs



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
  pres.gam <- gam( presabs ~ mfac + yfac + s(yfac, mfac, bs="re")+ s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                 family="binomial", data=cpuedat)

  summary(pres.gam)

  plot(pres.gam)




	 
##############################################################################################
# diagnostic plot 

gam.check(pres.gam, pch=19)  #, cex=.3)

 



################################################################################
#################################################################################
#################################################################################
# Model of cpue given presence


 # using month and year interaction, lat and month interaction, and lon and month interaction
  lcpue.gam <- gam( log(Cperhr) ~ mfac + yfac + s(yfac, mfac, bs="re") + s(avglatday, by=mfac) + s(avglonday, by=mfac) +  s(vessel_id, bs="re"), 
                  data=cpuedat[cpuedat$presabs ==1, ])

  summary(lcpue.gam )

  plot(lcpue.gam)


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


  
  param.names.pres <- names(coef(pres.gam))	
  param.ind.pres <- seq(1, length(param.names.pres))
  yr.coef.pres <- coef(pres.gam)[param.ind.pres[ substr(param.names.pres, 1, 4) %in% "yfac"] ]


  # annual estimates of relative abundance and standard errors - back transform year means with an infinite series lognormal bias correction as in Lo et al (1992)
  
  param.names <- names(coef(lcpue.gam))	
  param.ind <- seq(1, length(param.names))
  yr.coef <- coef(lcpue.gam)[param.ind[ substr(param.names, 1, 4) %in% "yfac"] ]

  # bias correction
  b <- sum( (lcpue.gam$residuals)^2)/lcpue.gam$df.residual


  # final index of abundance estimate

  index <- c( 0, yr.coef.pres*yr.coef*b )
  
  names(index) <- as.character( seq(2010, 2013) )



  plot( names(index), index, type="b", pch=16, ylim=c(-.15, .15), xlab="Year", ylab="Index" )




  

