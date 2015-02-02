#This Rcode reads in the data and cleans it up

datcleannew <- function(){

  
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
# Parse data some more to Remove NA or crazy vals
# ==============================================================================\

# Remove October because poor coverage in latitude

sum( month(dat$datetime)==10) #188 records

dat <- dat[ month(dat$datetime)!=10 , ]


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
                        #Cperhr - average catch per hour ( catch per day standardized by hours fished)

################################################################################
#################################################################################
#################################################################################

  return( list( dat=dat, cpuedat = cpuedat) )

}