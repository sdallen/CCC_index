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
  unique(dat10$interview_id)
  unique(dat10$lat)
  unique(dat10$lon)
  unique(dat10$capture_depth)

  # change "None" to NA
  dat10[ dat10 =="None"] <- NA

  dat10[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat10[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat10)

  head(dat10)

  #2011
  summary(dat11)
  unique(dat11$interview_id)
  unique(dat11$lat)
  unique(dat11$lon)
  unique(dat11$capture_depth)

  dat11[ dat11 =="None"] <- NA

  dat11[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat11[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat11)

  head(dat11)


  #2012
  summary(dat12)
  unique(dat12$interview_id)
  unique(dat12$lat)
  unique(dat12$lon)
  unique(dat12$capture_depth)

  dat12[ dat12 =="None"] <- NA

  dat12[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat12[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat12)

  head(dat12)


  #2013
  summary(dat13)
  unique(dat13$interview_id)
  unique(dat13$lat)
  unique(dat13$lon)
  unique(dat13$capture_depth)

  dat13[ dat13 =="None"] <- NA

  dat13[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] <- 
		as.data.frame(sapply( dat13[,c( "interview_id",  "lat",  "lon", "capture_depth", "probability1")] , as.numeric)) 
  summary(dat13)

  head(dat13)


  #change datid columns id, fishCount and vessel_id to numeric/remove "None" if present
  summary(datid)
  unique(datid$id)
  unique(datid$fishCount)
  unique(datid$vessel_id)


  # change "None" to NA
  datid[ datid =="None"] <- NA

  datid[,c( "id",  "fishCount",  "vessel_id")] <- 
		as.data.frame(sapply( datid[,c( "id",  "fishCount",  "vessel_id")] , as.numeric)) 
  summary(datid)

  head(datid)


# change dates and times to date objects

  dat10$date_str <- mdy(dat10$date_str)
  dat11$date_str <- mdy(dat11$date_str)
  dat12$date_str <- mdy(dat12$date_str)
  dat13$date_str <- mdy(dat13$date_str)

  datid$interviewdate <- ymd(datid$interviewdate)

  dat10$time_str <- hms(dat10$time_str)
  dat11$time_str <- hms(dat11$time_str)
  dat12$time_str <- hms(dat12$time_str)
  dat13$time_str <- hms(dat13$time_str)


# ==============================================================================
# Combine years 
# ==============================================================================\
  
  dat <- rbind( dat10, dat11, dat12, dat13)


# ==============================================================================
# Merge dat and datid to obtain vessel id for each fish then CHECK
# ==============================================================================\
  

  sum( is.na( match( dat$interview_id, datid$id) ))

  dat$vessel_id <- datid$vessel_id[ match( dat$interview_id, datid$id) ]


  # sort on date for year 2011 to check vessel_ids match with other file 
  datsort_d <- dat[ year(dat$date_str) == 2011,]

  datsort_d <- datsort_d[ order(datsort_d$date_str, datsort_d$time_str),]

  datsort_d[1:10, ]

  #looks right but GSI_2011 file doesn't contain records with NA for best estimate,
	# didn't check all vesselids


  
# ==============================================================================
# Remove NA assignments
# ==============================================================================\
  sum(is.na(dat$bestestimate1)) #~10,000 records

  dat <- dat[ !is.na(dat$bestestimate1), ]



# ==============================================================================
# Plot data
# ==============================================================================\
  
    
  #to determine effort time (hour, day), see how much lat\lon varies


  g <- ggplot(data=dat, mapping = aes( factor( month(date_str)), lat)) #per month over all boats, hours, years

  g + geom_boxplot()


  
  g2 <- ggplot(data=dat, mapping = aes( factor( vessel_id), lat)) #per boat over all hours, months and years

  g2 + geom_boxplot()



# boxplots for latitude by vessel and month 

  bwplot(lat~factor(vessel_id)|factor( month( date_str)),
   ylab="lat", xlab="vessel",
   main="lat by vessel and month", data=dat)  #,
   #layout=(c(1,3))


# boxplots for longitude by vessel and month 

  bwplot(lon~factor(vessel_id)|factor( month( date_str)),
   ylab="lon", xlab="vessel",
   main="lon by vessel and month", data=dat)  #,
   #layout=(c(1,3))




  
# ==============================================================================
# Remove October because poor coverage in latitude
# ==============================================================================\
  sum( month(dat$date_str)==10) #188 records

  dat <- dat[ month(dat$date_str)!=10 , ]


  
# ==============================================================================
# Remove record with crazy large lon or NAs
# ==============================================================================\
  summary( dat$lon) #188 records
  sum ( dat$lon < -1000, na.rm=TRUE)  #2
  sum ( is.na(dat$lon))  #12
  sum ( is.na(dat$lat))  #12



  dat <- dat[ !is.na(dat$lon), ]

  dat <- dat [ dat$lon > -1000, ] 






