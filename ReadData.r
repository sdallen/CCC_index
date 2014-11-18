#This Rcode reads in the data


rm(list = ls(all = TRUE))

# ==============================================================================
# Directories.

#Top.dir <-"C:\\Users\\shanae.allen\\Documents\\CCC Workshop"
#setwd( Top.dir )

#Dat.dir <- "Data"


# ==============================================================================
# Libraries/ Settings
#  library("stats")
#  library("lattice")

# =================================================================================
# 2011 WC GSI fine scale data

dat.fn <- "GSI_2011_OR_CA_All_Data_All_Probs_05102012.csv"


# ==============================================================================
# Read in data
# ==============================================================================\

Z <- read.csv(dat.fn,  sep=",", as.is=TRUE)
colnames(Z)

summary(Z)


# ==============================================================================
# Parse data
# ==============================================================================\

Z.1 <- Z[, c("barcode",          "interview_id",     "field_species_id", "Date",            
             "Time",             "capture_depth",    "lat",              "lon",             
              "fisherman_id",       
             "bestestimate1" ,   "probability1",    
             "bestestimate2" ,   "probability2")]

  