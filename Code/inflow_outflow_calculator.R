# Inflow/Outflow Calculator for Surface Irrigation
# R script designed to take bucket fill time readings and flume measurements to
# calculate water inflow/outflow quantities in volume and depth
# A.J. Brown
# CSU Ag Water Quality Program
# ansley.brown@colostate.edu
# 21 Feb. 2022

library(magrittr) # Adds pipe functionality
library(dplyr)
library(ggplot2)

# upload data: use template provided in Data folder
water.data = read.csv(file.choose())
head(water.data)

# Data column labels are as follows:
# plot - treatment type (i.e., CT, ST, MT)
# rep - replication #
# date - date of reading
# reading_1 - bucket fill time for attempt #1, seconds
# reading_2 - bucket fill time for attempt #2, seconds
# irr_time - irrigation set time, hours
# outflow_gal - water measured leaving the field over set time, gallons
# runoff_time - OPTIONAL: duration of runoff water leaving the field during set

# Convert 'date' to date object
water.data$date <- as.Date(water.data$date, format = "%m/%d/%y")

# Meta Variables
bucket.size = 7.2 # in Liters, L
row.spacing = 30 # in inches, in
field.length = 1050 # in feet, ft
num.rows.diverted = 2 # number of rows diverted into flume
#acreage of 1 wet row + 1 dry row
acreage.row = (row.spacing*2/12)*field.length/43560

# function to perform inflow calcs
inflow.func = function(df){
  # average bucket fill time, seconds
  df$inf.avg.sec = rowMeans(df[,c('reading_1', 'reading_2')], na.rm=TRUE)
  # convert to gallons per minute, gpm
  df$inf.avg.gpm = (bucket.size/df$inf.avg.sec)*60*0.264172
  # convert to total gallons in irrigation set
  df$inf.gal.trt = df$inf.avg.gpm*df$irr_time*60
  # convert total trt gallons in to acre/feet
  df$inf.acreft.trt = df$inf.gal.trt*.0000036889
  # convert acre/feet to acre/inches
  df$inf.acrein.trt = df$inf.acreft.trt*12
  # convert to inches applied
  df$inf.in.trt = df$inf.acrein/acreage.row
  # convert to milimeters applied
  df$inf.mm.trt = df$inf.in.trt/25.4
  return(df)
}

# function to perform outflow calcs
outflow.func = function(df) {
  # Outflow quantities
  # convert outflow gallons to acre/feete
  df$out.acreft.trt = df$outflow_gal*.0000036889
  # convert acre/feet to acre/inches
  df$out.acrein.trt = df$out.acreft.trt*12
  # convert to inches applied
  df$out.in.trt = df$out.acrein/acreage.row
  # convert to milimeters applied
  df$out.mm.trt = df$out.in.trt/25.4
  
  # Outflow times - only returns if runoff time is present in input csv file
  if('runoff_time' %in% colnames(df)) {
    # convert runoff time to hours
    df$runoff.time.hrs = df$runoff_time/60
    # derived starting hour of runoff relative to irrigation set time
    df$runoff.start.time.hr = df$irr_time - df$runoff.time.hrs
  }
  return(df)
}
# function to perform infiltration calcs
infiltration.func = function(df) {
  # calculate infiltration amounts, inches
  df$infiltration.in = df$inf.in.trt - df$out.in.trt
  # convert to millimeters
  df$infiltration.mm = df$infiltration.in*25.4
  # infiltration rate over set time, mm/hr
  df$infiltration.rate.mmhr = df$infiltration.mm/df$irr_time
  # Application efficiency % assuming deep percolation = 0 mm <- BAD ASSUMPTION
  df$bad_eff = (1 - (df$out.in.trt/df$inf.in.trt)) * 100
  return(df)
}

# Bringing it all together by applying the functions (must be applied in order)
water.data %<>%
  inflow.func() %>%
  outflow.func() %>%
  infiltration.func() %>%
  select(where(~sum(!is.na(.x)) > 0)) # Drop runoff time columns if not present

# View the dataframe for QA/QC
View(water.data)

#Save to csv as desired
write.csv(water.data, file.choose())


