# EXAMPLE OF EVAPOTRANSPIRATION ESTIMATION USING SOIL MOISTURE PROFILE TIME SERIES 
# BY OLIVER HARGREAVES, LAURA CHRISTIANSEN AND ALFONSO TORRES
# 2021, UTAH STATE UNIVERSITY

# NOTES

# 1. This code can only process a single soil profile at a time.
# 2. Soil moisture data is arranged in column per sensor depth and in daily scale 
# 3. No missing information (soil moisture values) occurs in the data.


# PACKAGES ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)


# LOAD DATA FROM EXCEL FILE ####
data.101=read_excel("DailySM.101.xlsx") # load the SM data from excel
data.101=na.omit(data.101)              # eliminate the days with no data

n=length(data.101$Date.101)             # Number of days with data
ETo=data.101$ETo_mm                     # Reference ET (mm)


# DESCRIPTION OF SOIL MOISTURE SENSOR DEPTH REPRESENTATION (mm) ####
sd1=1.5*305 # sensor 1 and 5: from surface to 1.5ft
sd2=1.0*305 # sensor 2 and 6: from 1.5ft to 2.5ft
sd3=1.0*305 # sensor 3 and 7: from 2.5ft to 3.5ft
sd4=1.0*305 # sensor 4 and 8: from 3.5ft to 4.5ft


# EXTRACTING SOIL MOISTURE DATA FROM EXCEL FILE (VOLUMETRIC, %) ####
sm1=data.101$sm1.101
sm2=data.101$sm2.101
sm3=data.101$sm3.101
sm4=data.101$sm4.101

sm1 =sm1/3 #initial penalization of top soil sensor

# CROP ROORT DEPTH (mm)
rd=data.101$Zr_Corn

#VISUALIZATION OF EXCEL DATA (SM) ####

## plot root depth with sensor depth
plot(data.101$Date.101, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-1800, 0),
     main='Crop root depth', ylab='Soil/Sensor depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral3', lwd=2)
abline(h=-sd2-sd1, col='steelblue3', lwd=2)
abline(h=-sd3-sd2-sd1, col='gold3', lwd=2)
abline(h=-sd4-sd3-sd2-sd1, col='olivedrab3', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('sensor 1','sensor 2','sensor 3','sensor 4'), col=c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)

# Plot daily soil moisture data
# Sub-station 1
plot (data.101$Date.101, sm1, ylim=c(0, 60), type='l', lwd=2, col='coral3', 
      main='Daily SM for sub-station 1', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm2, type='l', lwd=2, col='steelblue3')
lines(data.101$Date.101, sm3, type='l', lwd=2, col='gold3')
lines(data.101$Date.101, sm4, type='l', lwd=2, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('sensor 1 - 1ft','sensor 2 - 2ft','sensor 3 - 3ft','sensor 4 - 4ft'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)



# Method A: SOIL DEPLETION on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the 
# entire soil profile at once.

# Sub-station 1
# Total water content (mm) of the soil profile
wc.1=((sm1)*sd1+sm2*(sd2)+sm3*(sd3)+sm4*(sd4))/100

# ET from sub-station 1
ET.A.1=c()
for(i in 2:n) { 
  ET.A.1[i]=wc.1[i-1]-wc.1[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.1[i]<0) {ET.A.1[i]=NA}} 

plot(data.101$Date.101, ET.A.1, 
     ylim=c(0, 12),type='h', lwd=2, col='darkslategray3',
     main='METHOD A: SOIL PROFILE SM', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# # ET/ETo for sub-station 1
# Kc.A.1=ET.A.1/ETo
# plot(data.101$Date.101, Kc.A.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='METHOD A: SOIL PROFILE SOIL MOISTURE', xlab='', ylab='ET/ETo')
# 



# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# Sub-station 1

# ET from soil depth 1
ET1.101=c() 
for(i in 2:n) { 
  ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}
for(i in 2:n) { # eliminate negative ET values
  if (ET1.101[i]<0) {ET1.101[i]=NA}}         

# ET from soil depth 2
ET2.101=c() 
for(i in 2:n) { 
  ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.101[i]<0) {ET2.101[i]=NA}}         

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) { 
  ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.101[i]<0) {ET3.101[i]=NA}}         

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) { 
  ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.101[i]<0) {ET4.101[i]=NA}}         

# Total ET for sub-station 1
ET1.101[is.na(ET1.101)]=0 # Turn NAs into zeroes for the sum
ET2.101[is.na(ET2.101)]=0  
ET3.101[is.na(ET3.101)]=0  
ET4.101[is.na(ET4.101)]=0 

ET.B.1=ET1.101+ET2.101+ET3.101+ET4.101
ET.B.1[ET.B.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date.101, ET.B.1,
     ylim=c(0, 12), type='h', col='darkslategray3', lwd=2,
     main='METHOD B: PER SENSOR', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# # ET/ETo for sub-station 1
# Kc.B.1=ET.B.1/ETo
# plot(data.101$Date.101, Kc.B.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='METHOD B: PER SENSOR', xlab='', ylab='Kc')


# Method C: Water balance on each sensors soil depth accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method C takes root growth throughout the season in account.

# Adjusted root depth: the average with previous day is used for the calculations.
RD=c()      
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today

# ET from soil depth 1
ET1.101=c()
for(i in 2:n) {
  if (RD[i] < sd1) {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  else {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET1.101[i]<0) {ET1.101[i]=NA}}         

# ET from soil depth 2
ET2.101=c()
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.101[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2+sd1) {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET2.101[i]<0) {ET2.101[i]=NA}}    
ET2.101[ET2.101==0]=NA # Turns zeros into NAs

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) {
  if (RD[i] <= sd2+sd1) {
    ET3.101[i]=0}
  else if (sd2+sd1 < RD[i] & RD[i] <= sd3+sd2+sd1) {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2-sd1) }
  else {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET3.101[i]<0) {ET3.101[i]=NA} }   
ET3.101[ET3.101==0]=NA # Turns zeros into NAs

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) {
  if (RD[i] <= sd3+sd2+sd1) {
    ET4.101[i]=0}
  else if (sd1+sd2+sd3 < RD[i] & RD[i] <= sd4+sd3+sd2+sd1) {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3-sd2-sd1) }
  else {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET4.101[i]<0) {ET4.101[i]=NA} }         
ET4.101[ET4.101==0]=NA # Turns zeros into NAs

# Total ET for sub-station 1
ET1.101[is.na(ET1.101)]=0 # Turn NAs into zeroes for the sum
ET2.101[is.na(ET2.101)]=0  
ET3.101[is.na(ET3.101)]=0  
ET4.101[is.na(ET4.101)]=0 

ET.C.1=ET1.101+ET2.101+ET3.101+ET4.101
ET.C.1[ET.C.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date.101, ET.C.1,
     ylim=c(0, 12),
     type='h', col='darkslategray3', lwd=2,
     main='METHOD C: ROOT ZONE', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)


# # ET/ETo for sub-station 1
# Kc.C.1=ET.C.1/ETo
# plot(data.101$Date.101, Kc.C.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='METHOD C: ROOT ZONE', xlab='', ylab='Kc')
# 

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

w=5 # window used to calculate the derivatives in days

# Sub-station 1
# Total water content (mm) of the soil profile
wc.1=((sm1)*sd1+sm2*(sd2)+sm3*(sd3)+sm4*(sd4))/100

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
#  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)

#  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='.', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sub-station 1', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='.', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=wc.1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='darkslategray3',
     main='METHOD D: DERIVATIVES ON SM PROFILE', xlab='', ylab='ET (mm)',
     ylim=c(0,12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D.1=sensor1$ET

# # ET/ETo for sub-station 1
# Kc.D.1=ET.D.1/ETo
# plot(data.101$Date.101, Kc.D.1,
#      ylim=c(0, 2),
#      type='h', lwd=2, col='orchid3',
#      main='METHOD D: DERIVATIVES ON SM PROFILE', xlab='', ylab='Kc')



# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

w=5 # window used to calculate the derivatives in days

# Sub station 1

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
#  f1[i]=(SM1[i+1]-SM1[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 1', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)
# 
# sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
# plot(sensor1$Date, sensor1$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 1', xlab='', ylab='ET (mm)',
#      ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
#  f1[i]=(SM2[i+1]-SM2[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
  }
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
  
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 2', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor2$f1[i]<0 ) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

# plot(sensor2$Date, sensor2$ET,
#      type='h', lwd=2,
#      main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
#      ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  #f1[i]=(SM3[i+1]-SM3[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 3', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor3$f1[i]<0 ) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

# plot(sensor3$Date, sensor3$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 3', xlab='', ylab='ET (mm)',
#      ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
 # f1[i]=(SM4[i+1]-SM4[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 4', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor4$f1[i]<0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

# plot(sensor4$Date, sensor4$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 4', xlab='', ylab='ET (mm)',
#      ylim=c(0, 5))
# 

# Sub-station 1

# Total ET from the entire soil profile
sensor1$ET[is.na(sensor1$ET)]=0 # turn NA values with zero
sensor2$ET[is.na(sensor2$ET)]=0 # turn NA values with zero
sensor3$ET[is.na(sensor3$ET)]=0 # turn NA values with zero
sensor4$ET[is.na(sensor4$ET)]=0 # turn NA values with zero
ET.E.1=sensor1$ET+
  sensor2$ET+
  sensor3$ET+
  sensor4$ET
ET.E.1[ET.E.1==0]=NA # turn zeros into NA

plot(data.101$Date.101, ET.E.1, 
     type='h', lwd=2, col='darkslategray3',
     main='METHOD E: DERIVATIVE PER SENSOR', xlab='', ylab='ET (mm)',
     ylim=c(0,12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)


# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

w=5 # window used to calculate the derivatives in days

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve

f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
  }
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 1', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
# plot(sensor1$Date, sensor1$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 1', xlab='', ylab='ET (mm)',
#      ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 2', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor2$f1[i]<0 ) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd1) 
  {sensor2$ET[i]=NA}
}

# plot(sensor2$Date, sensor2$ET,
#      type='h', lwd=2,
#      main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
#      ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3)/100 # soil moisture (mm)
# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
  }
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 3', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor3$f1[i]<0 ) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd2+sd1) 
  {sensor3$ET[i]=NA}
}

# plot(sensor3$Date, sensor3$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 3', xlab='', ylab='ET (mm)',
#      ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4)/100 # soil moisture (mm)

	
# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 4', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor4$f1[i]<0 ) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd3+sd2+sd1) 
  {sensor4$ET[i]=NA}
}

# plot(sensor4$Date, sensor4$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 4', xlab='', ylab='ET (mm)',
#      ylim=c(0, 5))


# Sub-station 1

# Total ET from the entire soil profile
sensor1$ET[is.na(sensor1$ET)]=0 # turn NA values into zeros
sensor2$ET[is.na(sensor2$ET)]=0 # turn NA values into zeros
sensor3$ET[is.na(sensor3$ET)]=0 # turn NA values into zeros
sensor4$ET[is.na(sensor4$ET)]=0 # turn NA values into zeros
ET.F.1=sensor1$ET+
  sensor2$ET+
  sensor3$ET+
  sensor4$ET
ET.F.1[ET.F.1==0]=NA # turn zeros into NAs

plot(data.101$Date.101, ET.F.1, 
     type='h', lwd=2, col='darkslategray3',
     main='METHOD F: DERIVATIVE PER SENSOR CONSIDERING ROORT ZONE', xlab='', ylab='ET (mm)',
     ylim=c(0,12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)


# ET/ETo ~ crop coeficcient
# Kc.F.1=ET.F.1/ETo
# plot(data.101$Date.101, Kc.F.1,
#      ylim=c(0, 1),
#      type='h', lwd=2, col='orchid3',
#      main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')


# Comparison of the methods ####

# Sub-station 1
method=rep(c('A','B','C','D','E','F'), each=n)
ET=c(ET.A.1, ET.B.1, ET.C.1, ET.D.1, ET.E.1, ET.F.1) # Evapotranspiration
# Kc=c(Kc.A.1, Kc.B.1, Kc.C.1, Kc.D.1, Kc.E.1, Kc.F.1) # ET/ETos
comparison.1=data.frame(date=data.101$Date.101, method, ET)

# Box-plot for ET
ggplot(comparison.1, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison") +
  xlab("")+ylab("ET (mm)")

# # Box-plot for ET/ETo
# ggplot(comparison.1, aes(x = method, y = Kc, fill = date)) +
#   geom_boxplot() +
#   scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   ggtitle("Daily ET/ETo method comparison") +
#   xlab("") + ylab("ET/ETo")



# Save the results ####
# Write an excel spreadsheet with the results for ET and ET/ETo (~Kc) for all 6 methods

# sub-station 1
# Results.1=data.frame(Date=data.101$Date.101, ET.A.1, Kc.A.1, ET.B.1, Kc.B.1, ET.C.1, Kc.C.1, ET.D.1, Kc.D.1, ET.E.1, Kc.E.1, ET.F.1, Kc.F.1)
# write_xlsx(Results.1, path="C:/Users/Oliver/Box/DiviningWater/Wellsville2019/Soil sensor data/Sensor 101/Results.1.xlsx") # write the excel with the daily data

