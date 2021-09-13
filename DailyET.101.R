# ET estimation for sensor 101 in Wellsville 2019

# Packages ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)

# Load the data and define the variables for sensor 101####
setwd("C:/Users/Oliver/Box/DiviningWater/Wellsville2019/Soil Sensor Data/Sensor 101")
data.101=read_excel("DailySM.101.xlsx") # load the daily SM data from excel
data.101=na.omit(data.101) # eliminate the days with no data

n=length(data.101$Date.101) # Number of days with data
ETo=data.101$ETo_mm         # Reference ET (mm)
# Sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1 and 5: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2 and 6: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3 and 7: from 2.5ft to 3.5ft
sd4=sd8=4.5*305 # sensor 4 and 8: from 3.5ft to 4.5ft
# Soil moisture - volumetric water content %
sm1=data.101$sm1.101
sm2=data.101$sm2.101
sm3=data.101$sm3.101
sm4=data.101$sm4.101
sm5=data.101$sm5.101
sm6=data.101$sm6.101
sm7=data.101$sm7.101
sm8=data.101$sm8.101
# Top soil layer penalization: sensors 1 and 5
sm1=sm1/3 
sm5=sm5/3

# Root depth (mm)
rd=data.101$Zr_Corn

# Save the root depth and sensor depth diagram in a tiff file
tiff(file="CornRootDepth.tiff",
     width=420, height=297, units="mm", res=600)
plot(data.101$Date.101, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-1800, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral3', lwd=2)
abline(h=-sd2, col='steelblue3', lwd=2)
abline(h=-sd3, col='gold3', lwd=2)
abline(h=-sd4, col='olivedrab3', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('Sensor 1','Sensor 2','Sensor 3','Sensor 4'), col=c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)
dev.off()

# Plot daily soil moisture data
# Sub-station 1
plot (data.101$Date.101, sm1, ylim=c(0, 60), type='l', lwd=2, col='coral3', 
      main='Daily SM for sub-station 1', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm2, type='l', lwd=2, col='steelblue3')
lines(data.101$Date.101, sm3, type='l', lwd=2, col='gold3')
lines(data.101$Date.101, sm4, type='l', lwd=2, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('Sensor 1: 1ft','Sensor 2: 2ft','Sensor 3: 3ft','Sensor 4: 4ft'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)
# Sub-station 101
plot (data.101$Date.101, sm5, ylim=c(0, 60), type='l', lwd=2, col='coral3', 
      main='Daily SM for sub-station 101', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm6, type='l', lwd=2, col='steelblue3')
lines(data.101$Date.101, sm7, type='l', lwd=2, col='gold3')
lines(data.101$Date.101, sm8, type='l', lwd=2, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('Sensor 5: 1ft','Sensor 6: 2ft','Sensor 7: 3ft','Sensor 8: 4ft'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)


# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the 
# entire soil profile at once.

# Sub-station 1
# Total water content (mm) of the soil profile
wc.1=(sm1*sd1+sm2*(sd2-sd1)+sm3*(sd3-sd2)+sm4*(sd4-sd3))/100

# ET from sub-station 1
ET.A.1=c()
for(i in 2:n) { 
  ET.A.1[i]=wc.1[i-1]-wc.1[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.1[i]<0) {ET.A.1[i]=NA}}         
plot(data.101$Date.101, ET.A.1, 
     type='h', lwd=2, col='darkslategray3',
     main='Daily ET from sub-station 1: Method A', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# ET/ETo for sub-station 1
Kc.A.1=ET.A.1/ETo
plot(data.101$Date.101, Kc.A.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1: Method A', xlab='', ylab='Kc')

# Sub-station 101
# total water content (mm) of the soil profile
wc.101=(sm5*(sd5)+sm6*(sd6-sd5)+sm7*(sd7-sd6)+sm8*(sd8-sd7))/100

# ET from sub-station 101
ET.A.101=c()
for(i in 2:n) { 
  ET.A.101[i]=wc.101[i-1]-wc.101[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.101[i]<0) {ET.A.101[i]=NA}}         
plot(data.101$Date.101, ET.A.101, 
     type='h', lwd=2, col='darkslategray3',
     main='ET from sub-station 101', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# ET/ETo for sub-station 101
Kc.A.101=ET.A.101/ETo
plot(data.101$Date.101, Kc.A.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')

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
plot(data.101$Date.101, ET1.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c() 
for(i in 2:n) { 
  ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.101[i]<0) {ET2.101[i]=NA}}         
plot(data.101$Date.101, ET2.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) { 
  ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.101[i]<0) {ET3.101[i]=NA}}         
plot(data.101$Date.101, ET3.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) { 
  ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.101[i]<0) {ET4.101[i]=NA}}         
plot(data.101$Date.101, ET4.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# Total ET for sub-station 1
ET1.101[is.na(ET1.101)]=0 # Turn NAs into zeroes for the sum
ET2.101[is.na(ET2.101)]=0  
ET3.101[is.na(ET3.101)]=0  
ET4.101[is.na(ET4.101)]=0 

ET.B.1=ET1.101+ET2.101+ET3.101+ET4.101
ET.B.1[ET.B.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date.101, ET.B.1,
     ylim=c(0, 12), type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sub-station 1', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
ET1.101[ET1.101==0]=NA # Turn zeroes into NAs
ET2.101[ET2.101==0]=NA
ET3.101[ET3.101==0]=NA
ET4.101[ET4.101==0]=NA
x=data.101$Date.101
y=paste('sensor', 4:1)
ET=c(ET4.101, ET3.101, ET2.101, ET1.101)
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) for sub-station 1') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo for sub-station 1
Kc.B.1=ET.B.1/ETo
plot(data.101$Date.101, Kc.B.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')

# Sub-station 101

# ET from soil depth 5
ET5.101=c() 
for(i in 2:n) { 
  ET5.101[i]=(sm5[i-1]-sm5[i])/100*sd5}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.101[i]<0) {ET5.101[i]=NA}}         
plot(data.101$Date.101, ET5.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.101[i]<0) {ET6.101[i]=NA}}         
plot(data.101$Date.101, ET6.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.101[i]<0) {ET7.101[i]=NA} }         
plot(data.101$Date.101, ET7.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# ET from soil depth 8
ET8.101=c()
for(i in 2:n) {
  ET8.101[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.101[i]<0) {ET8.101[i]=NA} }         
plot(data.101$Date.101, ET8.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 8', xlab='', ylab='ET (mm)')

# Total ET for sub-station 101
ET5.101[is.na(ET5.101)]=0 # Turn NAs into zeroes for the sum
ET6.101[is.na(ET6.101)]=0  
ET7.101[is.na(ET7.101)]=0  
ET8.101[is.na(ET8.101)]=0 

ET.B.101=ET5.101+ET6.101+ET7.101+ET8.101
ET.B.101[ET.B.101==0]=NA # Turn zeroes into NAs

plot(data.101$Date.101, ET.B.101,
     ylim=c(0, 12), type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sub-station 101', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
ET5.101[ET1.101==0]=NA # Turn zeroes into NAs
ET6.101[ET2.101==0]=NA
ET7.101[ET3.101==0]=NA
ET8.101[ET4.101==0]=NA
x=data.101$Date.101
y=paste('sensor', 8:5)
ET=c(ET8.101, ET7.101, ET6.101, ET5.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) for sub-station 101') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo for sensor 101
Kc.B.101=ET.B.101/ETo
plot(data.101$Date.101, Kc.B.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')


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

plot(data.101$Date.101, ET1.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c()
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.101[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET2.101[i]<0) {ET2.101[i]=NA}}    
ET2.101[ET2.101==0]=NA # Turns zeros into NAs

plot(data.101$Date.101, ET2.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.101[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET3.101[i]<0) {ET3.101[i]=NA} }   
ET3.101[ET3.101==0]=NA # Turns zeros into NAs

plot(data.101$Date.101, ET3.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.101[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET4.101[i]<0) {ET4.101[i]=NA} }         
ET4.101[ET4.101==0]=NA # Turns zeros into NAs

plot(data.101$Date.101, ET4.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# ET from soil depth 5
ET5.101=c()
for(i in 2:n) { 
  if (RD[i] < sd5) {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  else {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*sd5}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET5.101[i]<0) {ET5.101[i]=NA}} 

plot(data.101$Date.101, ET5.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.101[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET6.101[i]<0) {ET6.101[i]=NA}}  
ET6.101[ET6.101==0]=NA

plot(data.101$Date.101, ET6.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.101[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
# Eliminate negative ET contributions
for(i in 2:n) { 
  if (ET7.101[i]<0) {ET7.101[i]=NA} }         
ET7.101[ET7.101==0]=NA

plot(data.101$Date.101, ET7.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# ET from soil depth 8
ET8.101=c()
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.101[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
# Eliminate negative ET contributions
for(i in 2:n) { 
  if (ET8.101[i]<0) {ET8.101[i]=NA} }         
ET8.101[ET8.101==0]=NA

plot(data.101$Date.101, ET8.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 8', xlab='', ylab='ET (mm)')

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
     main='Daily ET for sub-station 1', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 4:1)
ET=c(ET4.101, ET3.101, ET2.101, ET1.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo for sub-station 1
Kc.C.1=ET.C.1/ETo
plot(data.101$Date.101, Kc.C.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')

# Total ET for sub-station 101
ET5.101[is.na(ET5101)]=0 # Turn NAs into zeroes for the sum
ET6.101[is.na(ET6.101)]=0  
ET7.101[is.na(ET7.101)]=0  
ET8.101[is.na(ET8.101)]=0 

ET.C.101=ET5.101+ET6.101+ET7.101+ET8.101
ET.C.1[ET.C.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date.101, ET.C.101,
     ylim=c(0, 12),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sub-station 101', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 8:5)
ET=c(ET8.101, ET7.101, ET6.101, ET5.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo for sensor 101
Kc.C.101=ET.C.101/ETo
plot(data.101$Date.101, Kc.C.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

w=5 # window used to calculate the derivatives in days

# Sub-station 1
# Total water content (mm) of the soil profile
wc.1=(sm1*(sd1)+sm2*(sd2-sd1)+sm3*(sd3-sd2)+sm4*(sd4-sd3))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sub-station 1', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=wc.1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='darkslategray3',
     main='ET from sub-station 1', xlab='', ylab='ET (mm)',
     ylim=c(0,15))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D.1=sensor1$ET

# ET/ETo for sub-station 1
Kc.D.1=ET.D.1/ETo
plot(data.101$Date.101, Kc.D.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')

# Sub-station 101
# total water content (mm) of the soil profile
wc.101=(sm5*(sd5)+sm6*(sd6-sd5)+sm7*(sd7-sd6)+sm8*(sd8-sd7))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.101[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)), 
     main='SM derivatives for sub-station 101', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor101=data.frame(Date=data.101$Date.101, SM=wc.101, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor101$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor101$f1[i]<0 & sensor101$f2[i]>0) {sensor101$ET[i]=abs(sensor101$f1[i])} 
}
plot(sensor101$Date, sensor101$ET,
     type='h', lwd=3, col='darkslategray3',
     main='ET from sub-station 101', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D.101=sensor101$ET

# ET/ETo for sub-station 101
Kc.D.101=ET.D.101/ETo
plot(data.101$Date.101, Kc.D.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')

# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

w=5 # window used to calculate the derivatives in days

# Sub station 1

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 1', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=2,
     main='ET - soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2-sd1)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor2$f1[i]<0 & sensor2$f2[i]>0) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

plot(sensor2$Date, sensor2$ET,
     type='h', lwd=2,
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3-sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 3', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor3$f1[i]<0 & sensor3$f2[i]>0) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

plot(sensor3$Date, sensor3$ET,
     type='h', lwd=2,
     main='ET - soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4-sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 4', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor4$f1[i]<0 & sensor4$f2[i]>0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

plot(sensor4$Date, sensor4$ET,
     type='h', lwd=2,
     main='ET - soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 5
SM5=sm5*sd5/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 5', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor5=data.frame(Date=data.101$Date.101, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor5$f1[i]<0 & sensor5$f2[i]>0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}
plot(sensor5$Date, sensor5$ET,
     type='h', lwd=2,
     main='ET - soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6-sd5)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 6', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor6=data.frame(Date=data.101$Date.101, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor6$f1[i]<0 & sensor6$f2[i]>0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

plot(sensor6$Date, sensor6$ET,
     type='h', lwd=2,
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7-sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor7=data.frame(Date=data.101$Date.101, SM=SM7, f1, f2) # data frame with the results for sensor7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor7$f1[i]<0 & sensor7$f2[i]>0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

plot(sensor7$Date, sensor7$ET,
     type='h', lwd=2,
     main='ET - soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 8
SM8=sm8*(sd8-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM8[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor8=data.frame(Date=data.101$Date.101, SM=SM8, f1, f2) # data frame with the results for sensor8

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor8$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor8$f1[i]<0 & sensor8$f2[i]>0) {sensor8$ET[i]=abs(sensor8$f1[i])} 
}

plot(sensor8$Date, sensor8$ET,
     type='h', lwd=2,
     main='ET - soil depth 8', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

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
     main='Daily ET for sub-station 1', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 4:1)
ET=c(sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo for sub-station 1
Kc.E.1=ET.E.1/ETo
plot(data.101$Date.101, Kc.E.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')

# Sub-station 101

# Total ET from the entire soil profile
sensor5$ET[is.na(sensor5$ET)]=0 # turn NA values with zero
sensor6$ET[is.na(sensor6$ET)]=0 # turn NA values with zero
sensor7$ET[is.na(sensor7$ET)]=0 # turn NA values with zero
sensor8$ET[is.na(sensor8$ET)]=0 # turn NA values with zero
ET.E.101=sensor5$ET+
  sensor6$ET+
  sensor7$ET+
  sensor8$ET
ET.E.101[ET.E.101==0]=NA # turn zeros into NA
plot(data.101$Date.101, ET.E.101, 
     type='h', lwd=2, col='darkslategray3',
     main='Daily ET for sub-station 101', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 8:5)
ET=c(sensor8$ET, sensor7$ET, sensor6$ET, sensor5$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo ~ crop coefficient
Kc.E.101=ET.E.101/ETo
plot(data.101$Date.101, Kc.E.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')

# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

w=5 # window used to calculate the derivatives in days

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 1', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=2,
     main='ET - soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2-sd1)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor2$f1[i]<0 & sensor2$f2[i]>0) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd1) 
  {sensor2$ET[i]=NA}
}

plot(sensor2$Date, sensor2$ET,
     type='h', lwd=2,
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3-sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 3', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor3$f1[i]<0 & sensor3$f2[i]>0) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd2) 
  {sensor3$ET[i]=NA}
}

plot(sensor3$Date, sensor3$ET,
     type='h', lwd=2,
     main='ET - soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4-sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 4', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor4$f1[i]<0 & sensor4$f2[i]>0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd3) 
  {sensor4$ET[i]=NA}
}

plot(sensor4$Date, sensor4$ET,
     type='h', lwd=2,
     main='ET - soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 5
SM5=sm5*sd5/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 5', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor5=data.frame(Date=data.101$Date.101, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor5$f1[i]<0 & sensor5$f2[i]>0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}
plot(sensor5$Date, sensor5$ET,
     type='h', lwd=2,
     main='ET - soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6-sd5)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 6', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor6=data.frame(Date=data.101$Date.101, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor6$f1[i]<0 & sensor6$f2[i]>0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd5) 
  {sensor6$ET[i]=NA}
}

plot(sensor6$Date, sensor6$ET,
     type='h', lwd=2,
     main='ET from - soil depth 6', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7-sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 7', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor7=data.frame(Date=data.101$Date.101, SM=SM7, f1, f2) # data frame with the results for sensor7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor7$f1[i]<0 & sensor7$f2[i]>0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd6) 
  {sensor7$ET[i]=NA}
}

plot(sensor7$Date, sensor7$ET,
     type='h', lwd=2,
     main='ET - soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 8
SM8=sm8*(sd8-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM8[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 8', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor8=data.frame(Date=data.101$Date.101, SM=SM8, f1, f2) # data frame with the results for sensor8

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor8$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor8$f1[i]<0 & sensor8$f2[i]>0) {sensor8$ET[i]=abs(sensor8$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr_Corn[i]<sd7) 
  {sensor8$ET[i]=NA}
}

plot(sensor8$Date, sensor8$ET,
     type='h', lwd=2,
     main='ET - soil depth 8', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

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
     main='Daily ET from entire soil profile', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 4:1)
ET=c(sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo ~ crop coeficcient
Kc.F.1=ET.F.1/ETo
plot(data.101$Date.101, Kc.F.1,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 1', xlab='', ylab='Kc')

# Sub-station 101

# Total ET from the entire soil profile
sensor5$ET[is.na(sensor5$ET)]=0 # replaces NA values with zero
sensor6$ET[is.na(sensor6$ET)]=0 # replaces NA values with zero
sensor7$ET[is.na(sensor7$ET)]=0 # replaces NA values with zero
sensor8$ET[is.na(sensor8$ET)]=0 # replaces NA values with zero
ET.F.101=sensor5$ET+
  sensor6$ET+
  sensor7$ET+
  sensor8$ET
ET.F.101[ET.F.101==0]=NA # turn zeros into NAs
plot(data.101$Date.101, ET.F.101, 
     type='h', lwd=2, col='darkslategray3',
     main='Daily ET for sub-station 101', xlab='', ylab='ET (mm)',
     ylim=c(0,12))
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', 8:5)
ET=c(sensor8$ET, sensor7$ET, sensor6$ET, sensor5$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# ET/ETo ~ crop coefficient 
Kc.F.101=ET.F.101/ETo
plot(data.101$Date.101, Kc.F.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily ET/ETo for sub-station 101', xlab='', ylab='Kc')


# Comparison of the methods ####

# Sub-station 1
method=rep(c('A','B','C','D','E','F'), each=n)
ET=c(ET.A.1, ET.B.1, ET.C.1, ET.D.1, ET.E.1, ET.F.1) # Evapotranspiration
Kc=c(Kc.A.1, Kc.B.1, Kc.C.1, Kc.D.1, Kc.E.1, Kc.F.1) # ET/ETos
comparison.1=data.frame(date=data.101$Date.101, method, ET, Kc)

# Box-plot for ET
ggplot(comparison.1, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison for sub-station 1") +
  xlab("")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")

# Box-plot for ET/ETo
ggplot(comparison.1, aes(x = method, y = Kc, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET/ETo method comparison for sub-station 1") +
  xlab("") + ylab("ET/ETo") +
  geom_boxplot(color="black", fill="orchid3")

# Sub-station 101
method=rep(c('A','B','C','D','E','F'), each=n)
ET=c(ET.A.101, ET.B.101, ET.C.101, ET.D.101, ET.E.101, ET.F.101) # Evapotranspiration
Kc=c(Kc.A.101, Kc.B.101, Kc.C.101, Kc.D.101, Kc.E.101, Kc.F.101) # ET/ETos
comparison.101=data.frame(date=data.101$Date.101, method, ET, Kc)

# Box-plot ET
ggplot(comparison.101, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison for sub-station 101") +
  xlab("") + ylab("ET (mm)") +
  geom_boxplot(color="black", fill="darkslategray3")


# Box-plot ET/ETo
ggplot(comparison.101, aes(x = method, y = Kc, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET/ETo method comparison for substation 101") +
  xlab("") + ylab("ET/ETo") +
  geom_boxplot(color="black", fill="orchid3")




# Save the results ####
# Write an excel spreadsheet with the results for ET and ET/ETo (~Kc) for all 6 methods

# sub-station 1
Results.1=data.frame(Date=data.101$Date.101, ET.A.1, Kc.A.1, ET.B.1, Kc.B.1, ET.C.1, Kc.C.1, ET.D.1, Kc.D.1, ET.E.1, Kc.E.1, ET.F.1, Kc.F.1)
write_xlsx(Results.1, path="C:/Users/Oliver/Box/DiviningWater/Wellsville2019/Soil sensor data/Sensor 101/Results.1.xlsx") 

# sub-station 101
Results.101=data.frame(Date=data.101$Date.101, ET.A.101, Kc.A.101, ET.B.101, Kc.B.101, ET.C.101, Kc.C.101, ET.D.101, Kc.D.101, ET.E.101, Kc.E.101, ET.F.101, Kc.F.101)
write_xlsx(Results.101, path="C:/Users/Oliver/Box/DiviningWater/Wellsville2019/Soil sensor data/Sensor 101/Results.101.xlsx")
