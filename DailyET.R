# Estimate daily ET from midnight data for the Wellsville site 2019

# Packages ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)

# Data and variables ####
data=read_xlsx('data.001.xlsx')

# Dates
date=as.Date(data$Date)
n=length(date) # number of days

# Soil moisture data - volumetric water content
sm1=data$s1/100
sm2=data$s2/100
sm3=data$s3/100
sm4=data$s4/100

# Plot volumetric water content
plot (date, sm1,ylim=c(0.2, 0.5), type='l', lwd=3, col='coral3', 
      main='Daily soil moisture', xlab='', ylab='Volumetric water content') 
lines(date, sm2, type='l', lwd=3, col='steelblue3')
lines(date, sm3, type='l', lwd=3, col='gold3')
lines(date, sm4, type='l', lwd=3, col='olivedrab3')
legend('bottom', lty=1, lwd=3, inset=0.02, legend=paste(rep('Sensor',4), 1:4), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=4)
paste(rep('Sensor',4), 1:4)

# Sensor depth (mm)
z1=1.5*304.8 # sensor 1: [0, 1,5] ft
z2=2.5*304.8 # sensor 2: [1.5, 2.5] ft
z3=3.5*304.8 # sensor 3: [2.5, 3.5] ft
z4=4.5*304.8 # sensor 4: [3.5, 4.5] ft

# Root depth (mm)
zr=data$Zr

# Plot root depth with sensor depth
plot(date, -zr, 
     type='h', col='goldenrod', lwd='3', ylim=c(-1400, 0),
     main='Corn root depth - Wellsville 2019', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=3)
abline(h=-z1, col='coral3', lwd=3)
abline(h=-z2, col='steelblue3', lwd=3)
abline(h=-z3, col='gold3', lwd=3)
abline(h=-z4, col='olivedrab3', lwd=3)
legend('bottomleft', inset=0.02, lty=1, lwd=3, legend=paste(rep('Sensor',4), 1:4), col=c('coral3','steelblue3','gold3','olivedrab3'), ncol=1)

# Reference ET (ETr) (mm/day)
ETr=data$ETr

# Soil water content (mm)
wc1=sm1*z1          # soil layer 1
wc2=sm2*(z2-z1)     # soil layer 2
wc3=sm3*(z3-z2)     # soil layer 3
wc4=sm4*(z4-z3)     # soil layer 4
wc=wc1+wc2+wc3+wc4  # total

# window used to calculate the derivatives (days) - Methods D, E, F
w=3 

# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the entire soil profile at once.

# ET estimation
ET.A=c()
for(i in 1:n) { 
  ET.A[i]=wc[i]-wc[i+1]}
for(i in 1:(n-1)) { # eliminate negative ET contributions
  if (ET.A[i]<0) {ET.A[i]=0}}  

ET.A[ET.A==0]=NA

plot(date, ET.A, 
     type='h', lwd=3, col='darkslategray3',
     main='Method A', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# ET from soil depth 1
ET.B.1=c() 
for(i in 1:n) { 
  ET.B.1[i]=(sm1[i]-sm1[i+1])*z1}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.B.1[i]<0) {ET.B.1[i]=0}}         
plot(date, ET.B.1, 
     type='h', lwd=2, ylim=c(0, 12),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET.B.2=c()
for(i in 1:n) { 
  ET.B.2[i]=(sm2[i]-sm2[i+1])*(z2-z1)}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.B.2[i]<0) {ET.B.2[i]=0}}         
plot(date, ET.B.2, 
     type='h', lwd=2, ylim=c(0, 12),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET.B.3=c()
for(i in 1:n) { 
  ET.B.3[i]=(sm3[i]-sm3[i+1])*(z3-z2) }
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.B.3[i]<0) {ET.B.3[i]=0} }         
plot(date, ET.B.3, 
     type='h', lwd=2, ylim=c(0, 12),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET.B.4=c()
for(i in 1:n) { 
  ET.B.4[i]=(sm4[i]-sm4[i+1])*(z4-z3) }
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.B.4[i]<0) {ET.B.4[i]=0} }         
plot(date, ET.B.4, 
     type='h', lwd=2, ylim=c(0, 12),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# Total ET 
ET.B=ET.B.1+ET.B.2+ET.B.3+ET.B.4
ET.B[n]=NA # can't calculate ET the last day
ET.B[ET.B==0]=NA

plot(date, ET.B,
     type='h', col='darkslategray3', lwd=2,
     main='Method B', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
x=date
y=paste('Soil depth', 1:4)
ET=c(ET.B.4, ET.B.3, ET.B.2, ET.B.1)
ET[ET==0]=NA
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method B') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()



# Method C: Water balance on each sensors soil depth accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil layer and then adds them all up after having eliminated all negative contributions. Method C takes root growth throughout the season in account.

# ET from soil depth 1
ET.C.1=c()
for(i in 1:n) { 
  if (zr[i] < z1) {
    ET.C.1[i]=(sm1[i]-sm1[i+1])*zr[i] }
  else {
    ET.C.1[i]=(sm1[i]-sm1[i+1])*z1}}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.C.1[i]<0) {ET.C.1[i]=0}}         

plot(date, ET.C.1, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET.C.2=c()
for(i in 1:n) { 
  if (zr[i] <= z1) {
    ET.C.2[i]=0}
  else if (z1 < zr[i] & zr[i] <= z2) {
    ET.C.2[i]=(sm2[i]-sm2[i+1])*(zr[i]-z1) }
  else {
    ET.C.2[i]=(sm2[i]-sm2[i+1])*(z2-z1) }}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.C.2[i]<0) {ET.C.2[i]=0}}    

plot(date, ET.C.2, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET.C.3=c()
for(i in 1:n) {
  if (zr[i] <= z2) {
    ET.C.3[i]=0}
  else if (z2 < zr[i] & zr[i] <= z3) {
    ET.C.3[i]=(sm3[i]-sm3[i+1])*(zr[i]-z2) }
  else {
    ET.C.3[i]=(sm3[i]-sm3[i+1])*(z3-z2) }}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.C.3[i]<0) {ET.C.3[i]=0} }   

plot(date, ET.C.3, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET.C.4=c()
for(i in 1:n) {
  if (zr[i] <= z3) {
    ET.C.4[i]=0}
  else if (z3 < zr[i] & zr[i] <= z3) {
    ET.C.4[i]=(sm4[i]-sm4[i+1])*(zr[i]-z3) }
  else {
    ET.C.4[i]=(sm4[i]-sm4[i+1])*(z4-z3) }}
for(i in 1:(n-1)) { # eliminate negative ET values
  if (ET.C.4[i]<0) {ET.C.4[i]=0} }   

plot(date, ET.C.4, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# Total ET
ET.C=ET.C.1+ET.C.2+ET.C.3+ET.C.4
ET.C[ET.C==0]=NA

plot(date, ET.C,
     ylim=c(0,12),
     type='h', col='darkslategray3', lwd=3,
     main='Method C', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=3, inset=0.02)

# Heatmap
x=date
y=paste('Soil depth', 1:4)
ET=c(ET.C.4, ET.C.3, ET.C.2, ET.C.1)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method C') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()


# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc[low_val:hig_val]
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
plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

D.df=data.frame(Date=date, SM=wc, f1, f2) # data frame with the results for the D.df side

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
D.df$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (D.df$f1[i]<0 & D.df$f2[i]>0) {D.df$ET[i]=abs(D.df$f1[i])} 
}

plot(D.df$Date, D.df$ET,
     type='h', lwd=3, col='darkslategray3', ylim=c(0, 12),
     main='Method D', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D=D.df$ET


# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

# Soil depth 1

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc1[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 1', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

E.1.df=data.frame(Date=date, SM=wc1, f1, f2) # data frame with the results for E.1.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.1.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.1.df$f1[i]<0 & E.1.df$f2[i]>0) {E.1.df$ET[i]=abs(E.1.df$f1[i])} 
}
E.1.df$ET[is.na(E.1.df$ET)]=0 # replaces NA values with zero
plot(E.1.df$Date, E.1.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# Soil depth 2

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc2[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 2', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

E.2.df=data.frame(Date=date, SM=wc2, f1, f2) # data frame with the results for E.2.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.2.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.2.df$f1[i]<0 & E.2.df$f2[i]>0) {E.2.df$ET[i]=abs(E.2.df$f1[i])} 
}
E.2.df$ET[is.na(E.2.df$ET)]=0 # replaces NA values with zero
plot(E.2.df$Date, E.2.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Soil depth 3

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc3[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 3', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

E.3.df=data.frame(Date=date, SM=wc3, f1, f2) # data frame with the results for E.3.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.3.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.3.df$f1[i]<0 & E.3.df$f2[i]>0) {E.3.df$ET[i]=abs(E.3.df$f1[i])} 
}
E.3.df$ET[is.na(E.3.df$ET)]=0 # replaces NA values with zero
plot(E.3.df$Date, E.3.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Soil depth 4

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc4[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 4', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

E.4.df=data.frame(Date=date, SM=wc4, f1, f2) # data frame with the results for E.4.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.4.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.4.df$f1[i]<0 & E.4.df$f2[i]>0) {E.4.df$ET[i]=abs(E.4.df$f1[i])} 
}
E.4.df$ET[is.na(E.4.df$ET)]=0 # replaces NA values with zero
plot(E.4.df$Date, E.4.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Total ET 
ET.E=E.1.df$ET+
     E.2.df$ET+
     E.3.df$ET+
     E.4.df$ET
ET.E[ET.E==0]=NA

plot(date, ET.E, 
     type='h', lwd=3, col='darkslategray3', ylim=c(0,12),
     main='Method E', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=date
y=paste('Soil depth', 1:4)
ET=c(E.4.df$ET, E.3.df$ET, E.2.df$ET, E.1.df$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method E') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

# soil depth 1

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc1[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 1', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

F.1.df=data.frame(Date=date, wc=wc1, f1, f2) # data frame with the results for F.1.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
F.1.df$ET=c(rep(NA, n)) # create ET column
for (i in 2:(n-1)) {
  if (F.1.df$f1[i]<0 & F.1.df$f2[i]>0) {F.1.df$ET[i]=abs(F.1.df$f1[i])} 
}
F.1.df$ET[is.na(F.1.df$ET)]=0 # replaces NA values with zero
plot(F.1.df$Date, F.1.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# Soil depth 2

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc2[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 2', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

F.2.df=data.frame(Date=date, wc=wc2, f1, f2) # data frame with the results for F.2.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
F.2.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (F.2.df$f1[i]<0 & F.2.df$f2[i]>0) {F.2.df$ET[i]=abs(F.2.df$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<z1) 
  {F.2.df$ET[i]=NA}
}

F.2.df$ET[is.na(F.2.df$ET)]=0 # replaces NA values with zero
plot(F.2.df$Date, F.2.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Soil depth 3

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc3[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 3', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

F.3.df=data.frame(Date=date, wc=wc3, f1, f2) # data frame with the results for F.3.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
F.3.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (F.3.df$f1[i]<0 & F.3.df$f2[i]>0) {F.3.df$ET[i]=abs(F.3.df$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<z2) 
  {F.3.df$ET[i]=NA}
}

F.3.df$ET[is.na(F.3.df$ET)]=0 # replaces NA values with zero
plot(F.3.df$Date, F.3.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))

# Soil depth 4

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc4[low_val:hig_val]
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

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 4', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

F.4.df=data.frame(Date=date, wc=wc4, f1, f2) # data frame with the results for F.4.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
F.4.df$ET=c(rep(NA, n)) # create ET column
for (i in 2:(n-1)) {
  if (F.4.df$f1[i]<0 & F.4.df$f2[i]>0) {F.4.df$ET[i]=abs(F.4.df$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<z3) 
  {F.4.df$ET[i]=NA}
}

F.4.df$ET[is.na(F.4.df$ET)]=0 # replaces NA values with zero
plot(F.4.df$Date, F.4.df$ET,
     type='h', lwd=3,
     main='ET - Soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0,12))

# Total ET for the left side of the onion bed
ET.F=F.1.df$ET+
     F.2.df$ET+
     F.2.df$ET+
     F.4.df$ET
ET.F[ET.F==0]=NA

plot(date, ET.F, 
     type='h', lwd=3, col='darkslategray3',
     main='Method F', xlab='', ylab='ET (mm/day)',
     ylim=c(0, 12))
lines(date, ETr, type='l', col='palegreen3', lwd=3)

legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=date
y=paste('Soil depth', 1:4)
ET=c(F.4.df$ET, F.3.df$ET, F.2.df$ET, F.1.df$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method F') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Comparison of the methods ####
# Preliminary analysis before the gap filling

method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET=c(ET.A, ET.B, ET.C, ET.D, ET.E, ET.F) # Evapotranspiration
comparison=data.frame(date=data$Date, method, ET)

# Box-plot for ET
ggplot(comparison, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")



# Results ####
Results=data.frame(Date=as.Date(data$Date), Irrigation=data$Irrigation, ETr, ET.A, ET.B, ET.C, ET.D, ET.E, ET.F)
Results[is.na(Results)]=0

# Irrigation days: ET=ETr ####
for (i in 1:n) {
  if (Results$Irrigation[i]==1) 
  {Results$ET.A[i]=Results$ETr[i]
  Results$ET.B[i]=Results$ETr[i]
  Results$ET.C[i]=Results$ETr[i]
  Results$ET.D[i]=Results$ETr[i]
  Results$ET.E[i]=Results$ETr[i]
  Results$ET.F[i]=Results$ETr[i]}
}

# Calculate Kc=ET/ETr ####
Kc.A=Results$ET.A/ETr
Kc.B=Results$ET.B/ETr
Kc.C=Results$ET.C/ETr
Kc.D=Results$ET.D/ETr
Kc.E=Results$ET.E/ETr
Kc.F=Results$ET.F/ETr

Results=data.frame(Date=as.Date(data$Date), Irrigation=data$Irrigation, ETr, 
                   ET.A=Results$ET.A, Kc.A, ET.B=Results$ET.B, Kc.B, ET.C=Results$ET.C, Kc.C,
                   ET.D=Results$ET.D, Kc.D, ET.E=Results$ET.E, Kc.E, ET.F=Results$ET.F, Kc.F)

# Eliminate Kc values smaller than 0.2 - FAO 56
for (i in 1:n) {
  if (Results$Kc.A[i] < 0.2) {Results$Kc.A[i]=Results$ET.A[i]=0}
}
for (i in 1:n) { 
  if (Results$Kc.B[i] < 0.2) {Results$Kc.B[i]=Results$ET.B[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.C[i] < 0.2) {Results$Kc.C[i]=Results$ET.C[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.D[i] < 0.2) {Results$Kc.D[i]=Results$ET.D[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.E[i] < 0.2) {Results$Kc.E[i]=Results$ET.E[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.F[i] < 0.2) {Results$Kc.F[i]=Results$ET.F[i]=0}
}

# Eliminate Kc values greater than 1.44 - FAO 56
for (i in 1:n) {
  if (Results$Kc.A[i] > 1.44) {Results$Kc.A[i]=Results$ET.A[i]=0}
}
for (i in 1:n) { 
  if (Results$Kc.B[i] > 1.44) {Results$Kc.B[i]=Results$ET.B[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.C[i] > 1.44) {Results$Kc.C[i]=Results$ET.C[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.D[i] > 1.44) {Results$Kc.D[i]=Results$ET.D[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.E[i] > 1.44) {Results$Kc.E[i]=Results$ET.E[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.F[i] > 1.44) {Results$Kc.F[i]=Results$ET.F[i]=0}
}

# Save in datasheet ####
Results[4:15][Results[4:15]==0]=NA
write_xlsx(Results, path="Results.001.xlsx") 

