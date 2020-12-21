# CMEE 2020 HPC excercises R code challenge G proforma

rm(list=ls()) # nothing written elsewhere should be needed to make this work

# please edit these data to show your information.
name <- "Tsz So Acacia Tang"
preferred_name <- "Acacia"
email <- "tst116@ic.ac.uk"
username <- "tst116"

# don't worry about comments for this challenge - the number of characters used will be counted starting from here
f=function(p=4,q=0,D=pi/2,l=1,d=1){
m=c(0,8)
if(l>0.01){x=p+l*cos(D)
y=q+l*sin(D)
plot(c(p,x),c(q,y),xlim=m,ylim=m,type="l")
par(new=T)
f(x,y,D+pi/4*d,l*0.38,-d)
f(x,y,D,l*0.87,-d)}}