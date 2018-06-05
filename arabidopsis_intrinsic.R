#=============================================================================
#
#Start exploring the arabidopsis data. 
#Intrinsic growth rates as a function of soil type
#
#=============================================================================
#Load libraries
#=============================================================================
library(mgcv)
#=============================================================================
#Load data
#=============================================================================
intrins1=read.csv("Arabidopsis_23518.csv")
#=============================================================================
#Look at the association between height/siliques per RIL, per soil type
soils = unique(intrins1$tray)
rils = unique(intrins1$RIL)

#ratio of sand in soil
#soil_rats= 

nrils=length(rils)
nsoils= length(soils)

#Convert soil type to sand/soil ratio

ss_rat = matrix(0, dim(intrins1)[1],1)
ss_lookup = matrix( c(sort(soils), c(0,.167,0.33,0.42,0.25,.083)), nsoils, 2) 
ss_rat[] = as.matrix(ss_lookup[,2][match(unlist(intrins1$tray),ss_lookup[,1])])
colnames(ss_rat) = c("sand")
intrins1 = cbind(intrins1, ss_rat)


#Stem height with soil as continuous factor
ril_stems=NULL
for (s in 1:nrils) {
	ril.sub = subset(intrins1, RIL == rils[s])
	ril.gam=gam(height..mm.~s(sand,k=3), data=ril.sub)
	ril_stems[[s]] = ril.gam

}


#Stem height with soil as parametric term
ril_stems_p=NULL
for (s in 1:nrils) {
	ril.sub = subset(intrins1, RIL == rils[s])
	ril.gam_p=gam(height..mm.~sand, data=ril.sub)
	ril_stems_p[[s]] = ril.gam_p

}


#Siliques with soil as continuous factor
ril_sils=NULL
for (s in 1:nrils) {
	ril.sub = subset(intrins1, RIL == rils[s])
	ril.gam=gam(nr.siliques~s(sand,k=3), data=ril.sub)
	ril_sils[[s]] = ril.gam

}


#Siliques with soil as parametric term
ril_sils_p=NULL
for (s in 1:nrils) {
	ril.sub = subset(intrins1, RIL == rils[s])
	ril.gam_p=gam(nr.siliques~sand, data=ril.sub)
	ril_sils_p[[s]] = ril.gam_p

}