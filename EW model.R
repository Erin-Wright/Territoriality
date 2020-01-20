#open packages#
library(ape)
library(phytools)
library (geiger)
#read in data after setting WD#
Passerida<-read.csv("Passerida_BETA.csv")
#using read.tree seems to be able to read the phylogeny in when it is in the parentheses format like this phylogeny#
PTTREE<-read.tree("passerida.txt")
#set species names to match the tree, using treenames#
speciesNames<-Passerida[,5]
speciesNames
#try trimming data?#
Passerida.trimmed.min<-subset(Passerida,speciesNames%in%PTTREE$tip.label & Territory.Min%in%c("1","2","3","4","5"))
territoriality.min<-Passerida.trimmed.min$Territory.Min
names(territoriality.min)<-Passerida.trimmed.min$Treename
territoriality.min
#smaller font size more pleasing - is there a way to get rid of the labels?#
PTTREE.trimmed.min<-drop.tip(PTTREE,PTTREE$tip.label[which(!PTTREE$tip.label%in%Passerida.trimmed.min$Treename)])
plotTree(PTTREE.trimmed.min,type="fan",lwd=1,fsize=0.2)
#now to run the model#
fitER.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model="ER")
fitER.territoriality.min
save(fitER.territoriality.min,file="fitER.territoriality.min.RData")
load(fitER.territoriality.min)
plot(fitER.territoriality.min,show.zeros=FALSE, signif=5)
#now for the ARD model#
fitARD.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model="ARD")
fitARD.territoriality.min
save(fitARD.territoriality.min,file="fitARD.territoriality.min.RData")
plot(fitARD.territoriality.min,show.zeros=FALSE,signif=5)
#now to try an ordered/irreversible model where you cannot go from none to strong or vice versa#
model1.min<-matrix(c(0,1,2,3,0,4,0,5,6,7,8,9,0,10,11,12,13,14,0,15,0,16,17,18,0),5,5)
colnames(model1.min)<-rownames(model1.min)<-c("1","2","3","4","5")
model1.min
fit1.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model1.min)
fit1.territoriality.min
save(fit1.territoriality.min,file="fit1.territoriality.min.RData")
plot(fit1.territoriality.min,show.zeros=FALSE, signif=5)
#model2#
model2.min<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,5,0,0,0,6,0,7,0,0,0,8,0),5,5)
colnames(model2.min)<-rownames(model2.min)<-c("1","2","3","4","5")
model2.min
fit2.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model2.min)
fit2.territoriality.min
save(fit2.territoriality.min,file="fit2.territoriality.min.RData")
plot(fit2.territoriality.min,show.zeros=FALSE,signif=5)
#model3#
model3.min<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,0,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model3.min)<-rownames(model3.min)<-c("1","2","3","4","5")
model3.min
fit3.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model3.min)
fit3.territoriality.min
save(fit3.territoriality.min,file="fit3.territoriality.min.RData")
plot(fit3.territoriality.min,show.zeros=FALSE,signif=5)
#model4#
model4.min<-matrix(c(0,0,0,0,0,1,0,2,0,0,0,3,0,4,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model4.min)<-rownames(model4.min)<-c("1","2","3","4","5")
model4.min
fit4.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model4.min)
fit4.territoriality.min
save(fit4.territoriality.min,file="fit4.territoriality.min.RData")
plot(fit4.territoriality.min,show.zeros=FALSE,signif=5)
#model5#
model5.min<-matrix(c(0,1,2,0,0,3,0,4,0,0,5,6,0,0,0,0,0,7,0,8,0,0,0,9,0),5,5)
colnames(model5.min)<-rownames(model5.min)<-c("1","2","3","4","5")
model5.min
fit5.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model5.min)
fit5.territoriality.min
save(fit5.territoriality.min,file="fit5.territoriality.min.RData")
plot(fit5.territoriality.min,show.zeros=FALSE,signif=5)
#model6#
model6.min<-matrix(c(0,0,0,0,0,1,0,2,3,4,0,5,0,6,7,0,8,9,0,10,0,11,12,13,0),5,5)
colnames(model6.min)<-rownames(model6.min)<-c("1","2","3","4","5")
model6.min
fit6.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model6.min)
fit6.territoriality.min
save(fit6.territoriality.min,file="fit6.territoriality.min.RData")
plot(fit6.territoriality.min,show.zeros=FALSE,signif=5)
#model7#
model7.min<-matrix(c(0,0,0,0,0,1,0,2,0,0,0,3,0,0,0,0,0,4,0,5,0,0,0,6,0),5,5)
colnames(model7.min)<-rownames(model7.min)<-c("1","2","3","4","5")
model7.min
fit7.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model7.min)
fit7.territoriality.min
save(fit7.territoriality.min,file="fit7.territoriality.min.RData")
plot(fit7.territoriality.min,show.zeros=FALSE,signif=5)
#model8#
model8.min<-matrix(c(0,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,0,3,0,0,0,0,0,4,0),5,5)
colnames(model8.min)<-rownames(model8.min)<-c("1","2","3","4","5")
model8.min
fit8.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model8.min)
fit8.territoriality.min
save(fit8.territoriality.min,file="fit8.territoriality.min.RData")
plot(fit8.territoriality.min,show.zeros=FALSE,signif=5)
#model9#
model9.min<-matrix(c(0,1,0,0,0,2,0,0,0,0,0,3,0,4,5,0,0,6,0,7,0,0,8,9,0),5,5)
colnames(model9.min)<-rownames(model9.min)<-c("1","2","3","4","5")
model9.min
fit9.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model9.min)
fit9.territoriality.min
save(fit9.territoriality.min,file="fit9.territoriality.min.RData")
plot(fit9.territoriality.min,show.zeros=FALSE,signif=5)
#model10#
model10.min<-matrix(c(0,1,0,0,0,2,0,0,0,0,0,3,0,4,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model10.min)<-rownames(model10.min)<-c("1","2","3","4","5")
model10.min
fit10.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model10.min)
fit10.territoriality.min
save(fit10.territoriality.min,file="fit10.territoriality.min.RData")
plot(fit10.territoriality.min,show.zeros=FALSE,signif=5)
#model11#
model11.min<-matrix(c(0,1,2,3,0,4,0,5,6,0,7,8,0,9,0,10,11,12,0,0,0,0,0,13,0),5,5)
colnames(model11.min)<-rownames(model11.min)<-c("1","2","3","4","5")
model11.min
fit11.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model11.min)
fit11.territoriality.min
save(fit11.territoriality.min,file="fit11.territoriality.min.RData")
plot(fit11.territoriality.min,show.zeros=FALSE,signif=5)
#model12#
model12.min<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,5,0,0,0,6,0,0,0,0,0,7,0),5,5)
colnames(model12.min)<-rownames(model12.min)<-c("1","2","3","4","5")
model12.min
fit12.territoriality.min<-fitDiscrete(PTTREE.trimmed.min,territoriality.min,model=model12.min)
fit12.territoriality.min
save(fit12.territoriality.min,file="fit12.territoriality.min.RData")
plot(fit12.territoriality.min,show.zeros=FALSE,signif=5)
#now to compare the likelihoods#
aicc.min<-setNames(
  c(fitER.territoriality.min$opt$aicc,fit1.territoriality.min$opt$aicc,fitARD.territoriality.min$opt$aicc,fit2.territoriality.min$opt$aicc,fit3.territoriality.min$opt$aicc,fit4.territoriality.min$opt$aicc,fit5.territoriality.min$opt$aicc,fit6.territoriality.min$opt$aicc,fit7.territoriality.min$opt$aicc,fit8.territoriality.min$opt$aicc,fit9.territoriality.min$opt$aicc,fit10.territoriality.min$opt$aicc,fit11.territoriality.min$opt$aicc,fit12.territoriality.min$opt$aicc),
  c("fitER.territoriality.min","fit1.territoriality.min","fitARD.territoriality.min","fit2.territoriality.min","fit3.territoriality.min","fit4.territoriality.min","fit5.territoriality.min","fit6.territoriality.min","fit7.territoriality.min","fit8.territoriality.min","fit9.territoriality.min","fit10.territoriality.min","fit11.territoriality.min","fit12.territoriality.min"))
aic.w(aicc.min)
save(aicc.min,file="aicc.min.RData")
#now to repeat this with maximum classifications#
Passerida.trimmed.max<-subset(Passerida,speciesNames%in%PTTREE$tip.label & Territory.Max%in%c("1","2","3","4","5"))
territoriality.max<-Passerida.trimmed.max$Territory.Max
names(territoriality.max)<-Passerida.trimmed.max$Treename
territoriality.max
#smaller font size more pleasing - is there a way to get rid of the labels?#
PTTREE.trimmed.max<-drop.tip(PTTREE,PTTREE$tip.label[which(!PTTREE$tip.label%in%Passerida.trimmed.max$Treename)])
plotTree(PTTREE.trimmed.max,type="fan",lwd=1,fsize=0.2)
#now to run the model#
fitER.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model="ER")
fitER.territoriality.max
save(fitER.territoriality.max,file="fitER.territoriality.max.RData")
plot(fitER.territoriality.max,show.zeros=FALSE,signif=5)
#now for the ARD model#
fitARD.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model="ARD")
fitARD.territoriality.max
save(fitARD.territoriality.max,file="fitARD.territoriality.max.RData")
plot(fitARD.territoriality.max,show.zeros=FALSE, signif=5)
#now to try custom models#
model1.max<-matrix(c(0,1,2,3,0,4,0,5,6,7,8,9,0,10,11,12,13,14,0,15,0,16,17,18,0),5,5)
colnames(model1.max)<-rownames(model1.max)<-c("1","2","3","4","5")
model1.max
fit1.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model1.max)
fit1.territoriality.max
save(fit1.territoriality.max,file="fit1.territoriality.max.RData")
plot(fit1.territoriality.max,show.zeros=FALSE,signif=5)
#model2#
model2.max<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,5,0,0,0,6,0,7,0,0,0,8,0),5,5)
colnames(model2.max)<-rownames(model2.max)<-c("1","2","3","4","5")
model2.max
fit2.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model2.max)
fit2.territoriality.max
save(fit2.territoriality.max,file="fit2.territoriality.max.RData")
plot(fit2.territoriality.max,show.zeros=FALSE,signif=5)
#model3#
model3.max<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,0,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model3.max)<-rownames(model3.max)<-c("1","2","3","4","5")
model3.max
fit3.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model3.max)
fit3.territoriality.max
save(fit3.territoriality.max,file="fit3.territoriality.max.RData")
plot(fit3.territoriality.max,show.zeros=FALSE,signif=5)
#model4#
model4.max<-matrix(c(0,0,0,0,0,1,0,2,0,0,0,3,0,4,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model4.max)<-rownames(model4.max)<-c("1","2","3","4","5")
model4.max
fit4.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model4.max)
fit4.territoriality.max
save(fit4.territoriality.max,file="fit4.territoriality.max.RData")
plot(fit4.territoriality.max,show.zeros=FALSE,signif=5)
#model5#
model5.max<-matrix(c(0,1,2,0,0,3,0,4,0,0,5,6,0,0,0,0,0,7,0,8,0,0,0,9,0),5,5)
colnames(model5.max)<-rownames(model5.min)<-c("1","2","3","4","5")
model5.max
fit5.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model5.max)
fit5.territoriality.max
save(fit5.territoriality.max,file="fit5.territoriality.max.RData")
plot(fit5.territoriality.max,show.zeros=FALSE,signif=5)
#model6#
model6.max<-matrix(c(0,0,0,0,0,1,0,2,3,4,0,5,0,6,7,0,8,9,0,10,0,11,12,13,0),5,5)
colnames(model6.max)<-rownames(model6.max)<-c("1","2","3","4","5")
model6.max
fit6.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model6.max)
fit6.territoriality.max
save(fit6.territoriality.max,file="fit6.territoriality.max.RData")
plot(fit6.territoriality.max,show.zeros=FALSE,signif=5)
#model7#
model7.max<-matrix(c(0,0,0,0,0,1,0,2,0,0,0,3,0,0,0,0,0,4,0,5,0,0,0,6,0),5,5)
colnames(model7.max)<-rownames(model7.max)<-c("1","2","3","4","5")
model7.max
fit7.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model7.max)
fit7.territoriality.max
save(fit7.territoriality.max,file="fit7.territoriality.max.RData")
plot(fit7.territoriality.max,show.zeros=FALSE,signif=5)
#model8#
model8.max<-matrix(c(0,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,0,3,0,0,0,0,0,4,0),5,5)
colnames(model8.max)<-rownames(model8.max)<-c("1","2","3","4","5")
model8.max
fit8.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model8.max)
fit8.territoriality.max
save(fit8.territoriality.max,file="fit8.territoriality.max.RData")
plot(fit8.territoriality.max,show.zeros=FALSE,signif=5)
#model9#
model9.max<-matrix(c(0,1,0,0,0,2,0,0,0,0,0,3,0,4,5,0,0,6,0,7,0,0,8,9,0),5,5)
colnames(model9.max)<-rownames(model9.max)<-c("1","2","3","4","5")
model9.max
fit9.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model9.max)
fit9.territoriality.max
save(fit9.territoriality.max,file="fit9.territoriality.max.RData")
plot(fit9.territoriality.max,show.zeros=FALSE,signif=5)
#model10#
model10.max<-matrix(c(0,1,0,0,0,2,0,0,0,0,0,3,0,4,0,0,0,5,0,6,0,0,0,7,0),5,5)
colnames(model10.max)<-rownames(model10.max)<-c("1","2","3","4","5")
model10.max
fit10.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model10.max)
fit10.territoriality.max
save(fit10.territoriality.max,file="fit10.territoriality.max.RData")
plot(fit10.territoriality.max,show.zeros=FALSE,signif=5)
#model11#
model11.max<-matrix(c(0,1,2,3,0,4,0,5,6,0,7,8,0,9,0,10,11,12,0,0,0,0,0,13,0),5,5)
colnames(model11.max)<-rownames(model11.max)<-c("1","2","3","4","5")
model11.max
fit11.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model11.max)
fit11.territoriality.max
save(fit11.territoriality.max,file="fit11.territoriality.max.RData")
plot(fit11.territoriality.max,show.zeros=FALSE,signif=5)
#model12#
model12.max<-matrix(c(0,1,0,0,0,2,0,3,0,0,0,4,0,5,0,0,0,6,0,0,0,0,0,7,0),5,5)
colnames(model12.max)<-rownames(model12.max)<-c("1","2","3","4","5")
model12.max
fit12.territoriality.max<-fitDiscrete(PTTREE.trimmed.max,territoriality.max,model=model12.max)
fit12.territoriality.max
save(fit12.territoriality.max,file="fit12.territoriality.max.RData")
plot(fit12.territoriality.max,show.zeros=FALSE,signif=5)
#now to compare the likelihoods#
aicc.max<-setNames(
  c(fitER.territoriality.max$opt$aicc,fit1.territoriality.max$opt$aicc,fitARD.territoriality.max$opt$aicc,fit2.territoriality.max$opt$aicc,fit3.territoriality.max$opt$aicc,fit4.territoriality.max$opt$aicc,fit5.territoriality.max$opt$aicc,fit6.territoriality.max$opt$aicc,fit7.territoriality.max$opt$aicc,fit8.territoriality.max$opt$aicc,fit9.territoriality.max$opt$aicc,fit10.territoriality.max$opt$aicc,fit11.territoriality.max$opt$aicc,fit12.territoriality.max$opt$aicc),
  c("fitER.territoriality.max","fit1.territoriality.max","fitARD.territoriality.max","fit2.territoriality.max","fit3.territoriality.max","fit4.territoriality.max","fit5.territoriality.max","fit6.territoriality.max","fit7.territoriality.max","fit8.territoriality.max","fit9.territoriality.max","fit10.territoriality.max","fit11.territoriality.max","fit12.territoriality.max"))
aic.w(aicc.max)
save(aicc.max,file="aicc.max.RData")
#confidence is different in MAX vs MIN#
#fit table here#
models<-list(c(fitER.territoriality.min, fitARD.territoriality.min, fit1.territoriality.min, fit2.territoriality.min, fit3.territoriality.min, fit4.territoriality.min, fit5.territoriality.min, fit6.territoriality.min, fit7.territoriality.min, fit8.territoriality.min, fit9.territoriality.min, fit10.territoriality.min, fit11.territoriality.min, fit12.territoriality.min, fitER.territoriality.max, fitARD.territoriality.max, fit1.territoriality.max, fit2.territoriality.max, fit3.territoriality.max, fit4.territoriality.max, fit5.territoriality.max, fit6.territoriality.max, fit7.territoriality.max, fit8.territoriality.max, fit9.territoriality.max, fit10.territoriality.max, fit11.territoriality.max, fit12.territoriality.max))
ML<-list(AIC(fitER.territoriality.min, fitARD.territoriality.min, fit1.territoriality.min, fit2.territoriality.min, fit3.territoriality.min, fit4.territoriality.min, fit5.territoriality.min, fit6.territoriality.min, fit7.territoriality.min, fit8.territoriality.min, fit9.territoriality.min, fit10.territoriality.min, fit11.territoriality.min, fit12.territoriality.min, fitER.territoriality.max, fitARD.territoriality.max, fit1.territoriality.max, fit2.territoriality.max, fit3.territoriality.max, fit4.territoriality.max, fit5.territoriality.max, fit6.territoriality.max, fit7.territoriality.max, fit8.territoriality.max, fit9.territoriality.max, fit10.territoriality.max, fit11.territoriality.max, fit12.territoriality.max))
AICC

table<-data.frame(models,ML,AICCw,Q1)
#now to try and estimate ancestral character states#
levels(territoriality.min)<-c("none","strong","weak")
#can visualise territorial behaviour on the tips of the tree#
ER.simmap.min<-make.simmap(PTTREE.trimmed.min,territoriality.min,model="ER",nsim=100)
ER.simmap.min
plotSimmap(ER.simmap.min[[1]])
ER.summary.min<-summary(ER.simmap.min,plot=FALSE)
plot(ER.summary.min,fsize=0.2,ftype="i")
#now with max values#
ER.simmap.max<-make.simmap(PTTREE.trimmed.max,territoriality.max,model="ER",nsim=100)
ER.simmap.max
plotSimmap(ER.simmap.max[[1]])
ER.summary.max<-summary(ER.simmap.max,plot=FALSE)
plot(ER.summary.max,fsize=0.2,ftype="i")
