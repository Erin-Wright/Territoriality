#open packages#
library(ape)
library(phytools)
library (geiger)
#read in data after setting WD#
PTDATA<-read.csv("pigot_tobias_2019.csv")
head(PTDATA)
#read in tree#
PTTREE<-read.tree("passerida.txt")
#set species names to match the tree#
speciesNames<-PTDATA[,1]
speciesNames
#trim the data of no-entries#
PTDATA.trimmed<-subset(PTDATA,Species%in%PTTREE$tip.label & Territoriality%in%c("none","weak","strong"))
territoriality<-PTDATA.trimmed$Territoriality
names(territoriality)<-PTDATA.trimmed$Species
#trim tree#
PTTREE.trimmed<-drop.tip(PTTREE,PTTREE$tip.label[which(!PTTREE$tip.label%in%PTDATA.trimmed$Species)])
plotTree(PTTREE.trimmed,type="fan",lwd=1,fsize=0.2)
#set the discrete trait of territoriality - done above with the trimmed data which also sets levels#
#now to run the model#
fitER.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model="ER")
fitER.territoriality.PT
save(fitER.territoriality.PT,file="fitER.territoriality.PT.RData")
plot(fitER.territoriality.PT, signif=5)
#it doesn't seem to not be bifurcating, seems like it could be to do with the labels not matching up due to an extra underscore#
#now to run the model#
fitER.territoriality<-fitDiscrete(PTTREE.trimmed,territoriality,model="ER")
fitER.territoriality
plot(fitER.territoriality, signif=5)
#but this is giving an extra level other than none, weak, strong for some reason - probably because blanks in the data#
#remove blank entries from the dataset and it works#
#now for the ARD model#
fitARD.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model="ARD")
fitARD.territoriality.PT
save(fitARD.territoriality.PT,file="fitARD.territoriality.PT.RData")
plot(fitARD.territoriality.PT, signif=5)
#model1#
model1.PT<-matrix(c(0,0,1,0,0,2,3,4,0),3,3)
colnames(model1.PT)<-rownames(model1.PT)<-c("none","strong","weak")
model1.PT
fit1.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model1.PT)
fitordered.territoriality.PT
save(fit1.territoriality.PT, file="fit1.territoriality.PT.RData")
plot(fit1.territoriality.PT, signif=5)
#model2#
model2.PT<-matrix(c(0,0,0,0,0,1,2,0,0),3,3)
colnames(model2.PT)<-rownames(model2.PT)<-c("none","strong","weak")
model2.PT
fit2.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model2.PT)
fit2.territoriality.PT
save(fit2.territoriality.PT, file="fit2.territoriality.PT.RData")
plot(fit2.territoriality.PT, signif=5)
#model3#
model3.PT<-matrix(c(0,0,0,0,0,1,2,3,0),3,3)
colnames(model3.PT)<-rownames(model3.PT)<-c("none","strong","weak")
model3.PT
fit3.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model3.PT)
fit3.territoriality.PT
save(fit3.territoriality.PT, file="fit3.territoriality.PT.RData")
plot(fit3.territoriality.PT, signif=5)
#model4#
model4.PT<-matrix(c(0,0,1,0,0,2,3,0,0),3,3)
colnames(model4.PT)<-rownames(model4.PT)<-c("none","strong","weak")
model4.PT
fit4.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model4.PT)
fit4.territoriality.PT
save(fit4.territoriality.PT, file="fit4.territoriality.PT.RData")
plot(fit4.territoriality.PT, signif=5)
#model5#
model5.PT<-matrix(c(0,0,1,0,0,2,0,3,0),3,3)
colnames(model5.PT)<-rownames(model5.PT)<-c("none","strong","weak")
model5.PT
fit5.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model5.PT)
fit5.territoriality.PT
save(fit5.territoriality.PT, file="fit5.territoriality.PT.RData")
plot(fit5.territoriality.PT, signif=5)
#model6#
model6.PT<-matrix(c(0,0,0,0,0,0,1,2,0),3,3)
colnames(model6.PT)<-rownames(model6.PT)<-c("none","strong","weak")
model6.PT
fit6.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model6.PT)
fit6.territoriality.PT
save(fit6.territoriality.PT, file="fit6.territoriality.PT.RData")
plot(fit6.territoriality.PT, signif=5)
#model7#
model7.PT<-matrix(c(0,0,1,0,0,0,0,2,0),3,3)
colnames(model7.PT)<-rownames(model7.PT)<-c("none","strong","weak")
model7.PT
fit7.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model7.PT)
fit7.territoriality.PT
save(fit7.territoriality.PT, file="fit7.territoriality.PT.RData")
plot(fit7.territoriality.PT, signif=5)
#model8#
model8.PT<-matrix(c(0,0,1,0,0,0,2,3,0),3,3)
colnames(model8.PT)<-rownames(model8.PT)<-c("none","strong","weak")
model8.PT
fit8.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model8.PT)
fit8.territoriality.PT
save(fit8.territoriality.PT, file="fit8.territoriality.PT.RData")
plot(fit8.territoriality.PT, signif=5)
#model9#
model9.PT<-matrix(c(0,0,1,0,0,2,0,0,0),3,3)
colnames(model9.PT)<-rownames(model9.PT)<-c("none","strong","weak")
model9.PT
fit9.territoriality.PT<-fitDiscrete(PTTREE.trimmed,territoriality,model=model9.PT)
fit9.territoriality.PT
save(fit9.territoriality.PT, file="fit9.territoriality.PT.RData")
plot(fit9.territoriality.PT, signif=5)
#now to compare the likelihoods#
aicc<-setNames(
  c(fitER.territoriality.PT$opt$aicc,fit1.territoriality.PT$opt$aicc,fitARD.territoriality.PT$opt$aicc,fit2.territoriality.PT$opt$aicc,fit3.territoriality.PT$opt$aicc,fit4.territoriality.PT$opt$aicc,fit5.territoriality.PT$opt$aicc,fit6.territoriality.PT$opt$aicc,fit7.territoriality.PT$opt$aicc,fit8.territoriality.PT$opt$aicc,fit9.territoriality.PT$opt$aicc),
  c("fitER.territoriality.PT","fit1.territoriality.PT","fitARD.territoriality.PT","fit2.territoriality.PT","fit3.territoriality.PT","fit4.territoriality.PT","fit5.territoriality.PT","fit6.territoriality.PT","fit7.territoriality.PT","fit8.territoriality.PT","fit9.territoriality.PT"))
aic.w(aicc)
#now to try and estimate ancestral character states#
#can visualise territorial behaviour on the tips of the tree#
plotTree(PTTREE,type="fan",fsize=0.2,ftype="i",lwd=1)
cols<-setNames(c("red","blue","darkgreen"),levels(territoriality))
#next line gives error, subscript out of bounds#
tiplabels(pie=to.matrix(territoriality[PTTREE$tip.label],
                        levels(territoriality)),piecol=cols,cex=0.3)

add.simmap.legend(colors=cols,prompt=FALSE,x=0.9*par()$usr[1],
                  y=0.8*par()$usr[3],fsize=0.8)
#need to fix above but will see if can estimate ancestral states anyway#
fitER.territorialityanc<-ace(territoriality,PTTREE, model="ER", type="discrete")
fitER.territoriality$lik.anc
#returns a result of null#
#Using a simmap instead#
ER.simmap<-make.simmap(PTTREE.trimmed,territoriality,model="ER",nsim=100)
ER.simmap
plotSimmap(ER.simmap[[1]])
#And as a summary of the 100 trees#
ER.summary<-summary(ER.simmap,plot=FALSE)
plot(ER.summary,fsize=0.2,ftype="i")