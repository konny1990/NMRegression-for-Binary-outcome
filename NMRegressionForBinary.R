#*********************************************************************************
#             Load the libraries needed             
#*********************************************************************************
#####install.packages
install.packages("meta")
install.packages("metafor")
install.packages("netmeta")
install.packages("readxl")
install.packages("R2jags")
library(meta)
library(metafor)
library(netmeta)
library(readxl)
install.packages("devtools")
library(devtools)
install_github("esm-ispm-unibe-ch/NMAJags",force=TRUE)
library(NMAJags)
library(R2jags)
install.packages("parallel")
library(parallel)

###load data and models###
GRISELDA<-read_excel("Depression.xlsx")

source("modelNMRBinary.R")
###Data in a proper form
NMRdataBinary=make.jagsNMA.data(studyid=studyID,t=drug_name,r=Responders,n=Ntotal,data=GRISELDA,othervar=study_year-mean(GRISELDA$study_year,na.rm=TRUE),type="binary",reference = "Fluoxetine")
mean(GRISELDA$study_year,na.rm=TRUE)

#Network Metaregression for Griselta data
##!!!!!!!
# because of the thinning you have very few samples left, increase the n.iter and the burn in and use also parallele jags to increase speed
####### I changed it, but I think that is still very slow. Is there something wrong or is it normal?
NMRinJAGSB<- jags.parallel(data = NMRdataBinary, inits= NULL,
                           parameters.to.save = c("OR","ORref","tau", "d", "b"), n.chains = 2, n.iter = 50000,
                           n.burnin = 10000,DIC=F,n.thin=10,
                           model.file = modelNMRBinary)
print(c(NMRinJAGSB$BUGSoutput$mean$b,NMRinJAGSB$BUGSoutput$mean$ORref))



traceplot(NMRinJAGSB, varname="tau")          ##check for convergence
traceplot(NMRinJAGSB, varname="ORref")
traceplot(NMRinJAGSB, varname="b")

##forest plot

y=log(NMRinJAGSB$BUGSoutput$mean$ORref)
k=metagen(y,NMRinJAGSB$BUGSoutput$sd$ORref, sm="OR")
summary(k)
forest(k)

