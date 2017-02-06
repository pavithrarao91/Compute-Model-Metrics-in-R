#clear the memory of all objects 
rm(list = ls())

#This function calculates the numeric model metrics like MAD, MSE, MAPE R2.
#a = vector of actual values
#m = vector of predicted values
modelmetrics=function(a,m)
{
  metrics=c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,P90=0)
  metrics["MAD"]=mean(abs(a-m))
  metrics["MSE"]=mean((a-m)^2)
  metrics["MAPE"]=mean(abs((a-m)/a))
  metrics["MPSE"]= mean(((a-m)/a)^2)
  SST= sum(a-mean(a))^2
  SSE=sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  metrics["TMAD"]=mean(abs(a-m),trim = 0.05)
  metrics["P90"]=quantile(abs(a-m),probs = 0.9)
  return(metrics)
}

####Example to compute numeric model metrics
#NumericPred is the dataset which also contains predictions of 2 models
#Call the function
modelmetrics(NumericPred$Target,NumericPred$Model1)
modelmetrics(NumericPred$Target,NumericPred$Model2)
modelmetrics(m=NumericPred$Model2,a=NumericPred$Target)

#This function calculates the binary model metrics
#a = vector of actual values
#m = vector of predicted values
#k= no. of predictors

binmodelmetrics=function(a,m,k=10)
{
  metrics = c(LL=0,AIC=0,BIC=0,R2=0)
  metrics["LL"]=sum(ifelse(a==1,log(m),log(1-m)))#Compute log-likelihood
  metrics["AIC"]=-2*metrics["LL"]+2*k
  metrics["BIC"]=-2*metrics["LL"]+2*k*log(length(a))
  SST= sum(a-mean(a))^2
  SSE=sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  return(metrics)
}

######This function builds a contingency table for binary model prediction
#a = vector of actual values
#m = vector of predicted values
#p=cut-off probability value
#inc = increment value to  change p values in iterative loop
#Confusion matrix gives TN FP FN TP

binresults=function(a,m,inc)
{
  mresult=data.frame()
  cutoffs=seq(min(m)+inc,max(m),inc)
  for (p in cutoffs) 
  {
    mnew = ifelse(m<p,0,1)
    contable=table(factor(mnew),factor(a))
    #vec=as.vector(table(factor(mnew),factor(a)))
    vec=c(p,as.vector(table(factor(mnew),factor(a))))
    mresult=rbind(mresult,vec)
  }
  colnames(mresult)=c("Cut-off","TN","FP","FN","TP")
  return(mresult)
}

#Call the function
#BinaryPred is the dataset which also contains model predictions
x=binresults(BinaryPred$Target,BinaryPred$Model1,0.05)

###Plot ROC

fp1=x$FP/max(x$FP)
tp1=x$TP/max(x$TP)
plot(fp1,tp1,col="blue")

###Area Under the curve
AUC = mean(tp1)
AUC

