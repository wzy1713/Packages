# Node Harvest method
rm(list=ls(all=TRUE))
library(nodeHarvest)
library(glmnet)
library(xlsx)
path <- "D:/Projects/2015/Brace project" #changed
setwd(paste(path,"/results",sep=""))
raw_data <- read.table("final_model_data_comb.csv",header=T, sep=',')
dim(raw_data)
all_variables_list <- names(raw_data)

#variables for pre-index treatment and index treatment
treatment_variables <- c('idx_IFN','idx_GA','pre_idx_IFN','pre_idx_GA','pre_idx_NOMS')

#variables for switching flag
switching_flag <- c('flag_NT_IFN','flag_NT_GA','flag_IFN_GA','flag_IFN_IFN', 'flag_GA_IFN')

drop_var <- c('der_sexM','idx_paytypeSMRU','pre_ms_pharmacy_allowed2','num_pre_relapse_1yr2')

#version 3
variable_list_v3<-setdiff(all_variables_list,c(treatment_variables,drop_var))
model_data <- raw_data[,variable_list_v3]

set.seed(1)
sap_id<-rep(0,length=nrow(model_data))
sap_id[model_data$response==1]<-sample(rep(1:4,length=length(which(model_data$response==1))))
sap_id[model_data$response==0]<-sample(rep(1:4,length=length(which(model_data$response==0))))
table(sap_id,model_data$response)

NH_model_data <- model_data[sap_id !=4,]
model_var<-names(NH_model_data)
dim(NH_model_data)
NH_model_data_test <- model_data[sap_id ==4,]
dim(NH_model_data_test)

#Parameters Experiment
#nh_par_list<-expand.grid(mode=c(1,2),nodes=c(500,700,1000,1500,2000),maxinter=c(1,2,3,4,5),nodesize=c(1,5,10,15,20))
nh_par_list <- read.xlsx("nh_design.xlsx",sheetName='design')
mode_par=c(1,2)
nodes=c(500,700,1000,1500,2000)
maxinter=c(1,2,3,4,5)
nodesize=c(1,5,10,15,20)

#10 folds Cross Validation
k.folds <- 5
foldid<-rep(0,length=nrow(NH_model_data))
set.seed(1)
foldid[NH_model_data$response==1]<-sample(rep(1:k.folds,length=length(which(NH_model_data$response==1))))
foldid[NH_model_data$response==0]<-sample(rep(1:k.folds,length=length(which(NH_model_data$response==0))))
table(foldid,NH_model_data$response)

nh <- function(j)
{
  nh_mode<-ifelse(as.numeric(mode_par[nh_par_list$x1[j]])==1,"mean","outbag")
  nh_nodes<-nodes[as.numeric(nh_par_list$x2[j])]
  nh_maxinter<-maxinter[as.numeric(nh_par_list$x3[j])]
  nh_nodesize<-nodesize[as.numeric(nh_par_list$x4[j])]
  
  auc_temp=matrix(rep(0,2*k.folds),ncol=2,nrow=k.folds)	
  for (i in 1:k.folds)
  {
    cv_training <- NH_model_data[foldid!=i,]
    cv_test <- NH_model_data[foldid==i,]
    set.seed(456) 		
    y <- cv_training[,c("response")]
    x <- cv_training[,setdiff(names(cv_training),"response")]
    x_test <- NH_model_data_test[,setdiff(names(NH_model_data),"response")]
    fit_NH <- nodeHarvest(x,
                          y,
                          nodesize = nh_nodesize, 
                          nodes = nh_nodes, 
                          maxinter = nh_maxinter,
                          mode = nh_mode,
                          silent=T)
    pred_training <- predict(fit_NH,newdata=x)	
    auc_training <- auc(cv_training$response,pred_training)	  			
    pred_test <- predict(fit_NH,newdata=x_test)
    auc_test <- auc(cv_test$response,pred_test)
    
    auc_temp[i,1] <- auc_training
    auc_temp[i,2] <- auc_test	
  }
  auc_avg <- apply(auc_temp,2,mean)
  auc_avg2 <- data.frame(mode=nh_mode,nodes=nh_nodes,maxinter=nh_maxinter,nodesize=nh_nodesize,
                         AUC_training=auc_avg[1],AUC_test=auc_avg[2])
  
  eval(parse(text=paste('write.csv(auc_avg2,"process_',j,'.csv",row.names=F)',sep='')))
  return(auc_avg2)
}

#Parallel Run on multi-Cores
num_cpus <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
library("snowfall")
sfInit(parallel=T,cpus=num_cpus-1)   
sfClusterEval(library(glmnet))
sfClusterEval(library(nodeHarvest))
sfExport("k.folds","foldid","NH_model_data","nh_par_list","mode_par","nodes","maxinter","nodesize") 
start=Sys.time()  
Output<-sfClusterApplyLB(1:nrow(nh_par_list),nh)
end=Sys.time() 
end-start
sfStop()

#Drop the checking csv files
for (id in 1:nrow(nh_par_list))
{
  eval(parse(text=paste('file.remove("process_',id,'.csv")',sep='')))
}

#Output the final results
final_results <- Output[[1]]
for (i in c(2:nrow(nh_par_list)))
{
  eval(parse(text=paste('final_results <- rbind(final_results,Output[[',i,']])',sep='')))
}



#Fixed some parameters and repeat 100 times
#nh_mode<-"outbag"
#nh_nodes<-10000
#nh_maxinter<-3
#nh_nodesize<-20

nh_mode<-"outbag"
nh_nodes<-100
nh_maxinter<-1
nh_nodesize<-20

nh_final <- function(i)
{
  set.seed(i)
  y <- NH_model_data[,c("response")]
  x <- NH_model_data[,setdiff(names(NH_model_data),"response")]
  x_test <- NH_model_data_test[,setdiff(names(NH_model_data),"response")]
  fit_NH <- nodeHarvest(x,
                        y,
                        nodesize = nh_nodesize, 
                        nodes = nh_nodes, 
                        maxinter = nh_maxinter,
                        mode = nh_mode,
                        silent=F)
  pred_training <- predict(fit_NH,newdata=x)	
  auc_training <- auc(NH_model_data$response,pred_training)	  			
  pred_test <- predict(fit_NH,newdata=x_test)
  auc_test <- auc(NH_model_data_test$response,pred_test)
  
  cat('loop ',i,' finished!\n')
  
  #PPV for 10% and 25%
  pred_comb <- data.frame(pred_test=pred_test,response=NH_model_data_test$response)
  dim(pred_comb)
  pred_data <- pred_comb[order(-pred_comb[,1]),]
  
  num_pos_pred <- length(which(pred_data[,1]>0))
  
  num_pos_pred_10 <- round(length(pred_test)/10)
  ppv10 <-sum(pred_data[1:num_pos_pred_10,2])/ num_pos_pred_10
  
  num_pos_pred_25 <- round(length(pred_test)/4)
  ppv25 <-sum(pred_data[1:num_pos_pred_25,2])/ num_pos_pred_25
  temp <- cbind(auc_training,auc_test,ppv10,ppv25)
  eval(parse(text=paste('write.csv(temp,"nh_temp',i,'.csv",row.names=F)',sep=''))) 
  return(temp)
}

#Parallel Run on multi-Cores
num_cpus <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
library("snowfall")
sfInit(parallel=T,cpus=num_cpus-1)   
sfClusterEval(library(glmnet))
sfClusterEval(library(nodeHarvest))
sfExport("nh_mode","nh_nodes","nh_maxinter","nh_nodesize","NH_model_data","NH_model_data_test") 
start=Sys.time()  
Output<-sfClusterApplyLB(1:100,nh_final)
end=Sys.time() 
end-start
sfStop()

#Output the final results
NH_final <- Output[[1]]
for (i in c(2:100))
{
  eval(parse(text=paste('final_results <- rbind(final_results,Output[[',i,']])',sep='')))
}

NH_final_avg <- data.frame(type="Mean",t(apply(NH_final,2,mean)))
NH_final_sd <- data.frame(type="SD",t(apply(NH_final,2,sd)))
NH_final_results <- rbind(NH_final_avg,NH_final_sd)
file.remove("final_results_NH.xlsx")
write.xlsx(NH_final_results,"final_results_NH.xlsx",append=T,sheetName="AUC",row.names=F)


#Read the 100 temp files
setwd("D:/Projects/2015/Brace project/results/temp")
NH_final <- data.frame(auc_training=0,auc_test=0,ppv10=0,ppv25=0)
for (i in 1:100)
{
  eval(parse(text=paste('temp <- read.csv("nh_temp',i,'.csv",header=T)',sep='')))
  NH_final <- rbind(NH_final,temp)
}

NH_final <- NH_final[-1,]

#add 0508 RandomForest------------------------------------------------------------------------
rf_ntree=14000
rf_mtry=8
rf_nodesize=100
RF_final <- data.frame(auc_training=0,auc_test=0,ppv10=0,ppv25=0)
imp <-data.frame(t=rep(0,ncol(NH_model_data)-1))
loop <- 100

library(randomForest)
for (i in 1:loop)
{	
  set.seed(i)
  fit_RF<- randomForest(response~.,
                        data=NH_model_data,
                        #	  				  mtry=rf_mtry,
                        #	  				  ntree=rf_ntree,
                        #	  				  nodesize=rf_nodesize,
                        replace=T,
                        maxnodes=20,#add 0508
                        importance=F,
                        do.trace=F)
  
  pred_training=predict(fit_RF,newdata=NH_model_data)
  
  #	pred_training=predict(fit_RF,newdata=NH_model_data,
  #						type="prob")[,2]	
  auc_training=auc(NH_model_data$response,pred_training)
  
  pred_test=predict(fit_RF,newdata=NH_model_data_test)
  #	pred_test=predict(fit_RF,newdata=NH_model_data_test,type="prob")[,2]
  auc_test=auc(NH_model_data_test$response,pred_test)
  
  #Variable Importance
  var_importance <- data.frame(importance(fit_RF))	
  eval(parse(text=paste('colnames(var_importance)<- c("MeanDecreaseGini_',i,'")',sep='')))
  imp <- cbind(imp,var_importance)
  
  #PPV for 10% and 25%
  pred_comb <- data.frame(pred_test=pred_test,response=NH_model_data_test$response)
  dim(pred_comb)
  pred_data <- pred_comb[order(-pred_comb[,1]),]
  
  num_pos_pred <- length(which(pred_data[,1]>0))
  
  num_pos_pred_10 <- round(length(pred_test)/10)
  ppv10 <-sum(pred_data[1:num_pos_pred_10,2])/ num_pos_pred_10
  
  num_pos_pred_25 <- round(length(pred_test)/4)
  ppv25 <-sum(pred_data[1:num_pos_pred_25,2])/ num_pos_pred_25
  temp <- cbind(auc_training,auc_test,ppv10,ppv25)  
  RF_final <- rbind(RF_final,temp)
  cat('loop ',i,' finished!\n')
}

var <- rownames(imp)
final_importance_avg <- apply(imp[,-1],1,mean)
final_importance_sd <- apply(imp[,-1],1,sd)
final_importance <- data.frame(var,final_importance_avg,final_importance_sd)
rownames(final_importance) <- NULL
final_importance <- final_importance[order(-final_importance$final_importance_avg),]

RF_final <- RF_final[-1,]
rf_final_avg <- t(apply(RF_final,2,mean))
rf_final_sd <- t(apply(RF_final,2,sd))
rf_final_results <- rbind(rf_final_avg,rf_final_sd)

file.remove("final_results_RF_with_1_depth.xlsx")
write.xlsx(rf_final_results,"final_results_RF_with_1_depth.xlsx",append=T,sheetName="AUC",row.names=F)
write.xlsx(final_importance,"final_results_RF_with_1_depth.xlsx",append=T,sheetName="Importance",row.names=F)

