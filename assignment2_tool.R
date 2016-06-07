my.matrix.function<-function(table,vector){
 	my.table<-table
 	my.reference.vector<-vector
 	record1<-rownames(my.table)
	#find the same column
	record2<-intersect(my.reference.vector,record1)
	record3<-as.numeric(my.table)
	#find the missing column
    record4<-setdiff(my.reference.vector,record1)
    record5<-rep(0,length(record4))
    record6<-c(record2,record4) #name
    record7<-c(record3,record5) #value
    my.matrix<-matrix(record7,nrow=1)
    colnames(my.matrix)<-record6
    my.matrix<-my.matrix[,order(colnames(my.matrix))]
    return(my.matrix) 
    }

my.evaluation<-function(dataframe,label_col,predict_col){
    mydata<-dataframe
    Label<-label_col
    Predict<-predict_col
    the.TP<-subset(mydata,mydata[,Label]==1 & mydata[,Predict]==1)
    the.FP<-subset(mydata,mydata[,Label]==0 & mydata[,Predict]==1)
    the.FN<-subset(mydata,mydata[,Label]==1 & mydata[,Predict]==0)
    the.TN<-subset(mydata,mydata[,Label]==0 & mydata[,Predict]==0)
    my.TP<-nrow(the.TP)
    my.FP<-nrow(the.FP)
    my.FN<-nrow(the.FN)
    my.TN<-nrow(the.TN)
	Accuracy<-(my.TP+my.TN)/(my.TP+my.FP+my.FN+my.TN)
	Accuracy.logic.check<-ifelse((my.TP+my.FP+my.FN+my.TN)!=0,Accuracy,0 )
	Precision<-(my.TP)/(my.TP+my.FP)
	Precision.logic.check <-ifelse((my.TP+my.FP)!=0,Precision,0)
	Recall<-(my.TP)/(my.TP+my.FN)
	Recall.logic.check<-ifelse((my.TP+my.FN)!=0,Recall,0)
	Fscore<-(2*Precision*Recall)/(Precision+Recall)
	F.score.logic.check<-ifelse((Precision.logic.check+Recall.logic.check)!=0,Fscore,0)
	list.name<-c("Accuracy","Precision","Recall","F-score")
	list.value<-c(Accuracy.logic.check,Precision.logic.check,Recall.logic.check,F.score.logic.check)
	my.list<-list("name"=list.name,"value"=list.value)
	return(my.list)	
}
