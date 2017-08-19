hello <- function() {
  print("Hello, world!")
}
#知道各标签的频数，求熵
entropy<-function(type_num){
  len<-sum(type_num)
  ps<-sapply(type_num,function(x){p<-x/len;ifelse(p==0,0,-p*log(p,2))})
  sum(ps)
}
gain<-function(dataset,label,attr){
  label2<-dataset[,label]
  tb<-table(label2)
  names<-names(tb)
  type_num<-as.integer(tb)
  entropy_s<-entropy(type_num)
  #使用attr属性将S分成数个数据集
  attr<-dataset[,attr]
  tb<-table(attr)
  names<-names(tb)
  type_num<-as.integer(tb)
  #求出各属性子集占总的比率
  p<-sapply(type_num,function(x){x/length(attr)})
  type_nums<-lapply(names,function(x){data<-dataset[attr==x,];label_t<-data[,label];tb_t<-table(label_t);type_num_t<-as.integer(tb_t)})
  # entropy_t<-mapply(function(){p[x]*entropy(type_nums[x])},p,type_nums)
  entropy_t<-NULL
  for(i in 1:length(p)){
    entropy_t[i]<-p[i]*entropy(as.vector(type_nums[[i]]))
  }
  return(entropy_s-sum(as.numeric(entropy_t)))
}
chooseBest<-function(S,label){


}
