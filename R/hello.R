hello <- function() {
  print("Hello, world!")
}
#指定datasest的label，求dataset的熵
entropy<-function(dataset,label){
  label2<-dataset[,label]
  tb<-table(label2)
  #names<-names(tb)
  type_num<-as.integer(tb)
  len=sum(type_num)
  ps=sapply(type_num,function(x){p=x/len;ifelse(p==0,0,-p*log(p,2))})
  sum(ps)
}
#指定dataset的label和针对的属性，求信息增益
gain<-function(dataset,label,attr){
  entropy_s = entropy(dataset,label)
  #使用attr属性将S分成数个数据集
  attr = dataset[,attr]
  tb = table(attr)
  names = names(tb)
  type_num = as.integer(tb)
  #求出各属性子集占总的比率
  p = sapply(type_num,function(x){x/length(attr)})
  #求出被属性切割后的数据集的熵乘以该数据集占总得比例
  entropys = mapply(function(x,y){data=dataset[attr==x,];e=entropy(data,label);e*y},names,p)
  entropy_t = sum(entropys)
  return(entropy_s-entropy_t)
}
#挑选最佳属性，根据信息增益
chooseBest<-function(data,label,attrs){
  gains = sapply(attrs,function(x){gain(data,label,x)})
  attr = names(which(gains==max(gains)))
  return(attr)
}
#根据属性分割数据集
splitDataset<-function(dataset,label,attr){
  #获取attr的列索引
  attrs = colnames(dataset)
  index = which(attrs==attr)
  rm(attrs)
  #获取attr这一列的成员名
  attr = dataset[,attr]
  tb = table(attr)
  names = names(tb)
  #根据attr的不同值names,分割数据集，并且去掉本身这列
  datas = lapply(names,function(x){data=dataset[attr==x,-index]})
  return(datas)
}
#终止条件
isStop<-function(dataset,label){
  dim_data<-dim(dataset)
  if(dim_data[0]==0){
    print("没有可处理的数据")
    return(1)
  }
  if(dim_data[2]==1){
    ptint("没有可处理的特征")
    return(1)
  }
  #如果label已有一种，终止
  label2<-dataset[,label]
  tb<-table(label2)
  if(length(tb)==1){return(1)}
}
ID3<-function(dataset,label_name,attr_names){
  #create root
  root = NULL
  if(all(dataset[,label_name]==0)==TRUE){}
  if(all(dataset[,label_name]==1)==TRUE){}
  if(is.na(attr_names)){}
  #attribute中分类性能最好的属性




}
