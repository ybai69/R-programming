
combine<-function(data,start,end,index,r){
  arr<-1:end
  if((index-1)==r){
    print(data[1:r])
  }
  i=start
  while((i<=end)&(end-i>=r-index)){
    data[index]=arr[i]
    combine(data,i+1,end,index+1,r)
    i=i+1
  }
}
printcombine<-function(n,r){
  cat(combine(c(),1,n,1,r))
  
}