combine <- function(ar,ar_sz){
  result_set = c()
  for (x in seq(1,2**length(ar)-1)){
    result = ""
    i = 0 
    while (i < length(ar)){
      if (bitwAnd(x,bitwShiftL(1,i))){
        if (result != "")
        {
          result <- paste (result,'+',ar[i+1])
        }
        else {
          result = ar[i+1]
        }
        
        #print (i)
        #print (paste(result))
      }
      i <- i + 1
    }
    if (length(result) >= ar_sz){
      result_set <- c(result_set,result)
      #print(result)
    }
  }
  return(result_set)
}

runRegOnAll <- function (df){
  res = data.frame()
  #always the second column in DF is the dependent variable, all the rest are predictors
  arr=names(df[3:length(df)])
  all = combine(arr,1)
  for (s in combine(arr,1))
  {
    f <- (paste(names(df)[2],' ~',s))
    #print(f)
    m<-lmBF(as.formula(f), data=df)
    res = rbind(res,as.data.frame(m)[c('bf')])
  }
  
  return (res)
}


