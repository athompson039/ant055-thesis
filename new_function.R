CellP2 = function(data, m = 2){
  
  require(gtools)
  
  if (m>2) {
    print("At m>2, this function runs very slowly for large mapped.data sets. Be prepared for a long wait.")
  }
  
  nrow = nrow(data)
  ncol = ncol(data)
  
  if (m > (ncol-1)) {
    return(print("m is not a valid value"))
  } else {
    
    PossComs = function(n,k) {factorial(n)/(factorial(k)*factorial(n-k))}
    
    poss.pp = sum(PossComs(n=(ncol-1),k=1:m))
    
    output.Km1 = NA
    output.list.Km1 = NA
    
    mps.chk = function(k){
      mps = cells.numeric$Var2[k]
      
      distns = as.numeric(c(colnames(data)[which(colnames(data) != mps)]))
      
      distillations.list = list()
      
      for (p in 1:m){
        D.distillations = combinations(n = length(distns),r = p,v = distns)
        x = length(distillations.list)
        for (q in 1:nrow(D.distillations)){
          distillations.list[[(x+q)]] = D.distillations[q,]
        }
      }
      
      if(is.data.frame(output.Km1)) {output = output.Km1}else{
        output = mapped.data
        for (i in 1:ncol(output)){
          output[which(mapped.data[,i] == 0),i] = NA
          output[which(mapped.data[,i] != 0),i] = 0
        }
      }
      
      if(is.data.frame(output.list.Km1)) {output.list = output.list.Km1}else{
        output.list = output
        for (i in 1:ncol(outputlist)){
          output.list[which(output.list[,i] == 0),i] = NA
        }
      }
      
      for (q in 1:length(distillations.list)){
        distl = distillations.list[[q]]
        
        distSET = c(mps,unlist(distl))
        dataSET = mapped.data[,distSET]
        
        unique.predictors = unique(dataSET[,2:ncol(dataSET)])
        if(ncol(dataSET) == 2) {unique.predictors = as.data.frame(cbind(unique.predictors))}
        unique.predictors$prediction = 0
        unique.predictors$predictive = 1
        
        compre = function(x,y){
          
          x0 = unlist(x) #x is the chk row
          y0 = unlist(y)
          
          n = length(which(x0 == y0))
          if (n == length(x0)){return(1)}else{return(0)}
        }
        
        ncol.DS = ncol(dataSET)
        for (y in 1:nrow(dataSET)){
          preR = dataSET[y,2:ncol.DS]
          for (z in 1:nrow(unique.predictors)){
            preD = unique.predictors[z,1:(ncol.DS-1)]
            c = compre(x = preR, y = preD)
            if (c == 1) break
          }
          dataSET$index[y] = z
          if(unique.predictors$prediction[z] == 0){
            unique.predictors$prediction[z] = dataSET[y,1]
          }else{
            if (unique.predictors$prediction[z] != dataSET[y,1]){
              unique.predictors$predictive[z] = 0
            }
          }
        }
        for (y in 1:nrow(dataSET)){
          if(!is.na(output[y,k])){
            output[y,k] = output[y,k] + unique.predictors$predictive[dataSET$index[y]]
          }
        }
        
      }
      output.list.Km1 <<- output.list
      return(output)
    }
    
    
    for (v in 1:ncol(mapped.data)){
      output.Km1 = mps.chk(v)
    }
    
    output = output.Km1
    for (w in 1:ncol(output)){
      output[,w] = output[,w] / poss.pp
    }
    return(output)
  }
}
  