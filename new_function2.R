CellP2 = function(data, m = 2){
  
  require(gtools)
  
  if (m>2) {
    print("At m>2, this function runs very slowly for large data sets. Be prepared for a long wait.")
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
      mps = k
      
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
        output = data
        for (i in 1:ncol(output)){
          output[which(data[,i] == 0),i] = NA
          output[which(data[,i] != 0),i] = 0
          output[,i] = as.numeric(output[,i]) 
        }
      }
      
      if(is.data.frame(output.list.Km1)) {output.list = output.list.Km1}else{
        output.list = output
        for (i in 1:ncol(output.list)){
          output.list[,i] = ""
        }
      }
      
      for (q in 1:length(distillations.list)){
        
        if(typeof(distillations.list[[q]]) != "list"){
          
          distl = distillations.list[[q]]
          
          distSET = c(mps,unlist(distl))
          dataSET = data[,distSET]
          
        }else{
          
          distl = distillations.list[[q]][[1]]
          
          #####
          chkd = unique(distillations.list[[q]][[2]])
          
          for (y in 1:length(chkd)){
            class = chkd[y]
            output[class,k] = output[class,k] + 1
            if(output.list[class,k] == ""){
              output.list[class,k] = paste("<",paste(distl, collapse = ","),">",sep = "")
            }else{
              output.list[class,k] = paste(output.list[class,k], paste("<",paste(distl, collapse = ","),">",sep = ""), sep = ";")
            }
          }
          #####
          
          distSET = c(mps,unlist(distl))
          dataSET = data[,distSET]
          dataSET = dataSET[which(!((1:nrow(data)) %in% chkd)),]
        }
        
        if(nrow(dataSET) != 0){
        
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
          class = as.numeric(rownames(dataSET)[y])
          if(!is.na(output[class,k])){
            output[class,k] = output[class,k] + unique.predictors$predictive[dataSET$index[y]]
            if(unique.predictors$predictive[dataSET$index[y]] == 1){
              if(output.list[class,k] == ""){
                output.list[class,k] = paste("<",paste(distl, collapse = ","),">",sep = "")
              }else{
                output.list[class,k] = paste(output.list[class,k], paste("<",paste(distl, collapse = ","),">",sep = ""), sep = ";")
              }
              setL = length(distl)
              if (setL != m){
                setL = (PossComs(n=(ncol-1),k=setL))+1
              
              for (h in setL:length(distillations.list)){
                if(typeof(distillations.list[[h]]) != "list"){
                  if (length(which(distl %in% distillations.list[[h]])) == length(distl)){
                    distillations.list[[h]] = list(distillations.list[[h]],c(class))
                  }
                }else{
                  if (length(which(distl %in% distillations.list[[h]][[1]]) == length(distl))){
                    distillations.list[[h]][[2]] = append(x = (distillations.list[[h]][[2]]), values = class)
                  }
                }}
              }
            }
          }
        }
        
        }
        }
      
      output.list.Km1 <<- output.list
      return(output)
    }
    
    
    for (v in 1:ncol){
      output.Km1 = mps.chk(v)
    }
    
    output = output.Km1
    for (w in 1:ncol(output)){
      output[,w] = output[,w] / poss.pp
    }
    
    fl = paste("stumple_output_NEW/CellP_setDATA_m-",m,".csv",sep = "")
    
    dir.create(path = "stumple_output_NEW", showWarnings = FALSE)
    write.csv(x = output.list.Km1, file = fl)
    print("Principal Part Sets for 1 through",m,"Saved at",fl,sep="")
    
    return(output)
  }
}
