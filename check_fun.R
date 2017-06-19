  class = cells$Var1[k]
  mps = cells$Var2[k]
  form = cells$form[k]
  
  if(is.data.frame(input.DSTL)){
    distillations = input.DSTL
    for (m.c in 1:m){
      distillations[which(is.na(distillations[,m.c])),m.c] = "NaN"
      distillations = subset(distillations, distillations[,m.c] != mps)
      distillations[which(distillations[,m.c] == "NaN"),m.c] = NA
    }
  }else{
    distns = c(colnames(data)[which(colnames(data) != mps)])
    distillations = as.data.frame(combinations(n = length(distns),r = 1,v = distns),stringsAsFactors = FALSE)
    if(m>1){distillations[,2:m] = NA}
    
    distillations$set = NA
    for (q in 1:nrow(distillations)){
      distillations$set[q] = paste("<",paste(distillations[q,which(!is.na(distillations[q,]))],collapse = ","),">",sep = "")
    }
    
    if(m>1){
      for (p in 2:m){
        distns2 = as.data.frame(combinations(n = length(distns),r = p,v = distns),stringsAsFactors = FALSE)
        if(p != m){
          t = p+1
          distns2[,t:m] = NA
        }
        distns2$set = NA
        for (q in 1:nrow(distns2)){
          distns2$set[q] = paste("<",paste(distns2[q,which(!is.na(distns2[q,]))],collapse = ","),">",sep = "")
          
          #'     frac = nrow(distns2)/20
          #'     e20 = round(x = (1:frac), digits = 0) * 20
          #'     if(q %in% e20){print(paste(q,"of",nrow(distns2),sep = " "))}
          #'     >>>if you want a counter^^^
        }
        distillations = rbind(distillations,distns2)
      }
      rm(distns,distns2)
    }
    
    distillations$set = NA
    for (q in 1:nrow(distillations)){
      distillations$set[q] = paste("<",paste(distillations[q,which(!is.na(distillations[q,]))],collapse = ","),">",sep = "")
    }
  }
  
  poss.PP.set = unlist(distillations$set)
  
  check = function(distl){
    
    distSET = c(mps,unlist(distl[which(!is.na(distl))]))
    
    formSET = unlist(data[class,distSET])
    dataSET = data[(which(rownames(data) != class)),distSET]
    
    compre2 = function(x,y){
      
      x0 = unlist(x) #x is the chk row
      y0 = unlist(y)
      
      n = length(which(x0 == y0))
      if (n == length(x0)){return(0)}else{
        l = length(x0)
        n  = length(which(x0[2:l] == y0[2:l]))
        if(n == length(x0[2:l])){return(1)}else{return(0)}
      }
    }
    
    cnt = rep(x = NA,times = nrow(dataSET))
    for (q in 1:nrow(dataSET)){
      cnt[q] = compre2(x = formSET, y = unlist(dataSET[q,]))
    }
    if(sum(cnt) > 0) {return(0)}else{return(1)}
  }
  
  distillations$chk = NA
  w = 1
  nr.D = nrow(distillations)
  # require(numbers)
  for (r in 1:nr.D){
    
    DISTL = distillations
    
    if(w > nrow(DISTL)) break
    
    d = check(distl = DISTL[w,1:m])
    
    if (d == 0){
      w = w+1
      
    }else{
      sL = length(which(!is.na(unlist(DISTL[w,1:m]))))
      
      if(sL == 1){
        
        for (c.nm in 1:m){
          DISTL[which(is.na(distillations[,c.nm])),c.nm] = "NaN"
          DISTL <<- subset(DISTL, DISTL[,c.nm] != DISTL[w,1])
          DISTL[which(DISTL[,c.nm] == "NaN"),c.nm] = NA
        }
        
      }else{
        DISTL$cnt = NA
        DISTL$cnt[1:w] = "NaN"
        for (ln in (w+1):nrow(DISTL)){
          ch = DISTL[w,which(!is.na(DISTL[w,1:m]))]
          cch = DISTL[ln,which(!is.na(DISTL[ln,1:m]))]
          DISTL$cnt[ln] =  length(which(ch %in% cch))
        }
        DISTL = subset(x = DISTL, cnt != sL)
        DISTL$cnt = NA
        
      }
    }
    # PF = primeFactors(nrow(DISTL))
    # PF = PF[length(PF)]
    # s.PF = (1:(nrow(DISTL)/PF))*PF 
    # if((nrow(DISTL) - w) %in% s.PF){print(paste((nrow(DISTL) - w),"remaining",sep = " "))}
    distillations <<- DISTL
  }
  
  un.pred = unlist(distillations$set)
  PPs = poss.pp.set[which(!(poss.PP.set %in% un.pred))]