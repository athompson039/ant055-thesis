CellP2 = function(data, m = 2, input.DSTL = NA){
  
  require(gtools)
  
  if (m>2) {
    print("At m>2, this function runs very slowly for large data sets. Be prepared for a long wait.")
  }
  
  nrow = nrow(data)
  ncol = ncol(data)
  
  if (m+1 > (ncol-1)) {
    print("m is not a valid value")
  } else {
    
    # cells = expand.grid(row.names(data),colnames(data),stringsAsFactors = FALSE)
    # for (i in 1:nrow(cells)) {cells$form[i] = data[(cells$Var1[i]),(cells$Var2[i])]}
    # cells = cells[which(cells$form != "NaN"),]
    
    cells.numeric = expand.grid(row.names(mapped.data),colnames(mapped.data),stringsAsFactors = FALSE)
    for (i in 1:nrow(cells.numeric)) {cells.numeric$form[i] = mapped.data[(cells.numeric$Var1[i]),(cells.numeric$Var2[i])]}
    cells.numeric = cells.numeric[which(cells.numeric$form != 0),]
    
    
    PossComs = function(n,k) {factorial(n)/(factorial(k)*factorial(n-k))}
    
    poss.pp = sum(PossComs(n=(ncol-1),k=1:m))
    
    cell.chk = function(k){
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
        
        # distillations$set = NA
        # for (q in 1:nrow(distillations)){
        #   distillations$set[q] = paste("<",paste(distillations[q,which(!is.na(distillations[q,]))],collapse = ","),">",sep = "")
        }
      }
      
      # poss.PP.set = unlist(distillations$set)
      
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
            
            for (ln in (w+1):nrow(DISTL)){
              DISTL$cnt[ln] =  length(DISTL[w,1:m]) %in% which(DISTL[ln,1:m])
              DISTL <<- subset(DISTL, cnt != sL)
              DISTL$cnt = NA
            }
            
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
      return(PPs)
    }
    
    str.d = function(v){
      set = cell.chk(v)
      cells[v,"Sets"] <<- paste(set, collapse = " , ")
      cells[v,"Set.Cnt"] <<- length(set)
      cells$perPP[v] <<- cells$Set.Cnt[v] / poss.pp
      print(paste(v,"of",nrow(cells),sep = " "))
    }
    
    print("Starting Cell Check...")    
    lapply(X = 1:nrow(cells),FUN = str.d)
    print("Cell Check Complete!")    
    
    output = data
    print("Begining Data Restructuring...")
    restr = function(j){

      output[(cells$Var1[j]),(cells$Var2[j])] <<- as.numeric(round(cells$perPP[j],4))
      print(paste(j,"of",nrow(cells),sep = " "))
    }
    lapply(X = 1:nrow(cells),FUN = restr)
    print("Data Restructuring Complete!")
    
    fl = paste("stumple_output/CellP_setDATA_m-",m,".csv",sep = "")
    
    dir.create(path = "stumple_output", showWarnings = FALSE)
    write.csv(x = cells, file = fl)
    return(output)
  }
}
