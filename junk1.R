


if(m>1){distillations[,2:m] = NA}

if(m>1){
  for (p in 2:m){
    distns2 = as.data.frame(combinations(n = length(distns),r = p,v = distns),stringsAsFactors = FALSE)
    if(p != m){
      t = p+1
      distns2[,t:m] = NA
    }
    
    distillations = rbind(distillations,distns2)
  }
}
rm(distns,distns2)