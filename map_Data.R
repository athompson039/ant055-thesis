mapped.data = data

rownames(mapped.data) = 1:nrow(mapped.data)
colnames(mapped.data) = 1:ncol(mapped.data)

cells.numeric = expand.grid(row.names(mapped.data),colnames(mapped.data),stringsAsFactors = FALSE)
for (i in 1:nrow(cells.numeric)) {cells.numeric$form[i] = mapped.data[(cells.numeric$Var1[i]),(cells.numeric$Var2[i])]}
map.cells = as.data.frame(cbind(unlist(cells.numeric$form)))


for (i in 1:nrow(mapped.data)){
  for (j in 1:ncol(mapped.data)){
    CL = mapped.data[i,j]
    if (CL == "NaN") {n = 0}else{
    n = which(map.cells$V1 == CL)}
    mapped.data[i,j] = as.numeric(n)
  }
}

map.classes = as.data.frame(cbind(unlist(row.names(data))))

map.mps = as.data.frame(cbind(unlist(colnames(data))))
