install.packages("RMySQL")
install.packages("dbConnect")
install.packages("proxy")
install.packages("tictoc")
install.packages("ggplot2")
install.packages("RANN")
install.packages("FNN")
install.packages("rflann")
library(dbConnect)
library(DBI)
library(RMySQL)
library(proxy)
library(tictoc)
library(gWidgets)
library(dbConnect)
library(ggplot2)
library(RANN)
library(FNN)
library(rflann)

con_mysql = dbConnect(MySQL(), user = 'root', password = '*******', 
                    host = 'localhost', dbname = 'final')
dbListTables(con_mysql)

init = function(){
  query = "select people.FullName, peopleandcompanies.Role,peopleandcompanies.Start_,peopleandcompanies.End_ \
           FROM people inner join \
  peopleandcompanies on people.Dir_ID=peopleandcompanies.People_ID"
  data = dbGetQuery(con_mysql, query)
  data = as.data.frame(data)
  data = na.omit(data)
  sum(is.na(data))

  id = seq(nrow(data))
  df = cbind(id, data)
  #View(df)
  return(df)
}

pld = function(df2){
  ggplot(data=df2, aes(x=id, y=bin_s, group=1)) +
    geom_line(color = "blue", size = 0.5)+
    geom_point(color = "red", size =2) +
    ggtitle("Similarity Plot") +
    labs(x = "id", y = "Similarity") +
    theme_update()
}

# knn
myknn = function(ind,k) {
  df = init()
  df1 = df[,3:5]
  df1 = data.frame(model.matrix(~.,df1))
  df1 = df1[,-1]
  
  bin_s = 0;
  tic()
  #start.time <- Sys.time()
  for(i in 1:nrow(df1)){
    x = as.numeric(df1[ind,])
    y = as.numeric(df1[i,])
    z = list(x,y)
    bin_s[i] = simil(z, method = "binary")
  }
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #return(time.taken)
  print("Execution time")
  toc()
  df2 = cbind(df,bin_s)
  df3 = df2[order(-df2$bin_s),]
  print("Binary Similarity")
  print(df3$id[0:k+1])
  print(df3$FullName[0:k+1])
  print(df3$Role[0:k+1])
  print(df3$bin_s[0:k+1])
  pld(df2)
}

# kd-tree
mykdtree = function(ind, n){
  n = n + 1
  k1 = init()
  print("kd-tree results")
  k2 = k1[,3:5]
  k3 = data.frame(model.matrix(~.,k2))
  k3 = k3[,-1]
  tic()
  
  
  
  #start.time <- Sys.time()
  nn1 = Neighbour(k3, k3, k = n, build = "kdtree")
  end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #return(time.taken)
  print("Execution Time")
  toc()
  print(nn1$indices[ind,])
  print(k1$FullName[nn1$indices[ind,]])
  print(k1$Role[nn1$indices[ind,]])
  print(nn1$distances[ind,])
}

myknn(5,1)
mykdtree(5,1)

myknn(30,5)
mykdtree(30,5)

myknn(100,10)
mykdtree(100,10)

myknn(243,20)
mykdtree(243,20)

myknn(343,50)
mykdtree(343,50)

t1 = 0
for(i in 1:100){
t1[i] = myknn(i,1)
}

t2 = 0
for(i in 1:100){
  t2[i] = myknn(i,5)
}

t3 = 0
for(i in 1:100){
  t3[i] = myknn(i,10)
}

t4 = 0
for(i in 1:100){
  t4[i] = myknn(i,20)
}

t5 = 0
for(i in 1:100){
  t5[i] = myknn(i,50)
}

knn_time = cbind(t1,t2,t3,t4,t5)
knn_time = as.data.frame(knn_time)
write.csv(knn_time,"Knn.csv")

t6 = 0
for(i in 1:100){
  t6[i] = mykdtree(i,1)
}

t7 = 0
for(i in 1:100){
  t7[i] = mykdtree(i,5)
}

t8 = 0
for(i in 1:100){
  t8[i] = mykdtree(i,10)
}

t9 = 0
for(i in 1:100){
  t9[i] = mykdtree(i,20)
}

t10 = 0
for(i in 1:100){
  t10[i] = mykdtree(i,50)
}

kd_time = cbind(t6,t7,t8,t9,t10)
kd_time = as.data.frame(kd_time)
write.csv(kd_time,"Kd-tree.csv")

