setwd("C:\\Users\\Lenovo\\Desktop\\SensDx\\Kryterium")
data <- read.csv("dataset.csv")

n <- nrow(data)
n_l <- floor(2/3*n)
n_v <- floor(1/2*(n-n_l))
n_t <- n-n_l-n_v

index_learning <- sample(nrow(data), n_l)
index_val <- sample(setdiff(1:nrow(data),index_learning), n_v)
index_test <- setdiff(setdiff(1:nrow(data),index_learning),index_val)

data$add1 <- sapply(1:nrow(data),
                    function(x) data[x,paste("x",data$added[x]-1, sep="")])
data$add2 <- sapply(1:nrow(data),
                    function(x) data[x,paste("x",data$added[x], sep="")])
data$add3 <- sapply(1:nrow(data),
                    function(x) data[x,paste("x",data$added[x]+1, sep="")])
for(i in 2:7){
  data[,paste("y",i,sep="")] <-sapply(1:nrow(data),
                                      function(j) (data[j,paste("x",i,sep="")] -
                                                      data[j,"x1"])/data[j,"x1"])
}
data$yadd1 <- sapply(1:nrow(data),
                    function(x) data[x,paste("y",data$added[x]-1, sep="")])
data$yadd2 <- sapply(1:nrow(data),
                    function(x) data[x,paste("y",data$added[x], sep="")])
data$yadd3 <- sapply(1:nrow(data),
                    function(x) data[x,paste("y",data$added[x]+1, sep="")])

l_data <- data[index_learning,]
v_data <- data[index_val,]
t_data <- data[index_test,] 

p <- l_data[l_data$label=="p",]
n <- l_data[l_data$label=="n",]
m <- l_data[l_data$label=="m",]

plot(1:3,c(0,0,0), type = "l", col="green", ylim=c(-0.5,0.5))
for(i in 1:nrow(p)){
  lines(1:3, p[i,c("yadd1", "yadd2", "yadd3")])
}

for(i in 1:nrow(n)){
  lines(1:3, n[i,c("yadd1", "yadd2", "yadd3")], col="red")
}

for(i in 1:nrow(n)){
  lines(1:3, m[i,c("yadd1", "yadd2", "yadd3")], col="blue")
}
lines(1:3,c(0,0,0), type = "l", col="green")

means <- c()
sds <- c()
for(i in 2:7){
  means <- c(means,mean(n[,paste("y",i,sep="")]))
  sds <- c(sds,sd(n[,paste("y",i,sep="")]))
}
plot(means)
plot(sds)

for(i in 2:7){
  data[,paste("z",i,sep="")] <- sapply(1:nrow(data), function(j) ((data[j,paste("y",i,sep="")] - means[i-1])/sds[i-1]))
}

data$zadd1 <- sapply(1:nrow(data),
                    function(x) data[x,paste("z",data$added[x]-1, sep="")])
data$zadd2 <- sapply(1:nrow(data),
                    function(x) data[x,paste("z",data$added[x], sep="")])
data$zadd3 <- sapply(1:nrow(data),
                    function(x) data[x,paste("z",data$added[x]+1, sep="")])

l_data <- data[index_learning,]
v_data <- data[index_val,]
t_data <- data[index_test,] 

p <- l_data[l_data$label=="p",]
n <- l_data[l_data$label=="n",]
m <- l_data[l_data$label=="m",]


plot(1:3,c(0,0,0), type = "l", col="green", ylim=c(-5,5))
for(i in 1:nrow(p)){
  lines(1:3, p[i,c("zadd1", "zadd2", "zadd3")])
}

for(i in 1:nrow(n)){
  lines(1:3, n[i,c("zadd1", "zadd2", "zadd3")], col="red")
}


plot(1:3,c(0,0,0), type = "l", col="green", ylim=c(-5,5))
for(i in 1:nrow(p)){
  lines(1:3, p[i,c("zadd1", "zadd2", "zadd3")]-p[i,"zadd1"])
}

for(i in 1:nrow(n)){
  lines(1:3, n[i,c("zadd1", "zadd2", "zadd3")]-n[i,'zadd1'], col="red")
}

data$pred <- data$zadd3-data$zadd1

l_data <- data[index_learning,]
v_data <- data[index_val,]
t_data <- data[index_test,] 

p <- l_data[l_data$label=="p",]
n <- l_data[l_data$label=="n",]
m <- l_data[l_data$label=="m",]

plot(p$pred, rep(1,nrow(p)), xlim=c(-6,6), ylim=c(-3,4))
points(n$pred, rep(0,nrow(n)), col="red")
points(v_data$pred[v_data$label=="p"], rep(1,length(v_data$pred[v_data$label=="p"])), col="green")
points(v_data$pred[v_data$label=="n"], rep(0,length(v_data$pred[v_data$label=="n"])), col="green")

reglog1 <- glm(label ~ zadd1 + zadd2 + zadd3, family = binomial, data=l_data)
reglog2 <- glm(label ~ zadd2 + zadd3, family = binomial, data=l_data)


