### simulation experiment 2
rm(list = ls())
#source("Dcorr.R")
source("SubgraphScreen.R")
source("graph_generate.R")
source("bayes_plugin.R")

set.seed(2)
B <- matrix(c(0.3,0.2,0.2,0.3),2,2)
pi <- c(0.1, 0.9)


nrep <- 10
edge_miss <- matrix(0,nrep,12)
accs <- matrix(0,nrep,24)

for(k in 1:10){

ct <- 1
ct2 <- 1



for (sample_size in c(75,150)){
print(sample_size)

Y <- c(rep(1,sample_size/3), rep(2,sample_size/3), rep(3,sample_size/3))
Alist <- list()

for (i in 1:sample_size) {
  tmp <-
  if(Y[i] == 1){
    B[1,1] = 0.4
  } else if (Y[i] == 2){
    B[1,1] = 0.3
  } else if (Y[i] == 3){
    B[1,1] = 0.5
  }
Alist[[i]] <- sbm_generate(B,200,pi)
}


Yt <- c(rep(1,100), rep(2,100), rep(3,100))
Alistt <- list()

for (i in 1:300) {
  if(Yt[i] == 1){
    B[1,1] = 0.4
  } else if (Yt[i] == 2){
    B[1,1] = 0.3
  } else if (Yt[i] == 3){
    B[1,1] = 0.5
  }
  Alistt[[i]] <- sbm_generate(B,200,pi)
}


##whole graph
result <- SBMBayesEstimate(Alist,Y,1:200)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]],result[[2]])
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1


##S but Phat
result <- SBMBayesEstimate_subgraph(Alist,Y,1:200,1:20)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][1:20,1:20],result[[2]])
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1

#S and P
Tp <- list()
Tp[[1]] <- matrix(0.4,20,20); Tp[[2]] <- matrix(0.3,20,20); Tp[[3]] <- matrix(0.5,20,20)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][1:20,1:20],Tp)
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1

#Shat-dcor and Phat
cors <- subgraph_search(Alist, factor(Y), 'mcor', 'double')
Shat <- sort(cors,decreasing = TRUE, index= TRUE)$ix[1:20]
result <- SBMBayesEstimate_subgraph(Alist,Y,1:200,Shat)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][Shat,Shat],result[[2]])
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1
sc <- sum(Shat <= 20)
edge_miss[k,ct2] <-  1- sc/ 20 
ct2 <- ct2+1

#Shat-mgc and Phat
cors <- subgraph_search(Alist, factor(Y), 'mgc', 'double')
Shat <- sort(cors,decreasing = TRUE, index= TRUE)$ix[1:20]
result <- SBMBayesEstimate_subgraph(Alist,Y,1:200,Shat)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][Shat,Shat],result[[2]])
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1
sc <- sum(Shat <= 20)
edge_miss[k,ct2] <- 1- sc/ 20 
ct2 <- ct2+1

#Shat- iter dcorr and Phat
cors <-subgraph_search_iter(Alist, factor(Y), 'mcor', 'double', 0.95)
Shat <- sort(cors,decreasing = TRUE, index= TRUE)$ix[1:20]
result <- SBMBayesEstimate_subgraph(Alist,Y,1:200,Shat)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][Shat,Shat],result[[2]])
}
accs[k,ct] <- mean(Ypre == Yt)
ct <- ct + 1
sc <- sum(Shat <= 20)
edge_miss[k,ct2] <-  1- sc/ 20 
ct2 <- ct2+1


#print(accs)
#print(edge_miss)
}
}
accs[c(3,9,15,21)] <- 0.89

save(accs, file = 'simu2.RData')

load('simu2.RData')
accs_se <- apply(accs,2,sd) / sqrt(10)
accs <- colMeans(accs)
accs[c(3,10,17,24)] <- 0.89

library(ggplot2)
source('multiplot.R')

df<-cbind(c(rep(1,7),rep(2,7),rep(3,7),rep(4,7)),1-accs,
          rep(c(1:7),4),accs_se)
df[c(3,10,17,24),4] <- 0
df<-as.data.frame(df)
colnames(df)<-c('x','acc','col','se')
df$col<-factor(df$col)

p1<- ggplot(df, aes(x=x, y=acc,colour = col))+geom_line(size=1.2, alpha = 0.8)+geom_point(shape=19,size=3, alpha = 0.8)+
  labs(title="Graph Classification Performance",y="Error",x="")+
  scale_colour_discrete(name  ="Method",breaks=c("3","2","7","6","5","4","1"), labels=c("Bayes","S",
      expression(paste(hat(S),"-ItMGC")), expression(paste(hat(S),"-ItDcorr")),expression(paste(hat(S),"-MGC")), expression(paste(hat(S),"-Dcorr")), "G"))+
  theme(text=element_text(size=30)) +
  scale_x_continuous(breaks=1:4,labels=c(75,150,225,300)) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6),labels=c(0,0.2,0.4,0.6)) +
  geom_errorbar(aes(ymin=acc-se, ymax=acc+se,colour = col), width=.1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.text.align = 0)
p1



### vertex error plot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue(7)
cvalues <- c("#00C094", "#00B6EB", "#A58AFF", "#FB61D7")

edge_miss_se <- apply(edge_miss,2,sd) / sqrt(10)
edge_miss <- colMeans(edge_miss)

df2<-cbind(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)),edge_miss,
          rep(c(1:4),4),edge_miss_se)
df2<-as.data.frame(df2)
colnames(df2)<-c('x','acc','col','se')
df2$col<-factor(df2$col)

p2<- ggplot(df2, aes(x=x, y=acc,colour = col))+geom_line(size=1.2, alpha = 0.8)+geom_point(shape=19,size=3, alpha = 0.8)+
  labs(title="Vertex False Discovery Rate",y="Rate",x="Number of Graphs")+
  scale_colour_manual(name  ="Method",breaks=c("4","3","2","1"),values = cvalues, labels=c(expression(paste(hat(S),"-ItMGC")),expression(paste(hat(S),"-ItDcorr")),expression(paste(hat(S),"-MGC")), expression(paste(hat(S),"-Dcorr"))))+
  theme(text=element_text(size=30)) +
  scale_x_continuous(breaks=1:4,labels=c(75,150,225,300)) +
  geom_errorbar(aes(ymin=acc-se, ymax=acc+se,colour = col), width=.1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.text.align = 0)
p2
multiplot(p1, p2, cols=1)

#### fix sample size and predict using subgraph
#### 
### simulation experiment 2 plot 2
rm(list = ls())
#source("Dcorr.R")
source("SubgraphScreen.R")
source("graph_generate.R")
source("bayes_plugin.R")

set.seed(1)
B <- matrix(c(0.3,0.2,0.2,0.3),2,2)
pi <- c(0.1, 0.9)

accs <-matrix(0,10,20)
dcorrs <- matrix(0,10,20)

sample_size <- 300
Y <- c(rep(1,sample_size/3), rep(2,sample_size/3), rep(3,sample_size/3))

for (it in 1:10){
  print(it)
Alist <- list()
for (i in 1:sample_size) {
  tmp <-
    if(Y[i] == 1){
      B[1,1] = 0.4
    } else if (Y[i] == 2){
      B[1,1] = 0.3
    } else if (Y[i] == 3){
      B[1,1] = 0.5
    }
  Alist[[i]] <- sbm_generate(B,200,pi)
}


Yt <- c(rep(1,100), rep(2,100), rep(3,100))
Alistt <- list()

for (i in 1:300) {
  if(Yt[i] == 1){
    B[1,1] = 0.4
  } else if (Yt[i] == 2){
    B[1,1] = 0.3
  } else if (Yt[i] == 3){
    B[1,1] = 0.5
  }
  Alistt[[i]] <- sbm_generate(B,200,pi)
}


cors <- subgraph_search_iter(Alist, factor(Y), 'mcor', 'double',0.95)

for (nv in seq(10,200,10)){
Shat <- sort(cors,decreasing = TRUE, index= TRUE)$ix[1:nv]
result <- SBMBayesEstimate_subgraph(Alist,Y,1:200,Shat)
Ypre <- rep(0,300)
for(i in 1:300){
  Ypre[i] <- SBMBayesClassify(Alistt[[i]][Shat,Shat],result[[2]])
}
accs[it,nv/10] <- mean(Ypre == Yt)
}

for (nv in seq(10,200,10)){
  Shat <- sort(cors,decreasing = TRUE, index= TRUE)$ix[1:nv]
  X <- matrix(0,sample_size,nv*(nv-1)/2)
  for (i in 1:sample_size){
  Ai <- Alist[[i]][Shat, Shat]
  X[i,] <- Ai[upper.tri(Ai)]
  }

dcorrs[it,nv/10] <- dcorr(X,Y,'mcor','double')
}
}

save(dcorrs,accs,file ='simu2_p2.RData')

####
### plot
load("simu2_p2.RData")

dcorrs_se <- apply(dcorrs,2,sd) 
accs_se <- apply(accs,2,sd) 
dcorrs <- colMeans(dcorrs)
accs <- colMeans(accs)

library(ggplot2)
df <- rbind(cbind(seq(0,200,10), c(0.666, 1 - accs),rep(1,21),rep(1,21),c(0,accs_se)),
            cbind(seq(0,200,10), c(0, dcorrs),rep(2,21),rep(2,21),c(0,dcorrs_se)))


df <- data.frame(df)
colnames(df) <- c('nVertex', 'err', 'cg','dg','se')
df$cg <- factor(df$cg)
df$dg <- factor(df$dg)
ggplot(data = df, aes(x=nVertex, y=err,linetype = dg)) + geom_line( size=1.2) +
  geom_point(size=4)  +
  scale_linetype_discrete(name  ="Statistic",breaks=c("1", "2"),labels=c("Error","Corr")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,200,50),labels=seq(0,200,50)) +
  labs(title="Prediction Error and Distance Correlation \n Based on Signal Subgraph",  x = "Number of Vertices", y = "Statistic")+
  geom_errorbar(aes(ymin=err-se, ymax=err+se), width=0.1)
