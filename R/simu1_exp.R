### simulation experiment roc of screening
rm(list = ls())
# source("MGCSampleStat.R")
# source("MGCPermutationTest.R")
source("Dcorr.R")
source("SubgraphScreen.R")
source("graph_generate.R")
source("bayes_plugin.R")
library("ROCR")


set.seed(3)
nrep <- 1
aucs <- matrix(0,8,nrep)

for (n in 1:nrep){

B <- matrix(c(0.3,0.2,0.2,0.3),2,2)
pi <- c(0.1, 0.9)

sample_size <- 100

Y <- c(rep(1,sample_size/2), rep(2,sample_size/2))
Alist <- list()

for (i in 1:sample_size) {
  if(Y[i] == 1){
    B[1,1] = 0.3
  } else if (Y[i] == 2){
    B[1,1] = 0.4
  } else if (Y[i] == 3){
    B[1,1] = 0.5
  }
  Alist[[i]] <- sbm_generate(B,200,pi)
}

t1<- proc.time()
cors1 <- subgraph_search(Alist, Y, 'mcor', 'double')
print(proc.time() - t1)

t1<- proc.time()
cors2 <- subgraph_search(Alist, Y, 'mgc', 'double')
print(proc.time() - t1)

t1<- proc.time()
#cors3 <- subgraph_search_iter(Alist, Y, 'mcor', 'double', 0.5)
print(proc.time() - t1)

t1<- proc.time()
cors4 <- subgraph_search_iter(Alist, Y, 'mcor', 'double', 0.95)
print(proc.time() - t1)

t1<- proc.time()
cors5 <- subgraph_search(Alist, Y, 'cca', 'double')
print(proc.time() - t1)

t1<- proc.time()
cors6 <- subgraph_search(Alist, Y, 'rv', 'double')
print(proc.time() - t1)

t1<- proc.time()
#cors7 <- subgraph_search_iter(Alist, Y, 'mgc', 'double', 0.5)
print(proc.time() - t1)

t1<- proc.time()
#cors8 <- subgraph_search_iter(Alist, Y, 'mgc', 'double', 0.95)
print(proc.time() - t1)


vlabel <- c(rep(1,20),rep(0,180))
pred <- prediction(cors1, vlabel)
perf1 <- performance(pred,"tpr","fpr")
aucs[1,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

pred <- prediction(cors2, vlabel)
perf2 <- performance(pred,"tpr","fpr")
aucs[2,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

#pred <- prediction(cors3, vlabel)
#perf3 <- performance(pred,"tpr","fpr")
#aucs[3,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

pred <- prediction(cors4, vlabel)
perf4 <- performance(pred,"tpr","fpr")
aucs[4,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

pred <- prediction(cors5, vlabel)
perf5 <- performance(pred,"tpr","fpr")
aucs[5,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]


pred <- prediction(cors6, vlabel)
perf6 <- performance(pred,"tpr","fpr")
aucs[6,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

#pred <- prediction(cors7, vlabel)
#perf7 <- performance(pred,"tpr","fpr")
#aucs[7,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

#pred <- prediction(cors8, vlabel)
#perf8 <- performance(pred,"tpr","fpr")
#aucs[8,n] <- attr(performance(prediction.obj = pred, measure = "auc"),'y.values')[[1]]

}





df <- rbind(
            cbind(attr(perf1, 'x.values')[[1]], attr(perf1, 'y.values')[[1]], rep(1,length(attr(perf2, 'x.values')[[1]]))),
            cbind(attr(perf4, 'x.values')[[1]], attr(perf4, 'y.values')[[1]], rep(2,length(attr(perf4, 'x.values')[[1]]))))
            #cbind(attr(perf5, 'x.values')[[1]], attr(perf5, 'y.values')[[1]], rep(3,length(attr(perf5, 'x.values')[[1]]))),
            #cbind(attr(perf6, 'x.values')[[1]], attr(perf6, 'y.values')[[1]], rep(4,length(attr(perf6, 'x.values')[[1]]))))

df <- data.frame(df)
colnames(df) <- c('tpr', 'fpr', 'cg')
df$cg <- factor(df$cg)


library(ggplot2)
p1 <-ggplot(df,aes(x = tpr,y = fpr, group = cg, colour=cg)) + geom_line(size = 1.5, alpha=0.7) +
  scale_colour_discrete(name  ="Method",breaks=c("2", "1"),labels=c("ItMGC-0.05", "MGC")) +
  geom_segment(aes(0, 0, xend = 1,yend=  1),colour = 'black', linetype = 2) +
  labs(title= "ROC Curve of Vertex Screening", x = "False Positive Rate", y = "True Positive Rate") +
  theme(text=element_text(size=30)) +
  theme(plot.title = element_text(hjust = 0.5))

p1
