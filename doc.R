library(cluster)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
library(SnowballC)
library(pracma)
library(tools)
library(gdata)
new.function<-function(){
co <- Corpus(DirSource(directory="C:\\Users\\admin\\Documents\\project"))
#summary(co)
ndocs <- length(co)
corpus_clean=tm_map(co,tolower)
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5

dtm = DocumentTermMatrix(corpus_clean,control = list(
  stopwords = TRUE, 
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stemming = TRUE,
  stripWhitespace=TRUE,
  bounds = list(global = c(minTermFreq, maxTermFreq))
))
write.csv((as.matrix(dtm)),"t.csv")
dtm <- weightTfIdf(dtm,normalize = TRUE)
#inspect(dtm)
#dtm
write.csv((as.matrix(dtm)), "test.csv")
m  <- as.matrix(dtm)
library(philentropy)
#distMatrix <-dist(m, method = "euclidean")
require(vegan)
distMatrix <-dist(m, method = "jaccard")
distMatrix
#thr=min(min(distMatrix))
#thr<-thr+0.2
#thr
clust <- hclust(distMatrix,method = "ward.D")
plot(clust,hang = -1,cex=0.9)
groups<-cutree(clust, k=2)
which(groups==2)
s
for(i in seq(1:2)){
  y<-list()
  j=0
  index=0
  max=0
  s=which(groups==i)
  
    for(v in seq(from=1,to=ndocs)){
      for(k in seq(1:length(s))){
          #print(a[[k]][n])
          if(strcmp(co[[v]]$meta$id,names(s[k]))){
            x<-co[[v]]$content
            #Remove YAML front matter on Rmd
            if(length(grep("---",x))>0){x<-x[-seq(1,max(grep("---",x)))]}
            wrds<-0
            for(line in x){
              #Removes non character and splits
              split_line<-strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
              #Removes empty string
              split_line<-split_line[split_line!=""]
              wrds<-wrds+length(split_line)
            }
            y[co[[v]]$meta$id]<-wrds
            #print(paste(co[[v]]$meta$id,wrds))
          }
        }
        
    }
  for(j in seq(1:length(y))){
    if(max<y[[j]]){
      max=y[[j]]
      index=j
    }
  }  
  filename="C:\\Users\\admin\\Documents\\web\\a.txt"
  write(names(y[index]), file=filename, append=TRUE, sep="\n")
  cat("Best Match for the query : ",names(y[index]),"\n")
  for(b in seq(1:length(y))){
    if(!strcmp(names(y[b]),names(y[index]))){
      cat("\t","\t","\t"," Similar Results : ",names(y[b]),"\n")
    }
  }
  cat("\n","\n")
}

}


