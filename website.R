library(cluster)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
library(SnowballC)
library(pracma)
library(tools)
library(XML)
library(RCurl)
library(rvest)
library(stringr)

new.function<-function(){

getGoogleURL <- function(search.term, domain = '.co.uk', quotes=TRUE) 
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
                        search.term, sep='')
}

getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                           (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                        (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

search.term <- "apple"

c <- "youtube"
quotes <- "FALSE"
search.url <- getGoogleURL(search.term=search.term, quotes=quotes)
x<-0
list<-NULL
list
while(x<40){
  
  ad <- "&start="
  ad<-paste(ad, toString(x), sep="")
  x<-x+10
  print(ad)
  
  search.url<-paste(search.url, ad, sep="")
  
  links <- append(links,getGoogleLinks(search.url))
  print(links)
}

l<-gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1)) 
l
newlinks<-list()
for(i in seq(1:length(l))){
  newlinks<-append(newlinks,l[i][!grepl(c,l[i])])
}
newlinks
co <- VCorpus(VectorSource(newlinks))
x <- 1
d=list()
for(i in seq(1:length(co))){
  a<-(co[[i]]$content)
  print(a)
  url <- getURL(a)
  # Read and parse HTML file
  doc.text <- function(url){
    read_html(url) %>% 
      html_nodes(xpath = '//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]') %>% 
      html_text() %>% 
      toString()
  }
  
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  doc.text
  # Replace all \n by spaces
  doc.text = gsub('\\n', ' ', doc.text)
  
  # Join all the elements of the character vector into a single
  # character string, separated by spaces
  doc.text = paste(doc.text, collapse = ' ')
  d<-append(d, doc.text)
 
}
d
length(d)

print(co)
co <- Corpus(VectorSource(d))
ndocs <- length(co)
corpus_clean=tm_map(co,tolower)
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents

maxTermFreq <- ndocs * .5
# ignore overly common words i.e. terms that appear in more than 50% of the documents

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
inspect(dtm)
dtm
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
s=which(groups==2)

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
  print(names(y[index]))
  for(b in seq(1:length(y))){
    if(!strcmp(names(y[b]),names(y[index]))){
      print(names(y[b]))
    }
  }
  cat("\n","\n")
}
}
