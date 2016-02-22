# Install function for packages    
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(wordcloud)
packages(tm)
packages(rvest)
packages(RColorBrewer)

wrong.selection <- F

data.source <- %%textselection%%
field.list <- ''
if(data.source== 'web'){
u <- '%%url%%'
   if( u =='') wrong.selection <- T
selector <- '%%selector%%'
sample.text <- read_html(u)
ps<-html_nodes(sample.text, selector) %>%
  html_text()
} 


if(data.source =='local'){
path <- '%%path%%'
   if( path =='') wrong.selection <- T
  ps <- readLines(path)
} 

if(data.source =='modeler'){
   if('%%data%%' =='') wrong.selection <- T
          tmp <- strsplit('%%data%%',',')
          field.list <- gsub(" ", '', tmp[[1]])

if(length(field.list) == 1){
             ps <- modelerData[['%%data%%']]
    } else{
    ps <- lapply(field.list, function(x) {modelerData[[x]]})
}
}

if(nchar('%%savepath%%') > 0){
     save.path <-  '%%savepath%%'
     input.name <-  '%%filename%%'
     save <- T
}

name.count <<- 1

makeCloud <- function(x){
    ps <- paste(x, sep=" ", collapse="") 
    myCorpus = Corpus(VectorSource(ps))
    
    myCorpus = tm_map(myCorpus, tolower)
    if(%%rp%%) myCorpus =  tm_map(myCorpus, removePunctuation)
    if(%%rn%%)   myCorpus = tm_map(myCorpus, removeNumbers)
    if(%%rsw%%) myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
    corpus_clean <- tm_map(myCorpus, PlainTextDocument)
    myDTM = TermDocumentMatrix(corpus_clean, control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    v  = sort(rowSums(m), decreasing = TRUE)
    set.seed(4363)
    palette <- brewer.pal(8,%%pal%%)

# save the image in png format

wordcloud(names(v), v, min.freq = %%min%%, max.words = %%max%%, random.order = F, random.color = T,rot.per=%%rotation%%, colors=palette)


# save the image in png format
if(save){
 name <- paste(save.path,'/' ,input.name,' ' , name.count, '.png',sep='')
png(name, width=%%wid%%, height= %%ht%%, units='in', res=300)
wordcloud(names(v), v, min.freq = %%min%%, max.words = %%max%%, random.order = F, random.color = T,rot.per=%%rotation%%, colors=palette)
dev.off()
name.count <<- name.count +1 
}


if(%%termfreqs%%)  print(v)  #prints term frequencies


}

graph.out <- ifelse((length(field.list) >1) || (%%byrow%%), lapply(ps,makeCloud),makeCloud(ps)) 
if(wrong.selection) print('You selected an invalid text source.  Update your selection of Modeler, Web, or Local data')

