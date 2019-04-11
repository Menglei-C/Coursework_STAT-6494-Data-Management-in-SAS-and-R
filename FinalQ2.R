########Final Exam#####################################
#Unzip the file stories.zip
unzip("stories.zip",overwrite=T)
#get all the .txt file names
file_name<-list.files("./Final")
file_name<-gsub(".txt","",file_name) #file names vector
#######################################################
#Part 2
#a.Function getWords
getWords<-function(txt){
  txt=tolower(txt)                    #trandform the input into lower case
  txt2=gsub("\u2019","'",txt)         #transform apostrophes with ASCII:"\u2019" to valid "'"
  txt3=gsub(paste("[^[:alpha:]']",sep="")," ",txt2) #only alphabetic values and apostrophes are kept
  txt3=gsub("\\s\'{1,}"," ",txt3)     #remove invalid valid apostrophes
  txt3=gsub("\'{1,}\\s"," ",txt3)
  txt4=unlist(strsplit(txt3," "))     #seperate into words
  txt5=txt4[txt4!=""]                 #remove empty characters
  return(txt5)
} 

#b.Create a list called allWords,so that allWords[[i]] contains the words in the i-th story
#set initial values
N=10   #total stories number, here it is 10
story_name=rep("",N)  #story names vector
author=rep("",N)      #author vector
story=rep("",N)       #story contents vector
allWords=list()       #set allWords as a list
path=paste("./Final/",file_name,".txt",sep="")  #file paths vector
#For each of the 10 stories
for (i in 1:N){
  #read the content of the file into variable story
  all=readLines(path[i],warn=F,encoding = "UTF-8")
  all=gsub("\ufb02","fl",all)       #transform latin letter ASCII:"\ufb02" to valid "fl"
  all=gsub("\ufb01","fi",all)       #transform latin letter ASCII:"\ufb01" to valid "fi"
  story_name[i]=all[1]     #get the story name
  author[i]=all[2]     #get the author
  story[i]=paste(all[-c(1,2)],collapse=" ")
  #get all words in the i-th story
  allWords[[i]]=getWords(story[i])
}

#c.Create a vector called sortedWordss, which contains all the unique words from all the stories, sorted alphabetically.
all_unlist=unlist(allWords)
sortedWords<-unique(all_unlist)
sortedWords<-sortedWords[order(sortedWords)]

#d.Create a numeric matrix called BoW, so that each row corresponds to a story, and each column corresponds to a word from the sortedWords vector
n=length(sortedWords)   #unique words number
author<-gsub("by ","",author) #the author names
BoW<-matrix(0,nrow=N,ncol=n,dimnames=list(author,sortedWords))

#e.Get the values of BoW
for(i in 1:N){
  for(j in 1:n){
    if (sortedWords[j] %in% allWords[[i]]) BoW[i,j]=1
  }
}

#f.Create a numeric vector called wordNdoc, which contain the number of words that appear in k stories
wordN=colSums(BoW)   #vector wordN contains the time of apperances of each word
wordNdoc=rep(0,N)    #initial the value of wordNdoc to zero
for(j in 1:n){
  wordNdoc[wordN[j]]=wordNdoc[wordN[j]]+1
}

#g.Show the values in wordNdoc graphically
wordNdoc_table<-as.table(wordNdoc)
column<-as.character(c(1:10))
row.names(wordNdoc_table)<-column
library(RColorBrewer)
opar <- par(no.readonly=TRUE)  # saves a copy of the current settings into a variable called "opar".
par(bg="lightblue",fg="white",col="White",col.axis="white",col.main="white",col.sub="white",col.lab="white",font=3,font.main=4,cex.main=1.5,cex.lab=1.2)
barplot(wordNdoc_table, ylim=c(0,max(wordNdoc)+500),
        col="white", width=1,space=0.2,
        main="The number of words that appear in k stories", 
        xlab="k", ylab="The number of words")
ruler<-max(wordNdoc)%/%500
abline(h=seq(0,ruler*500,500),lty=3)
abline(h=0)
par(opar)   # restore the original settings

#h.Compute the Manhattan distance between each pair of documents
distance=dist(BoW,method="manhattan")
distance=as.matrix((distance))
#Use a graphical representation to show a visualization of the distances
opar <- par(no.readonly=TRUE)  # saves a copy of the current settings into a variable called "opar".
par(bg="grey97",mar=c(4.1,4.1,5.1,7.1))
image(x=1:N,y=1:N,z=distance,
      main="The Manhattan distance between each pair of documents",
      xlab="",ylab="")
location=c(1:N)
for (j in 1:N){
  mtext(author[j],side=3,line=0.2,at=location[j],cex=0.8,font=3)
  mtext(author[j],side=4,line=0.2,at=location[j],cex=0.8,las=2,font=3)
}
par(opar)   # restore the original settings

##From the plot, we could figure out that MarkTwain.txt and RudyardKipling.txt are the most similar, because the pair's color is the deepest on the plot, which means the distance between them is the smallest. The and OHenry.txt and DHLawrence.txt are the least similar to one another, because the pair's color is the lightest on the plot, which means the distance between them is the largest.

#i.
#Print the list of words that appear in all 10 stories
print(sortedWords[wordN==10])
#Print the list of words that appear in 9 of the 10 stories.
print(sortedWords[wordN==9])

#j.The variable stopwords will contain a list of "stop-words"--common words in English
library(hunspell)
stopwords <-unlist(hunspell_parse(readLines('https://jeroen.github.io/files/stopwords.txt')))

#h.Create a vector called sortedWordsNoStop, which contains all the words from all the stories, except for the ones that appear in the stopwords variable.
sortedWordsNoStop<-setdiff(sortedWords,stopwords)

#l.Repeat steps d-h, but use sortedWordsNoStop instead of sortedWords. 
##l-d.Create a numeric matrix called BoW
n=length(sortedWordsNoStop)   #unique words number
BoW<-matrix(0,nrow=N,ncol=n,dimnames=list(author,sortedWordsNoStop))

##l-e.Get the values of BoW
for(i in 1:N){
  for(j in 1:n){
    if (sortedWordsNoStop[j] %in% allWords[[i]]) BoW[i,j]=1
  }
}

##l-f.Create a numeric vector called wordNdoc 
wordN=colSums(BoW)
wordNdoc=rep(0,N)
for(j in 1:n){
  wordNdoc[wordN[j]]=wordNdoc[wordN[j]]+1
} 

##l-g.Show the values in wordNdoc graphically
wordNdoc_table<-as.table(wordNdoc)
column<-as.character(c(1:10))
row.names(wordNdoc_table)<-column

opar <- par(no.readonly=TRUE)  # saves a copy of the current settings into a variable called "opar".
par(bg="lightblue",fg="white",col="White",col.axis="white",col.main="white",col.sub="white",col.lab="white",font=3,font.main=4,cex.main=1.5,cex.lab=1.2)
barplot(wordNdoc_table, ylim=c(0,max(wordNdoc)+500),
        col="white", width=1,space=0.2,
        main="The number of words that appear in k stories\n(removed common words)", 
        xlab="k", ylab="The number of words")
ruler<-max(wordNdoc)%/%500
abline(h=seq(0,ruler*500,500),lty=3)
abline(h=0)
par(opar)   # restore the original settings


##l-h.Compute the Manhattan distance between each pair of documents
distance=dist(BoW,method="manhattan")
distance=as.matrix((distance))
#Use a graphical representation to show a visualization of the distances
opar <- par(no.readonly=TRUE)  # saves a copy of the current settings into a variable called "opar".
par(bg="grey97",mar=c(4.1,4.1,5.1,7.1))
image(x=1:N,y=1:N,z=distance,
      main="The Manhattan distance between each pair of documents\n(removed common words)",
      xlab="",ylab="")
location=c(1:N)
for (j in 1:N){
  mtext(author[j],side=3,line=0.2,at=location[j],cex=0.8,font=3)
  mtext(author[j],side=4,line=0.2,at=location[j],cex=0.8,las=2,font=3)
}
par(opar)   # restore the original settings
##From the plot, we could figure out that MarkTwain.txt and RudyardKipling.txt are the most similar, because the pair's color is the deepest on the plot, which means the distance between them is the smallest. The and AntonChekhov.txt and DHLawrence.txt are the least similar to one another, because the pair's color is the lightest on the plot, which means the distance between them is the largest.

#m.create a word cloud using the 100 most frequent words in the story by D.H. Lawrence
library(wordcloud2)
i=match("D. H. Lawrence",author)
#get all words and their frequencies in the story i
allwords<-allWords[[i]]
allfreq<-table(allwords)
#get unique words in the story i 
sortedwords<-unique(allwords)
sortedwords<-sortedwords[order(sortedwords)]
#remove the common words
nostop<-setdiff(sortedwords,stopwords)
#get frequencies correspond the non-stop-words in the story i
m=length(nostop)
nostop_freq<-rep(0,m)
for (i in 1:m){
  mark=match(nostop[i],sortedwords)
  nostop_freq[i]<-allfreq[mark]
}
#save the 100 most frequent words and their frequencies in a data frame Freq
Freq<-data.frame("Words"=nostop,"Frequency"=nostop_freq,stringsAsFactors = F)
Freq<-Freq[order(Freq$Frequency,decreasing = T),]
Freq<-Freq[1:100,]
#create a word cloud 
wordcloud2(Freq, color = "random-light", backgroundColor = "grey",size=0.8)
