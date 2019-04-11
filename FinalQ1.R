########Final Exam#####################################
#Unzip the file stories.zip
unzip("stories.zip",overwrite=T)
#get all the .txt file names
file_name<-list.files("./Final")
file_name<-gsub(".txt","",file_name) #file names vector
#######################################################
#Part1
#a.Function entropy
entropy<-function(prob){
  n=length(prob)
  h=rep(0,n)
  for (i in 1:n){
    if (prob[i]==0){
      h[i]=0
    }else{
      h[i]=prob[i]*log2(prob[i])
    }
  }
  H=sum(h)
  return(H)
}

#b.Function getLetters
getLetters<-function(txt){
  txt=tolower(txt)                 #trandform the input into lower case
  txt2=unlist(strsplit(txt,""))    #seperate into letters
  txt2=txt2[txt2 %in% letters]     #only alphabetic values are kept
  return(txt2)
}

#c.For each story:read content and calculate the entropy
#set initial values
N=length(file_name)  #total stories number, here it is 10
story_name=rep("",N) #story names vector
author=rep("",N)     #author vector
story=rep("",N)      #story contents vector
H=rep(0,N)           #entropy values vector
path=paste("./Final/",file_name,".txt",sep="")  #file paths vector
#For each of the 10 stories
for (i in 1:N){
  #read the content of the file into variable story
  all=readLines(path[i],warn=F,encoding = "UTF-8")
  all=gsub("\ufb02","fl",all)      #transform latin letter ASCII:"\ufb02" to valid "fl"
  all=gsub("\ufb01","fi",all)      #transform latin letter ASCII:"\ufb01" to valid "fi"
  story_name[i]=all[1]     #get the story name
  author[i]=all[2]     #get the author
  story[i]=paste(all[-c(1,2)],collapse="")
  #get letters' probability
  i_letters=getLetters(story[i])
  i_counts=table(i_letters)
  i_prob=prop.table(i_counts)
  prob=as.vector(i_prob)
  #calculate the entropy of the file
  H[i]=entropy(i_prob)
}

#d.Display a table with 3 columns: the name of the story, the author, and the entropy.
library(knitr)
author<-gsub("by ","",author)
Summary<-data.frame("StoryName"=story_name,"Author"=author,"Entropy"=H,stringsAsFactors = F)
kable(Summary,format="rst")

#e.Calculate the Shannon entropy of a text which consists of characters drawn uniformly from the English alphabet (a-z)
text<-letters   #get the text drawn uniformly from the English alphabet
text_prob<-rep(0,length(text))    #set initial probabilities
text_letters<-getLetters(text) #seperate the text into letters
#get each character's probability
for (i in 1:26){
  text_prob[i]=length(grep(letters[i],text_letters,value=T))/length(text_letters)
}

entropy(text_prob)   #Calculate the Shannon entropy

#Comment on my found in part d
#Summary of the Entropy in part d
print(summary(H))
#I found that the entropy of a story is around -4.17, and the entropies of the ten stories are close to each other. All the entropies of the stories are smaller than the one of alphabet, so with the same length of the probability vector, different probabilities of different letters would smaller the entropy.


#f.Plot of the probability of each letter in Oscar Wilde
#get the row number of Oscar Wilde
i=match("Oscar Wilde",author)
#get letters' probabilities in a table i_prob, sort descendingly
i_letters=getLetters(story[i])
i_counts=table(i_letters)
i_prob=prop.table(i_counts)
i_prob=i_prob[order(i_prob,decreasing = T)]
#draw the plot
library(RColorBrewer)
opar <- par(no.readonly=TRUE)  # saves a copy of the current settings into a variable called "opar".
par(bg="grey97",cex.main=1.5,cex.lab=1.2)
barplot(i_prob,ylim=c(0,max(i_prob)+0.02),
        col=rev(rep(brewer.pal(9,'YlOrRd'),each=3)),
        ylab="Probabilities",xlab="Letters"
        )
title(paste('Probabilities of letters\n\"',story_name[i],'\"\nby ',author[i]))
text(seq(0.7,length.out=26,by=1.21),i_prob+0.01,label=round(i_prob,2),cex=.7,font=3)  
ruler<-max(i_prob)%/%0.02
abline(h=seq(0,ruler*0.02,0.02),lty=3,col="brown ")
abline(h=0)
par(opar)   # restore the original settings








