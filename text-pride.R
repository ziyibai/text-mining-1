library(tnum)
library(tidyverse)
library(magrittr)
library(ggplot2)

tnum.authorize()

test <- tnum.getDatabasePhraseList("subject", levels = 3)
test


# Count word count in this book
num1 <- tnum.query("*pride* has count:word")
num1

# Get sentences with word "rich" in it, save it to a dataframe
rich <- tnum.query("*pride* has text = REGEXP(\"rich\")", max =100)
richdf <- tnum.objectsToDf(rich)

married <- tnum.query("*pride* has text = REGEXP(\"married\") ", max = 100 )
marriedDf <- tnum.objectsToDf(married)

# Plot a relation graph of where "rich" words are in this book
picRich <- tnum.makePhraseGraphFromPathList(richdf$subject)
tnum.plotGraph(picRich)

picMarried <- tnum.makePhraseGraphFromPathList(marriedDf$subject)
tnum.plotGraph(picMarried)

# tag sentences with "rich" and  draw a bar plot of distribution
tnum.tagByQuery("*pride* has text = REGEXP(\"rich\")", "reference:rich")
rich_2 <- tnum.query("@reference:rich", max=100)
richdf_2 <- tnum.objectsToDf(rich_2)
richdf_2$subject <- str_split(richdf_2$subject, " ", simplify = T)
richdf_2 %<>% separate(subject,, into=c("turenum1", "turenum2","turenum","turenum4","turenum5","chapter", "Chapter"))
ggplot(data=richdf_2)+
  geom_bar(aes(x=richdf_2$Chapter))+
  labs(x="Chapter", title="Distribution of Word: RICH")


# see sentences where Darcy and Elizabeth co-occur
Darcy <- tnum.query("*pride* has text = REGEXP(\"Darcy\") ", max = 1000 )
DarcyDf <- tnum.objectsToDf(Darcy)
DarcyDf2<- data.frame(subject=rep(NA, 74), string.value=rep(NA, 74))
j<- 1
for (i in 1:394){
  if (str_detect(DarcyDf$string.value[i], "Elizabeth")){
    DarcyDf2$subject[j] <- DarcyDf$subject[i]
    DarcyDf2$string.value[j] <- DarcyDf$string.value[i]
    j=j+1
  }
}
picDarcy <- tnum.makePhraseGraphFromPathList(DarcyDf2$subject)
tnum.plotGraph(picDarcy) # Plot a relation graph

# Bar plot the Distribution of Sentences Where Darcy And Elizabeth Co-occur
DarcyDf3 <- DarcyDf2
DarcyDf3$subject <- str_split(DarcyDf3$subject, " ", simplify = T)
DarcyDf3 %<>% separate(subject,, into=c("turenum1", "turenum2","turenum","turenum4","turenum5","chapter", "Chapter"))
ggplot(data=DarcyDf3)+
  geom_bar(aes(x=DarcyDf3$Chapter))+
  labs(x="Chapter", title="Distribution of Sentences Where Darcy And Elizabeth Co-occur")
