cname <- "C:/Users/comp/Desktop/Project/txtFiles/"
qa_sections <- VCorpus(DirSource(cname))
inspect(qa_sections[1]) 
dir(cname) 
class(qa_sections) 
class(qa_sections[[1]])
summary(qa_sections) 
install.packages("magrittr")
library(magrittr)
viewDocs <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()} 
viewDocs(qa_sections, 3)
install.packages("readr")
library(readr)
mystring <- read_file("C:/Users/comp/Desktop/Project/txtFiles/2003.txt")

qa_sections <- tm_map(qa_sections, content_transformer(tolower))
qa_sections <- tm_map(qa_sections, removeNumbers)
qa_sections <- tm_map(qa_sections, removeWords, stopwords("english"))
viewDocs(qa_sections, 1) 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
qa_sections <- tm_map(qa_sections, toSpace, "/|<|>|”|=|@|\\|:|;|-|\"")
viewDocs(qa_sections, 1) 

qa_sections <- tm_map(qa_sections, stripWhitespace)
viewDocs(qa_sections, 1)
qa_sections <- tm_map(qa_sections, removePunctuation)

qa_sections <- qa_sections[qa_sections != ""]
qa_sections <- tm_map(qa_sections, removeWords, c("b", "q", "a", "i", "e", "font",
                                                  "style","n","trim","size","font", "can", "also", "e", "mail", "via", "td", "align","border",
                                                  "familytimes", "roman", "p", "tr", "nbsp", "with", "table", "cellspacing", "valign", "cellpadding",
                                                  "width", "top", "left", "sizepx", "telephone", "if", "may", "help", "us", "will", "please", "unless",
                                                  "visit", "thnbsp","toppx","bottompx", "nnn", "address", "nonbsp", "new", "bottom", "em", "&nbsp", "ffffff", "div", "br", "margin", "right", "text", "indent", "\n"))

viewDocs(qa_sections, 1) 

qa_sections <- tm_map(qa_sections, stripWhitespace)
viewDocs(qa_sections, 1)


install.packages("SnowballC") 
qa_sections <-tm_map(qa_sections, stemDocument)
viewDocs(qa_sections, 1)

qa_sections <-tm_map(qa_sections, PlainTextDocument) 
qa_dtm <- DocumentTermMatrix(qa_sections)
qa_dtm
freq <- colSums((as.matrix(qa_dtm)))
freq
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 15)
tail(table(freq), 15)
m<-as.matrix(qa_dtm)
dim(m) 
write.csv(m,file= "C:/Users/comp/Desktop/Project/qa_dtm.csv")

dtms <- removeSparseTerms(qa_dtm, 0.1)
dim(dtms)
inspect(dtms)
freq <-colSums(as.matrix(dtms))
freq
table(freq) 
findFreqTerms(dtms, lowfreq=50) 
findFreqTerms(dtms, lowfreq=100) 
findAssocs(dtms, "cola", corlimit = .9) 
freq <- sort(colSums(as.matrix(dtms)), decreasing = TRUE) 
head(freq, 14) 
wf <- data.frame(word=names(freq), freq=freq)
head(wf) 
install.packages("ggplot2")
library(ggplot2)
subset(wf, freq>200) %>% ggplot(aes(word, freq))
install.packages("dplyr")
library(dplyr)
p<-subset(wf, freq>200) %>% ggplot(aes(word, freq))

p<- p+ geom_bar(stat="identity")

p <- p+ theme(axis.text.x=element_text(angle=45,face="bold",hjust=1))
p
install.packages("wordcloud")
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=50, colors = brewer.pal(8,"Dark2"))                              
            
                                 
