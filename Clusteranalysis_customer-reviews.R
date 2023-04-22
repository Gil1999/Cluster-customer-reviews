#Link to data: https://www.kaggle.com/datasets/shitalkat/amazonearphonesreviews

rm(list = ls())

library('tm')
library('koRpus')
library('tidyEmoji')
library('tidytext')
library('dplyr')
library('tokenizers')
library('wordcloud')
library('NbClust')
library('cluster')
library('factoextra')
library('textclean')
library('syuzhet')


dat <- read.csv('')
#Filter the record so that it only contains reviews for the product "boAt Rockerz 255".
dat <- dat[dat$Product == 'boAt Rockerz 255',]
#Setting a seed for the replicability of the sample
set.seed(107173)
#Drawing a sample of 500 reviews from the filtered dataset.
dat <- dat[sample(nrow(dat),500),]
#Delete all empty documents 
dat <- na.omit(dat)
text_df <-  data.frame(doc_id = rownames(dat), title = dat$ReviewTitle,text = dat$ReviewBody)
#Performing an initial emoji analysis, as these are to be transformed in pre-processing.
top_n_emojis(text_df, text, n = 20, duplicated_unicode = "no")
emoji_summary(text_df, text)

#Create the corpus
corpus.prepro <- Corpus(VectorSource(text_df$text))
#Clean up unnecessary spaces
corpus.prepro <- tm_map(corpus.prepro,content_transformer(stripWhitespace))
#Unification in small letters
corpus.prepro <- tm_map(corpus.prepro,content_transformer(tolower))
#Replace emojis with matching word equivalents
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_emoji))
#Replace emoticon with matching word equivalents
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_emoticon))
#Cleanup all non-ascii characters
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_non_ascii))
#Clean all special characters (during pre-processing, it was found that the data set still contained
#that after the previous cleanup step still special characters were contained, therefore 
#uncovered special characters could be removed with this step)
corpus.prepro <- tm_map(corpus.prepro, content_transformer(function(x) gsub(x, pattern = "[^[:alnum:]]", replacement = " ")))
#Remove figures
corpus.prepro <- tm_map(corpus.prepro,content_transformer(removeNumbers))
#Clean up pointuations
corpus.prepro <- tm_map(corpus.prepro,removePunctuation)

#Standardize negations
negation_df <- data.frame(doc_id = 1:length(sapply(corpus.prepro,as.character)),
                          text = sapply(corpus.prepro,as.character))

#Create dictionary with negations to be standardized
#Include spelling errors (without ')
dictionary.neg <- c("ain't ","aren't ","can't ","couldn't ","didn't ",
                    "doesn't ","don't ","hasn't ","isn't ","mightn't ",
                    "mustn't ", "neither ","never ","no ","nobody ","nor ",      
                    "not ","shan't ","shouldn't ","wasn't ","weren't ","won't ",
                    "wouldn't ",
                    'aint ', 'arent ','cant ','couldnt ','didnt ','doesnt ',
                    'dont ','hasnt ','isnt ','mightnt ','mustnt ','shant ',
                    'shouldnt ','wasnt ','werent ','wont ','wouldnt ')

#Create an empty vector in which the Doc_ID's and the respective docs are to be inserted
#with standardized negations are to be inserted
vec_array <- array(
  dim = c(nrow(negation_df),2),
  dimnames = list(NULL, c('doc_ID','text'))
)

for (k in 1:nrow(negation_df)){
  vec1 <- negation_df$text[k]
   #tokenize document k from negation_df 
  vec1 <- tokenize_words(vec1)
  vec1 <- unlist(vec1)
  vec1 <- paste(vec1, "")
  for(i in 1:length(vec1)){
    for (n in 1:length(dictionary.neg)){
      #Match the token i with every word from the created dictionary
      #if the word is contained and is not "never" (strong negation)
      #then replace it with "not_".
      if (vec1[i] == dictionary.neg[n]){
        if (dictionary.neg[n] != "never "){
        vec1[i] <- gsub(vec1[i],"not_",vec1[i])
        print(paste(c("Negation -",dictionary.neg[n],"-" ,"in row",k,"swapped with - not_ -"), collapse = " "))
        vec_array[k,'doc_ID'] <- k
        vec1[i] <- gsub(" ","",paste(vec1[i],vec1[i+1]))
        vec1[i+1] <- ""
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
        }
        else {
        vec1[i] <- gsub(vec1[i],"never_",vec1[i])
        print(paste(c("Negation -",dictionary.neg[n],"-" ,"in row",k,"swapped with - never_ -"), collapse = " "))
        vec_array[k,'doc_ID'] <- k
        vec1[i] <- gsub(" ","",paste(vec1[i],vec1[i+1]))
        vec1[i+1] <- ""
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
        }
      }
      else {
        vec_array[k,'doc_ID'] <- k
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
      }
    }
  }
}

text_df_changed_negations <- data.frame(vec_array)
names(text_df_changed_negations)[names(text_df_changed_negations) == 'data.doc_ID'] <-'doc_ID' 
names(text_df_changed_negations)[names(text_df_changed_negations) == 'data.text'] <-'text' 

stopwords <- c(stopwords('SMART'),'ago','totally','lot','erience','ected','upto','boat rockerz', 'boat')
#Clean documents from stop words
text_df_changed_negations$text <- removeWords(text_df_changed_negations$text, stopwords)
#Remove unnecessary spaces again
text_df_changed_negations$text<- stripWhitespace(text_df_changed_negations$text)
#Remove empty documents if present
text_df_changed_negations <- text_df_changed_negations[text_df_changed_negations$text != "",]
text_df_cleaned <- text_df_changed_negations
#Create Corpus and TDM to use a wordcloud to #analyze pre-processing results to analyze
corpus<- Corpus(VectorSource(text_df_cleaned$text))
tdm.wordcloud <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm.wordcloud)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df1 <- data.frame(word = names(words),freq=words)
wordcloud(words = df1$word, freq = df1$freq, min.freq = 2,           
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

export_frequent_words <- function(df, lower_freq_threshold){
  corpus<- Corpus(VectorSource(df))
  tdm.wordcloud <- TermDocumentMatrix(corpus)
  matrix <- as.matrix(tdm.wordcloud)
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df_new <- data.frame(word = names(words),freq=words)
  df_new <- df_new[df_new$freq >= lower_freq_threshold,]
  rownames(df_new) <- 1:nrow(df_new)
  return(df_new)
}

export_frequent_words(text_df_cleaned$text, 2)
#Abspeicherung der Wörterliste mit Fequenzen
write.csv(export_frequent_words(text_df_cleaned$text, 2),
          "Your_path.csv", 
          row.names=FALSE)
                                                           
dtm_bow <- DocumentTermMatrix(corpus)

#Remove words that do not appear in more than 2% of documents                                                        
dtm_bow_98 <- removeSparseTerms(dtm_bow,0.98)

#Remove words that do not appear in more than 4% of documents                                                          
dtm_bow_96 <- removeSparseTerms(dtm_bow,0.96)

#Remove words that do not appear in more than 6% of documents                                                          
dtm_bow_94 <- removeSparseTerms(dtm_bow,0.94)
                                                           
#Remove words that do not appear in more than 8% of documents                                                          
dtm_bow_92 <- removeSparseTerms(dtm_bow,0.92)

#Remove words that do not appear in more than 10% of documents                                                                                                                 
dtm_bow_90 <- removeSparseTerms(dtm_bow,0.90)

#Transform respective matrices - TD-IDF values
dtm_tfidf <- weightTfIdf(dtm_bow)
dtm_tfidf_98 <- weightTfIdf(dtm_bow_98)
dtm_tfidf_96 <- weightTfIdf(dtm_bow_96)
dtm_tfidf_94 <- weightTfIdf(dtm_bow_94)
dtm_tfidf_92 <- weightTfIdf(dtm_bow_92)
dtm_tfidf_90 <- weightTfIdf(dtm_bow_90)

dtm_tfidf_matrix <- as.matrix(dtm_tfidf)
dtm_tfidf_matrix_98 <- as.matrix(dtm_tfidf_98)
dtm_tfidf_matrix_96 <- as.matrix(dtm_tfidf_96)
dtm_tfidf_matrix_94 <- as.matrix(dtm_tfidf_94)
dtm_tfidf_matrix_92 <- as.matrix(dtm_tfidf_92)
dtm_tfidf_matrix_90 <- as.matrix(dtm_tfidf_90)

#Create distance matrices for the Euclidean distance and the respective sparsity levels
distMatrix_tfidf_euclidean <- dist(dtm_tfidf_matrix, method = 'euclidean')
distMatrix_tfidf_euclidean_98 <- dist(dtm_tfidf_matrix_98, method = 'euclidean')
distMatrix_tfidf_euclidean_96 <- dist(dtm_tfidf_matrix_96, method = 'euclidean')
distMatrix_tfidf_euclidean_94 <- dist(dtm_tfidf_matrix_94, method = 'euclidean')
distMatrix_tfidf_euclidean_92 <- dist(dtm_tfidf_matrix_92, method = 'euclidean')
distMatrix_tfidf_euclidean_90 <- dist(dtm_tfidf_matrix_90, method = 'euclidean')

detect_optimal_ncluster <- function(dtm_matrix,distMatrix,method,kmax){
  
  fviz_nbclust(dtm_matrix, 
               FUN = hcut, 
               diss = distMatrix, 
               method = method,
               k.max = kmax)
  
}

#Create graph for elbow criterion for output matrix
detect_optimal_ncluster(dtm_tfidf_matrix,distMatrix_tfidf_euclidean , "wss", 20)
#Erstelle Graph für Silhouetten-Kriterium für Ausgangsmatrix
detect_optimal_ncluster(dtm_tfidf_matrix,distMatrix_tfidf_euclidean, "silhouette", 20)

#Create respective graphs for the different sparsity levels
detect_optimal_ncluster(dtm_tfidf_matrix_98, distMatrix_tfidf_euclidean_98, "wss", 20)
detect_optimal_ncluster(dtm_tfidf_matrix_98, distMatrix_tfidf_euclidean_98, "silhouette", 20)

detect_optimal_ncluster(dtm_tfidf_matrix_96, distMatrix_tfidf_euclidean_96, "wss", 20)
detect_optimal_ncluster(dtm_tfidf_matrix_96, distMatrix_tfidf_euclidean_96, "silhouette", 20)

detect_optimal_ncluster(dtm_tfidf_matrix_94, distMatrix_tfidf_euclidean_94, "wss", 20)
detect_optimal_ncluster(dtm_tfidf_matrix_94, distMatrix_tfidf_euclidean_94, "silhouette", 20)

detect_optimal_ncluster(dtm_tfidf_matrix_92, distMatrix_tfidf_euclidean_92, "wss", 20)
detect_optimal_ncluster(dtm_tfidf_matrix_92, distMatrix_tfidf_euclidean_92, "silhouette", 20)

detect_optimal_ncluster(dtm_tfidf_matrix_90, distMatrix_tfidf_euclidean_90, "wss", 20)
detect_optimal_ncluster(dtm_tfidf_matrix_90, distMatrix_tfidf_euclidean_90, "silhouette", 20)

#Create function clusterhc that performs an agglomerative cluster analysis from a given distance matrix and method cluster analysis
clusterhc <- function(distMatrix,method){
  return(hclust(distMatrix, method = method))
}

#Create a function that returns the cluster assignments per document
generate_labels <- function(clusterhc,num_cluster){
  return(cutree(clusterhc, k = num_cluster))
}

#Create a function that adds the cleaned output dataset text_df_cleaned by the column with the respective cluster numbers
generate_df <- function(labels){
  return(mutate(text_df_cleaned, cluster = labels))
}

#Create a function with which a dendrogram can be visualized
plot_dendogram<- function(dist_matrix_input, method){
  
  cluster <- hclust(dist_matrix_input, method = method)
  plot(cluster, hang = -1, cex=0.9)
}

#compare clusters created with single-linkage method for the appropriate respective sparsity level and the respective optimal number of clusters
generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'single'),3))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'single'),3)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'single'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'single'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'single'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'single'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'single'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'single'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'single'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'single'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'single'),11))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'single'),11)),cluster),cluster)

cophematrix_single_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'single'))
cophematrix_single_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'single'))
cophematrix_single_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'single'))
cophematrix_single_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'single'))
cophematrix_single_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'single'))
cophematrix_single_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'single'))

#Calculate correlation coefficients from cophenetic matrix and output distance matrix.
#The higher the correlation, the better the distance method fits and the better the
#Dendrogram represents the actual distances
cor(distMatrix_tfidf_euclidean,cophematrix_single_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_single_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_single_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_single_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_single_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_single_tfidf_90)

plot_dendogram(distMatrix_tfidf_euclidean,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'single'), k = 3, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_98,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'single'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_96,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'single'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_94,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'single'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_92,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'single'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_90,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'single'), k = 11, border = 2:5)

#compare clusters created with average linkage method for the #respective 
#respective sparsity level and the respective optimal number of clusters
generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'average'),3))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'average'),3)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'average'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'average'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'average'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'average'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'average'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'average'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'average'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'average'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'average'),11))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'average'),11)),cluster),cluster)

cophematrix_average_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'average'))
cophematrix_average_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'average'))
cophematrix_average_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'average'))
cophematrix_average_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'average'))
cophematrix_average_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'average'))
cophematrix_average_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'average'))

cor(distMatrix_tfidf_euclidean,cophematrix_average_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_average_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_average_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_average_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_average_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_average_tfidf_90)

plot_dendogram(distMatrix_tfidf_euclidean,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'average'), k = 3, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_98,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'average'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_96,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'average'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_94,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'average'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_92,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'average'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_90,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'average'), k = 11, border = 2:5)

#compare clusters created with complete linkage method for the respective sparsity level and the respective optimal number of clusters
generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'complete'),3))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'complete'),3)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'complete'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_98,'complete'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'complete'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_96,'complete'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'complete'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_94,'complete'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'complete'),2))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_92,'complete'),2)),cluster),cluster)

generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'complete'),11))
count(group_by(generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'complete'),11)),cluster),cluster)

cophematrix_complete_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'complete'))
cophematrix_complete_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'complete'))
cophematrix_complete_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'complete'))
cophematrix_complete_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'complete'))
cophematrix_complete_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'complete'))
cophematrix_complete_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'complete'))

cor(distMatrix_tfidf_euclidean,cophematrix_complete_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_complete_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_complete_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_complete_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_complete_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_complete_tfidf_90)

plot_dendogram(distMatrix_tfidf_euclidean,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'complete'), k = 3, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_98,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'complete'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_96,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'complete'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_94,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'complete'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_92,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'complete'), k = 2, border = 2:5)

plot_dendogram(distMatrix_tfidf_euclidean_90,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'complete'), k = 11, border = 2:5)

#Create function that plots the words with the highest frequency per cluster
print_cluster_freqanalysis<- function(df,cluster,max_words){
  
  df <- df[as.numeric(df$cluster) == cluster,]
  corpus_wordcloud_cluster <- Corpus(VectorSource(df$text))
  tdm_wordcloud_cluster <- TermDocumentMatrix(corpus_wordcloud_cluster)
  tdm_matrix_cluster <- as.matrix(tdm_wordcloud_cluster)
  words_cluster <- sort(rowSums(tdm_matrix_cluster),decreasing=TRUE) 
  df_wordcloud_cluster <- data.frame(word = names(words_cluster),freq=words_cluster)
  barplot(words_cluster[1:max_words],las = 2, main = c('Cluster:',cluster))
  par(mar=c(6,7,4,2)+.1)
}

#Choose average linkage distance method with 11 clusters and sparsity of 90% (see report for justification)
average_df_90 <- generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'average'),11))
par(mfrow = c(4,3))
for (i in 1:11){
  print_cluster_freqanalysis(
    average_df_90,
    i,
    50)
}
par(mfrow = c(1,1))

#assign the respective NRC sentiments/emotions to the text from target dataframe
d <- get_nrc_sentiment(average_df_90$text)
#Append created vectors to the target dataframe
average_df_90 <- cbind(average_df_90,d)
#Take only the NRC columns and the respective cluster number
df1_sentiment <- average_df_90[,3:ncol(average_df_90)]
df2_sentiment<- split(df1_sentiment, f=df1_sentiment$cluster)


colnames <- c('cluster',"anger","anticipation","disgust","fear","joy","sadness"
              ,"surprise","trust","negative","positive") 

par(mfrow = c(4,3))
for (i in 1:length(df2_sentiment)){
  
  df <- df2_sentiment[i]
  df <- data.frame(df)
  colnames(df) <- colnames
  
  matrix <- as.matrix(df)

  tdf <- data.frame(t(matrix))
  str(tdf)
  tdf_new <- data.frame(count = rowSums(tdf))

  tdf_new <- cbind("sentiment" = rownames(tdf_new), tdf_new)
  rownames(tdf_new) <- NULL
  
  tdf_new <- tdf_new[-1,]
  tdf_new <- tdf_new[order(-tdf_new$count),]
  tdf_new2<-tdf_new[1:5,]
  
  barplot(tdf_new2$count,las = 2, main = c('Cluster:',i), ylab='count', names = tdf_new2$sentiment)
}
par(mfrow = c(1,1))

syuzhet_vector <- get_sentiment(average_df_90$text, method="syuzhet")
average_df_90$sentiment_score <- syuzhet_vector
sentiment_scores <- average_df_90$sentiment_score
df_sentiment_scores <- data.frame(cluster=average_df_90$cluster,sentiment_score=average_df_90$sentiment_score)
df_sentiment_scores_2 <- split(df_sentiment_scores, f=df_sentiment_scores$cluster)
hist(sentiment_scores)

par(mfrow = c(3,4))
for (i in 1:length(df_sentiment_scores_2)){
  
  df <- df_sentiment_scores_2[i]
  df_new <- data.frame(df)
  colnames(df_new) <- c('cluster','sentiment_score')
  sentiment_score_per_cluster <- df_new$sentiment_score
  hist(sentiment_score_per_cluster,
       main = c('Cluster:',i),
       ylab='score')
}
par(mfrow = c(1,1))
