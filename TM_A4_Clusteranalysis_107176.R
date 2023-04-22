#Link to data: https://www.kaggle.com/datasets/shitalkat/amazonearphonesreviews

rm(list = ls())

#Laden der benötigten Packages
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


##################### - Laden des Datensatzes - ################################

dat <- read.csv('')
#Filtern des Datensatzes, sodass er nur noch Rezenssionen für das Produkt "boAt Rockerz 255" enthält
dat <- dat[dat$Product == 'boAt Rockerz 255',]
#Setzen eines seeds zur Replizierbarkeit des Samples
set.seed(107173)
#Ziehen eines Samples mit 500 Rezenssionen aus dem gefilterten Datensatzes
dat <- dat[sample(nrow(dat),500),]
#Löschen aller leeren Dokumente (keine enthalten)
dat <- na.omit(dat)
text_df <-  data.frame(doc_id = rownames(dat), title = dat$ReviewTitle,text = dat$ReviewBody)
#Durchführen einer ersten Emoji-Analyse, da diese im Pre-processing transformiert werden sollen
top_n_emojis(text_df, text, n = 20, duplicated_unicode = "no")
emoji_summary(text_df, text)

##################### - Durchführung Pre-processing - ##########################

#Erstellen des Corpus
corpus.prepro <- Corpus(VectorSource(text_df$text))
#Bereinigung  von überflüssigen Leerzeichen
corpus.prepro <- tm_map(corpus.prepro,content_transformer(stripWhitespace))
#Vereinheitlichung in Kleinschreibweise
corpus.prepro <- tm_map(corpus.prepro,content_transformer(tolower))
#Ersetzen von Emojis mit passenden Wortequivalenten
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_emoji))
#Ersetzen von Emoticon mit passenden Wortequivalenten
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_emoticon))
#Bereinigung aller Nicht-Ascii-Zeichen
corpus.prepro <- tm_map(corpus.prepro,content_transformer(replace_non_ascii))
#Bereiniguen aller Sonderzeichen (im Laufe des Pre-processings wurde in Bezug auf den Datensatz
#festgestelllt, das nach dem vorherigen Bereinigungsschritt noch Sonderzeichen enthalten waren, deswegen werden 
#nicht abgedeckte Sonderzeichen konnten mit diesem Schritt entfernt werden)
corpus.prepro <- tm_map(corpus.prepro, content_transformer(function(x) gsub(x, pattern = "[^[:alnum:]]", replacement = " ")))
#Bereinge Zahlen
corpus.prepro <- tm_map(corpus.prepro,content_transformer(removeNumbers))
#Bereinige Punktuationen
corpus.prepro <- tm_map(corpus.prepro,removePunctuation)

#Standardisiere Negationen
negation_df <- data.frame(doc_id = 1:length(sapply(corpus.prepro,as.character)),
                          text = sapply(corpus.prepro,as.character))

#Erstelle Wörterbuch mit Negationen die Standardisiert werden sollen
#Beziehe Rechtschreibfehler (ohne ') mit ein
dictionary.neg <- c("ain't ","aren't ","can't ","couldn't ","didn't ",
                    "doesn't ","don't ","hasn't ","isn't ","mightn't ",
                    "mustn't ", "neither ","never ","no ","nobody ","nor ",      
                    "not ","shan't ","shouldn't ","wasn't ","weren't ","won't ",
                    "wouldn't ",
                    'aint ', 'arent ','cant ','couldnt ','didnt ','doesnt ',
                    'dont ','hasnt ','isnt ','mightnt ','mustnt ','shant ',
                    'shouldnt ','wasnt ','werent ','wont ','wouldnt ')

#Erstelle einen leeren Vektor in welchen die Doc_ID's und die jeweiligen Dokuemente
#mit standardisierten Negierungen eingefügt werden sollen
vec_array <- array(
  dim = c(nrow(negation_df),2),
  dimnames = list(NULL, c('doc_ID','text'))
)

for (k in 1:nrow(negation_df)){
  vec1 <- negation_df$text[k]
  #tokenisiere Dokument k aus negation_df 
  vec1 <- tokenize_words(vec1)
  vec1 <- unlist(vec1)
  vec1 <- paste(vec1, "")
  for(i in 1:length(vec1)){
    for (n in 1:length(dictionary.neg)){
      #Gleiche das Token i mit jedem Wort aus dem angelegten Wörterbuch ab
      #falls das Wort enthalten ist und nicht "never" ist (starke Negierung)
      #dann tausche es mit "not_" aus
      if (vec1[i] == dictionary.neg[n]){
        if (dictionary.neg[n] != "never "){
        vec1[i] <- gsub(vec1[i],"not_",vec1[i])
        print(paste(c("Negation -",dictionary.neg[n],"-" ,"in row",k,"swapped with - not_ -"), collapse = " "))
        #Füge die jeweilige Doc_ID zu vec_array
        vec_array[k,'doc_ID'] <- k
        #Füge not_ und das Folgewort zusammen
        vec1[i] <- gsub(" ","",paste(vec1[i],vec1[i+1]))
        #Entferne das Folgewort aus vec1 um Dopplung zu vermeiden
        vec1[i+1] <- ""
        #Füge vec1 als string zu vec_array
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
        }
        #wenn die Negierung "never " ist, dann tausche sie mit "never_" aus
        #und führe die gleichen Schritte wie für "not_" durch
        else {
        vec1[i] <- gsub(vec1[i],"never_",vec1[i])
        print(paste(c("Negation -",dictionary.neg[n],"-" ,"in row",k,"swapped with - never_ -"), collapse = " "))
        vec_array[k,'doc_ID'] <- k
        vec1[i] <- gsub(" ","",paste(vec1[i],vec1[i+1]))
        vec1[i+1] <- ""
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
        }
      }
      #Wenn Dokument keine Negierungen aus dem Wörterbuch enthält, dann füge 
      #Text zu vec_array hinzu
      else {
        vec_array[k,'doc_ID'] <- k
        vec_array[k,'text'] <- gsub(",","",toString(vec1))
      }
    }
  }
}

#Erstelle Dataframe aus vec_array
text_df_changed_negations <- data.frame(vec_array)
names(text_df_changed_negations)[names(text_df_changed_negations) == 'data.doc_ID'] <-'doc_ID' 
names(text_df_changed_negations)[names(text_df_changed_negations) == 'data.text'] <-'text' 
#text_df_changed_negations$doc_ID <- as.numeric(text_df_changed_negations$doc_ID)

#Erstelle Liste bestehend aus Stopwörtern
stopwords <- c(stopwords('SMART'),'ago','totally','lot','erience','ected','upto','boat rockerz', 'boat')
#Bereinige Dokumente um Stopwörter
text_df_changed_negations$text <- removeWords(text_df_changed_negations$text, stopwords)
#Entferne erneut überflüssige Leerzeichen
text_df_changed_negations$text<- stripWhitespace(text_df_changed_negations$text)
#rEntferne leere Dokumente falls vorhanden
text_df_changed_negations <- text_df_changed_negations[text_df_changed_negations$text != "",]
text_df_cleaned <- text_df_changed_negations
#Erstelle Corpus und TDM um mittels einer wordcloud die Pre-processing-Ergebnisse
#zu analysieren
corpus<- Corpus(VectorSource(text_df_cleaned$text))
tdm.wordcloud <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm.wordcloud)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df1 <- data.frame(word = names(words),freq=words)
wordcloud(words = df1$word, freq = df1$freq, min.freq = 2,           
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

#Implementierung der Funktion zur Aufgabe - Implement a function to obtain the cumulative frequencies of all words from the 
#text that have been mentioned at least twice over all texts from your corpus. 
#Save the results as a .csv file and add it to your submission file. -

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
          "/Users/gilbert/Documents/Wise22:23/Text Mining/A4/Wordfrequencies_exported.csv", 
          row.names=FALSE)

##################### - Durchführung Clusteranalyse - ##########################
dtm_bow <- DocumentTermMatrix(corpus)

dtm_bow_98 <- removeSparseTerms(dtm_bow,0.98)
#Entferne Wörter die nicht in mehr als 2% der Dokumente vorkommen

dtm_bow_96 <- removeSparseTerms(dtm_bow,0.96)
#Entferne Wörter die nicht in mehr als 4% der Dokumente vorkommen

dtm_bow_94 <- removeSparseTerms(dtm_bow,0.94)
#Entferne Wörter die nicht in mehr als 6% der Dokumente vorkommen

dtm_bow_92 <- removeSparseTerms(dtm_bow,0.92)
#Entferne Wörter die nicht in mehr als 8% der Dokumente vorkommen

dtm_bow_90 <- removeSparseTerms(dtm_bow,0.90)
#Entferne Wörter die nicht in mehr als 10% der Dokumente vorkommen

#Transformiere jeweilige Matrizen mit TD-IDF-Werten
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

#Erstelle-Distanzmatrizen für die euklidische Distanz und die jeweiligen Sparsity-Level
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

#Erstelle Graph für Ellenbogenkriterium für Ausgangsmatrix
detect_optimal_ncluster(dtm_tfidf_matrix,distMatrix_tfidf_euclidean , "wss", 20)
#Erstelle Graph für Silhouetten-Kriterium für Ausgangsmatrix
detect_optimal_ncluster(dtm_tfidf_matrix,distMatrix_tfidf_euclidean, "silhouette", 20)

#Erstelle jeweilige Graphen für die verschiedenen Sparsity-Level
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

#Erstelle Funktion clusterhc die aus einer jewieligen Distanzmatrix und -methode eine agglomerative
#Clusteranalyse durchführt
clusterhc <- function(distMatrix,method){
  return(hclust(distMatrix, method = method))
}

#Erstelle eine Funktion die die Clusterzuweisungen je Dokument wiedergibt
generate_labels <- function(clusterhc,num_cluster){
  return(cutree(clusterhc, k = num_cluster))
}

#Erstelle eine Funktion die den bereinigten Ausgangsdatensatz  
#text_df_cleaned um die Spalte mit den jeweiligen Clusternummern ergänzt
generate_df <- function(labels){
  return(mutate(text_df_cleaned, cluster = labels))
}

#Erstelle eine Funktion mit der ein Dendrogram visualisiert werden kann
plot_dendogram<- function(dist_matrix_input, method){
  
  cluster <- hclust(dist_matrix_input, method = method)
  plot(cluster, hang = -1, cex=0.9)
}

#vergleiche Cluster die mit Single-Linkage-Methode erstellt wurden für die 
#jeweiligen Sparsity level und der jeweiligen optimalen Anzahl an Clustern
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

#Erstelle kophenetische Matrix für verwendete Single-Methode und jeweiliges Sparsity-Level
cophematrix_single_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'single'))
cophematrix_single_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'single'))
cophematrix_single_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'single'))
cophematrix_single_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'single'))
cophematrix_single_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'single'))
cophematrix_single_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'single'))

#Berechne Korrelationskoeffizienten aus kophenetischer Matrix und Ausgangsdistanzmatrix
#Je höher Korrelation, desto besser passt die Distanzmethode und desto besser bildet das
#Dendrogram die tatsächlichen Distanzen ab
cor(distMatrix_tfidf_euclidean,cophematrix_single_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_single_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_single_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_single_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_single_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_single_tfidf_90)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'single'), k = 3, border = 2:5)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean_98,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'single'), k = 2, border = 2:5)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean_96,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'single'), k = 2, border = 2:5)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean_94,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'single'), k = 2, border = 2:5)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean_92,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'single'), k = 2, border = 2:5)

#Kettenbildung kann identifiziert werden
plot_dendogram(distMatrix_tfidf_euclidean_90,'single')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'single'), k = 11, border = 2:5)

#vergleiche Cluster die mit Average-Linkage-Methode erstellt wurden für die 
#jeweiligen Sparsity level und der jeweiligen optimalen Anzahl an Clustern
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

#Erstelle kophenetische Matrix für verwendete Average-Linkage-Methode und jeweiliges Sparsity-Level
cophematrix_average_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'average'))
cophematrix_average_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'average'))
cophematrix_average_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'average'))
cophematrix_average_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'average'))
cophematrix_average_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'average'))
cophematrix_average_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'average'))

#Berechne Korrelationskoeffizienten aus kophenetischer Matrix und Ausgangsdistanzmatrix
cor(distMatrix_tfidf_euclidean,cophematrix_average_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_average_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_average_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_average_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_average_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_average_tfidf_90)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'average'), k = 3, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_98,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'average'), k = 2, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_96,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'average'), k = 2, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_94,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'average'), k = 2, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_92,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'average'), k = 2, border = 2:5)

#Mäßige Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_90,'average')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'average'), k = 11, border = 2:5)

#vergleiche Cluster die mit Complete-Linkage-Methode erstellt wurden für die 
#jeweiligen Sparsity level und der jeweiligen optimalen Anzahl an Clusterngenerate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean,'complete'),3))
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

#Erstelle kophenetische Matrix für verwendete Complete-Linkage-Methode und jeweiliges Sparsity-Level
cophematrix_complete_tfidf <- cophenetic(clusterhc(distMatrix_tfidf_euclidean,'complete'))
cophematrix_complete_tfidf_98 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_98,'complete'))
cophematrix_complete_tfidf_96 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_96,'complete'))
cophematrix_complete_tfidf_94 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_94,'complete'))
cophematrix_complete_tfidf_92 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_92,'complete'))
cophematrix_complete_tfidf_90 <- cophenetic(clusterhc(distMatrix_tfidf_euclidean_90,'complete'))

#Berechne Korrelationskoeffizienten aus kophenetischer Matrix und Ausgangsdistanzmatrix
cor(distMatrix_tfidf_euclidean,cophematrix_complete_tfidf)
cor(distMatrix_tfidf_euclidean_98,cophematrix_complete_tfidf_98)
cor(distMatrix_tfidf_euclidean_96,cophematrix_complete_tfidf_96)
cor(distMatrix_tfidf_euclidean_94,cophematrix_complete_tfidf_94)
cor(distMatrix_tfidf_euclidean_92,cophematrix_complete_tfidf_92)
cor(distMatrix_tfidf_euclidean_90,cophematrix_complete_tfidf_90)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean,'complete'), k = 3, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_98,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_98,'complete'), k = 2, border = 2:5)

#Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_96,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_96,'complete'), k = 2, border = 2:5)

#Mäßige Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_94,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_94,'complete'), k = 2, border = 2:5)

#Mäßige Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_92,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_92,'complete'), k = 2, border = 2:5)

#Mäßige Kettenbildung
plot_dendogram(distMatrix_tfidf_euclidean_90,'complete')
rect.hclust(clusterhc(distMatrix_tfidf_euclidean_90,'complete'), k = 11, border = 2:5)

##################### - Deskriptive Analyse der Cluster - ######################

#Erstelle Funktion die die Wörter mit der höchsten Frequenz je Cluster plottet
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

#Wähle Average-Linkage-Distanz-Methode mit 11 Cluster und Sparsity von 90% (Begründung siehe Report)
average_df_90 <- generate_df(generate_labels(clusterhc(distMatrix_tfidf_euclidean_90,'average'),11))
par(mfrow = c(4,3))
for (i in 1:11){
  print_cluster_freqanalysis(
    average_df_90,
    i,
    50)
}
par(mfrow = c(1,1))

#Weise die jeweiligen NRC Sentimente/Emotionen dem Text aus Ziel-Dataframe zu
d <- get_nrc_sentiment(average_df_90$text)
#Hänge erstellte Vektoren an das Zieldataframe
average_df_90 <- cbind(average_df_90,d)
#Nimm nur die NRC-SPalten und die jeweilige Clusternummer
df1_sentiment <- average_df_90[,3:ncol(average_df_90)]
#Unterteile Dataframe in Gruppen, wobei jede Gruppe ein Cluster mit den 
#jweiligen Dokumenten und NRC-Werten enthält
df2_sentiment<- split(df1_sentiment, f=df1_sentiment$cluster)


colnames <- c('cluster',"anger","anticipation","disgust","fear","joy","sadness"
              ,"surprise","trust","negative","positive") 

#Plotte für jedes Cluster die Anzahl an vergebenen Sentimenten/Emotionen
#welche je Wort gezählt werden
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

#Plotte die Verteilung der Net-Sentiment-Scores für jedes Cluster
syuzhet_vector <- get_sentiment(average_df_90$text, method="syuzhet")
average_df_90$sentiment_score <- syuzhet_vector
sentiment_scores <- average_df_90$sentiment_score
df_sentiment_scores <- data.frame(cluster=average_df_90$cluster,sentiment_score=average_df_90$sentiment_score)
df_sentiment_scores_2 <- split(df_sentiment_scores, f=df_sentiment_scores$cluster)
#Plotte erst für gesamten bereinigten Datensatz
hist(sentiment_scores)

#Plotte Verteilung für jedes Cluster
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

