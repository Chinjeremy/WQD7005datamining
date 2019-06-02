# Install all required packages.
#install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))

# Load file from directory
stocknews=read.csv('edgenews_row_wise.csv',header=FALSE,stringsAsFactors = FALSE)
# Label column name
names(stocknews) <- c("News")
#Check any missing data
length(which(!complete.cases(stocknews)))
#add news category column
stocknews$NewsClass=NA
#categorize news function
checker <-function(positive){
  grepl(pattern="up", x=positive) |
    grepl(pattern="rises", x=positive)|
    grepl(pattern="rise", x=positive)|
    grepl(pattern="jump", x=positive)|
    grepl(pattern="jumps", x=positive)|
    grepl(pattern="advances", x=positive)|
    grepl(pattern="surges", x=positive)|
    grepl(pattern="surge", x=positive)|
    grepl(pattern="recovers", x=positive)|
    grepl(pattern="climb", x=positive)|
    grepl(pattern="appreciates", x=positive)|
    grepl(pattern="rebound", x=positive)|
    grepl(pattern="rebounds", x=positive)|
    grepl(pattern="gains", x=positive)
}
#categorize news
stocknews$NewsClass <- checker(stocknews$News)
#Rename news category to positive and negative
stocknews$NewsClass <- ifelse(stocknews$NewsClass == TRUE, "Positive", "Negative")
# Convert our class label into a factor.
stocknews$NewsClass <- as.factor(stocknews$NewsClass)
#Look at distribution
prop.table(table(stocknews$NewsClass))
#Look at distribution of text length
stocknews$TextLength <- nchar(stocknews$News)
summary(stocknews$TextLength)

# Visualize distribution with ggplot2, adding segmentation for ham/spam.
library(ggplot2)
ggplot(stocknews, aes(x = stocknews$TextLength, fill = NewsClass)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")

install.packages('udpipe')
library(udpipe)
model <- udpipe_download_model(language = "english")
model <- udpipe_load_model(model$file_model)
x <- udpipe_annotate(model, x = stocknews$News)
x <- as.data.frame(x)

library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring adjectives", xlab = "Freq")

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

install.packages('ggraph')
library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")

#correlation
x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)

#https://bnosac.github.io/udpipe/docs/doc5.html