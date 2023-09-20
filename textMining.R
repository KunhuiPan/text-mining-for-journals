#33660638
#### Loading useful packages and loading data #### 
library(dplyr)
library(tidytext)
library(textdata)
library(tm)
library(stringr)
library(wordcloud)
library(tidyverse)
library(topicmodels)
library(textstem)
library(ggpubr)
library(corrplot)
library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)
library(cluster)
library(factoextra)
library(fpc)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# Rread data set 
setwd("/Users/maoyubohe618/Documents/R/R_script/assignments/individual_final")
df <- read.csv("/Users/maoyubohe618/Documents/R/R_script/assignments/individual_final/original_data/journal_data.csv", header = TRUE)

dim(df) #4384*9
summary(df)
table(df$journal)



#### Pre-Processing ####
# Check the ratio of missing values and empty entries
lapply(df, function(x) any(is.na(x) | x == "" ))
# Drop rows with empty content of abstract
df <- df[!df$abstract == "", ]
lapply(df, function(x) any(is.na(x) | x == "" )) #4142*9


# Remove duplicates
df <- distinct(df)


# Convert character type into integer for variable "views" 
df$views <- gsub(",", "", df$views)
df$views <- as.integer(df$views)


# Rename journals
df["journal"][df["journal"] == "Journal of the Operational Research Society"] <- "Operational Research"
df["journal"][df["journal"] == "Journal of Simulation"] <- "Simulation"


# Put the abstract into a Tidy text format
# Remove punctuation and makes everything lower case
tidy_df<- df %>% 
  mutate(paper_number = row_number()) %>%
  unnest_tokens(word, abstract) 


# Word lemmatization
tidy_df$word <- lemmatize_words(tidy_df$word) 

# Remove digits and words with digits
tidy_df <- tidy_df %>%
  filter(!grepl("[[:digit:]]", word)) 

#  Remove stop words
data(stop_words)
tidy_df <- tidy_df %>% anti_join(stop_words, by = "word")



##### Word frequency #####
###### One word analysis ######
popular_words <- tidy_df %>%
  count(word, sort = TRUE) %>% 
  arrange(desc(n)) 


# Top 15 popular words of journals
popular_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  labs(title = "Common Words in Journals") +
  theme(plot.title = element_text(face = "bold", size = 14))

# Word cloud of journals
pal <- brewer.pal(8, "Set1")
set.seed(1234)
popular_words %>% 
  top_n(50) %>% 
  with(wordcloud(word, n, random.order = FALSE, colors = pal))





###### 2-gram analysis ######
# Split the abstract into two consecutive words
df_2gram <- df %>%
  mutate(paper_number = row_number()) %>%
  unnest_tokens(word, abstract, token = "ngrams", n = 2)


# Count the frequency of two-word phase
df_2gram %>% count(word, sort = TRUE) %>% 
  top_n(10)

# Remove stop words from data set
popular_words_bigram <- df_2gram %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) 
head(popular_words_bigram)


# Recombine words and find the most common bigram
popular_words_bigram_united <- popular_words_bigram %>% 
  unite(bigram, word1, word2, sep = " ")


# Plot top 15 word pair for whole data set
popular_words_bigram_united %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(15) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(fill = "#69b3a2") + 
  xlab(NULL) + 
  coord_flip() + 
  labs(title = "Common Two-Word Pairs in Journals") +
  theme(plot.title = element_text(face = "bold", size = 14))


# Popular word pair in different journals
popular_words_bigram_united %>% 
  group_by(journal) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, journal)) %>% 
  ggplot(aes(bigram, n, fill=journal)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~journal, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw()  +
  labs(title = "Top 15 Two-Word Pairs in Differnt Journals") +
  theme(plot.title = element_text(face = "bold", size = 14))


# Popular word pair over years(top 4)
popular_words_bigram_united %>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(4) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, year)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw() + 
  labs(title = "Top Four Word Pairs Over Years")+
  theme(plot.title = element_text(face = "bold", size = 14))

# Eliminate some words from plot and see are there new things occur?
uninformatic_stop_words <- data.frame(bigram = c("mo mo", "society 51", "society 52", "society 53",
                                                 "false mo", "mo stretchy","research society", "envelopment analysis",
                                                 "proposed model", "proposed method"))

popular_words_bigram_united_new <- popular_words_bigram_united %>% 
  anti_join(uninformatic_stop_words, by = "bigram")

# This plot tells us more information after deleting some stop words
popular_words_bigram_united_new %>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(4) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, year)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw()  +
  labs(title = "Top Four Word Pairs Over Years", subtitle = "Ranking by annual frequency")+
  theme(plot.title = element_text(face = "bold", size = 14))

# Above plots give us an overall look of three journals over different years
# Next we go further and see what are these themes in details in three journals.

###### Health System Research Contents ###### 
health_bigram <- popular_words_bigram_united_new[popular_words_bigram_united_new$journal=="Health Systems",] 

health_stop_bigram <- data.frame(bigram = c("health care", "health information", "health system",
                                           "health informatics", "health systems", "arm 1", "health related",
                                           "event simulation", "discrete event", "mrow mrow", "bcdr f03", 
                                           "f03 dataset", "hs sa", "data set", "style spent", "simulation methods" ))

health_bigram <- health_bigram %>% 
  anti_join(health_stop_bigram, by = "bigram")


health_bigram%>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(4) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, year)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw()  +
  labs(title = "Top Four Frequency of Bigram Over Years",
       subtitle	= "Ranking by annual frequency")+
  theme(plot.title = element_text(face = "bold", size = 14))

 
###### Operational Research Research Contents ###### 
operational_bigram <- popular_words_bigram_united_new[popular_words_bigram_united_new$journal=="Operational Research",] 

operational_stop_bigram <- data.frame(bigram = c("operational research", "decision makers", "simulation methods", 
                                                 "analysis dea", "computational results","computational experiments",
                                                 "proposed approach", "data envelopment", "programming model",
                                                 "mixed integer", "dea models", "data sets", "type 1", "type 2", "52 5",
                                                 "53 12", "np hard", "real world", "real life", "dea model", "paper describes",
                                                 "2 customer", "1 customer"))

operational_bigram <- operational_bigram %>% 
  anti_join(operational_stop_bigram, by = "bigram")

operational_bigram%>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, year)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw() + 
  labs(title = "Top Five Word Pairs Over Years",
       subtitle	= "Ranking by annual frequency")+
  theme(plot.title = element_text(face = "bold", size = 14))



###### Simulation  Research Contents ###### 
simulation_bigram <- popular_words_bigram_united_new[popular_words_bigram_united_new$journal=="Simulation",] 

simulation_stop_bigram <- data.frame(bigram = c("simulation models","simulation model","simulation optimization", 
                                                "simulation packages", "simulation study","based simulation", "based simulations", "based modelling", 
                                                "ab des", "ed visits", "ns 3"))

simulation_bigram <- simulation_bigram %>% 
  anti_join(simulation_stop_bigram, by = "bigram")

simulation_bigram%>% 
  group_by(year) %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  mutate(bigram = reorder_within(bigram, n, year)) %>% 
  ggplot(aes(bigram, n)) + 
  geom_col(show.legend = FALSE) + 
  xlab(NULL) + 
  facet_wrap(~year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+
  theme_bw() + 
  labs(title = "Top Five Word Pairs Over Years",
       subtitle	= "Ranking by annual frequency")+
  theme(plot.title = element_text(face = "bold", size = 14))




##### TF-IDF Analysis #####
# Supervised topic modelling with TF-IDF
# Using journal name and paper id as the index of doing TF-IDF analysis
tidy_df <- tidy_df %>% 
  mutate(pn=paper_number, abstract_id = paste(journal, pn, sep="_")) %>% 
  select(-pn)

# Informative words changed over past 9 years based on TF-IDF(from 2014 to 2022)
# as we pay more attention about journals in recent years of three journals

tf_idf_stop_words <- data.frame(word = c("mo", "palgrave.jors", "to:journal", "sarker", "balkhi's",
                                         "balkhi", "geneshan", "o'brien", "luo", "archary", "bonney", "bonneys",
                                         "cam", "log", "pm", "norta", "de", "des", "ed", "os", "da"))

journal_df <- tidy_df %>% anti_join(tf_idf_stop_words, by="word")


for (year in c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)){
  
  yearly_journal_words <- journal_df[tidy_df$year==year,]
  
  yearly_journal_tf_idf <- yearly_journal_words %>% 
    count(abstract_id, word, sort = TRUE) %>% 
    bind_tf_idf(word, abstract_id, n) 
  
  # Separate journal name and paper id so that we can do the analysis across different journals
  yearly_journal_tf_idf <- yearly_journal_tf_idf %>% 
    separate(abstract_id, c("journal", "paper_number"), sep = "_", convert = TRUE)
  
  tf_plot <- yearly_journal_tf_idf %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(journal) %>%
    top_n(15) %>%
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = journal)) +
    geom_col(show.legend = FALSE) +
    labs(x=NULL, y="TF-IDF") +
    facet_wrap(~journal, scales = "free") +
    coord_flip() +
    theme_bw() 
  
  print(tf_plot)
}





##### Topic Modelling #####


# Prepare topic modelling data set
# We can see the document number is dominated by operational and simulation, especially operational journal,
# To help us get a better topic distribution, we do under-sampling in operational journal in order to get a better
# data balanced data set.
table(df$journal)
# Deal with imbalanced data: under-sampling
# Shuffle the data set
shuffled_df_data <- df[sample(1:nrow(df)), ]

# Put all the health system data in a separate data set: minority_df_data 
minority_df_data <- shuffled_df_data[shuffled_df_data$journal %in% c("Health Systems"), ]

# Randomly select 200 observations from the majority class: Operational & Simulation
majority_operational_df_data <- shuffled_df_data %>% 
  filter(shuffled_df_data$journal== "Operational Research") %>% 
  sample_n(208)

majority_simulation_df_data <- shuffled_df_data %>% 
  filter(shuffled_df_data$journal== "Simulation") %>% 
  sample_n(208)

# Concatenate three data frames
df_data_balanced <- rbind(minority_df_data, majority_operational_df_data, majority_simulation_df_data)
table(df_data_balanced$journal)
# Here we get a balanced data set contains 208 abstracts of each journal




# Focus on time period from 2019 to 2022
modern_df <- df_data_balanced %>% 
  filter(year>=2019) %>% 
  mutate(paper_number = row_number()) %>%
  unnest_tokens(word, abstract)

# Word lemmatization
modern_df$word <- lemmatize_words(modern_df$word) 

# Remove digits and words with digits
modern_df <- modern_df %>%
  filter(!grepl("[[:digit:]]", word)) 

#  Remove stop words
data(stop_words)
tidy_modern_df <- modern_df %>% anti_join(stop_words, by = "word")


# Anti join some common used words before doing topic modelling since 
# they are less helpful to distinct topics vries journals
topic_stop_words <- popular_words %>% top_n(70)
topic_words <- tidy_modern_df %>% 
  anti_join(topic_stop_words, by="word") %>% 
  anti_join(tf_idf_stop_words, by="word")

# Set up LDA
for (year in c(2019, 2020, 2021, 2022)){
  
  yearly_topic_words <- topic_words[topic_words$year==year,]
  
  # Get document term matrix
  yearly_words <- yearly_topic_words %>% 
    count(paper_number, word, sort = TRUE)
    
  yearly_topic_dtm <- yearly_words %>%  
    cast_dtm(paper_number, word, n)
  
  # LDA set up, here we set 3 topics to do the research of topic distribution over different years
  yearly_topic_lda <- LDA(yearly_topic_dtm, k=3, control = list(seed=1234))
  yearly_topics <- tidy(yearly_topic_lda, matrix="beta")
  
  # Top topic words based on beta value
  yearly_top_topic_words <- yearly_topics %>% 
    group_by(topic) %>% 
    slice_max(beta, n=10) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  # Plot topics words for each year and see how these topics varies in different years
  yearly_topic_plot <- yearly_top_topic_words %>% 
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() + 
    theme_bw() 
  
  print(yearly_topic_plot)
}











###### Model 1-A ######
df_cluster <- df %>% select(year, pages, views, citations, altmetric, journal, abstract)
df_cluster2 <- df_cluster[which(df_cluster$pages <= 20 & df_cluster$views <= 750 &
                                  df_cluster$citations <= 15 & df_cluster$altmetric<=15), ]
reg_data <- df_cluster2[, !names(df_cluster2) %in% c("abstract")] 



# Model 1-A set up 
set.seed(456)
citations_TraSamples <- createDataPartition(reg_data$citations , p=0.8, list=FALSE)
citations_training_data <- reg_data[citations_TraSamples,]
citations_test_data <- reg_data[-citations_TraSamples,]

citations_reg1 <- lm(citations~., data = citations_training_data)
summary(citations_reg1)

# Model 1-A train set
pred1tr <- citations_reg1 %>% predict(citations_training_data)
RMSE1 <- RMSE(pred1tr, citations_training_data$citations)
R2_1<- R2(pred1tr, citations_training_data$citations)


# Model 1-A test set
pred1test <- citations_reg1 %>% predict(citations_test_data)
RMSE2 <- RMSE(pred1test, citations_test_data$citations)
R2_2<- R2(pred1test, citations_test_data$citations)






###### Model 2-A ######
# Square root transformation for variables
reg_data2 <- df_cluster2[, !names(df_cluster2) %in% c("abstract")] 
reg_data2[2:5] <- lapply(reg_data2[2:5], sqrt)

# Model 2-A set up
set.seed(456)
citations_TraSamples2 <- createDataPartition(reg_data2$citations , p=0.8, list=FALSE)
citations_training_data2 <- reg_data2[citations_TraSamples2,]
citations_test_data2 <- reg_data2[-citations_TraSamples2,]  

citations_reg2 <- lm(citations~., data = citations_training_data2)
summary(citations_reg2)

# Model 2-A train set
pred2tr <- citations_reg2 %>% predict(citations_training_data2)
RMSE3 <- RMSE(pred2tr, citations_training_data2$citations)
R2_3<- R2(pred2tr, citations_training_data2$citations)

# Model 2-A test set
pred2test <- citations_reg2 %>% predict(citations_test_data2)
RMSE4 <- RMSE(pred2test, citations_test_data2$citations)
R2_4 <- R2(pred2test, citations_test_data2$citations)



table <- matrix(c(RMSE1,RMSE2,R2_1,R2_2,RMSE3,RMSE4,R2_3,R2_4),ncol=4,byrow=TRUE)
colnames(table) <- c("RMSE Train"," RMSE Test","R2 Train"," R2 Test")
rownames(table) <- c("Model 1","Model 2")
RMSE_R2_table <- as.table(table)
RMSE_R2_table



##### Clustering ######
# Prepare numeric data set from original df
df_cluster <- df %>% select(year, pages, views, citations, altmetric, journal, abstract)


# Convert the type of journal variable as factor
df_cluster$journal <- as.factor(df_cluster$journal)
# Box plot to see the data distribution 
q1 <- ggplot(data=df_cluster) +
  geom_boxplot(mapping=aes(x=journal,y=year))
q2 <- ggplot(data=df_cluster)+
  geom_boxplot(mapping=aes(x=journal,y=pages))
q3 <- ggplot(data=df_cluster) +
  geom_boxplot(mapping=aes(x=journal,y=views))
q4 <- ggplot(data=df_cluster) +
  geom_boxplot(mapping=aes(x=journal,y=citations))
q5 <- ggplot(data=df_cluster) +
  geom_boxplot(mapping=aes(x=journal,y=altmetric))

g1 <- ggarrange(q1, q2, q3, q4, q5, 
                labels = c("A", "B", "C", "D", "E"),
                ncol = 3, nrow = 2) 
# We can see there are many outlines in pages, views, citations and altmetric. 
# This can be improved with the following code, where the pages are limited to 20, the views
# are limited to 2000, the citations are limited to 20 and the altmetric are limited to 15.
df_cluster2 <- df_cluster[which(df_cluster$pages <= 20 & df_cluster$views <= 750 &
                                  df_cluster$citations <= 15 & df_cluster$altmetric<=15), ]

# 529 observations are removed from data set, and the situation is improved
# We can see the result from below box plots


q6 <- ggplot(data=df_cluster2) +
  geom_boxplot(mapping=aes(x=journal,y=pages))
q7 <- ggplot(data=df_cluster2) +
  geom_boxplot(mapping=aes(x=journal,y=views))
q8 <- ggplot(data=df_cluster2) +
  geom_boxplot(mapping=aes(x=journal,y=citations))
q9 <- ggplot(data=df_cluster2) +
  geom_boxplot(mapping=aes(x=journal,y=altmetric))
g2 <- ggarrange(q1, q6, q7, q8, q9, 
                labels = c("A", "B", "C", "D", "E"),
                ncol = 3, nrow = 2) 



# Find the optimal number of clusters using average silhouette method with new data set
cluster_data <- df_cluster2[, !names(df_cluster2) %in% c("abstract", "journal")] 
# Get k number suggestion
fviz_nbclust(cluster_data, clara, method = "silhouette") + theme_classic()


cluster_plot <- function(data, k){
  clara_res <- clara(cluster_data, k, metric = "euclidean", stand = FALSE)
  
  k_plot <- fviz_cluster(clara_res,
                 palette = c("#00AFBB", "#FC4E07", "#00FF00"), # color palette
                 ellipse.type = "t", # Concentration ellipse
                 geom = "point", pointsize = 1,
                 ggtheme = theme_classic())
  print(k_plot)
  return(clara_res)
}    


# Set k=2 and k=3 respectively and see the result of clustering
clara_res2 <- cluster_plot(cluster_data, 2)
clara_res3 <- cluster_plot(cluster_data, 3)


# Append the cluster information and abstracts into the data set, we choose the k=3 version
dd <- cbind(df_cluster2, cluster = clara_res2$cluster)
    
# TF-IDF analysis
dd_words <- dd %>%unnest_tokens(word, abstract)
# Word lemmatization
dd_words$word <- lemmatize_words(dd_words$word) 
# Remove digits and words with digits
dd_words <- dd_words %>%filter(!grepl("[[:digit:]]", word)) 


# get tf-idf values of clusters
cluster_tf_idf <- dd_words %>%
      count(cluster, word, sort = TRUE) %>% 
      bind_tf_idf(word, cluster, n)

# cluster based on tf-idf visualization
cluster_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(cluster) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  labs(x=NULL, y="TF-IDF") +
  facet_wrap(~cluster, scales = "free") +
  coord_flip() +
  theme_bw()  


##### PCA on data clustering #####
# Standardize the data scale and implement PCA
dd_pca <- dd[,!names(dd) %in% c("cluster", "abstract", "journal")] %>% 
  prcomp(center = TRUE, scale = TRUE)
summary(dd_pca)

# Cumulative prob of at least 81% by PC1+PC2+PC3, thus the first three component will be used
dd_pca_transform <- as.data.frame(-dd_pca$x[,1:3])


# Find the optimal number of cluster, k=2 or 3
fviz_nbclust(dd_pca_transform, clara, method = "silhouette") + theme_classic()


clara_clara_transform <- clara(dd_pca_transform, 3, metric = "euclidean", stand = FALSE)
fviz_cluster(clara_clara_transform,
             palette = c("#00AFBB", "#FC4E07", "#00FF00"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())





##### Classification #####
# Prepare classification data set
classi_data <- df_cluster2[, !names(df_cluster2) %in% c("abstract")] 
table(classi_data$journal)

# Deal with imbalanced data: under-sampling method
# Shuffle the data set
shuffled_classi_data <- classi_data[sample(1:nrow(classi_data)), ]

# Put all the health system data in a separate data set: minority_clssi_data 
minority_clssi_data <- shuffled_classi_data[shuffled_classi_data$journal %in% c("Health Systems"), ]

# Randomly select 200 observations from the majority class: Operational & Simulation
majority_operational_classi_data <- shuffled_classi_data %>% 
  filter(shuffled_classi_data$journal== "Operational Research") %>% 
  sample_n(200)

majority_simulation_classi_data <- shuffled_classi_data %>% 
  filter(shuffled_classi_data$journal== "Simulation") %>% 
  sample_n(200)

# Concatenate three data frames
classi_data_balanced <- rbind(minority_clssi_data, majority_operational_classi_data, majority_simulation_classi_data)
table(classi_data_balanced$journal)
# Here we get a balanced data set contains 155 abstracts from health journal, and
# 200 abstracts from operational and simulation respectively




# CART
# Split journal data into training data set and test data
set.seed(234) 
journal_TraSamples <- createDataPartition(classi_data_balanced$journal, p=0.8, list=FALSE)
journal_training_data <- classi_data_balanced[journal_TraSamples,]
journal_test_data <- classi_data_balanced[-journal_TraSamples,]

journal_DecisionTree_CART <- rpart(journal~., data=journal_training_data, 
                                   method = "class", parms = list(split="gini"))
rpart.plot(journal_DecisionTree_CART, type=4, extra=101, nn=TRUE, 
           box.palette=list("pink", "lightblue", "gray"))

journal_pre_tra_cart <- table(predict(object = journal_DecisionTree_CART, 
                                      newdata = journal_test_data[,1:5], type="class"))

confusionMatrix(predict(object = journal_DecisionTree_CART,
                        newdata = journal_test_data[,1:5], type="class"), journal_test_data$journal)
# Accuracy: 47.75%




# Random Forest
set.seed(123)
journal_RandomForest <- randomForest(journal~., data = journal_training_data, ntree=800,
                                     mtry=3, importance = TRUE)

journal_RandomForest
plot(journal_RandomForest, main = "Error Plot")

varImpPlot(journal_RandomForest)
journal_RandomForest$importance

journal_pred_test_RandomForest <- predict(object = journal_RandomForest, newdata = journal_test_data[,1:5])
confusionMatrix(journal_pred_test_RandomForest,journal_test_data$journal)
#Accuracy : 48.65%







##### Finding pairwise counts and correlations #####
abstract_word_cooccurences <- tidy_df %>% 
  group_by(journal) %>% 
  pairwise_count(item = "word", feature = "paper_number", sort=TRUE)

# Check the statistics of word co-occurrences for three journals respectively
tapply(abstract_word_cooccurences$n, abstract_word_cooccurences$journal, summary)

health_word_cooccurences <- abstract_word_cooccurences[abstract_word_cooccurences$journal=="Health Systems", ]
operational_word_cooccurences <- abstract_word_cooccurences[abstract_word_cooccurences$journal=="Operational Research", ]
simulation_word_cooccurences <- abstract_word_cooccurences[abstract_word_cooccurences$journal=="Simulation", ]



# Plot the links between words in different abstracts for three journals respectively
head(health_word_cooccurences, 100) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n))+
  geom_node_point(color = "lightgreen", size = 3) +
  geom_node_text(aes(label = as.character(name)), vjust = 2, col = "darkblue") +
  ggtitle(sprintf("\n%s", "Abstract Of Health System: Co-Occurrence of Words(Top 100)")) +
  theme_void()

head(operational_word_cooccurences, 100) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n))+
  geom_node_point(color = "lightgreen", size = 3) +
  geom_node_text(aes(label = as.character(name)), vjust = 2, col = "darkblue") +
  ggtitle(sprintf("\n%s", "Abstract Of Operational Research Society: Co-Occurrence of Words(Top 100)")) +
  theme_void()

head(simulation_word_cooccurences, 100) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n))+
  geom_node_point(color = "lightgreen", size = 3) +
  geom_node_text(aes(label = as.character(name)), vjust = 2, col = "darkblue") +
  ggtitle(sprintf("\n%s", "Abstract Of Simulation: Co-Occurrence of Words(Top 100)")) +
  theme_void()






##### Regression on prediction of citations #####

# Prepare data set for regression
# Count the frequency of popular words and popular bigrams as new features
top_30_popular_bigram <- popular_words_bigram_united %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(10)

top_30_popular_words <- popular_words %>% top_n(50)

regression_data <- df %>% 
  select(year, pages, views, citations, altmetric, journal, abstract)

regression_data$popular_words_frequency<- 0
for (nrowindex in 1:nrow(regression_data)){
  regression_data$popular_words_frequency[nrowindex] <- sum(sapply(top_30_popular_words$word, 
                                                                   grepl, df$abstract[nrowindex]))
}
regression_data$popular_bigram_frequency<- 0
for (nrowindex in 1:nrow(regression_data)){
  regression_data$popular_bigram_frequency[nrowindex] <- sum(sapply(top_30_popular_bigram$bigram, 
                                                                    grepl, df$abstract[nrowindex]))
}

regression_data_new <- regression_data[which(regression_data$pages <= 20 & regression_data$views <= 750 &
regression_data$citations <= 15 & regression_data$altmetric<=15), ]
regression_data_new <- regression_data[,!names(regression_data) %in% c("abstract")]
regression_data_new[2:5] <- lapply(regression_data_new[2:5], sqrt)

###### Model 0-A ######
set.seed(456)
citations_TraSamples_new <- createDataPartition(regression_data_new$citations , p=0.8, list=FALSE)
citations_training_data_new <- regression_data_new[citations_TraSamples_new,]
citations_test_data_new <- regression_data_new[-citations_TraSamples_new,]

citations_reg_new <- lm(citations~., data = citations_training_data_new)
summary(citations_reg_new)





###### Model 1-A ######
df_cluster2 <- df_cluster[which(df_cluster$pages <= 20 & df_cluster$views <= 750 &
                                  df_cluster$citations <= 15 & df_cluster$altmetric<=15), ]
reg_data <- df_cluster2[, !names(df_cluster2) %in% c("abstract")] 



# Model 1-A set up 
set.seed(456)
citations_TraSamples <- createDataPartition(reg_data$citations , p=0.8, list=FALSE)
citations_training_data <- reg_data[citations_TraSamples,]
citations_test_data <- reg_data[-citations_TraSamples,]

citations_reg1 <- lm(citations~., data = citations_training_data)
summary(citations_reg1)

# Model 1-A train set
pred1tr <- citations_reg1 %>% predict(citations_training_data)
RMSE1 <- RMSE(pred1tr, citations_training_data$citations)
R2_1<- R2(pred1tr, citations_training_data$citations)


# Model 1-A test set
pred1test <- citations_reg1 %>% predict(citations_test_data)
RMSE2 <- RMSE(pred1test, citations_test_data$citations)
R2_2<- R2(pred1test, citations_test_data$citations)






###### Model 2-A ######
# Square root transformation for variables
reg_data2 <- df_cluster2[, !names(df_cluster2) %in% c("abstract")] 
reg_data2[2:5] <- lapply(reg_data2[2:5], sqrt)

# Model 2-A set up
set.seed(456)
citations_TraSamples2 <- createDataPartition(reg_data2$citations , p=0.8, list=FALSE)
citations_training_data2 <- reg_data2[citations_TraSamples2,]
citations_test_data2 <- reg_data2[-citations_TraSamples2,]  

citations_reg2 <- lm(citations~., data = citations_training_data2)
summary(citations_reg2)

# Model 2-A train set
pred2tr <- citations_reg2 %>% predict(citations_training_data2)
RMSE3 <- RMSE(pred2tr, citations_training_data2$citations)
R2_3<- R2(pred2tr, citations_training_data2$citations)

# Model 2-A test set
pred2test <- citations_reg2 %>% predict(citations_test_data2)
RMSE4 <- RMSE(pred2test, citations_test_data2$citations)
R2_4 <- R2(pred2test, citations_test_data2$citations)



table <- matrix(c(RMSE1,RMSE2,R2_1,R2_2,RMSE3,RMSE4,R2_3,R2_4),ncol=4,byrow=TRUE)
colnames(table) <- c("RMSE Train"," RMSE Test","R2 Train"," R2 Test")
rownames(table) <- c("Model 1","Model 2")
RMSE_R2_table <- as.table(table)
RMSE_R2_table

