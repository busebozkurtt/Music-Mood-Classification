library("scales")
library("purrr")
library("tidyverse")  # data manipulation
library("cluster")    # clustering algorithms
library("factoextra")
library("NbClust")
library("ggplot2")
library("dplyr")
library("mclust")
library("fpc")
library("plyr")
library("meltt")
library("class")
library("reshape")
library("reshape2")
library("ISLR")
library("caret")
library("ff")

library("scales")
library("purrr")
library("tidyverse") # data manipulation
library("cluster") # clustering algorithms
library("factoextra")
library("NbClust")
library("ggplot2")
library("dplyr")
library("mclust")
library("fpc")
library("plyr")
library("meltt")
library("class")
library("reshape")
library("reshape2")
library("ISLR")
library("caret")
library("ClusterR")
library("colormap")
library("hrbrthemes")
library("ggpubr")
library("wesanderson")
memory.limit(size = 512000)
music_data <- read.csv("C:/Users/berki/Desktop/data.csv", sep=",", encoding = "UTF-8")
cluster_data <- read.csv("C:/Users/berki/Desktop/final.csv", sep=",", encoding = "UTF-8")
# check for missing values
colSums(is.na(music_data))
# check for duplicate values
sum(duplicated(music_data))
# Change line color and fill color
p1 <- ggplot(music_data, aes(x=valence))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p2 <- ggplot(music_data, aes(x=year))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p3 <- ggplot(music_data, aes(x=acousticness))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p4 <- ggplot(music_data, aes(x=danceability))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p5 <- ggplot(music_data, aes(x=energy))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p6 <- ggplot(music_data, aes(x=instrumentalness))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p7 <- ggplot(music_data, aes(x=liveness))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p8 <- ggplot(music_data, aes(x=tempo))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# Change line color and fill color
p9 <- ggplot(music_data, aes(x=speechiness))+
geom_histogram(color="darkblue", fill="lightblue")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9 ,
ncol = 3, nrow = 3)
list_kmeans = list()
list_hclust = list()
max_kmeans=0
max_hclust=0
sample_K=data.frame()
sample_H=data.frame()
list_subset = list()
############################## find the best clustered sample
for (i in 1:100){
sub<-sample_n(music_data[,c("acousticness" , "energy" ,
"danceability" , "valence",
"instrumentalness", "speechiness" ,
"tempo" , "loudness")], 18000)
list_subset[[i]]=sub
}
print("Kmeans")
for (i in 1:100){
k2 <- eclust(list_subset[[i]], "kmeans", k = 6,
nstart = 25,graph = FALSE)
sil <- silhouette(k2$cluster, dist(list_subset[[i]]))
silinfo <- k2$silinfo
dd <- dist(list_subset[[i]] ,method ="euclidean")
# Statistics for k-means clustering
km_stats <- cluster.stats(dd, k2$cluster)
list_kmeans[i] <- silinfo$avg.width
if(silinfo$avg.width>max_kmeans){
max_kmeans=silinfo$avg.width
sample_K=list_subset[[i]]
}
cat("Iteration",i,'average:',silinfo$avg.width,"\n")
cat('Size:',k2$size,"\n")
}
print(max_kmeans)
print("Hclust")
for (i in 1:100){
k2 <- eclust(list_subset[[i]], "hclust", k = 6,
method = "complete", graph = FALSE)
sil <- silhouette(k2$cluster, dist(list_subset[[i]]))
silinfo <- k2$silinfo
dd <- dist(list_subset[[i]], method ="euclidean")
# Statistics for k-means clustering
km_stats <- cluster.stats(dd, k2$cluster)
list_hclust[i] <- silinfo$avg.width
if(silinfo$avg.width>max_hclust){
max_hclust=silinfo$avg.width
sample_H=list_subset[[i]]
}
cat("Iteration",i,'average:',silinfo$avg.width,"\n")
cat('Size:',k2$size,"\n")
}
print( max_hclust)
##############################
df <- list_hclust %>% tibble::enframe() %>% tidyr::unnest()
colnames(df) <- c("samples", "scores")
df <- list_kmeans %>% tibble::enframe() %>% tidyr::unnest()
colnames(df) <- c("samples", "scores")
# Plot
ggplot(df, aes(x=samples, y=scores)) +ggtitle("k-means") +
geom_line(lwd = 0.8) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))
# corelation matrix
cormat <- round(cor(cluster_data[,1:8]),2)
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
cormat[upper.tri(cormat)] <- NA
return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
cormat[lower.tri(cormat)]<- NA
return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
geom_tile(color = "white")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab",
name="Pearson\nCorrelation") +
theme_minimal()+
theme(axis.text.x = element_text(angle = 45, vjust = 1,
size = 12, hjust = 1))+
coord_fixed()
reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
geom_tile(color = "white")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab",
name="Pearson\nCorrelation") +
theme_minimal()+ # minimal theme
theme(axis.text.x = element_text(angle = 45, vjust = 1,
size = 12, hjust = 1))+
coord_fixed()
# Print the heatmap
print(ggheatmap)
ggheatmap +
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
theme(
axis.title.x = element_blank(),
axis.title.y = element_blank(),
panel.grid.major = element_blank(),
panel.border = element_blank(),
panel.background = element_blank(),
axis.ticks = element_blank(),
legend.justification = c(1, 0),
legend.position = c(0.6, 0.7),
legend.direction = "horizontal")+
guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
title.position = "top", title.hjust = 0.5))
############################
# we should consider dropping loudness column with the value 0.78 (between 0.7 - 0.9)
# kmeans++ #################
k2 <- eclust(cluster_data, "hclust", k = 6,
method = "complete", graph = FALSE)
k2$size
# sizes are not really equal this might be problematic
############################
# Change line color and fill color
p1 <- ggplot(cluster_data, aes(x=mood))+
geom_histogram(color="darkblue", fill="lightblue",stat="count")+
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
### Cluster 1
cat("############################")
df <- cluster_data[k2$cluster==1,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c1 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
### Cluster 2
cat("############################")
df <- cluster_data[k2$cluster==2,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c2 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
### Cluster 3
cat("############################")
df <- cluster_data[k2$cluster==3,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c3 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
### Cluster 4
cat("############################")
df <- cluster_data[k2$cluster==4,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c4 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
### Cluster 5
cat("############################")
df <- cluster_data[k2$cluster==5,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c5 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
### Cluster 6
cat("############################")
df <- cluster_data[k2$cluster==6,]
cat("acousticness",mean(df$acousticness))
cat("energy",mean(df$energy))
cat("danceability",mean(df$danceability))
cat("valence",mean(df$valence))
cat("instrumentalness",mean(df$instrumentalness))
cat("speechiness",mean(df$speechiness))
cat("tempo",mean(df$tempo))
cat("loudness",mean(df$loudness))
c6 <- c(mean(df$acousticness),mean(df$energy),mean(df$danceability),mean(df$valence),
mean(df$instrumentalness),mean(df$speechiness),mean(df$tempo),mean(df$loudness))
cat("############################")
cluster_data[k2$cluster==1,"mood"]<-"happy"
cluster_data[k2$cluster==2,"mood"]<-"gleeful"
cluster_data[k2$cluster==3,"mood"]<-"sad"
cluster_data[k2$cluster==4,"mood"]<-"calm"
cluster_data[k2$cluster==5,"mood"]<-"romantic"
cluster_data[k2$cluster==6,"mood"]<-"sensual"
cluster_data$mood <- as.factor(cluster_data$mood)
################################
# Plotting the clusters
# Create data
set.seed(1)
data <- t(data.frame(c1, c2, c3, c4, c5, c6))
colnames(data) <- c("acousticness" , "energy" ,
"danceability" , "valence",
"instrumentalness", "speechiness" ,
"tempo" , "loudness")
rownames(data) <- c("Happy", "Gleeful", "Sad", "Calm",
"Romantic","Sensual")
data <- as.data.frame(data)
# Barplot
data <- data %>% slice(c(1:8)) %>%
t() %>%
as.data.frame() %>%
add_rownames() %>%
mutate(rowname=factor(rowname, rowname)) %>%
gather(key=name, value=mark, -1)
#Recode
data$name <- recode(data$name, V1 = "Cluster1", V2 = "Cluster2",
V3 = "Cluster3", V4 = "Cluster4", V5 = "Cluster5",
V6 = "Cluster6")
# Plot
data %>% ggplot( aes(x=rowname, y=mark)) +
geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
coord_flip() +
theme_ipsum() +
theme(
panel.grid.minor.y = element_blank(),
panel.grid.major.y = element_blank(),
axis.text = element_text( size=48 )
) +
ylim(0,1) +
ylab("range") +
xlab("") +
facet_wrap(~name, ncol=6)
################################
# create final dataset with 1000 1000 1000 1000 484 643 distribution
sub_happy<-sample_n(cluster_data[cluster_data$mood=="happy",], 2000)
sub_gleeful<-sample_n(cluster_data[cluster_data$mood=="gleeful",], 2000)
sub_sad<-sample_n(cluster_data[cluster_data$mood=="sad",], 2000)
sub_calm<-sample_n(cluster_data[cluster_data$mood=="calm",], 2000)
romantic <- cluster_data[cluster_data$mood=="romantic",]
sensual <- cluster_data[cluster_data$mood=="sensual",]
final <- rbind(sub_happy, sub_gleeful,sub_sad,sub_calm,romantic,sensual)
write.csv(cluster_data,"C:/Users/berki/Desktop/final.csv", row.names = FALSE)
library("caret")
control <- trainControl(method='repeatedcv',
number=10,
repeats=3)
x <- cluster_data[,1:8]
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(mood~.,
data=cluster_data,
method='rf',
metric='Accuracy',
tuneGrid=tunegrid,
trControl=control)
print(rf_default)