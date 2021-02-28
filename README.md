# Music Mood Classification
Music mood classification project is a machine learning project developed using R language. Clustering method was used to create a target feature in the dataset while developing the project. K-Means Clustering Algorithm and Hierarchical Clustering Algorithm are used in the clustering method. Support Vector Machine Model, Random Forest Model, K-Nearest Neighbors Model, Decision Tree Model, Logistic Regression Model, Naive Bayes Model were used to classify music according to the mood of the person. The following libraries must be installed in order to run the project.

- library("scales")
- library("purrr")
- library("tidyverse") # data manipulation
- library("cluster") # clustering algorithms
- library("factoextra")
- library("NbClust")
- library("ggplot2")
- library("dplyr")
- library("mclust")
- library("fpc")
- library("plyr")
- library("meltt")
- library("class")
- library("reshape")
- library("reshape2")
- library("ISLR")
- library("caret")
- library("ClusterR")
- library("colormap")
- library("hrbrthemes")
- library("ggpubr")
- library("wesanderson")

## Data Set
For the data set in the project, https://www.kaggle.com/ was used.
Data set link used:
>https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks?select=data.csv
### Details of Data Set
The dataset consists of 15 numerical attributes and 4 categorical attributes making up to 19 total columns. The raw dataset contains 170,654 rows which are representing unique songs.

- "valence" 
The positiveness of the track
- "year" The release year of the track (1921 - 2020)
- "acousticness" The relative metric of the track being acoustic
- "artists" The list of artists credited for the production of the track
- "danceability" The relative measurement of the track being danceable
- "duration_ms" The length of the track in milliseconds (ms)
- "energy" The energy of the track
- "explicit" The binary value whether the track contains explicit content or not
- "id" The primary identifier for the track, generated by Spotify
- "instrumentalness" The relative ratio of the track being instrumental
- "key" The primary key of the track encoded as integers between 0 and 11
- "liveness" The relative duration of the track sounding as a live performance
- "loudness" Relative loudness of the track in the typical range [-60, 0] in decibel (dB)
- "mode" The binary value representing whether the track starts with a major (1) chord progression or not (0)
- "name" The title of the track
- "popularity" The popularity of the song lately, default country = the US
- "release_date" The date of release of the track in yyyy-mm-dd, yyyy-mm
- "speechiness" The relative length of the track containing any kind of human voice
- "tempo" The tempo of the track in Beat Per Minute (BPM)

There exist no missing values and duplicate rows in the dataset.






