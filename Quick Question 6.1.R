#option 1
install.packages("RCurl")
library(RCurl)

myfile <- getURL('http://files.grouplens.org/datasets/movielens/ml-100k/u.item', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

movies <- read.table(textConnection(myfile), header=F, sep = "|", quote="\"")


#option 2
movies = read.table("movieLens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure",
                     "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir",
                     "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")


str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)



sum(movies$Comedy)
sum(movies$Western)
sum(movies$Romance*movies$Drama)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

distances = dist(movies[2:20],method = "euclidean")

clusterMovies = hclust(distances, method = "ward.D")

plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

spl = split(movies[2:20],clusterGroups)
lapply(spl, colMeans)

clusterGroups = cutree(clusterMovies, k = 2)
spl = split(movies[2:20],clusterGroups)
lapply(spl, colMeans)
