library(dplyr)
library(recommenderlab)
data("iris")
iris%>%summary
library(reshape2)
library(data.table)
mov_rat<-read.csv("C:/Users/AKHIL/Desktop/New folder/movies_recomend.csv")
View(mov_rat)
dim(mov_rat)
head(mov_rat)
levels(mov_rat$title)###shows title

movie_ratings<-acast(mov_rat,title~critic,value.var="rating")
movie_ratings

movie_ratings<-as.data.frame(movie_ratings)
class(movie_ratings)

similarity_users<-cor(movie_ratings)###na values appear###
similarity_users

similarity_users<-cor(movie_ratings,use = "complete.obs")##na values omitted using complete.obs####
similarity_users


rating_by_critic<-setDT(movie_ratings[colnames(movie_ratings)[5]] ,keep.rownames = TRUE)[]
rating_by_critic

names(rating_by_critic)<-c("title","rating by E ")
rating_by_critic

titles_na_critic<-rating_by_critic$title[is.na(rating_by_critic$`rating `)]#####movies not seen###### 
titles_na_critic

ratings_t<-mov_rat[mov_rat$title %in% titles_na_critic,]
ratings_t#####movies not watched by E and the ratings given by other users#######


x=(setDT(data.frame(similarity_users[,5]),keep.rownames=TRUE)[])
names(x)<-c("critic","similarity")#####similarity of E with all users######
x

ratings_t<-merge(x=ratings_t,y=x,by="critic",all.x=TRUE)
ratings_t

ratings_t$sim_rating<-(ratings_t$rating*ratings_t$similarity)
ratings_t$sim_rating
View(ratings_t)

result<-ratings_t %>% group_by(title)%>% summarise(sum(sim_rating)/sum(similarity))
result
meanE<-mean(na.omit(rating_by_critic$rating_by_critic))######mean###########
meanE
