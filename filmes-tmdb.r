library(plotly)
library(dplyr)
library(lubridate)
library(jsonlite)
library(stringr)

movies <- read.csv("~/Desktop/Treinamento/tmdb-movies-dataset/movies.csv")
credits <- read.csv("~/Desktop/Treinamento/tmdb-movies-dataset/credits.csv")

glimpse(movies)
glimpse(credits)

################################################################################################################
################################################## TIDYING #####################################################

############# Movies
movies$genres                 <- as.character(movies$genres)
movies$production_companies   <- as.character(movies$production_companies)
movies$production_countries   <- as.character(movies$production_countries)
movies$spoken_languages       <- as.character(movies$spoken_languages)
movies$release_date           <- as.character(movies$release_date)
movies$original_language      <- as.character(movies$original_language)
movies$original_title         <- as.character(movies$original_title)
movies$overview               <- as.character(movies$overview)
movies$status                 <- as.character(movies$status)
movies$tagline                <- as.character(movies$tagline)
movies$title                  <- as.character(movies$title)
movies$keywords               <- as.character(movies$keywords)

#Parse de movies$production_companies
lista4 = list()
for (i in 1:length(movies$production_companies)) {
  if(movies$production_companies[i]=="[]"){
    lista4[[i]] <- "none"
  } else {
    lista4[[i]] <- fromJSON(txt=movies$production_companies[i])
    lista4[[i]] <- paste(lista4[[i]][[1]], collapse=",")
  }
}

movies <- mutate(movies, production_companies_parsed = lista4 )
movies$production_companies <- NULL

# Gerar CSV de companies
companies = str_split(lista4, ",")
str(companies)
companies <- unlist(companies, use.names=FALSE)
csv_companies <- data.frame(Companies = unique(companies))
write.csv(csv_macrocanal, "~/Desktop/Treinamento/csv_companies.csv", row.names = FALSE, na = "", quote = FALSE)

#Parse de movies$production_countries
lista5 = list()
for (i in 1:length(movies$production_countries)) {
  if(movies$production_countries[i]=="[]"){
    lista5[[i]] <- "none"
  } else {
    lista5[[i]] <- fromJSON(txt=movies$production_countries[i])
    lista5[[i]] <- paste(lista5[[i]][[2]], collapse=",")
  }
}

movies <- mutate(movies, production_countries_parsed = lista5 )
movies$production_countries <- NULL

#Parse de movies$genres
lista3 = list()
for (i in 1:length(movies$genres)) {
  if(movies$genres[i]=="[]"){
    lista3[[i]] <- "none"
  } else {
    lista3[[i]] <- fromJSON(txt=movies$genres[i])
    lista3[[i]] <- paste(lista3[[i]][[2]], collapse=",")
  }
}

movies <- mutate(movies, genre_parsed = lista3 )
movies$genres <- NULL

# Gerar CSV de genres
genres = str_split(lista3, ",")
str(genres)
genres <- unlist(genres, use.names=FALSE)
csv_genres <- data.frame(Genres = unique(genres))
write.csv(csv_genres, "~/Desktop/Treinamento/csv_genres.csv", row.names = FALSE, na = "", quote = FALSE)

#Parse de movies$spoken_languages
lista6 = list()
for (i in 1:length(movies$spoken_languages)) {
  if(movies$spoken_languages[i]=="[]"){
    lista6[[i]] <- "none"
  } else {
    lista6[[i]] <- fromJSON(txt=movies$spoken_languages[i])
    lista6[[i]] <- paste(lista6[[i]][[2]], collapse=",")
  }
}

movies <- mutate(movies, spoken_languages_parsed = lista6 )
movies$spoken_languages <- NULL

#Parse de movies$keywords
lista7 = list()
for (i in 1:length(movies$keywords)) {
  if(movies$keywords[i]=="[]"){
    lista7[[i]] <- "none"
  } else {
    lista7[[i]] <- fromJSON(txt=movies$keywords[i])
    lista7[[i]] <- paste(lista7[[i]][[2]], collapse=",")
  }
}

movies <- mutate(movies, keywords_parsed = lista7 )
movies$keywords <- NULL

# Gerar CSV de keywords
keywords = str_split(lista7, ",")
str(keywords)
keywords <- unlist(keywords, use.names=FALSE)
csv_keywords <- data.frame(Keywords = unique(keywords))
write.csv(csv_keywords, "~/Desktop/Treinamento/csv_keywords.csv", row.names = FALSE, na = "", quote = FALSE)

############# Credits
credits$title  <- as.character(credits$title)
credits$cast   <- as.character(credits$cast)
credits$crew   <- as.character(credits$crew)

#Parse de credits$cast
lista = list()
for (i in 1:length(credits$cast)) {
  if(credits$cast[i]=="[]"){
    lista[[i]] <- "none"
  } else {
    lista[[i]] <- fromJSON(txt=credits$cast[i])
    lista[[i]] <- paste(lista[[i]][[2]], collapse=",")
  }
}

credits <- mutate(credits, cast_parsed = lista )
credits$cast <- NULL

#Parse de credits$crew
lista2 = list()
for (i in 1:length(credits$crew)) {
  if(credits$crew[i]=="[]"){
    lista2[[i]] <- "none"
  } else {
    lista2[[i]] <- fromJSON(txt=credits$crew[i])
    lista2[[i]] <- paste(lista2[[i]][[6]], collapse=",")
  }
}

credits <- mutate(credits, crew_parsed = lista2 )
credits$crew <- NULL

#Juntando datasets
csv <- left_join(movies, credits, by=c("id" = "movie_id"))
csv$title.y <- NULL
colnames(csv)[colnames(csv)=="title.x"] <- "title"
colnames(csv)[colnames(csv)=="production_companies_parsed"] <- "production_companies"
colnames(csv)[colnames(csv)=="production_countries_parsed"] <- "production_countries"
colnames(csv)[colnames(csv)=="genre_parsed"] <- "genre"
colnames(csv)[colnames(csv)=="spoken_languages_parsed"] <- "spoken_languages"
colnames(csv)[colnames(csv)=="keywords_parsed"] <- "keywords"
colnames(csv)[colnames(csv)=="cast_parsed"] <- "cast"
colnames(csv)[colnames(csv)=="crew_parsed"] <- "crew"

csv$homepage               <- as.character(csv$homepage)
csv$production_companies   <- as.character(csv$production_companies)
csv$production_countries   <- as.character(csv$production_countries)
csv$genre                  <- as.character(csv$genre)
csv$spoken_languages       <- as.character(csv$spoken_languages)
csv$keywords               <- as.character(csv$keywords)
csv$cast                   <- as.character(csv$cast)
csv$crew                   <- as.character(csv$crew)
csv$budget                 <- as.character(csv$budget)
csv$popularity             <- as.character(csv$popularity)
csv$revenue                <- as.character(csv$revenue)
csv$runtime                <- as.character(csv$runtime)
csv$vote_average           <- as.character(csv$vote_average)
csv$vote_count             <- as.character(csv$vote_count)
csv$id                     <- as.character(csv$id)

csv = select(csv, id, title, original_title, budget, homepage, original_language, overview, popularity, revenue, 
             runtime, status, tagline, vote_average, vote_count, production_companies, production_countries,
             genre, spoken_languages, keywords, cast, crew, release_date)

glimpse(csv)

# Gerar novo csv
write.csv(csv, "~/Desktop/Treinamento/csv_tmdb_pos_tidying.csv", row.names = FALSE, na = "")

################################################################################################################
################################################## Visualization #####################################################

##################### Filmes Por Idiomas
idioma = csv$original_language

filmes_x_idioma <- plot_ly(
  x = names(table(idioma)),
  y = table(idioma),
  name = "Idiomas",
  type = "bar"
) %>%
  layout(title = 'Filmes Por Idiomas')

##################### Filmes Por Produtora - TOP 10
produtoras = csv$production_companies
View(produtoras)
produtoras = str_split(produtoras, ",")
produtoras = unlist(produtoras)

tail(sort(table(produtoras)),10)
glimpse(produtoras)

filmes_x_produtora <- plot_ly(
  x = names(tail(sort(table(produtoras)),10)),
  y = tail(sort(table(produtoras)),10),
  name = "Produtora",
  type = "bar"
  ) %>%
  layout(title = 'Filmes Por Produtora')

##################### Filmes Por Ano
csv$release_date =as.Date(csv$release_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

x <- sort(as.integer(unique(year(csv$release_date))))
y <- table(year(csv$release_date))
data <- data.frame(x, y)

filmes_x_ano <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Filmes Por Ano')

##################### Filmes Por Gênero
genres = str_split(lista3, ",")
str(genres)
generos <- unlist(genres, use.names=FALSE)

filmes_x_genero <- plot_ly(
  x = names(table(generos)),
  y = table(generos),
  name = "Gêneros",
  type = "bar"
) %>%
  layout(title = 'Filmes Por Gênero')

##################### Correlação entre Popularidade x Receita
movies$revenue      <- as.numeric(movies$revenue)
movies$popularity   <- as.double(movies$popularity)

lm(movies$popularity ~ movies$revenue) 
cor(movies$popularity , movies$revenue) 

plot(movies$popularity ~ movies$revenue)
abline(lm(movies$popularity ~ movies$revenue))
