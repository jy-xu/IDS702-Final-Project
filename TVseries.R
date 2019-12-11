library(knitr)
library(dplyr)
library(ggplot2)
library(lattice)
library(rms)
library(gridExtra)
library(xtable)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(lme4)
library(rlist)
library(sjPlot)
library(broom)
library(data.table)
library(knitr)
library(dplyr)
library(ggplot2)
library(arm)
library(gridExtra)
library(caret)
library(pROC)
library(car)
library(GGally)
library(corrplot)

tvseries <- read.csv('Data.csv')
#tvseries$TV.Season <- factor(tvseries$TV.Season)
tvseries$FirstSeason <- 0
tvseries$FirstSeason[tvseries$Season == 1] <- 1
tvseries$Network = relevel(tvseries$Network, "NBC")
tvseries$AirDayInitial = relevel(tvseries$AirDayInitial, "Sun")

#web scrap
library(rvest)

for (show in tvseries$Show) {
  showlink = gsub(" ", "+", show)
  url <- paste('http://www.imdb.com/search/title/?title=', showlink, '&title_type=TVMini-Series&tvseries', sep = "")
  webpage <- read_html(url)
  genre_data_html <- html_nodes(webpage,'.genre')
  genre_data <- html_text(genre_data_html)[1]
  genre_data <- trimws(genre_data, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  tvseries$Genre[tvseries$Show == show] <- genre_data
  
  runtime_data_html <- html_nodes(webpage,'.runtime')
  runtime_data <- html_text(runtime_data_html)[1]
  runtime_data <- trimws(runtime_data, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  runtime_data <- strsplit(runtime_data, " ")[[1]][1]
  tvseries$Runtime[tvseries$Show == show] <- runtime_data
  
  certificate_data_html <- html_nodes(webpage,'.certificate')
  certificate_data <- html_text(certificate_data_html)[1]
  certificate_data <- trimws(certificate_data, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  tvseries$Certificate[tvseries$Show == show] <- certificate_data
  
  releaseyr_data_html <- html_nodes(webpage,'.unbold')
  releaseyr_data <- html_text(releaseyr_data_html)[2]
  releaseyr_data <- gsub("[^0-9.-]", " ", releaseyr_data)
  releaseyr_data <- trimws(releaseyr_data, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  releaseyr_data <- strsplit(releaseyr_data, " ")
  tvseries$ReleaseYr[tvseries$Show == show] <- releaseyr_data[[1]][1]
  tvseries$EndYr[tvseries$Show == show] <- releaseyr_data[[1]][2]
  
  imdbrating_data_html <- html_nodes(webpage,'strong')
  imdbrating_data <- html_text(imdbrating_data_html)[3]
  imdbrating_data <- trimws(imdbrating_data, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  tvseries$IMDBrating[tvseries$Show == show] <- imdbrating_data
}

tvseries$Certificate[is.na(tvseries$Certificate)] <- "Not Rated"
tvseries$EndYr[is.na(tvseries$EndYr)] <- "On-Going"
tvseries$EndedSeries <- 1
tvseries$EndedSeries[tvseries$EndYr == "On-Going"] <- 0

genrelist <- unique(unlist(strsplit(tvseries$Genre, split = ", ")))
for (genre in genrelist) {
  tvseries[genre] <- 0
  tvseries[genre][tvseries$Genre %like% genre,] <- 1
  if (genre == "Music") {
    tvseries[genre][tvseries$Genre %like% "Musical",] <- 0
  }
}

tvseries$Genre_Count <- rowSums(tvseries[genrelist])

# genre aggregate counts
genreagg <- data.frame(unlist(strsplit(tvseries$Genre, split = ", ")))
colnames(genreagg) <- c("genre")
genre_count <- genreagg %>%
  group_by(genre) %>%
  summarise(number = n())

#tvseries$GenreMain <- do.call(rbind, strsplit(tvseries$Genre, split = ", "))[,1]
GenreList = c("Action","Comedy", "Drama", "Crime")
tvseries$GenreMain <- ""
tvseries$GenreMain[tvseries$Action == 1] <- "Action"
tvseries$GenreMain[tvseries$Comedy == 1] <- paste(tvseries$GenreMain, "Comedy", sep = "")
tvseries$GenreMain[tvseries$Drama == 1] <- paste(tvseries$GenreMain, "Drama", sep = "")
tvseries$GenreMain[tvseries$Crime == 1] <- paste(tvseries$GenreMain, "Crime", sep = "")
tvseries$GenreMain[tvseries$GenreMain == ""] <- "Other"

write.csv(tvseries, "tvseries.csv")

tvseries_clean <- tvseries[,c(1, 4:33, 90:122)]
cols <- c("ChangeAirDayTime", "FirstSeason", "Genre", "Certificate", genrelist, "EndedSeries", "GenreMain", "Genre_Count")
tvseries_clean[cols] <- lapply(tvseries_clean[cols], factor)
tvseries_clean$Premier.Date <- as.numeric(as.Date(tvseries_clean$Premier.Date, format="%m/%d/%y"))
tvseries_clean$Finale.Date <- as.numeric(as.Date(tvseries_clean$Finale.Date, format="%m/%d/%y"))
tvseries_clean$Runtime <- as.numeric(tvseries_clean$Runtime)
tvseries_clean$ReleaseYr <- as.numeric(tvseries_clean$ReleaseYr)
tvseries_clean$ReleaseYr[is.na(tvseries$ReleaseYr)] <- 2014
tvseries_clean$EndYr[tvseries$EndYr == "On-Going"] <- 0
tvseries_clean$EndYr <- as.numeric(tvseries_clean$EndYr)
tvseries_clean$IMDBrating <- as.numeric(tvseries_clean$IMDBrating)
tvseries_clean$X18to49_MinDropPremiere <- (tvseries_clean$X18to49_Premiere - tvseries_clean$X18to49_Min)/tvseries_clean$X18to49_Premiere
tvseries_clean$Total_MinDropPremiere <- (tvseries_clean$Total_Premiere - tvseries_clean$Total_Min)/tvseries_clean$Total_Premiere
summary(tvseries_clean)

tvseries_clean$TVSeason2013 <- tvseries_clean$TVSeason - min(tvseries_clean$TVSeason)
tvseries_clean$Season1 <- tvseries_clean$Season - 1
tvseries_clean$Episode1 <- tvseries_clean$Episode - 1
tvseries_clean$ReleaseYr1967 <- tvseries_clean$ReleaseYr - min(tvseries_clean$ReleaseYr)
tvseries_clean$Network <- relevel(tvseries_clean$Network, "CBS")
cont <- c("TVSeason2013", "Season1", "Episode1", "DaysfromTVSeasonPremiere", 
          "X18to49_Min", 
          "X18to49_YrtoYrChange", 
          "Runtime", "ReleaseYr1967", "IMDBrating", 
          "Genre_Count", "X18to49_MinDropPremiere")
contall <- c("X18to49_Avg", "X18to49_Premiere", "X18to49_Max","X18to49_Min", 
          "X18to49_YrtoYrChange", "X18to49_LossGainPremiere", "X18to49_.DropPremiere"
          "Total_Avg", "Total_Premiere", "Total_Max", "Total_Min", "Total_YrtoYrChange", 
          "Runtime", "ReleaseYr1967", "IMDBrating", 
          "Genre_Count", "X18to49_MinDropPremiere")
cor(train[cont])
ggpairs(train, columns=cont)
corrplot(cor(train[cont]), method = "circle")


#train test
set.seed(123)
tvseries_clean$train <- 0
tvseries_clean$train[sample(nrow(tvseries_clean),nrow(tvseries_clean)*0.8, replace=F)] <- 1

train <- tvseries_clean[tvseries_clean$train == 1,]
test <- tvseries_clean[tvseries_clean$train == 0,]
summary(train)
str(train)
table(train$Renewal)
table(train$Network)
table(train$GenreMain6)

#Eda
par(mfrow = c(2,2))
plot(tapply(train$Renewal, train$X18to49_Min, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Adults 18-49 Key Demo Rating - Min", xaxt="n")
axis(1, at=1:35, labels=levels(train$X18to49_Min))
plot(tapply(train$Renewal, train$Episode1, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:35, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$Season1, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Season", xaxt="n")
axis(1, at=1:35, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$IMDBrating, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="IMDB Rating", xaxt="n")
axis(1, at=1:100, labels=levels(train$Episode))

plot(tapply(train$Renewal, train$X18to49_Avg, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Adults 18-49 Key Demo Rating - Avg", xaxt="n")
axis(1, at=1:500, labels=levels(train$X18to49_Avg))
plot(tapply(train$Renewal, train$X18to49_YrtoYrChange, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="X18to49_Min", xaxt="n")
axis(1, at=1:35, labels=levels(train$X18to49_Min))
plot(tapply(train$Renewal, train$X18to49_MinDropPremiere, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="X18to49_Min", xaxt="n")
axis(1, at=1:35, labels=levels(train$X18to49_Min))


plot(tapply(train$Renewal, train$Total_Min, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Total_Min", xaxt="n")
axis(1, at=1:5, labels=levels(train$Total_Min))
plot(tapply(train$Renewal, train$Total_Avg, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Total_Max", xaxt="n")
axis(1, at=1:5, labels=levels(train$Total_Min))
plot(tapply(train$Renewal, train$Total_Max, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Total_Avg", xaxt="n")
axis(1, at=1:5, labels=levels(train$Total_Min))
plot(tapply(train$Renewal, train$Total_Premiere, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Total_Premier", xaxt="n")
axis(1, at=1:5, labels=levels(train$Total_Min))

plot(tapply(train$Renewal, train$DaysfromTVSeasonPremiere, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="DaysfromTVSeasonPremiere", xaxt="n")
axis(1, at=1:5, labels=levels(train$DaysfromTVSeasonPremiere))
plot(tapply(train$Renewal, train$Genre_Count, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="DaysfromTVSeasonPremiere", xaxt="n")
axis(1, at=1:5, labels=levels(train$DaysfromTVSeasonPremiere))
plot(tapply(train$Renewal, train$Episode1, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:5, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$Season1, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:5, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$TVSeason2013, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:5, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$IMDBrating, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:5, labels=levels(train$Episode))
plot(tapply(train$Renewal, train$Runtime, mean),col='blue4',pch=10, 
     ylab="Renewal", xlab="Episode", xaxt="n")
axis(1, at=1:5, labels=levels(train$Episode))

ggplot(train, aes(y=X18to49_Min, x=factor(Network), fill=factor(Renewal)))+
  geom_boxplot() +
  xlab('Network')+
  ylab('Adults 18-49 Key Demo Rating - Min')+
  ggtitle('Adults 18-49 Key Demo Rating - Min vs Renewal by Network')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train, aes(y=X18to49_Min, x=factor(Genre_Count), fill=factor(Renewal)))+
  geom_boxplot() +
  xlab('Win')+
  ylab('Adults 18-49 Key Demo Rating - Min')+
  ggtitle('TeamPoints vs Win')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none')

ggplot(train, aes(y=Episode1, x=factor(Network), fill=factor(Renewal)))+
  geom_boxplot() +
  xlab('Win')+
  ylab('TeamPoints')+
  ggtitle('TeamPoints vs Win')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none')

chisq.test(table(train[,c("Renewal","Network")]))
chisq.test(table(train[,c("Renewal","Genre_Count")]))
chisq.test(table(train[,c("Renewal","Debut.on.Different.Day")]))
chisq.test(table(train[,c("Renewal","Comedy")]))
chisq.test(table(train[,c("Renewal","Crime")]))
chisq.test(table(train[,c("Renewal","Action")]))
chisq.test(table(train[,c("Renewal","Drama")]))
chisq.test(table(train[,c("Renewal","Mystery")]))
chisq.test(table(train[,c("Renewal","Romance")]))
chisq.test(table(train[,c("Renewal","Adventure")]))
chisq.test(table(train[,c("Renewal","Sci-Fi")]))
chisq.test(table(train[,c("Renewal","Animation")]))
chisq.test(table(train[,c("Renewal","Thriller")]))
chisq.test(table(train[,c("Renewal","Fantasy")]))
chisq.test(table(train[,c("Renewal","Horror")]))
chisq.test(table(train[,c("Renewal","Sport")]))
chisq.test(table(train[,c("Renewal","Biography")]))
chisq.test(table(train[,c("Renewal","Animation")]))
chisq.test(table(train[,c("Renewal","Reality-TV")]))
chisq.test(table(train[,c("Renewal","Family")]))
chisq.test(table(train[,c("Renewal","Musical")]))
chisq.test(table(train[,c("Renewal","Music")]))
chisq.test(table(train[,c("Renewal","Documentary")]))
chisq.test(table(train[,c("Renewal","History")]))
chisq.test(table(train[,c("Renewal","Western")]))
chisq.test(table(train[,c("Renewal","Short")]))
chisq.test(table(train[,c("Renewal","War")]))

Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi` + Sport + Biography + Animation + 
  Thriller + Fantasy + `Reality-TV` + Family + Musical + Music + 
  Horror + Documentary + History + Western + Short + War 

Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi`  + Animation + Thriller + Fantasy  + Horror

glmnull = glm(Renewal ~ 1, data = train, family = binomial(link="logit"))
FormulaFull = "Renewal ~ . - EndedSeries - EndYr - Premier.Date - Finale.Date - Show - Genre - train"
FormulaFull <- as.formula(paste(FormulaFull, "-", paste(genrelist, collapse= "-")))
FormulaFull = paste("Renewal ~ ", paste(colnames(train), collapse= "+"))

FormulaSeason = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere

FormulaShow = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating + Genre_Count

FormulaShowGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating + GenreMain + Genre_Count

FormulaGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi` + Sport + Biography + Animation + 
  Thriller + Fantasy + `Reality-TV` + Family + Musical + Music + 
  Horror + Documentary + History + Western + Short + War + 
  Genre_Count

FormulaGenre12 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi`  + Animation + Thriller + Fantasy  + Horror  + 
  Genre_Count

FormulaGenre4 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama  + 
  Genre_Count

#####
FormulaSeason = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere

FormulaShow = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating 

FormulaShowGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating + GenreMain 

FormulaGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi` + Sport + Biography + Animation + 
  Thriller + Fantasy + `Reality-TV` + Family + Musical + Music + 
  Horror + Documentary + History + Western + Short + War

FormulaGenre12 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi`  + Animation + Thriller + Fantasy  + Horror

FormulaGenre4 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere + SeasonDays + X18to49_Avg + 
  X18to49_Premiere + X18to49_Max + X18to49_Min + X18to49_YrtoYrChange + 
  X18to49_LossGainPremiere + X18to49_.DropPremiere + X18to49_MinDropPremiere + Total_Avg + 
  Total_Premiere + Total_Max + Total_Min + Total_YrtoYrChange + 
  Total_LossGainPremiere + Total_.DropPremiere + Total_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama 

#####
FormulaSeason = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere + Network:X18to49_Min 

FormulaShow = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating + Genre_Count + Network:X18to49_Min 

FormulaShowGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating + GenreMain + Genre_Count + Network:X18to49_Min 

FormulaGenre = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi` + Sport + Biography + Animation + 
  Thriller + Fantasy + `Reality-TV` + Family + Musical + Music + 
  Horror + Documentary + History + Western + Short + War + 
  Genre_Count + Network:X18to49_Min 

FormulaGenre12 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama + Mystery + 
  Romance + Adventure + `Sci-Fi`  + Animation + Thriller + Fantasy  + Horror  + 
  Genre_Count + Network:X18to49_Min 

FormulaGenre4 = Renewal ~ TVSeason2013 + Network + Season1 + FirstSeason + Episode1 + AirDayInitial + AirTimeInitial + AirDayTimeInitial + 
  ChangeAirDayTime + Debut.on.Different.Day + DaysfromTVSeasonPremiere  + 
  X18to49_Min + X18to49_YrtoYrChange + X18to49_MinDropPremiere +
  Runtime + Certificate + ReleaseYr1967 + IMDBrating +
  Comedy + Crime + Action + Drama  + 
  Genre_Count + Network:X18to49_Min 
  
glmseason = glm(FormulaSeason, data = train, family = binomial(link="logit"))
summary(glmseason)

glmshow = glm(FormulaShow, data = train, family = binomial(link="logit"))
summary(glmshow)

glmgenre = glm(FormulaGenre, data = train, family = binomial(link="logit"))
summary(glmgenre)

glmgenre12 = glm(FormulaGenre12, data = train, family = binomial(link="logit"))
summary(glmgenre12)

glmgenre4 = glm(FormulaGenre4, data = train, family = binomial(link="logit"))
summary(glmgenre4)

glmgenreMain = glm(FormulaShowGenre, data = train, family = binomial(link="logit"))
summary(glmgenreMain)


n <- nrow(train)
glmstepbic1 = step(glmnull, scope = formula(glmseason), direction="both",trace=0)
summary(glmstepbic1)

glmstepaic1 = step(glmnull, scope = formula(glmseason), direction="both",trace=0)
summary(glmstepaic1)

glmstepaic2 = step(glmstepaic1, scope = formula(glmshow), direction="both",trace=0)
summary(glmstepaic2)

glmstepaic3 = step(glmstepaic2, scope = formula(glmgenre), direction="both",trace=0)
summary(glmstepaic3)

glmstepaic4 = step(glmstepaic2, scope = formula(glmgenre12), direction="both",trace=0)
summary(glmstepaic4)

glmstepaic5 = step(glmstepaic2, scope = formula(glmgenre4), direction="both",trace=0)
summary(glmstepaic5)

glmstepaic6 = step(glmstepaic2, scope = formula(glmgenreMain), direction="both",trace=0)
summary(glmstepaic6)

FormulaFinal = Renewal ~ Episode1 + X18to49_Min + Network + TVSeason2013 + 
  DaysfromTVSeasonPremiere + Debut.on.Different.Day + Season1 + 
  IMDBrating + X18to49_MinDropPremiere + Genre_Count + Runtime + 
  X18to49_Min:Network

FormulaFinal1 = Renewal ~ Episode1 + X18to49_Min + Network + TVSeason2013 + 
  DaysfromTVSeasonPremiere + Debut.on.Different.Day + Season1 + 
  IMDBrating + X18to49_MinDropPremiere + Genre_Count + Runtime + 
  X18to49_Min:Network + Episode1:Network

0.1311
0.07651
0.1354
0.07674
0.06568

glm <- glmstepaic4

glm = glm(formula = FormulaFinal, data = train, family = binomial(link="logit"))
summary(glm)
vif(glm)
exp(glm$coefficients)
glm1 = glm(formula = FormulaFinal1, data = train, family = binomial(link="logit"))
summary(glm1)
vif(glm1)
anova(glm, glm1, test='Chisq')
glmer = glmer(formula = Renewal ~ (1|Network) + Episode + X18to49_Min + TVSeason + Runtime + Debut.on.Different.Day + 
                Season + DaysfromTVSeasonPremiere, data = train, family = binomial(link="logit"))
isSingular(glmer)
summary(glmer)
ranef(glmer)
dotplot(ranef(glmer, condVar=TRUE))
train$pred <- predict(glmer, type = "response")

roc(glm$y,glm$fitted.values,plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
roc(train$Renewal,train$pred,plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

conf_mat <- confusionMatrix(as.factor(ifelse(glm$fitted.values >= 0.712, "1","0")),
                            as.factor(glm$y),positive = "1")
conf_mat$table
conf_mat$overall["Accuracy"];
conf_mat$byClass[c("Sensitivity","Specificity")]

conf_mat <- confusionMatrix(as.factor(ifelse(train$pred >= 0.649, "1","0")),
                            as.factor(train$Renewal),positive = "1")
conf_mat$table
conf_mat$overall["Accuracy"];
conf_mat$byClass[c("Sensitivity","Specificity")]

rawresid <- residuals(glmstepaic4,"resp")
binnedplot(x=predict(glmstepaic4, type="response"),y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$Episode,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$IMDBrating,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$X18to49_Min,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$TVSeason,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$Runtime,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$Season,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$DaysfromTVSeasonPremiere,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=train$Genre_Count,y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")



#test
roc(test$Renewal,predict(glm, newdata = test, "response"),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

conf_mat <- confusionMatrix(as.factor(ifelse(predict(glm, newdata = test, "response") >= 0.712, "1","0")),
                            as.factor(test$Renewal),positive = "1")
conf_mat$table
conf_mat$overall["Accuracy"];
conf_mat$byClass[c("Sensitivity","Specificity")]
