songs = read.csv("songs.csv" )
str(songs)
songs_2010 = subset(songs, year == 2010)
nrow(songs_2010)
songs_MJ = subset(songs,artistname == "Michael Jackson")
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
nrow(songs_MJ)
summary(songs_MJ)
str(songs_MJ)
# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply. 
songs_MJ_top10 = subset(songs_MJ,Top10 == 1)
unique(songs_MJ_top10$songtitle)
# The variable corresponding to the estimated time signature (timesignature) is discrete,
# meaning that it only takes integer values (0, 1, 2, 3, . . . ). 
# What are the values of this variable that occur in our dataset? Select all that apply.
unique(songs$timesignature)
# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)
# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
pos1 = which.max(songs$tempo)
songs$songtitle[pos1]

# Creating prediction model 
SongsTest = subset(songs,year==2010)
SongsTrain = subset(songs,year<=2009)

# Creating our Prediction Model 
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# Beware of Multicollinearity Issues! 
cor(SongsTrain[c("loudness","energy")])

# Model 2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# Model 3 
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Validating Our Model 
pred3 = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, pred3 > 0.45)

# Baseline Model 
table(SongsTest$Top10)

# Problem 4.3 
pred3 = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, pred3 >= 0.45)












