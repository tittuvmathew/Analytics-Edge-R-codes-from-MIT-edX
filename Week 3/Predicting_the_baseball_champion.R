baseball = read.csv("baseball.csv")
str(baseball)

# How many team/year pairs are there in the whole dataset? 
nrow(baseball)

# Limiting to Teams Making the Playoffs 
length(table(baseball$Year))

baseball = subset(baseball,baseball$Playoffs == 1)
nrow(baseball)

# Limiting to Teams Making the Playoffs 
unique(table(baseball$Year))

# Adding an Important Predictor 
PlayoffTable = table(baseball$Year)
names(PlayoffTable)     # Returns vector of strings stored as 

# Adding an Important Predictor 
PlayoffTable[c("1990","2001")] 
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
Eight_team_playoff = subset(baseball,baseball$NumCompetitors == 8)
nrow(Eight_team_playoff)

# Bivariate Models for Predicting World Series Winner
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
nrow(subset(baseball,baseball$WorldSeries == 0))

# Bivariate Models for Predicting World Series Winner 
Model_year = glm(WorldSeries ~ Year,data=baseball, family = binomial)
Model_RS = glm(WorldSeries ~ RS,data=baseball, family = binomial)
Model_RA = glm(WorldSeries ~ RA,data=baseball, family = binomial)
Model_W = glm(WorldSeries ~ W,data=baseball, family = binomial)
Model_OBP = glm(WorldSeries ~ OBP,data=baseball, family = binomial)
Model_SLG = glm(WorldSeries ~ SLG,data=baseball, family = binomial)
Model_BA = glm(WorldSeries ~ BA,data=baseball, family = binomial)
Model_RankSeason = glm(WorldSeries ~ RankSeason,data=baseball, family = binomial)
Model_OOBP = glm(WorldSeries ~ OOBP,data=baseball, family = binomial)
Model_OSLG = glm(WorldSeries ~ OSLG,data=baseball, family = binomial)
Model_NumCompetitors = glm(WorldSeries ~ NumCompetitors,data=baseball, family = binomial)
Model_League = glm(WorldSeries ~ League,data=baseball, family = binomial)

# Multivariate Models for Predicting World Series Winner 
multimodal = glm(WorldSeries ~ Year+RA+RankSeason + NumCompetitors,data=baseball,family = binomial)
summary(multimodal)

# Correlation 
cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])

