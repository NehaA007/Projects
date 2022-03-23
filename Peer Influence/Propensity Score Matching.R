install.packages("MatchIt")
install.packages("ggplot2")       
install.packages("GGally")
install.packages("gridExtra")

library(MatchIt)
library(dplyr)             
library(ggplot2)           
library(igraph)

library(psych)             
library(corrplot) 
library(gridExtra)                     
library(GGally)
library(logistic.display)

setwd("D:/")

HighNote <- read.csv("Platform Data.csv", header=T)

#PART-1 SUMMARY STATISTICS
head(HighNote)
dim(HighNote)
summary(HighNote)

sum(is.na(HighNote))

#Generate descriptive statistics for the key variables in the data set, 
#similar to the table on the last page of the case.
#To analyze differences in mean values of variables, comparing adopter and non-adopter subsamples

table(HighNote$adopter)

columns = c('age','male','friend_cnt','avg_friend_male','avg_friend_age','friend_country_cnt',
            'subscriber_friend_cnt', 'songsListened','lovedTracks','posts','playlists',
            'shouts','tenure','good_country')

describeBy(HighNote[,columns], group = HighNote$adopter, skew=FALSE) #Can obtain descriptive statistics by group

lapply(HighNote[,columns], function(x) t.test(x ~ HighNote$adopter))



#PART-2 DATA VISUALIZATION
#1. Demographics

#Correlation matrix
correlDem = HighNote[, c('age','male','friend_cnt','good_country','adopter')]
methodDem = cor(correlDem, method = c("pearson", "kendall", "spearman"))
corrplot(methodDem, method="number", title = "Correlation matrix - Demographics", number.cex=1.2)

ggpairs(correlDem)

#Age
meanAge <- ddply(correlDem, "adopter", summarise, age_mean = mean(age))
ggplot(correlDem, aes(x = age, group = adopter, fill = adopter)) +
  ggtitle("Density Plot - Age") +
  geom_vline(data = meanAge, aes(xintercept = age_mean, colour = adopter), linetype = "dashed") + 
  geom_density(alpha = 0.5)

Countrylabel <- paste("Country:", c("US/UK/Germany", "Rest Of the World"))
UserLabel <- paste(c("Premium User", "Free User"))
correlDem %>% mutate(user = ifelse(adopter == 1, UserLabel[1], UserLabel[2]))%>%
  mutate(countryType = ifelse(good_country == 1, Countrylabel[1], Countrylabel[2]))%>%
  ggplot(aes(x = age, y = friend_cnt, color = user, shape = user)) +
  ggtitle("Country-wise User-wise Age vs Friend") + 
  labs(y = "Count of Friends", x = "Age") +
  geom_point(size = 1) +
  facet_wrap(~countryType) + ylim(0,1500)

correlDem$country <- factor(correlDem$good_country, levels = c(0, 1))
correlDem$adopterFactor <- factor(correlDem$adopter, levels = c(0, 1))
correlDem %>% group_by(adopterFactor, country) %>% tally() %>%
  ggplot(aes(country, n, group = as.factor(adopterFactor), fill = adopterFactor)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = 'country', y = 'count') + ggtitle('Count of CountryType by Adopter')

ggplot(correlDem, aes(x = male, group = adopterFactor, fill = adopterFactor)) + 
  geom_bar(aes(fill = adopterFactor)) +
  labs(title="Gender-wise Adopter vs Non-Adopter", x = "Gender (Female:0, Male:1)", y = "Count of Users") 


#2. Peer Influence

#Correlation matrix
correlPeer = HighNote[, c('friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt','subscriber_friend_cnt','adopter')]
methodPeer = cor(correlPeer, method = c("pearson", "kendall", "spearman"))
corrplot(methodPeer, method="number", title = "Correlation matrix - Peer Influence", number.cex=1.2)

ggpairs(correlPeer)

correlPeer$adopterFactor <- factor(correlPeer$adopter, levels = c(0, 1))
meanFrnd <- ddply(correlPeer, "adopterFactor", summarise, frndage_mean = mean(avg_friend_age))
ggplot(correlPeer, aes(x = avg_friend_age, group = adopterFactor, fill = adopterFactor)) +
  ggtitle("Density Plot - Friend Age") +
  labs(x = "Average age of friend") + 
  geom_vline(data = meanFrnd, aes(xintercept = frndage_mean, colour = adopterFactor), linetype = "dashed") + 
  geom_density(alpha = 0.5)

correlPeer %>% mutate(user = ifelse(adopter == 1, UserLabel[1], UserLabel[2])) %>%
  ggplot(aes(x = friend_cnt, y = subscriber_friend_cnt, shape=user, color=user)) + 
  geom_point(size = 2) + ggtitle("User-wise Friend-count vs Subscriber Friend Count") + 
  labs(y = "Count of Friends who are Subscribers", x = "Count of Friends") +
  ylim(0,100) + xlim(0,1500)

ggplot(correlPeer, aes(x=adopterFactor, y=friend_cnt)) + 
  geom_boxplot(aes(fill=adopterFactor),outlier.colour="red", outlier.shape=8,outlier.size=1) + 
  scale_fill_manual(values=c("#FF4500", "#32CD32"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(correlPeer$avg_friend_male, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())

ggplot(correlPeer, aes(x = adopterFactor, y = avg_friend_male, fill = adopterFactor)) + 
  geom_bar(stat = 'summary', fun.y = 'mean') +
  ggtitle("Adopter-wise mean average male friends") + 
  labs(x = "Adopter", y = "Average Male Friends")

ggplot(correlPeer, aes(x=adopterFactor, y=friend_country_cnt)) + 
  geom_bar(aes(fill = adopterFactor), position = "dodge", stat="identity") + 
  labs(x = "Adopter", y = "Count of countries of friends") +
  ggtitle("Count of Friend Countries Adopter vs Non-Adopter")

ggplot(correlPeer, aes(x = adopterFactor, y = friend_country_cnt, fill = adopterFactor)) + 
  geom_bar(stat = 'summary', fun.y = 'mean') +
  ggtitle("Adopter-wise mean of friends countries") + 
  labs(x = "Adopter", y = "Average Friends countries")

ggplot(correlPeer, aes(x = adopterFactor, y = friend_cnt, fill = adopterFactor)) + 
  geom_bar(stat = 'summary', fun.y = 'mean') +
  ggtitle("Adopter-wise average number of friends") + 
  labs(x = "Adopter", y = "Average no. of Friends")

#3. User Engagement

correlEng = HighNote[, c('male','songsListened','lovedTracks','posts','playlists','shouts','adopter')]
methodEng = cor(correlEng, method = c("pearson", "kendall", "spearman"))
corrplot(methodEng, method="number", title = "Correlation matrix - User Engagement", number.cex=1.2)

ggpairs(correlEng)

correlEng$adopterFactor <- factor(correlPeer$adopter, levels = c(0, 1))

Genderlabel <- paste("Gender:", c("Male", "Female"))
correlEng %>% mutate(user = ifelse(adopter == 1, UserLabel[1], UserLabel[2])) %>% 
  mutate(gender = ifelse(male == 1, Genderlabel[1], Genderlabel[2])) %>%
  ggplot(aes(x = songsListened, y = playlists, shape=user, color=user)) + 
  geom_point(size = 2) + ggtitle("User-wise Songs Listened vs Playlists") + 
  labs(y = "Count of Playlists", x = "Count of songs listened") +
  ylim(0,40) + facet_wrap(~gender)

correlEng %>% mutate(user = ifelse(adopter == 1, UserLabel[1], UserLabel[2])) %>% 
  mutate(gender = ifelse(male == 1, Genderlabel[1], Genderlabel[2])) %>%
  ggplot(aes(x = lovedTracks, y = playlists, shape=user, color=user)) + 
  geom_point(size = 2) + ggtitle("User-wise Loved tracks vs Playlists") + 
  labs(y = "Count of Playlists", x = "Count of Loved tracks") +
  ylim(0,40) + facet_wrap(~gender)

correlEng %>% mutate(user = ifelse(adopter == 1, UserLabel[1], UserLabel[2])) %>% 
  mutate(gender = ifelse(male == 1, Genderlabel[1], Genderlabel[2])) %>%
  ggplot(aes(x = songsListened, y = lovedTracks, shape=user, color=user)) + 
  geom_point(size = 2) + ggtitle("User-wise songs listened vs Loved tracks") + 
  labs(x = "Count of songs listened", y = "Count of Loved tracks") +
  ylim(0,10000) + facet_wrap(~gender)

#plot as a grid all the mean value as bar chart
plot1 = ggplot(correlEng, aes(x = adopterFactor, y = songsListened, fill = adopterFactor)) + 
  geom_bar(stat = 'summary', fun.y = 'mean') + labs(x="Adopter") 
plot2<- ggplot(correlEng, aes(x = adopterFactor, y = lovedTracks, fill = adopterFactor))+ 
  geom_bar(stat = 'summary', fun.y = 'mean') + labs(x="Adopter")
plot3<-ggplot(correlEng, aes(x = adopterFactor, y = posts, fill = adopterFactor))+ 
  geom_bar(stat = 'summary', fun.y = 'mean') + labs(x="Adopter")
plot4<-ggplot(correlEng, aes(x = adopterFactor, y = playlists, fill = adopterFactor))+ 
  geom_bar(stat = 'summary', fun.y = 'mean') + labs(x="Adopter")
plot5<-ggplot(correlEng, aes(x = adopterFactor, y = shouts, fill = adopterFactor))+ 
  geom_bar(stat = 'summary', fun.y = 'mean') + labs(x="Adopter")

grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)



#PART-3 Propensity Score Matching
HighNote$treatment <- ifelse(HighNote$subscriber_friend_cnt >=1,1,0)
head(HighNote)
table(HighNote$treatment)
HighNote %>%
  group_by(adopter) %>%
  summarise(mean_treatment = mean(treatment))

#aggregate(treatment ~ adopter, HighNote, mean ) #another way
t.test(HighNote$treatment ~ HighNote$adopter)

cols = c('age','male','friend_cnt','avg_friend_male','avg_friend_age','friend_country_cnt',
         'songsListened','lovedTracks','posts','playlists',
         'shouts','tenure','good_country')

HighNote[,c(cols,'treatment')] %>%
  group_by(treatment) %>%   
  summarise_all(funs(mean(., na.rm = T)))

# Difference-in-means: pre-treatment covariates

lapply(HighNote[,cols], function(x) t.test(x ~ HighNote$treatment))

# Propensity score estimation
# Run a logit model with binomial as family, and here, there is a binary outcome variable,i.e., treatment

ps_model <- glm(treatment ~  age + male + friend_cnt + avg_friend_age + avg_friend_male
              + friend_country_cnt + songsListened + lovedTracks + posts + playlists +
                shouts + tenure + good_country,
                 family = binomial(), data = HighNote)
summary(ps_model)
# Propensity score Calc
propensityScoreDF <- data.frame(pr_score = predict(ps_model, type = "response"),
                               treatment=ps_model$model$treatment)
head(propensityScoreDF)

head(ps_model$model)

# Now we do matched sampling. The method we use is to find pairs of observations that have very similar propensity scores, 
# but that differ in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and then matches observations based on "nearest" method

#1st check and remove missing values
sum(is.na(HighNote))

#Remove Null values just in case, since MatchIt does not allow missing values
HighNoteNoMiss <- HighNote %>%  
  select(treatment, adopter, one_of(cols)) %>%  na.omit()
dim(HighNoteNoMiss)

#Since now no missing values, we use MatchIt
mod_matchit <- matchit(treatment ~ age + male + friend_cnt + avg_friend_age + avg_friend_male
                     + friend_country_cnt + songsListened + lovedTracks + posts + playlists +
                       shouts + tenure + good_country,
                     method = "nearest", data = HighNoteNoMiss)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)
summary(mod_matchit)
plot(mod_matchit)

# Create a dataframe containing only the matched observations, use the match.data() function
data_match <- match.data(mod_matchit)
dim(data_match)
head(data_match)
table(data_match$treatment)
# The final dataset contains a variable called distance, which is the propensity score.

# Examining covariate balance in the matched sample: Difference of means
data_match %>%
  group_by(treatment) %>%    
  select(one_of(cols)) %>%
  summarise_all(funs(mean))


lapply(cols, function(v) {
  t.test(data_match[, v] ~ data_match$treatment)   
})

# Estimating treatment effects
# Estimating the treatment effect is simple once we have 
# a matched sample that we are happy with. We can use a t-test:

with(data_match, t.test(adopter ~ treatment))

glm_model <- glm(adopter ~ treatment, data = data_match,family='binomial')
summary(glm_model)

#Calculating odds ratio
exp(coef(glm_model))

#Test all attributes
glm_modelAll <- glm(adopter ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + 
                      songsListened + lovedTracks + posts + playlists + shouts + 
                      tenure + good_country + treatment, data = data_match,family='binomial')
summary(glm_modelAll)
#Calculating odds ratio
exp(coef(glm_modelAll))

#Test only the significant ones from previous model 
glm_modelSig <- glm(adopter ~ age + male + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + 
                      playlists + tenure + good_country + treatment, data = data_match,family='binomial')
summary(glm_modelSig)
#Calculating odds ratio
exp(coef(glm_modelSig))

#Testing the significant predictors on original dataset

glm_modelOrig <- glm(adopter ~ age + male + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + 
                      playlists + tenure + good_country + treatment, data = HighNote,family='binomial')
summary(glm_modelOrig)
#Calculating odds ratio
exp(coef(glm_modelOrig))



