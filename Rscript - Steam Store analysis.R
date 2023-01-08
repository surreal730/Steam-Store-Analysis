
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)

## Exploratory Data Analysis (EDA)

### Group K - Syed Ahmed Masood, Yujin Kim, Nadia Khalili, Sylvia Cao

#### What is Steam?

##### Steam is a video game digital distribution service and storefront by Valve. It was launched as a software client in September 2003 as a way for Valve to provide automatic updates for their games, and expanded to distributing and offering third-party game publishers' titles in late 2005.

steam <- read.csv("~/Documents/MSBA Databases:Files/MGTA 452 -  Collecting & Analyzing Large Data/Group Project/data/steam.csv")

#parsing platforms
steam[c('Windows', 'Mac', 'Linux')] <- str_split_fixed(steam$platforms, ';', 3)

#parsing categories
df <- str_split_fixed(steam$categories, ';', n = Inf)

steam[c('Single Player', 'Multi-Player', 'Other')] <-str_split_fixed(steam$categories, ';', n = 3)

#parsing years
steam[c('Year')] <- substr(steam$release_date, 1, 4)

steam[c('Month')] <- substr(steam$release_date, 6,7)

#parsing steamspytags
steam[c('Genre1', 'Genre2', 'Genre3')] <- str_split_fixed(steam$steamspy_tags, ";", n = 3)



#### When did Steam release it's first game?
#first release
min(steam$release_date)

#creating total reviews column
steam$total_reviews <- steam$positive_ratings + steam$negative_ratings

#creating positive rate column
steam$positive_rate <- steam$positive_ratings/steam$total_reviews

## average positive reviews
avg_pos <- mean(steam$positive_ratings)
avg_pos

##average negative reviews
avg_neg <- mean(steam$negative_ratings)
avg_neg

##average reviews
avg_rev <- mean(steam$total_reviews)
avg_rev

#top 10 games with highest positivity rate
top10positive_rate <- steam %>%
  arrange(desc(positive_rate)) %>%
  filter(positive_ratings > avg_pos & negative_ratings > avg_neg & total_reviews > avg_rev) %>%
  head(10)


#releases per year
releases_per_year <- steam %>%
  group_by(Year) %>%
  summarize(n = n())

p<-ggplot(data=releases_per_year, aes(x=Year, y=n,fill=Year)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 9000, by = 1000)) +geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position = "none") +
  
s <- p + q 


#positive rate per year
positiverate_per_year <- steam %>%
  group_by(Year) %>%
  summarize(avg_posrate = mean(positive_rate))

p<-ggplot(data=positiverate_per_year, aes(x=Year, y=avg_posrate, fill=avg_posrate)) +
   geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_text(aes(label=round(avg_posrate,2)), position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position = "none") +
  theme(text = element_text(size=rel(4)))

#avg rate per year
average_playtime_peryear <- steam %>%
  group_by(Year) %>%
  summarize(avg_playtime = mean(median_playtime))

p<-ggplot(data=average_playtime_peryear, aes(x=Year, y=avg_playtime)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 1000, by = 20)) +
  geom_text(aes(label=round(avg_playtime,1)), position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position = "none") +
  theme(text = element_text(size=rel(4)))


p<- ggplot(data=average_playtime_peryear, aes(x=avg_playtime)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
p <- ggplot(data=average_playtime_peryear, aes(x=Year, y=avg_playtime, group=1)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) + 
  theme(text = element_text(size=rel(4)))

#top publishers
publishers_game_count <- steam %>%
  group_by(publisher) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


developers_game_count <- steam %>%
  group_by(developer) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

#worst publishers with highest ownership
worst_publishers_highestownership <- steam %>%
  group_by(publisher) %>%
  filter (owners == "10000000-20000000") %>%
  arrange(positive_rate)

#games with most upvotes
mostpopular_games <- steam %>%
  arrange(desc(positive_ratings)) %>%
  head(15)



###average selling price for reviews > 1000

avgprice_reviews1000 <- steam %>%
  filter(total_reviews > 1000)




cc <- mean(avgprice_reviews1000$price)



p<-ggplot(data=mostpopular_games, aes(x=name, y=price,fill=publisher)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 50, by =2.5))+
  labs(x = "", y ="Price") + geom_text(aes(label=round(price,2)), position=position_dodge(width=0.4), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face='bold', size=15),legend.position = "none", text = element_text(size=rel(4))) + 
  geom_hline(yintercept=cc, linetype='dotted', col = 'maroon')+
  annotate("text", x = "", y = 12.5, label = "AAA Average", vjust = 0, col='maroon', size=4)




#popular games that are 18+
mostpopular_games_18 <- steam %>%
  filter(required_age >= 18) %>%
  arrange(desc(positive_ratings)) %>%
  head(15)


#popular games that are priced > $50
mostpopular_games_50 <- steam %>%
  filter(price >= 49.99) %>%
  arrange(desc(positive_ratings)) %>%
  head(15)

#popular mac games
mostpopular_games_mac <- steam %>%
  filter(Mac == 'mac') %>%
  arrange(desc(positive_ratings)) %>%
  head(15)


#average achievements
mean(steam$achievements)


#publisher mean positive_rate
publishercount <- steam %>%
  group_by(publisher) %>%
  summarize(n = n()) %>%
  filter(n > 10) %>%
  arrange(desc(n))



meanpositiverate <- steam %>%
  inner_join(publishercount, by = c('publisher')) %>%
  filter(total_reviews > avg_rev) %>%
  group_by(publisher) %>%
  summarize(mean = mean(positive_rate)) %>%
  arrange(desc(mean))


#worst games
lowestpositiverate <- steam %>%
  arrange(positive_rate) %>%
  filter(total_reviews > 50000)

#mostplayed
mostplayed <- steam %>%
  arrange(desc(average_playtime)) %>%
  filter(total_reviews > 50000) %>% 
  head(15)


#best valve games
valve_games_best <- steam %>%
  filter(developer %in% "Valve") %>%
  arrange(desc(positive_rate))

#most played games on steam pre streaming era (year < 2010)
mostplayed_prestream <- steam %>%
  arrange(desc(average_playtime)) %>%
  filter(total_reviews > 50000 & Year <= 2010) %>% 
  head(15)

#most played games on steam post streaming era (year > 2010)
mostplayed_poststream <- steam %>%
  arrange(desc(average_playtime)) %>%
  filter(total_reviews > 50000 & Year > 2010) %>% 
  head(15) %>%
  arrange(desc(positive_rate))


#yearly quality of games
yearlyquality <- steam %>%
  group_by(Year) %>%
  summarize(meanscore = mean(positive_rate))

q<-ggplot(data=yearlyquality, aes(x=Year, y=meanscore,group=1)) +
  geom_path()+
  geom_point()
  



#publishers with the most reviews
publisher_review_sum <- steam %>%
  group_by(publisher) %>%
  summarize(total_positivereviews = sum(positive_ratings)) %>%
  arrange(desc(total_positivereviews)) %>%
  head(10)

publisher_avg_price <- steam %>%
  group_by(publisher) %>%
  summarize(avg_price = mean(price))

toppublisheravgprice <- publisher_review_sum %>%
  inner_join(publisher_avg_price, by = 'publisher') %>%
  arrange(desc(avg_price))

toppublisheravgprice


avg_price <- mean(steam$price)

toppublisheravgprice[1,1] <- "2K Games"

p<-ggplot(data=toppublisheravgprice[1:10,1:3], aes(x=publisher, y=avg_price,fill=publisher)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 50, by =2.5))+
  labs(x = "Publisher", y ="Average Selling Price") + geom_text(aes(label=round(avg_price,2)), position=position_dodge(width=0.4), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face='bold', size=15),legend.position = "none", text = element_text(size=rel(4))) + 
  geom_hline(yintercept=cc, linetype='dotted', col = 'maroon')+
  annotate("text", x = "", y = 7, label = "Average", vjust = -0.5, col='maroon', size=6)








#pie chart for publishers
publisher_review_sum$prop <- publisher_review_sum$total_positivereviews/sum(publisher_review_sum$total_positivereviews)



ggplot(publisher_review_sum, aes(x="", y=total_positivereviews, fill=publisher)) +
  geom_col(color="white") +
  geom_label(aes(label = round(prop*100,1), size=25),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + 
  coord_polar(theta = "y") +
  theme(text = element_text(size=rel(4)),legend.position = 'left', legend.text=element_text(size=15), axis.text.x = element_text(vjust = 0.5, hjust=1, face='bold', size=10)) + 
  scale_fill_viridis_d() +
  labs(x = "", y="Total Positive Reviews") 

#release per month
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'July', 'Aug ','Sep', 'Oct', 'Nov', 'Dec')

releases_per_month <- steam %>%
  group_by(Month) %>%
  summarize(n = n()) %>%
  mutate(Month = months)

#TOP MAC games
topmacgames <- steam %>%
  filter(Mac == 'mac' & owners == "10000000-20000000") %>%
  arrange(desc(positive_rate))

topwindowsgames <- steam %>%
  filter(Windows == 'windows' & owners == "10000000-20000000") %>%
  arrange(desc(positive_rate))

#most popular genres with high ownership
genregroupingpositiverate <- steam %>%
  filter(owners == '10000000-20000000') %>%
  group_by(Genre1) %>% 
  summarize(mean_rate = mean(positive_rate)) %>%
  arrange(desc(mean_rate))

#most games mades in genres with high ownership
genregroupingcount <- steam %>%
  filter(owners == '10000000-20000000') %>%
  group_by(Genre1) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

#top free 2 play games
f2p <- steam %>%
  filter(price == '0') %>%
  arrange(desc(total_reviews)) %>%
  head(10)
  
#genres with most games
genregamecount <- steam %>%
  group_by(Genre1) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

#histogram for Genre 
p<-ggplot(data=head(genregamecount,10), aes(x=Genre1, y=n,fill=Genre1)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 5000, by = 500))+
  labs(x = "Genre", y ="# of games") + geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

p<-ggplot(data=head(genregamecount,10), aes(x=Genre1, y=n,fill=Genre1)) +
  geom_bar(stat="identity") + scale_y_continuous(breaks = seq(0, 5000, by = 500))+
  labs(x = "Genre", y ="# of games") + geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

year <- c(yearlyquality$Year)
mean <- c(yearlyquality$meanscore)
count <- c(releases_per_year$n)


