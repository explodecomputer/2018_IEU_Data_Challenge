library(data.table)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(MASS)


formatTwDate <- function(datestring, format="datetime"){
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    if (format=="date"){
        date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }   
    return(date)
}


rntransform <- function(var)
{
	out <- rank(var) - 0.5
	out[is.na(var)] <- NA
	mP <- .5/max(out,na.rm=T)
	out <- out/(max(out,na.rm=T)+.5)
	out <- qnorm(out)
	return(out)
}


# Read in data
flu <- fread("Twitter_flutweets/flu.csv")
nhs <- fread("Twitter_nhstweets/nhs.csv")

# Remove retweets
flu <- subset(flu, !grepl("^RT", tweettext))
NHS <- subset(NHS, !grepl("^RT", tweettext))

grepl(":)", flu$tweettext) %>% sum
grepl(":\\(", flu$tweettext) %>% sum

grepl(":)", nhs$tweettext) %>% sum
grepl(":\\(", nhs$tweettext) %>% sum

tidy_flu <- flu %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, tweettext)
tidy_flu %>% inner_join(sentiments) %>% count(sentiment, sort=TRUE)

tidy_nhs <- nhs %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, tweettext)
tidy_nhs %>% inner_join(sentiments) %>% count(sentiment, sort=TRUE)


flu$tweettext[subset(tidy_flu %>% inner_join(sentiments), sentiment == "litigious")$linenumber %>% unique] %>% head

temp1 <- tidy_flu %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 5000, sentiment, created_at) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative, date=formatTwDate(created_at)) %>%
  group_by(index) %>%
  summarise(date=mean(date), sentiment=sum(sentiment), negative=sum(negative), positive=sum(positive), search="flu")

temp2 <- tidy_nhs %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 5000, sentiment, created_at) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative, date=formatTwDate(created_at)) %>%
  group_by(index) %>%
  summarise(date=mean(date), sentiment=sum(sentiment), negative=sum(negative), positive=sum(positive), search="nhs")
temp <- rbind(temp1, temp2)

temp_mod <- temp
temp_mod$date[temp_mod$search == "nhs"] <- temp_mod$date[temp_mod$search == "nhs"] + hours(8)


ggplot(temp, aes(x=date, y=sentiment)) +
  geom_line(aes(colour=search)) +
  # geom_text(data=subset(temp, sentiment < -2400), aes(label=date)) +
geom_smooth(aes(colour=search)) +
geom_hline(yintercept=0, linetype="dotted") +
# geom_vline(xintercept=subset(temp_mod, sentiment==sort(sentiment)[2])$date, linetype="dashed") +
labs(x="Date/time (GMT)", y="(Positive - negative) tweet sentiments")
ggsave("posneg_tweets.pdf")

###

users <- rbind(flu, nhs) %>%
	select(screenname, userfollowercount, userfriendcount)
users$status <- users$userfollowercount / users$userfriendcount

st <- tidy_flu %>%
inner_join(get_sentiments("bing")) %>% group_by(screenname) %>%
	summarise(userfollowercount=first(userfollowercount), userfriendcount=first(userfriendcount), status=first(userfollowercount / userfriendcount), n=n(), positive=sum(sentiment=="positive"), negative=sum(sentiment=="negative"), sentiment=sum(positive) - sum(negative))

ggplot(subset(st, userfollowercount < 1000 & userfollowercount < 1000), aes(x=rntransform(userfriendcount), y=rntransform(sentiment))) +
	geom_point() +
	geom_smooth() +
	geom_smooth(method="lm")

ggplot(subset(st, userfollowercount < 1000 & userfriendcount < 1000 & userfriendcount > 10 & userfollowercount > 10), 
	aes(x=(log(userfriendcount)), y=rntransform(sentiment))) +
	geom_point() +
	geom_smooth() +
	geom_smooth(method="lm")



summary(rlm(sentiment ~ log(userfriendcount), data=subset(st, userfollowercount < 1000 & userfriendcount < 1000 & userfriendcount > 10 & userfollowercount > 10)), na.action='na.omit')

summary(rlm(sentiment ~ log(userfollowercount), data=subset(st, userfollowercount < 1000 & userfriendcount < 1000 & userfriendcount > 10 & userfollowercount > 10)), na.action='na.omit')

summary(rlm(sentiment ~ userfollowercount, data=subset(st, userfollowercount < 1000 | userfriendcount < 1000) )))

##

## Get user sentiments

tidy_nhs$search <- "nhs"
tidy_flu$search <- "flu"
tidy_tweets <- rbind(tidy_nhs, tidy_flu)

st <- tidy_tweets %>%
inner_join(sentiments) %>% 
group_by(screenname, sentiment, search) %>%
summarise(
	userfollowercount=first(userfollowercount), 
	userfriendcount=first(userfriendcount), 
	status=first(userfollowercount / userfriendcount), 
	n=n()
)

st <- group_by(st, screenname, search) %>%
mutate(count=sum(n), s=n/count)

st2 <- subset(st, userfollowercount < 1000 & userfriendcount < 1000)
st2$statusgr <- cut(rntransform(st2$status), breaks=5)

ggplot(subset(st2, !is.na(sentiment)), aes(x=as.numeric(statusgr), y=s)) +
geom_bar(stat = "summary", fun.y = "mean", aes(fill=statusgr), position="dodge") +
geom_smooth(method="lm") +
facet_grid(search ~ sentiment) +
# scale_fill_brewer(type="qual") +
labs(x="User status", y="Propensity for sentiment in tweets") +
theme(legend.position="none")
ggsave("tweet_sentiments.pdf")


## Cluster analysis of sentiments

tt <- tidy_tweets %>% inner_join(sentiments) %>% dplyr::select(linenumber, sentiment) %>%
	filter(!is.na(sentiment)) %>%
	group_by(linenumber, sentiment) %>%
	summarise(n=n())

a <- spread(tt, sentiment, n, fill=0)

av <- cor(a[,-1])

pdf(file="heatmap.pdf")
gplots::heatmap.2(av)
dev.off()



