#!/usr/bin/Rscript

library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep='\t')

# group users by age and find the mean and median friend count for each age group
library(dplyr)
pf.fc_by_age <- pf %>%
                group_by(age) %>%
                summarise(friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n()) %>%
                arrange(age)
head(pf.fc_by_age, 20)

# plot friend count by age
ggplot(aes(x=age, y=friend_count), data = pf) +
  xlim(13,90)+
  geom_point(alpha=0.05, color='orange') +
  coord_trans(y='sqrt')

# plot friend count by age together with a line for the mean
p <- ggplot(aes(x=age, y=friend_count), data = pf) +
  xlim(13,90)+
  geom_point(alpha=0.05, color='orange') +
  coord_trans(y='sqrt')

p +
  geom_line(stat = 'summary', fun.y = mean)

# plot it together with lines for the mean and .1 quantile
p +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.1), linetype = 2, color = 'blue')

# plot it together with lines for the mean, .1, .9 quantiles
p +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.9), linetype = 2, color = 'blue')

# plot it together with lines for the mean, .1, .5, .9 quantiles
p +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.9), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.5), color = 'blue')

# use coord_cartesian to zoom into the densest data (less than 1000 friends)
ggplot(aes(x=age, y=friend_count), data = pf) +
  geom_point(alpha=0.05, color='orange') +
  coord_cartesian(xlim=c(13,90), ylim=c(0,1000)) +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.9), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args=list(probs = 0.5), color = 'blue')

# use the pearson method to calculate correlation between age and friend count
cor.test(pf$age, pf$friend_count, method="pearson")
with(pf, cor.test(age, friend_count, method='pearson'))

# take a subset of the users who are 70 and younger for the correlation
with(subset(pf, age <= 70), cor.test(age, friend_count, method='pearson'))

# plot likes received on the desketop vs all likes received (heavily correlated)
ggplot(aes(x=www_likes_received,y=likes_received), data = pf) +
  geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95))

# calculate the correlation
with(pf, cor.test(www_likes_received, likes_received))
