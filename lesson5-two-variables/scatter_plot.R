#!/usr/bin/Rscript

library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep='\t')
summary(pf)

# simple scatter plot
ggplot(aes(x=age, y=friend_count), data=pf) +
       geom_point()

# overplotting - lots of points in one place
# setting alpha to 1/20 means that 20 points will produce a completely black dot
ggplot(aes(x=age, y=friend_count), data=pf) +
       geom_point(alpha=1/20) +
       xlim(13,90)

# changing y coordinate, clearly shows distribution in the range in which most points are
ggplot(aes(x=age, y=friend_count), data=pf) +
       geom_point(alpha=1/20) +
       xlim(13,90) +
       coord_trans(y="sqrt")
