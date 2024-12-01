###Trying things
library(ggplot2)
library(gganimate)

### creating a data frame with the sorted lists and the differences
day1df<-cbind(q=1:length(leftlist),leftlist=sort(leftlist),rightlist=sort(rightlist),listdiffs)
td<-c()
### showing the sum of the differences as it moves through the list
for(i in 1:nrow(day1df)){
  td<-c(td,sum(listdiffs[1:i]))}
day1df<-cbind(day1df,td)


day1plot <- ggplot(day1df) +
  geom_text(aes(x = 5, y = 0, label = str_c("left = ",leftlist), size = 5)) +
  geom_text(aes(x = 10, y = 0, label = str_c("right = ",rightlist), size = 5)) +
  geom_text(aes(x = 15, y = 0, label = str_c("diff = ",listdiffs), size = 5)) +
  geom_text(aes(x = 20, y = 0, label = str_c("total = ",td), size = 5)) +
  theme_void() +
  guides(size = "none") +
  scale_size_area(max_size = 4) +
  transition_states(q,wrap=FALSE)

day1animation <- animate(day1plot, nframes = 1001, duration = .5,end_pause =50)
day1animation