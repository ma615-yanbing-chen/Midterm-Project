library(ggplot2)
library(dplyr)
all_states <- map_data("state")  
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey30" )


ny <- filter(all_states, region == "new york")
ca <- filter(all_states, region == "california")
fl <- filter(all_states, region == "florida")
wa <- filter(all_states, region == "washington")


p + geom_polygon(data = all_states, aes(x=long, y=lat, group = group),fill="lightblue") +
  geom_polygon(data = ny, aes(x=long, y=lat, group = group),fill="red") +
  geom_polygon(data = ca, aes(x=long, y=lat, group = group),fill="blue")+
  geom_polygon(data = fl, aes(x=long, y=lat, group = group),fill="green")+
  geom_polygon(data = wa, aes(x=long, y=lat, group = group),fill="yellow")

