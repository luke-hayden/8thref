library(dplyr)
library(ggplot2)

tdpos <- read.csv("TDs-8threfposition.csv", encoding = "UTF-8") #data fro the IT


numpercons <- tdpos %>%
  group_by(Constituency) %>%
  summarise(ntd = n())


tdpos4 <- tdpos %>%
  group_by(Q.1, Constituency) %>%
  summarise(count=n()) %>%
  left_join(numpercons, by="Constituency") %>%
  mutate(perctd = 100*count/ntd)


prelim <- read.csv("prelim2.csv") 
prelim$pc <- "% Yes vote"
prelim$consord <- factor(prelim$Constituency, levels=prelim$Constituency[order(prelim$percyes)])


tdpos4 <- tdpos4 %>%
  left_join(prelim, by='Constituency')

#1st plot: unordered

(p= ggplot(tdpos4, aes(x=Constituency, fill=Q.1, y=perctd)) +
    geom_bar(stat='identity', alpha=0.65) +
    scale_fill_manual(values=c("red3", "cornflower blue", "forest green"), name="TDs' stated position") +
    theme_minimal() +
    ylab("% Yes vote \n TD positions") +
    theme(axis.text.x = element_text(angle = 80, hjust = 1, size=7)) +
    ggtitle("Do TDs' positions on the 8th match their constituents'?", subtitle=" Ireland's 8th Amendment repeal referendum 2018 \n Source: Irish Times Abortion Referendum Tracker")+
    geom_point(data=prelim ,inherit.aes=F, aes(x=Constituency, y=percyes, colour=pc), size=6) +
    scale_colour_manual(values="yellow2", name= "Referendum vote")
)


ggsave("tdvsconstit8th.png", plot=p, device="png", height=7, width=11,dpi=300)

#2nd plot: ordered by yes vote

(p= ggplot(tdpos4, aes(x=consord, fill=Q.1, y=perctd)) +
    geom_bar(stat='identity', alpha=0.65) +
    scale_fill_manual(values=c("red3", "cornflower blue", "forest green"), name="TDs' stated position") +
    theme_minimal() +
    ylab("% Yes vote \n TD positions") +
    theme(axis.text.x = element_text(angle = 80, hjust = 1, size=7)) +
    ggtitle("Do TDs' positions on the 8th match their constituents'?", subtitle=" Ireland's 8th Amendment repeal referendum 2018 \n Source: Irish Times Abortion Referendum Tracker")+
    geom_point(data=prelim ,inherit.aes=F, aes(x=Constituency, y=percyes, colour=pc), size=6) +
    scale_colour_manual(values="yellow2", name= "Referendum vote") +
    xlab("Constituency")
)


ggsave("tdvsconstit8thord.png", plot=p, device="png", height=7, width=11,dpi=300)