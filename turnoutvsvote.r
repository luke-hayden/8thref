library(dplyr)
library(ggplot2)
library(ggrepel)

refres <- read.csv("full_res_ref8.csv")


tylm<-lm(formula=percYes~percTurnout, data=refres, weights=refres$Electorate)

summary(tylm)

refres$lmpred <- predict(tylm)
pval <- round(summary(tylm)$coefficients[2,4], digits=3)
rsq <- round(summary(tylm)$r.squared, digits=4)


(p= ggplot(refres, aes(x=percTurnout, y=percYes, label=Constituency, fill=Verdict))+
    geom_line(aes(y=lmpred), colour="blue",size=2, alpha=0.3, linetype=2)+
    geom_point(aes(size=Total_poll), shape=21)+
    geom_text_repel(direction='y', size=3)+
    scale_size_continuous(name="Votes cast")+
    theme_minimal()+
    xlab("Turnout (%)")+
    ylab("Yes vote (%)")+
    scale_fill_manual(values=c("red3", "forest green"))+ 
    ggtitle("Turnout and Yes votes are correlated", subtitle="Ireland's 8th amendment referendum \ngraphling.org")+
    annotate("text", x=73, y=50, label=paste0("R-squared: ",   rsq, 
                                              "\np-value: ", pval) ))



ggsave("turnoutvsvote.png", plot=p, device="png", height=8, width=10,dpi=300)
