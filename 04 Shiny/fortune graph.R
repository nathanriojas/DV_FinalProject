ggplot(data=fortune, 
       mapping=aes(x = REVENUE, fill = COMPANY,color=COMPANY)) + 
  coord_cartesian() + 
  theme(plot.background = element_rect(fill = "white",colour= "white",size = 3))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.title = element_text(colour = "black",size = 15))+
  theme(legend.position="top")+
  scale_x_continuous() +
  labs(title='How Disease Affects Number of Deaths Worldwide \n When Compared to Death Per 100,000 Statistics from 1970 to 2010') +
  labs(x="Revenue", y=paste("Count")) +
  geom_histogram(binwidth = 15000,colour = "black") + 
  scale_fill_manual(values=c("#006666", "#CC3300", "#FF0000"))+
  geom_vline(aes(xintercept=mean(REVENUE)),
              color="blue", linetype="dashed", size=1)

