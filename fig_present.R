ggplot(data_may,aes(x=mean_B,y=mean_A,group=meansoiltemp))+
  geom_point(aes(color=meansoiltemp),size=1.5,alpha=0.3)+
  geom_smooth(aes(color=meansoiltemp),method="lm",se=F,size=0.75)+
  my_theme()+
  xlab("Mean soil temperature (ºC)")+ylab("Mean air temperature (ºC)")+
  scale_color_viridis()+labs(colour="Mean soil\ntemperature\n(ºC)")

# pairs 57 and 43

ggplot(subset(data_may,pair==57|pair==43),aes(x=mean_B,y=mean_A,group=as.factor(pair),color=as.factor(pair)))+
  geom_point(size=2,alpha=0.4)+
  geom_smooth(method="lm",se=F,size=0.75)+
  my_theme()+
  xlab("Soil temperature (ºC)")+ylab("Air temperature (ºC)")+
  scale_color_manual(values=c("brown1","cornflowerblue"))

ggplot(subset(data_may,pair==57|pair==43),aes(x=max_B,y=max_A,group=as.factor(pair),color=as.factor(pair)))+
  geom_point(size=2,alpha=0.4)+
  geom_smooth(method="lm",se=F,size=0.75)+
  my_theme()+
  xlab("Soil temperature (ºC)")+ylab("Air temperature (ºC)")+
  scale_color_manual(values=c("brown1","cornflowerblue"))

ggplot(rbind(data.frame(pred_fitness_17)%>%
               mutate(year=as.factor(2017)),
             data.frame(pred_fitness_18)%>%
               mutate(year=as.factor(2018))),
       aes(x,predicted,colour=group,fill=group))+
  facet_grid(~year,scales="free")+
  geom_line(aes(color=as.numeric(as.character(group))),
            size=0.5)+
  my_theme_legend()+
  theme(legend.position="right")+ggtitle("B)")+
  labs(colour="Soil\ntemperature\n(ºC)")+
  xlab("Standardized FFD")+
  ylab("Predicted nrelative fitness")+
  theme(plot.title=element_text(hjust=-0.3,vjust=-5))+
  theme(plot.margin = unit(c(-0.6,0.3,0,0.3), "cm"))+
  scale_y_continuous(limits=c(-0.4,2.8),
                     breaks=c(0,0.5,1,1.5,2,2.5))+
  scale_color_gradient(low="blue",high="red")

ggplot(data.frame(pred_fitness_17),
       aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=as.numeric(as.character(group))),
            size=0.5)+
  my_theme()+ggtitle("B)")+
  xlab("FFD")+
  ylab("Predicted fitness")+
  theme(plot.title=element_text(hjust=-0.6,vjust=-5))+
  theme(plot.margin = unit(c(-0.6,0.3,0,0.3), "cm"))+
  scale_y_continuous(limits=c(-0.4,2.8),
                     breaks=c(0,0.5,1,1.5,2,2.5))+
  scale_color_gradient(low="blue",high="red")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())

ggplot(data.frame(pred_fitness_18),
       aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=as.numeric(as.character(group))),
            size=0.5)+
  my_theme()+ggtitle("B)")+
  xlab("FFD")+
  ylab("Predicted fitness")+
  theme(plot.title=element_text(hjust=-0.6,vjust=-5))+
  theme(plot.margin = unit(c(-0.6,0.3,0,0.3), "cm"))+
  scale_y_continuous(limits=c(-0.4,2.8),
                     breaks=c(0,0.5,1,1.5,2,2.5))+
  scale_color_gradient(low="blue",high="red")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())

ggplot(rbind(data.frame(pred_fitness_17)%>%
               mutate(year=as.factor(2017)),
             data.frame(pred_fitness_18)%>%
               mutate(year=as.factor(2018))),
       aes(x,predicted,colour=group,fill=group))+
  facet_grid(~year,scales="free")+
  geom_line(aes(color=as.numeric(as.character(group))),
            size=0.5)+
  my_theme()+ggtitle("B)")+
  xlab("FFD")+
  ylab("Predicted fitness")+
  theme(plot.title=element_text(hjust=-0.3,vjust=-5))+
  theme(plot.margin = unit(c(-0.6,0.3,0,0.3), "cm"))+
  scale_y_continuous(limits=c(-0.4,2.8),
                     breaks=c(0,0.5,1,1.5,2,2.5))+
  scale_color_gradient(low="blue",high="red")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())
