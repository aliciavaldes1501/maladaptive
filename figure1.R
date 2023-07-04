# Upper panel

x <- seq(-10, 10, by = 0.05)
y1 <- log10(x)
y2 <- log10(rev(x))
y3 <- (y1*y2)/2
df <- data.frame(x, y1, y2, y3)

df$y4<-1/(1+(exp(-1*x)/2))
df$y5<-1/(1+(exp(-1*rev(x))/2))
df$y6<-with(df,(y4*y5))

ggplot(df) + 
  geom_line(aes(x,y4),color="lightgreen",size=1.5)+
  geom_line(aes(x,y5),color="orange",size=1.5)+
  geom_line(aes(x,y6),color="black",size=1.5,linetype=3)+
  my_theme()+
  scale_y_continuous(name=NULL,breaks=seq(0,1,1),limits=c(0,1.8),
                     sec.axis=sec_axis(trans=~.*1,name=NULL,breaks=seq(0,1,1)))+
  scale_x_continuous(labels=NULL,breaks=NULL,name=NULL)

# ggplot(df) + 
#   geom_line(aes(x,y1),color="lightgreen",size=1)+
#   geom_line(aes(x,y2),color="orange",size=1)+
#   geom_line(aes(x,y3),color="black",size=1,linetype=3)+
#   my_theme()+
#   scale_y_continuous(name=NULL,breaks=seq(0,1,0.2),
#                      sec.axis=sec_axis(trans=~.*1,name=NULL,breaks=seq(0,1,0.2)))+
#   scale_x_continuous(labels=NULL,breaks=NULL,name=NULL)
# max(df$y3)
max(df$y6)

# Lower panel

x <- seq(-10, 10, by = 0.05)
y1 <- log10(x)
y2 <- log10(rev(x))
y3 <- (y1*y2)/2
df <- data.frame(x, y1, y2, y3)

df$y4<-1/(1+(exp(-1*(x+6))/2))
df$y5<-1/(1+(exp(-1*(rev(x)-6))/2))
df$y6<-with(df,(y4*y5))

ggplot(df) + 
  geom_line(aes(x,y4),color="lightgreen",size=1.8)+
  geom_line(aes(x,y5),color="orange",size=1.5)+
  geom_line(aes(x,y6),color="black",size=1.5,linetype=3)+
  my_theme()+
  scale_y_continuous(name=NULL,breaks=seq(0,1,1),limits=c(0,1.8),
                     sec.axis=sec_axis(trans=~.*1,name=NULL,breaks=seq(0,1,1)))+
  scale_x_continuous(labels=NULL,breaks=NULL,name=NULL)
max(df$y6)




# OLD
#########################################################

x <- c(-0.05,0,0.25,0.5,1,1.4,1.75,2,2.5,3,4,5,
         6,7,8,9,10,
         11,12,13,14,15,
         16,17,18,19,20,
         21,22,23,24,25)
y1 <- c(-0.05,0,0.1,0.16,0.2,0.25,0.32,0.37,0.42,0.5,0.6,
        0.66,0.71,0.76,0.79,0.82,
        0.85,0.88,0.9,0.92,0.93,
        0.94,0.95,0.96,0.97,0.975,
        0.98,0.985,0.9875,0.99,0.995,0.9975)
y2 <- 1-y1
y3 <-y1*y2
df<-data.frame(x,y1,y2,y3)

ggplot(df) + 
  geom_smooth(aes(x,y1),method="gam",se=F,color="lightgreen")+
  geom_smooth(aes(x,y2),method="gam",se=F,color="orange")+
  geom_smooth(aes(x,y3),method="gam",se=F,color="black",linetype=3)+
  my_theme()+
  scale_y_continuous(name=NULL,breaks=seq(0,1,0.2),
                     sec.axis=sec_axis(trans=~.*1,name=NULL,breaks=seq(0,1,0.2)))+
  scale_x_continuous(labels=NULL,breaks=NULL,name=NULL)
max(df$y3)

legend<-get_legend(df%>%rename(prob_long_season=y1,prob_frosts=y2,Fitness=y3)%>%
  pivot_longer(cols=prob_long_season:Fitness,names_to="names",values_to="values")%>%
  ggplot(aes(x,values,color=names,linetype=names))+ 
  geom_smooth(method="gam",se=F)+
  my_theme_legend()+theme(legend.position="top")+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=NULL,breaks=NULL,name=NULL)+
  scale_y_continuous(name=NULL,breaks=seq(0,1,0.2),
                       sec.axis=sec_axis(trans=~.*1,name=NULL,
                                         breaks=seq(0,1,0.2)))+
  scale_color_manual(limits=c("prob_long_season","prob_frosts","Fitness"),
                     values=c("lightgreen","orange","black"),
                     labels=c("Probability of a sufficiently long growing season",
                              "Probability of avoiding spring frosts", 
                              "Fitness"))+
  scale_linetype_manual(limits=c("prob_long_season","prob_frosts","Fitness"),
                        values=c(1,1,3),
                        labels=c("Probability of a sufficiently long growing season",
                                 "Probability of avoiding spring frosts", 
                                 "Fitness"))+
  guides(color=guide_legend(nrow=3,byrow=TRUE)))

plot(legend)
