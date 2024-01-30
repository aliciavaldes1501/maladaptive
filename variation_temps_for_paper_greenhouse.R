
data_var<-logger_data_pairs%>%
  mutate(month = month(datetime),date=date(datetime))%>% 
  # new variables "month" and "date"
  filter(month==4|month==5|month==6)%>% # keep data from april-june
  group_by(pair,above_below)%>% 
  summarise(mean=mean(temp,na.rm=T),var=var(temp,na.rm=T),sd=sd(temp,na.rm=T),
            max=max(temp),min=min(temp))%>% 
  mutate(range=max-min)%>%
  pivot_wider(names_from="above_below",
              values_from=c("mean","var","sd","max","min","range"))%>%
  group_by(pair)%>%
  left_join(logger_data_pairs%>%
              mutate(month = month(datetime))%>%
              filter(month==4|month==5|month==6)%>%
              filter(above_below=="B")%>%
              group_by(pair)%>%
              summarise(meansoiltemp=mean(temp)))


summary(lm(var_A~meansoiltemp,data_var))
summary(lm(sd_A~meansoiltemp,data_var))
summary(lm(range_A~meansoiltemp,data_var))

summary(lm(var_B~meansoiltemp,data_var))
summary(lm(sd_B~meansoiltemp,data_var))
summary(lm(range_B~meansoiltemp,data_var))

grid.arrange(
  grid.arrange(
    ggplot(data_var,aes(x=meansoiltemp,y=var_A))+
      xlab("Mean soil temperature (ºC)")+ylab("Aboveground temperature variance")+
      geom_point()+geom_smooth(method="lm"),
    ggplot(data_var,aes(x=meansoiltemp,y=sd_A))+
      xlab("Mean soil temperature (ºC)")+ylab("Aboveground temperature SD")+
      geom_point()+geom_smooth(method="lm"),
    ggplot(data_var,aes(x=meansoiltemp,y=range_A))+
      xlab("Mean soil temperature (ºC)")+ylab("Aboveground temperature range")+
      geom_point()+geom_smooth(method="lm"),
    ncol=1),
  grid.arrange(
    ggplot(data_var,aes(x=meansoiltemp,y=var_B))+
      xlab("Mean soil temperature (ºC)")+ylab("Belowground temperature variance")+
      geom_point()+geom_smooth(method="lm"),
    ggplot(data_var,aes(x=meansoiltemp,y=sd_B))+
      xlab("Mean soil temperature (ºC)")+ylab("Belowground temperature SD")+
      geom_point()+geom_smooth(method="lm"),
    ggplot(data_var,aes(x=meansoiltemp,y=range_B))+
      xlab("Mean soil temperature (ºC)")+ylab("Belowground temperature range")+
      geom_point()+geom_smooth(method="lm"),
    ncol=1),
  ncol=2)

logger_data_pairs%>%
  mutate(month = month(datetime),date=date(datetime))%>% 
  # new variables "month" and "date"
  filter(month==4|month==5|month==6)%>% # keep data from april-june
  group_by(date,pair,above_below)%>% 
  summarise(mean=mean(temp,na.rm=T),var=var(temp,na.rm=T),sd=sd(temp,na.rm=T),
            max=max(temp),min=min(temp))%>%
  pivot_wider(names_from="above_below",values_from=c("mean","var","sd","max","min"))%>%
  group_by(pair)%>%
  left_join(logger_data_pairs%>%
              mutate(month = month(datetime))%>%
              filter(month==4|month==5|month==6)%>%
              filter(above_below=="B")%>%
              group_by(pair)%>%
              summarise(meansoiltemp=mean(temp)))%>%
# calculate mean soil temperature for april-june
  ggplot(aes(x=meansoiltemp,y=var_A))+geom_point()+geom_smooth(method="lm")

logger_data_pairs%>%
  mutate(month = month(datetime),date=date(datetime))%>% 
  # new variables "month" and "date"
  filter(month==4|month==5|month==6)%>% # keep data from april-june
  group_by(date,pair,above_below)%>% 
  summarise(mean=mean(temp,na.rm=T),var=var(temp,na.rm=T),sd=sd(temp,na.rm=T),
            max=max(temp),min=min(temp))%>%
  pivot_wider(names_from="above_below",values_from=c("mean","var","sd","max","min"))%>%
  group_by(pair)%>%
  left_join(logger_data_pairs%>%
              mutate(month = month(datetime))%>%
              filter(month==4|month==5|month==6)%>%
              filter(above_below=="B")%>%
              group_by(pair)%>%
              summarise(meansoiltemp=mean(temp)))%>%
  # calculate mean soil temperature for april-june
  ggplot(aes(x=meansoiltemp,y=var_B))+geom_point()+geom_smooth(method="lm")

summary(lm(var_A~meansoiltemp,
           data=logger_data_pairs%>%
             mutate(month = month(datetime),date=date(datetime))%>% 
             # new variables "month" and "date"
             filter(month==4|month==5|month==6)%>% # keep data from april-june
             group_by(date,pair,above_below)%>% 
             summarise(mean=mean(temp,na.rm=T),var=var(temp,na.rm=T),sd=sd(temp,na.rm=T),
                       max=max(temp),min=min(temp))%>%
             pivot_wider(names_from="above_below",values_from=c("mean","var","sd","max","min"))%>%
             group_by(pair)%>%
             left_join(logger_data_pairs%>%
                         mutate(month = month(datetime))%>%
                         filter(month==4|month==5|month==6)%>%
                         filter(above_below=="B")%>%
                         group_by(pair)%>%
                         summarise(meansoiltemp=mean(temp)))))

summary(lm(var_B~meansoiltemp,
           data=logger_data_pairs%>%
             mutate(month = month(datetime),date=date(datetime))%>% 
             # new variables "month" and "date"
             filter(month==4|month==5|month==6)%>% # keep data from april-june
             group_by(date,pair,above_below)%>% 
             summarise(mean=mean(temp,na.rm=T),var=var(temp,na.rm=T),sd=sd(temp,na.rm=T),
                       max=max(temp),min=min(temp))%>%
             pivot_wider(names_from="above_below",values_from=c("mean","var","sd","max","min"))%>%
             group_by(pair)%>%
             left_join(logger_data_pairs%>%
                         mutate(month = month(datetime))%>%
                         filter(month==4|month==5|month==6)%>%
                         filter(above_below=="B")%>%
                         group_by(pair)%>%
                         summarise(meansoiltemp=mean(temp)))))
