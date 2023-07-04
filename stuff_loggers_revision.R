data_plants%>%mutate(ffd_minus_one=ffd-30)

logger_data%>%mutate(date_doy=yday(datetime),year=year(datetime))%>%
  filter(year==2018,above_below=="A")%>%
  ggplot(aes(x=date_doy,y=temp,color=temp_term,group=logger_nr))+geom_line()


logger_data%>%
  mutate(month = month(datetime),date=date(datetime))%>% 
  mutate(date_doy=yday(datetime),year=year(datetime))%>%
  filter(year==2018,above_below=="A")%>%
  filter(month==4|month==5|month==6)%>% # keep data from april-may-june
  group_by(date_doy,logger_nr,temp_term)%>% 
  summarise(mean=mean(temp,na.rm=T),max=max(temp),min=min(temp))%>% 
  ggplot(aes(x=date_doy,y=min,color=temp_term,group=logger_nr))+geom_line()

data_model<-logger_data%>%
  mutate(month = month(datetime),date=date(datetime))%>% 
  mutate(date_doy=yday(datetime),year=year(datetime))%>%
  filter(year==2018,above_below=="A")%>%
  filter(month==4|month==5|month==6)%>% # keep data from april-may-june
  group_by(date_doy,logger_nr,temp_term)%>% 
  summarise(mean=mean(temp,na.rm=T),max=max(temp),min=min(temp))


summary(lm(min~date_doy*temp_term,data_model))

plot(ggpredict(lm(min~date_doy*temp_term,data_model),terms=c("date_doy","temp_term")))

########################################################################################

left_join(
  #
  logger_data%>%
    mutate(month = month(datetime),date=date(datetime))%>% 
    mutate(date_doy=yday(datetime),year=year(datetime))%>%
    filter(year==2018,above_below=="A")%>%
    filter(month==4|month==5|month==6),
  #
  data_plants%>%mutate(ffd_minus_one=ffd-30)%>%filter(year==2018)%>%dplyr::select(id,temp,ffd,ffd_minus_one),
  by=join_by(temp_term==temp) ## ffd_minus_one not covered by logger period in many cases
)%>%
  filter(date_doy>=ffd_minus_one&date_doy<ffd)%>%
  group_by(date,id,temp_term)%>% 
  summarise(min=min(temp))%>%
  ggplot(aes(x=date,y=min,group=id,color=temp_term))+geom_line()
  
  group_by()%>%
  summarise()


