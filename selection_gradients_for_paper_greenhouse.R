# Selection gradients

ggplot()+
  geom_line(data=data.frame(ggpredict(selection_2018,
                                 terms = c("ffd_std[all]","temp[14]"))),
            aes(x=x,y=predicted),color="blue",size=1)+
  geom_line(data=data.frame(ggpredict(selection_2018,
                                 terms = c("ffd_std[all]","temp[21]"))),
            aes(x=x,y=predicted),color="red",size=1)+
  xlab("Standardized FFD")+ylab("Predicted relative fitness")+
  labs(colour="Soil\ntemperature\n(ºC)")
                  
ggplot()+
  geom_line(data=data.frame(ggpredict(selection_2018,
                                      terms = c("ffd_std[all]","temp[all]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),size=1)+
  geom_line(data=data.frame(ggpredict(selection_2018,
                                      terms = c("ffd_std[all]","temp[14]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),color="black",size=1,linetype="dotted")+
  geom_line(data=data.frame(ggpredict(selection_2018,
                                      terms = c("ffd_std[all]","temp[21]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),color="black",size=1,,linetype="dashed")+
  xlab("Standardized FFD")+ylab("Predicted relative fitness")+
  labs(colour="Soil\ntemperature\n(ºC)")+scale_color_viridis()

as.numeric(coef(lm(predicted~x,data=data.frame(ggpredict(selection_2018,
                                                 terms = c("ffd_std[all]","temp[14]")))))[2])
as.numeric(coef(lm(predicted~x,data=data.frame(ggpredict(selection_2018,
                                              terms = c("ffd_std[all]","temp[21]")))))[2])

df_bytemp<-data.frame(ggpredict(selection_2018,
                                 terms = c("ffd_std[all]","temp[4:34,by=1]")))

selgrads_temp <- tibble(
  group = unique(df_bytemp$group),
  slope = sapply(by(df_bytemp, df_bytemp$group, function(subgroup) {
    model <- lm(predicted ~ x, data = subgroup)
    coef(model)  # Extract all coefficients
    }), `[`, "x")  # Extract the slope coefficient for each group
  )

selgrads_temp

write_csv(selgrads_temp,file="C:/Users/alici/Dropbox/SU/Projects/cerastium_greenhouse/data/clean/selgrads_temp_from_Ecology_paper.csv")

# Selection differentials

ggplot()+
  geom_line(data=data.frame(ggpredict(selection_2018_withoutnfl,
                                      terms = c("ffd_std[all]","temp[14]"))),
            aes(x=x,y=predicted),color="blue",size=1)+
  geom_line(data=data.frame(ggpredict(selection_2018_withoutnfl,
                                      terms = c("ffd_std[all]","temp[21]"))),
            aes(x=x,y=predicted),color="red",size=1)+
  xlab("Standardized FFD")+ylab("Predicted relative fitness")+
  labs(colour="Soil\ntemperature\n(ºC)")

ggplot()+
  geom_line(data=data.frame(ggpredict(selection_2018_withoutnfl,
                                      terms = c("ffd_std[all]","temp[all]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),size=1)+
  geom_line(data=data.frame(ggpredict(selection_2018_withoutnfl,
                                      terms = c("ffd_std[all]","temp[14]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),color="black",size=1,linetype="dotted")+
  geom_line(data=data.frame(ggpredict(selection_2018_withoutnfl,
                                      terms = c("ffd_std[all]","temp[21]"))),
            aes(x=x,y=predicted,color=as.numeric(group),group=group),color="black",size=1,,linetype="dashed")+
  xlab("Standardized FFD")+ylab("Predicted relative fitness")+
  labs(colour="Soil\ntemperature\n(ºC)")+scale_color_viridis()

as.numeric(coef(lm(predicted~x,data=data.frame(ggpredict(selection_2018_withoutnfl,
                                                         terms = c("ffd_std[all]","temp[14]")))))[2])
as.numeric(coef(lm(predicted~x,data=data.frame(ggpredict(selection_2018_withoutnfl,
                                                         terms = c("ffd_std[all]","temp[21]")))))[2])

df_bytemp_new<-data.frame(ggpredict(selection_2018_withoutnfl,
                                terms = c("ffd_std[all]","temp[4:34,by=1]")))

seldiffs_temp <- tibble(
  group = unique(df_bytemp_new$group),
  slope = sapply(by(df_bytemp_new, df_bytemp_new$group, function(subgroup) {
    model <- lm(predicted ~ x, data = subgroup)
    coef(model)  # Extract all coefficients
  }), `[`, "x")  # Extract the slope coefficient for each group
)

seldiffs_temp

write_csv(seldiffs_temp,file="C:/Users/alici/Dropbox/SU/Projects/cerastium_greenhouse/data/clean/seldiffs_temp_from_Ecology_paper.csv")
