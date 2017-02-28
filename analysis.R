# load libraries
library(MASS) # to avoid shadowing of select from dplyr
library(ggplot2) 
library(dplyr)
library(scales)
# load pre-labelled data
source("./load_data.R")

ggplot(veh,aes(x=YEAR))+
    geom_line(aes(y=Moto,colour='Moto'))+
    geom_line(aes(y=Car,colour='Car'))+
    geom_line(aes(y=LICENSED,colour='Lic'))+
    geom_smooth(method='lm',aes(y=Moto,colour='Moto'))+
    geom_smooth(method='lm',aes(y=Car,colour='Car'))+
    geom_smooth(method='lm',aes(y=LICENSED,colour='Lic'))+
    scale_colour_manual(values = c("Moto"='red',"Car"='blue','Lic'='green'),breaks = c('Moto','Car','Lic'),name='Type')+
    scale_y_continuous("number of registered vehicles",labels = scales::comma)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
ggplot(counts_veh_year,aes(x=YEAR_,y=Counts))+
    geom_line(aes(x=YEAR_,y=Counts,colour='Accidents'))+
    geom_line(aes(x=YEAR_,y=total/100,colour='Total/100'))+
    ggtitle('By Year')+
    scale_y_continuous("Counts",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+expand_limits(y = 0)+
    theme_bw()+
    scale_colour_manual("",values = c("Accidents"='red',"Total/100"='green'))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(counts_veh_month,aes(x=C_MNTH,y=Counts,colour='Counts'))+
    geom_boxplot(colour='red',fill=NA,position='dodge')+
    geom_boxplot(aes(x=C_MNTH,y=Counts/total_r,colour='Cor Counts'),fill=NA,position='dodge')+
    scale_y_continuous("number of accidents",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Month",y="Counts")+
    theme_bw()+expand_limits( y = 0)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual("",values = c("Counts"='red',"Cor Counts"='green'))

png("Accidents_by_vehicle_and_severity.png",width=1200,height=1024)
ggplot(counts_veh_month_sev,aes(x=C_DATE,y=Counts))+
    geom_line(data=counts_veh_month,aes(x=C_DATE,y=total/1000,lty='Total/1000'),fill='black')+
    geom_bar(aes(x=C_DATE,y=Counts,fill=P_ISEV),stat='identity',alpha=1,position='dodge')+
    scale_x_date(date_breaks="1 year",date_minor_breaks = "1 month",date_labels = "%Y")+
    scale_y_continuous("number of accidents",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Date",y="Counts")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_linetype_manual("",values = c("Total/1000"=2),labels=c("Total registerd / 1000"))+
    scale_fill_manual("Severity",breaks = levels(counts_veh_month_sev$P_ISEV),values=c('green','blue','red','gray'))

    
    
    
png("Accidents_by_vehicle_and_severity_ru.png",width=1200,height=1024)
ggplot(counts_veh_month_sev,aes(x=C_DATE,y=Counts))+
    geom_line(data=counts_veh_month,aes(x=C_DATE,y=total/1000,lty='Total/1000'),fill='black')+
    geom_bar(aes(x=C_DATE,y=Counts,fill=P_ISEV),stat='identity',alpha=1,position='dodge')+
    scale_x_date(date_breaks="1 year",date_minor_breaks = "1 month",date_labels = "%Y")+
    scale_y_continuous("Количество проишествий",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Дата",y="Количество проишествий")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_linetype_manual("",values = c("Total/1000"=2),labels=c("Всего на дорогах/1000"))+
    scale_fill_manual("Последствия",breaks = levels(counts_veh_month_sev$P_ISEV),values=c('green','blue','red','gray'))
    
counts_veh_year_sev$RU_ISEV<-factor(counts_veh_year_sev$P_ISEV,
  levels=c('No Injury', 'Injury', 'Fatality', 'Unknown'),
  labels=c('без травм','с травмами','смертельный исход','неизвестно'))
  
png("Accidents_by_vehicle_and_severity_by_year_ru.png",width=800,height=800)
ggplot(counts_veh_year_sev,aes(x=YEAR_,y=Counts))+
    geom_line(data=counts_veh_year_sev,aes(x=YEAR_,y=total/1000,lty='Total/1000'),col='red',size=2,alpha=0.5)+
    geom_bar(aes(x=YEAR_,y=Counts,fill=RU_ISEV),stat='identity',position='dodge',alpha=0.7)+
    scale_y_continuous("Количество проишествий",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Год",y="Количество проишествий")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_linetype_manual("",values = c("Total/1000"=1),labels=c("Всего на дорогах/1000"))+
    scale_fill_manual("Последствия",breaks = levels(counts_veh_year_sev$RU_ISEV),values=c('green','blue','red','gray'))
    
    
    
ggplot(counts_veh_month,aes(x=C_DATE,y=Counts*1000/total))+
    geom_bar(stat='identity',alpha=0.5)+
    scale_x_date(date_breaks="1 year",date_minor_breaks = "1 month",date_labels = "%Y")+
    scale_y_continuous("number of accidents per 1000 of registered vehicles",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Date",y="Counts")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    

model_month=glm(Counts~1+C_DATE*V_TYPE+C_MNTH*V_TYPE,data=counts_veh_month,family=poisson(link=log))
# model responce
counts_veh_month$fit<-predict(model_month,se.fit=F,type = "response")

ggplot(counts_veh_month,aes(x=C_DATE,y=Counts))+
    geom_bar(stat='identity',alpha=0.5)+
    geom_line(aes(y=fit), alpha=0.6,size=0.5,linetype=2)+
    scale_x_date(date_breaks="1 year",date_minor_breaks = "1 month",date_labels = "%Y")+
    scale_y_continuous("number of accidents",labels = scales::comma)+
    facet_grid(V_TYPE~.,scales='free')+
    labs(x="Date",y="Counts")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
# now let's see by month
ggplot(data,aes(x=C_MNTH,colour=V_TYPE,fill=V_TYPE))+geom_bar()+ggtitle('By Month')

# now let's see the outcome
ggplot(data,aes(x=P_ISEV,colour=V_TYPE,fill=V_TYPE))+geom_bar()+ggtitle('By Outcome')

# in percent
ggplot(data,aes(x=P_ISEV,colour=V_TYPE,fill=V_TYPE))+geom_bar(position = "fill")+ggtitle('By Outcome in %%')


#moto_model_month_1=glm(Counts~1+C_DATE+C_MNTH,data=counts_m,family=poisson(link=log))
#moto_model_month_2=glm(Counts~1+C_DATE+C_MNTH+total,data=counts_m,family=poisson(link=log))
#moto_model_month_3=glm(Counts~1+C_DATE+C_MNTH+total+total_cars,data=counts_m,family=poisson(link=log))
#moto_model_month_4=glm(Counts~1+C_DATE+C_MNTH+total*total_cars,data=counts_m,family=poisson(link=log))
#moto_model_month_5=glm(Counts~1+C_MNTH+total*total_cars,data=counts_m,family=poisson(link=log))
#moto_model_month_4.s=stepAIC(moto_model_month_4)


# plot distribution by month
ggplot(counts_moto_month,aes(x=C_MNTH,y=Counts))+geom_boxplot()
moto_model.nb=glm.nb(Counts~1+R_YEAR+C_MNTH,data=counts_moto_month)
#moto_model.s=stepAIC(moto_model)
#moto_model.nbs=stepAIC(moto_model.nb)


counts_moto_month$rs<-resid(moto_model.nb,type="deviance")
counts_moto_month$fit<-predict(moto_model.nb)

ggplot(counts_moto_month,aes(x=fit,y=rs))+geom_point()+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")

P=predict(moto_model.nb,type="link",se.fit=TRUE)
#PP=predict(moto_model_month_4,type="response",se.fit=F)
crit_val=1.96
#counts_m$PP=PP
counts_moto_month$fit=moto_model.nb$family$linkinv(P$fit)
counts_moto_month$fit_lo=moto_model.nb$family$linkinv(P$fit-crit_val*P$se.fit)
counts_moto_month$fit_hi=moto_model.nb$family$linkinv(P$fit+crit_val*P$se.fit)

ggplot(counts_moto_month,aes(x=C_DATE,y=Counts))+
    geom_bar(stat='identity',alpha=0.5)+
    geom_line(aes(y=fit),linetype=3,alpha=1.0,col='red')+
    geom_ribbon(aes(ymin=fit_lo,ymax=fit_hi),alpha=0.2,col='red')+
    ggtitle('Accidents By Month')+
    scale_x_date(date_breaks="1 year",date_minor_breaks = "1 month",date_labels = "%Y")+
    labs(x="Date",y="Counts") +
    guides(linetype=guide_legend())
    

moto_model_wday.nb=glm.nb(Counts~1+R_YEAR+C_MNTH*C_WDAY,data=counts_moto_wday)
#moto_model.s=stepAIC(moto_model)
moto_model_wday.nbs=stepAIC(moto_model_wday.nb)
summary(moto_model_wday.nbs)

    
# remove seasonal effects:
counts_mw$Counts_RES<-counts_mw$Counts/predict(moto_model_month_4)

counts_mwa<-ddply(counts_mw, .(C_WDAY), summarize, mean_res_count=mean(Counts_RES),mean_count=mean(Counts))


ggplot(counts_mwa,aes(x=C_WDAY,y=mean_res_count))+
    geom_bar(aes(y=mean_res_count,fill='res_count'),stat='identity',alpha=0.5)+
    geom_bar(aes(y=mean_count,fill='count'),stat='identity',alpha=0.5)+
    scale_y_continuous("mean number of accidents per weekday",labels = scales::comma)+
    scale_fill_manual(values = c("red","blue"))


# first by equipment
ggplot(moto,aes(x=P_ISEV,colour=P_SAFE,fill=P_SAFE))+geom_bar(position = "fill")+ggtitle('Moto By equipment')
ggplot(moto,aes(x=P_ISEV,colour=C_CONF,fill=C_CONF))+geom_bar()+ggtitle('Moto By Config')
ggplot(moto,aes(x=P_ISEV,colour=C_RCFG,fill=C_RCFG))+geom_bar()+ggtitle('Moto By Road Config')


ggplot(car,aes(x=P_ISEV,colour=P_SAFE,fill=P_SAFE))+geom_bar(position = "fill")+ggtitle('Car By equipment')
ggplot(car,aes(x=P_ISEV,colour=P_SAFE,fill=P_SAFE))+geom_bar()+ggtitle('Car By equipment')

summary(moto$P_SAFE)

moto$INJURY<-moto$P_ISEV!="No Injury"
moto$FATAL<-moto$P_ISEV=="Fatality"


ggplot(moto,aes(x=P_SEX,colour=P_SAFE,fill=P_SAFE))+
    geom_bar(position = "fill")+ggtitle('By equipment')+
    facet_grid(P_ISEV~.)

ggplot(moto,aes(1,colour=P_SAFE,fill=P_SAFE))+
    geom_bar()+ggtitle('By equipment')+
    facet_grid(P_ISEV~YEAR)
      

# let's see how often men and women use equipment
ggplot(moto,aes(x=P_SAFE,colour=P_SEX,fill=P_SEX))+
    geom_bar(position = "fill")+ggtitle('By equipment')




