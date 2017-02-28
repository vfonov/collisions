load('ncdb_lab.Rdata')
veh<-read.csv('cansim/cansim-vehicles.csv')
lic<-read.csv('tc/licensed.csv')

veh<-select(veh,YEAR,Car,Moto)
lic<-select(lic,YEAR,LICENSED)

lic$LICENSED=lic$LICENSED*1000 # convert to real counts (table contains 1000s)
veh<-merge(veh,lic,by=c('YEAR'))

# convert to date format
veh$C_DATE=as.Date(paste(veh$YEAR,12,31,sep='-'),format='%Y-%m-%d') # make a fake date


# prepare subset of data
# let's analyze only the main driver of either car or motorcycle
# where the outcome was reported

data <- filter(ncdb,
  P_PSN=="Driver"|P_PSN=="Front row, center"|P_PSN=="Occupant",
    P_USER!="Motor Vehicle Passenger",
    P_ISEV!="NA" ,
    P_ISEV!="Uknown" ,
    (V_TYPE=="Car" | V_TYPE=="Moto") ,
    C_MNTH!='UU Unknown',
    P_SAFE!='NA' ,
    P_SAFE!='Unknown' ,
    P_SAFE!='Dummy' ,
    P_SAFE!='Other' ,
    P_SAFE!='Bus' ,
    P_SEX!='N' ,
    P_SEX!='U' ,
    C_WDAY!='U' ,
    !is.na(C_DATE) )

# remove unneeded levels
data$V_TYPE<-droplevels(data$V_TYPE)
data$C_MNTH<-droplevels(data$C_MNTH)
data$P_ISEV<-droplevels(data$P_ISEV)
data$P_SAFE<-droplevels(data$P_SAFE)
data$P_SEX <-droplevels(data$P_SEX)
data$C_WDAY<-droplevels(data$C_WDAY)

# make years a factor
data$YEAR<-as.factor(data$C_YEAR)
data$INJURY<-data$P_ISEV!="No Injury"
data$FATAL<-data$P_ISEV=="Fatality"

moto <- filter(data,
            V_TYPE=="Moto"
)

moto$P_SAFE<-droplevels(moto$P_SAFE)
moto$P_SEX<-droplevels(moto$P_SEX)
moto$YEAR<-as.factor(moto$C_YEAR)
moto$C_WTHR<-droplevels(moto$C_WTHR)

car <- filter(data,V_TYPE=="Car" )

car$P_SAFE<-droplevels(car$P_SAFE)
car$P_SEX<-droplevels(car$P_SEX)
car$YEAR<-as.factor(car$C_YEAR)
car$C_WTHR<-droplevels(car$C_WTHR)

# let's see distribution by year
counts_veh_year<-summarise(group_by(data,YEAR,V_TYPE), Counts=n())
counts_veh_year$total<-NA
counts_veh_year$lic<-NA
counts_veh_year$YEAR_<-as.numeric(as.character(counts_veh_year$YEAR))
counts_veh_year$total[counts_veh_year$V_TYPE=='Moto']=approx(veh$YEAR,veh$Moto,xout=counts_veh_year$YEAR_[counts_veh_year$V_TYPE=='Moto'],      rule=2)$y
counts_veh_year$total[counts_veh_year$V_TYPE=='Car'] =approx(veh$YEAR,veh$Car ,xout=counts_veh_year$YEAR_[counts_veh_year$V_TYPE=='Car'],       rule=2)$y
counts_veh_year$lic[counts_veh_year$V_TYPE=='Car']   =approx(veh$YEAR,veh$LICENSED ,xout=counts_veh_year$YEAR_[counts_veh_year$V_TYPE=='Car'],  rule=2)$y
counts_veh_year$lic[counts_veh_year$V_TYPE=='Moto']  =approx(veh$YEAR,veh$LICENSED ,xout=counts_veh_year$YEAR_[counts_veh_year$V_TYPE=='Moto'], rule=2)$y


#  distribution by month, with number of vehicles in circulation
counts_veh_month<-summarise(group_by(data, C_DATE,V_TYPE),Counts=n(), Injury=sum(P_ISEV=='Injury'), Fatality=sum(P_ISEV=='Injury'))
counts_veh_month$total<-NA
counts_veh_month$lic<-NA
counts_veh_month$total_r<-NA

# interpolating number of registered vehicles 
counts_veh_month$total[counts_veh_month$V_TYPE=='Moto']=approx(veh$C_DATE,veh$Moto,xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Moto'],      rule=2)$y
counts_veh_month$total[counts_veh_month$V_TYPE=='Car'] =approx(veh$C_DATE,veh$Car ,xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Car'],       rule=2)$y

# interpolating number of licensed users
counts_veh_month$lic[counts_veh_month$V_TYPE=='Car']   =approx(veh$C_DATE,veh$LICENSED ,xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Car'],  rule=2)$y
counts_veh_month$lic[counts_veh_month$V_TYPE=='Moto']  =approx(veh$C_DATE,veh$LICENSED ,xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Moto'], rule=2)$y

counts_veh_month$C_MNTH<-as.factor(format(counts_veh_month$C_DATE,'%m'))

counts_veh_month$C_MNTH<-factor(format(counts_veh_month$C_DATE,'%m'),
                      levels=c('01','02','03','04','05','06','07','08','09','10','11','12'),
                      labels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov','Dec'))

counts_veh_month$total_r[counts_veh_month$V_TYPE=='Moto']=approx(veh$C_DATE,veh$Moto/mean(veh$Moto),xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Moto'],rule=2)$y
counts_veh_month$total_r[counts_veh_month$V_TYPE=='Car'] =approx(veh$C_DATE,veh$Car/mean(veh$Car) ,xout=counts_veh_month$C_DATE[counts_veh_month$V_TYPE=='Car'], rule=2)$y




counts_veh_month_sev<-summarise(group_by(data, C_DATE,V_TYPE,P_ISEV),Counts=n())
counts_veh_month_sev$total<-NA
counts_veh_month_sev$lic<-NA
counts_veh_month_sev$total_r<-NA

# interpolating number of registered vehicles 
counts_veh_month_sev$total[counts_veh_month_sev$V_TYPE=='Moto']=approx(veh$C_DATE,veh$Moto,xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Moto'],      rule=2)$y
counts_veh_month_sev$total[counts_veh_month_sev$V_TYPE=='Car'] =approx(veh$C_DATE,veh$Car ,xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Car'],       rule=2)$y

# interpolating number of licensed users
counts_veh_month_sev$lic[counts_veh_month_sev$V_TYPE=='Car']   =approx(veh$C_DATE,veh$LICENSED ,xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Car'],  rule=2)$y
counts_veh_month_sev$lic[counts_veh_month_sev$V_TYPE=='Moto']  =approx(veh$C_DATE,veh$LICENSED ,xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Moto'], rule=2)$y

counts_veh_month_sev$C_MNTH<-as.factor(format(counts_veh_month_sev$C_DATE,'%m'))

counts_veh_month_sev$C_MNTH<-factor(format(counts_veh_month_sev$C_DATE,'%m'),
                      levels=c('01','02','03','04','05','06','07','08','09','10','11','12'),
                      labels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov','Dec'))

counts_veh_month_sev$total_r[counts_veh_month_sev$V_TYPE=='Moto']=approx(veh$C_DATE,veh$Moto/mean(veh$Moto),xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Moto'],rule=2)$y
counts_veh_month_sev$total_r[counts_veh_month_sev$V_TYPE=='Car'] =approx(veh$C_DATE,veh$Car/mean(veh$Car) ,xout=counts_veh_month_sev$C_DATE[counts_veh_month_sev$V_TYPE=='Car'], rule=2)$y




counts_veh_year_sev<-summarise(group_by(data, YEAR,V_TYPE,P_ISEV),Counts=n())
counts_veh_year_sev$total<-NA
counts_veh_year_sev$lic<-NA
counts_veh_year_sev$total_r<-NA
counts_veh_year_sev$P_ISEV<-droplevels(counts_veh_year_sev$P_ISEV)

counts_veh_year_sev$YEAR_<-as.numeric(as.character(counts_veh_year_sev$YEAR))
counts_veh_year_sev$total[counts_veh_year_sev$V_TYPE=='Moto']=approx(veh$YEAR,veh$Moto,xout=counts_veh_year_sev$YEAR_[counts_veh_year_sev$V_TYPE=='Moto'],      rule=2)$y
counts_veh_year_sev$total[counts_veh_year_sev$V_TYPE=='Car'] =approx(veh$YEAR,veh$Car ,xout=counts_veh_year_sev$YEAR_[counts_veh_year_sev$V_TYPE=='Car'],       rule=2)$y
counts_veh_year_sev$lic[counts_veh_year_sev$V_TYPE=='Car']   =approx(veh$YEAR,veh$LICENSED ,xout=counts_veh_year_sev$YEAR_[counts_veh_year_sev$V_TYPE=='Car'],  rule=2)$y
counts_veh_year_sev$lic[counts_veh_year_sev$V_TYPE=='Moto']  =approx(veh$YEAR,veh$LICENSED ,xout=counts_veh_year_sev$YEAR_[counts_veh_year_sev$V_TYPE=='Moto'], rule=2)$y



# let's focus on motorcycles with known safety eq
#  distribution by month, with number of vehicles in circulation
counts_moto_month<-summarise(group_by(moto, C_DATE),Counts=n())

# interpolating number of registered vehicles 
counts_moto_month$r_total=approx(veh$C_DATE,100*veh$Moto/mean(veh$Moto)-100,xout=counts_moto_month$C_DATE,rule=2)$y
counts_moto_month$r_total_cars=approx(veh$C_DATE,100*veh$Car/mean(veh$Car)-100,xout=counts_moto_month$C_DATE,rule=2)$y
counts_moto_month$C_MNTH<-factor(format(counts_moto_month$C_DATE,'%m'),
                        levels=c('01','02','03','04','05','06','07','08','09','10','11','12'),
                        labels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov','Dec'))

counts_moto_month$R_YEAR=(counts_moto_month$C_DATE-as.Date("2007-01-01",format='%Y-%m-%d'))/365


# counts by the weekday
counts_moto_wday<-summarise(group_by(moto, C_DATE,C_WDAY),Counts=n())
counts_moto_wday$total<-NA

# interpolating number of registered vehicles 
counts_moto_wday$total=approx(veh$C_DATE,veh$Moto/1000,xout=counts_moto_wday$C_DATE,rule=2)$y
counts_moto_wday$total_cars=approx(veh$C_DATE,veh$Car/1000,xout=counts_moto_wday$C_DATE,rule=2)$y
counts_moto_wday$R_YEAR=(counts_moto_wday$C_DATE-as.Date("2007-01-01",format='%Y-%m-%d'))/365
counts_moto_wday$C_MNTH<-factor(format(counts_moto_wday$C_DATE,'%m'),
                                 levels=c('01','02','03','04','05','06','07','08','09','10','11','12'),
                                 labels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov','Dec'))
