# read data table

#ncdb<-read.csv('NCDB_1999_to_2014.csv')
#save(ncdb,file='ncdb.Rdata')
load('ncdb.Rdata')

# rename some variables, based on 'Data_Dictionary_NCDB_EN.doc'
ncdb$MNTH<-factor(ncdb$C_MNTH,
                    levels=c(
                        "01",
                        "02",
                        "03",
                        "04",
                        "05",
                        "06",
                        "07",
                        "08",
                        "09",
                        "10",
                        "11",
                        "12",
                        "UU" ),
                    labels=c(
                        "January",
                        "February",
                        "March",
                        "April",
                        "May",
                        "June",
                        "July",
                        "August",
                        "September",
                        "October",
                        "November",
                        "December",
                        "Unknown") )

ncdb$WDAY<-factor(ncdb$C_WDAY,
    levels=c(
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "U"),
    labels=c(
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday",
            "Unknown"))

ncdb$C_SEV<-factor(ncdb$C_SEV,
    levels=c(
    "1",
    "2"
    ),
    labels=c(
    "Fatality",
    "Injury"
    ))
    
ncdb$C_TYPE<-factor(ncdb$C_CONF)
levels(ncdb$C_TYPE)<-list(
  "Single"=c("01",
          "02",
          "03",
          "04",
          "05",
          "06"),
  "Two Same"=c(
    "21",
    "22",
    "23",
    "24",
    "25"),
    "Two Opposite"=c(
      "31",
      "32",
      "33",
      "34",
      "35",
      "36"
    ),
    "Hit Parked"=c("41"),
    "Unknown"=c(
      "QQ",
      "UU",
      "XX")
  )



ncdb$C_CONF<-factor(ncdb$C_CONF,
    levels=c(
"01",
"02",
"03",
"04",
"05",
"06",
"21",
"22",
"23",
"24",
"25",
"31",
"32",
"33",
"34",
"35",
"36",
"41",
"QQ",
"UU",
"XX"
    ),
    labels=c(
    "Hit a moving object E.g. a person or an animal",
    "Hit a stationary object E.g. a tree",
    "Ran off left shoulder Including rollover in the left ditch",
    "Ran off right shoulder Including rollover in the right ditch",
    "Rollover on roadway",
    "Any other single vehicle collision configuration",
    "Rear-end collision",
    "Side swipe",
    "One vehicle passing to the left of the other, or left turn conflict",
    "One vehicle passing to the right of the other, or right turn conflict",
    "Any other two vehicle - same direction of travel configuration",
    "Head-on collision",
    "Approaching side-swipe",
    "Left turn across opposing traffic",
    "Right turn, including turning conflicts",
    "Right angle collision",
    "Any other two-vehicle - different direction of travel configuration",
    "Hit a parked motor vehicle",
    "Choice is other than the preceding values",
    "Unknown",
    "Unavalable"
    ))
    
    
ncdb$C_RCFG<-factor(ncdb$C_RCFG,
    levels=c(
"01",
"02",
"03",
"04",
"05",
"06",
"07",
"08",
"09",
"10",
"11",
"12",
"QQ",
"UU",
"XX"),
    labels=c(
"Non-intersection",
"At an intersection of at least two public roadways",
"Intersection with parking lot entrance/exit, private driveway or laneway",
"Railroad level crossing",
"Bridge, overpass, viaduct",
"Tunnel or underpass",
"Passing or climbing lane",
"Ramp",
"Traffic circle",
"Express lane of a freeway system",
"Collector lane of a freeway system",
"Transfer lane of a freeway system",
"Choice is other than the preceding values",
"Unknown",
"Unavalable"
    ))
    
ncdb$C_WTHR<-factor(ncdb$C_WTHR,
    levels=c(
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "Q",
        "U",
        "X"
    ),
    labels=c(
    "Sunny",
    "Overcast",
    "Raining",
    "Snowing",
    "Freezing rain",
    "Visibility limitation",
    "Windy",
    "Other",
    "Unknown",
    "Unavalable"
    ))

ncdb$C_RSUR<-factor(ncdb$C_RSUR,
    levels=c(
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"Q",
"U",
"X"
    ),
    labels=c(
"Normal",
"Wet",
"Fresh snow",
"Wet snow",
"Icy",
"Sand/gravel/dirt",
"Muddy",
"Oil",
"Flooded",
"Other",
"Unknown",
"Unavalable"
    ))

ncdb$C_RALN<-factor(ncdb$C_RALN,
    levels=c(
"1",
"2",
"3",
"4",
"5",
"6",
"Q",
"U",
"X"
    ),
    labels=c(
"Straight and level",
"Straight with gradient",
"Curved and level",
"Curved with gradient",
"Top of hill or gradient",
"Bottom of hill or gradient",
"Choice is other than the preceding values",
"Unknown",
"Unavalable"
    ))

ncdb$C_TRAF<-factor(ncdb$C_TRAF,
    levels=c(
"01",
"02",
"03",
"04",
"05",
"06",
"07",
"08",
"09",
"10",
"11",
"12",
"13",
"14",
"15",
"16",
"17",
"18",
"QQ",
"UU",
"XX"
    ),
    labels=c(
"Traffic signals fully operational",
"Traffic signals in flashing mode",
"Stop sign",
"Yield sign",
"Warning sign Yellow diamond shape sign",
"Pedestrian crosswalk",
"Police officer",
"School guard, flagman",
"School crossing",
"Reduced speed zone",
"No passing zone sign",
"Markings on the road e.g. no passing",
"School bus stopped with school bus signal lights flashing",
"School bus stopped with school bus signal lights not flashing ",
"Railway crossing with signals, or signals and gates",
"Railway crossing with signs only",
"Control device not specified",
"No control present",
"Choice is other than the preceding values",
"Unknown",
"Jurisdiction does not provide this data element"
))
    
ncdb$V_TYPE<-factor(ncdb$V_TYPE,
    levels=c(
"01",
"05",
"06",
"07",
"08",
"09",
"10",
"11",
"14",
"16",
"17",
"18",
"19",
"20",
"21",
"22",
"23",
"NN",
"QQ",
"UU",
"XX"
    ),
    labels=c(
"Car",
"Van",
"Van2",
"Truck",
"Tractor",
"Bus1",
"Bus2",
"Bus3",
"Moto",
"Off road",
"Bicycle",
"RV",
"Farm",
"Construction",
"Fire",
"Snowmobile",
"Tram",
"D",
"O",
"U",
"J"
    ))
    
ncdb$P_PSN<-factor(ncdb$P_PSN,
    levels=c(
    "11",
    "12",
    "13",
    "21",
    "22",
    "23",
    "31",
    "32",
    "33",
    "96",
    "97",
    "98",
    "99",
    "NN",
    "QQ",
    "UU",
    "XX"
    ),
    labels=c(
    "Driver",
    "Front row, center",
    "Front row, right outboard, including motorcycle passenger in sidecar",
    "Second row, left outboard, including motorcycle passenger",
    "Second row, center",
    "Second row, right outboard",
    "Third row, left outboard",
    "Third row, center",
    "Third row, right outboard",
    "Occupant",
    "Lap",
    "Outside",
    "Pedestrian",
    "D",
    "O",
    "U",
    "J"
    ))
    
ncdb$P_ISEV<-factor(ncdb$P_ISEV,
    levels=c(
    "1",
    "2",
    "3",
    "N",
    "U"
    ),
    labels=c(
    "No Injury",
    "Injury",
    "Fatality",
    "NA", # no person involved
    "Unknown"
    ))
    

    
ncdb$P_SAFE_F<-factor(ncdb$P_SAFE,
    levels=c(
"01",
"02",
"09",
"10",
"11",
"12",
"13",
"NN",
"QQ",
"UU",
"XX"
    ),
    labels=c(
"No safety device used or No child restraint used",
"Safety device used or child restraint used",
"Helmet worn For motorcyclists, bicyclists, snowmobilers, all-terrain vehicle riders",
"Reflective clothing worn For motorcyclists, bicyclists, snowmobilers, all-terrain vehicle riders and pedestrians",
"Both helmet and reflective clothing used For motorcyclists, bicyclists, snowmobilers, all-terrain vehicle riders and pedestrians",
"Other safety device used",
"No safety device equipped e.g. buses",
"Data element is not applicable e.g. dummy person record created for parked cars",
"Choice is other than the preceding values",
"Unknown e.g. applies to runaway cars",
"Jurisdiction does not provide this data element"
    ))    
      
    
ncdb$P_SAFE<-factor(ncdb$P_SAFE,
    levels=c(
"01",
"02",
"09",
"10",
"11",
"12",
"13",
"NN",
"QQ",
"UU",
"XX"
    ),
    labels=c(
"Nothing",
"Standard",
"Helmet worn",
"Reflective clothing worn",
"Both helmet and reflective clothing used",
"Other safety device used",
"Bus",
"Dummy",
"Other",
"Unknown",
"NA"
    ))
    
  
    
ncdb$P_USER<-factor(ncdb$P_USER,
    levels=c(
        "1",
        "2",
        "3",
        "4",
        "5",
        "U"
        ),
   labels=c(
        "Motor Vehicle Driver",
        "Motor Vehicle Passenger",
        "Pedestrian",
        "Bicyclist",
        "Motorcyclist",
        "Unknown"
    ))

    
    
# add dates, fake month day
ncdb$C_DATE<-as.Date(paste(ncdb$C_YEAR,ncdb$C_MNTH,1,sep='-'),'%Y-%m-%d')

save(ncdb,file='ncdb_lab.Rdata')

