##########1. Read data ####################
setwd("/Users/Louis/Downloads/data")
for (pack in c('tidyr','lpSolve', 'lpSolveAPI', 'dplyr', 'shiny','geosphere','readxl','stringi')){if(!require(pack, character.only = TRUE)){
  install.packages(pack)}
  require(pack, character.only = TRUE, quietly = TRUE)
}

load(file = "cngStations_BE.Rdata")
load(file = "lngStations_BE.Rdata")
load(file = "Roads.Rdata")
load(file = "petrolStations_BE.Rdata")
location_terminals<-read_excel("Location_terminals.xlsx")

location_terminals <- location_terminals %>% 
  rename(
    terminal_lon = lon,
    terminal_lat = lat
  )

##########2. Data preparation ###############


any(is.na(cngStations))
any(is.na(BELGIUM))
any(is.na(lngStations))
any(is.na(petrolStations))
any(is.na(location_terminals))


BELGIUM$car_daily_avg<-BELGIUM$total_daily_avg-BELGIUM$truck_daily_avg
cngStations$country<-cngStations$status_gasstation<-NULL
lngStations$country<-lngStations$status_gasstation<-NULL

AllStations<-rbind(petrolStations,cngStations,lngStations)

AllStations$region<-ifelse(AllStations$lat>=51.0109,"Flanders","Wallonia")
AllStations<-unique(AllStations)

################## Compute distance between terminals and stations

Stations_coordinates <- AllStations[,c('lat','lon')]
Terminals_coordinates <- location_terminals[,c('terminal_lat','terminal_lon')]

distancesToTerminal <- data.frame(matrix(nrow = nrow(Stations_coordinates), ncol = nrow(Terminals_coordinates)))

#Distance from stations to all 6 terminals
for (i in 1:nrow(Stations_coordinates)){
  print(i)
  for (j in 1:nrow(Terminals_coordinates)){
    distancesToTerminal[i, j] <- (distHaversine(Stations_coordinates[i,],Terminals_coordinates[j,])/1000) #divide by thousand to get distance in km
    names(distancesToTerminal)[j] <- paste0('distance_To_Terminal_',j)
  }
}


AllStations<-cbind(AllStations,distancesToTerminal)
################## Compute distance between counters and stations

Counter_coordinates <- BELGIUM[,c('lat','lon')]
Counter_names <- data.frame(BELGIUM[,'counter_name'])

distancesToCounter <- data.frame(matrix(nrow = nrow(Stations_coordinates), ncol = nrow(Counter_coordinates)))


#Distance from stations to all counters
for (i in 1:nrow(Stations_coordinates)){
  print(i)
  for (j in 1:nrow(Counter_coordinates)){
    distancesToCounter[i, j] <- (distHaversine(Stations_coordinates[i,],Counter_coordinates[j,])/1000) #divide by thousand to get distance in km
    names(distancesToCounter)[j] <- as.character(Counter_names[j,])
  }
}


#Density of all counters

Density <- t(BELGIUM)
colnames(Density) <- Density['counter_name',]
Density <- Density[c('truck_daily_avg','car_daily_avg','total_daily_avg'),]

Traffic_parameter = 'total_daily_avg'

Density['truck_daily_avg',] = as.numeric(Density['truck_daily_avg',]) * 0.05 #Percentage that tanks
Density['truck_daily_avg',] = as.numeric(Density['truck_daily_avg',]) * 0.02 #Percentage that drives on LNG
Density['truck_daily_avg',] = as.numeric(Density['truck_daily_avg',]) * 1080 #Volume of tank (in Liters)

Density['car_daily_avg',] = as.numeric(Density['car_daily_avg',]) * 0.05 #Percentage that tanks
Density['car_daily_avg',] = as.numeric(Density['car_daily_avg',]) * 0.0008 #Percentage that drives on LNG
Density['car_daily_avg',] = as.numeric(Density['car_daily_avg',]) * 70 #Volume of tank (in Liters)

Density['total_daily_avg',] = as.numeric(Density['car_daily_avg',]) + as.numeric(Density['truck_daily_avg',])

#Calculate Density of all Stations
Station_density <- data.frame(matrix(nrow = nrow(distancesToCounter), ncol = ncol(Density)))
#Station_density = Station_density[,1:429]

Max_distance_counter = 100

for (i in 1:ncol(Density)){
  print(i)
  for (j in 1:nrow(distancesToCounter)){
    #if(distancesToCounter[j,i] < Max_distance_counter){
    Station_density[j,i] = as.numeric(Density[Traffic_parameter,i]) / distancesToCounter[j,i] 
    #}
    #else Station_density[j,i] = 0
  }
}

Station_density$sum = rowSums(Station_density)
Station_density2 = data.frame(Station_density[,'sum'])

AllStations<-cbind(AllStations,Station_density2)

#Cleanup, renaming
AllStations$name_gasstation <- as.character(AllStations$name_gasstation)
AllStations[grepl('dats', tolower(AllStations$name_gasstation)),]$name_gasstation <- "Dats24-BE"
AllStations[grepl('devagro', tolower(AllStations$name_gasstation)),]$name_gasstation <- "Devagro"
AllStations[grepl('enora', tolower(AllStations$name_gasstation)),]$name_gasstation <- "Enora"
AllStations[grepl('gps', tolower(AllStations$name_gasstation)),]$name_gasstation <- "GPS"
AllStations[grepl('maes', tolower(AllStations$name_gasstation)),]$name_gasstation <- "Maes-BE"
AllStations[grepl('q8', tolower(AllStations$name_gasstation)),]$name_gasstation <- "Q8-BE"

#change lay out names
AllStations$name_gasstation1<-substr(AllStations$name_gasstation,0
                                     , stri_locate_first_regex(pattern=' ',AllStations$name_gasstation
                                     )-1)

AllStations$name_gasstation2<-substr(AllStations$name_gasstation,0
                                     , stri_locate_first_regex(pattern='-',AllStations$name_gasstation
                                     )-1)
AllStations$name_gasstation3<-substr(AllStations$name_gasstation,0
                                     , stri_locate_first_regex(pattern='_',AllStations$name_gasstation
                                     )-1)
AllStations$name_gasstation4<-coalesce(AllStations$name_gasstation1,AllStations$name_gasstation2,AllStations$name_gasstation3)
AllStations$name_gasstation4<-coalesce(AllStations$name_gasstation4,AllStations$name_gasstation)

AllStations$name_gasstation<-AllStations$name_gasstation4
AllStations$name_gasstation4<-AllStations$name_gasstation3<-AllStations$name_gasstation2<-AllStations$name_gasstation1<-NULL



AllStationsDIRKOIL<-AllStations[AllStations$name_gasstation=="DIRKOIL",]
AllStationsnotDIRKOIL<-AllStations[AllStations$name_gasstation!="DIRKOIL",]

#####LP####

# VARIABLES
Possible_stations = AllStationsDIRKOIL
numberOfStationsSelected = 10
signNumberOfStationsSelected = '='
coeffObjectiveFunction = Possible_stations$Station_density....sum..
Dist_between_stations = 15
Capacity_constraint = 10^10

# Create empty LP Model
lprog <- make.lp(0,nrow(Possible_stations)) 

# Set objective function coefficients
set.objfn(lprog, coeffObjectiveFunction)

# Specify max vs min problem (default = min)
lp.control(lprog,sense='max')

# Add constraints
## CONSTRAINT SELECT MAX NUMBER OF STATIONS
allCoefSTEP1 <- rep(c(1), nrow(Possible_stations))

add.constraint(lprog, allCoefSTEP1, signNumberOfStationsSelected, numberOfStationsSelected)

# Add constraint
## CONSTRAINT DISTANCE BETWEEN STATIONS IS MINIMUM 15 KM (minimize proximity 1/25=0.04)
Stations_coordinates = AllStationsDIRKOIL[,c('lat','lon')]

distanceMatrix <- data.frame(matrix(nrow = nrow(Stations_coordinates), ncol = nrow(Stations_coordinates)))

for (i in 1:nrow(Stations_coordinates)){
  print(i)
  for (j in 1:nrow(Stations_coordinates)){
    distanceMatrix[i, j] <- (distHaversine(Stations_coordinates[i,],Stations_coordinates[j,])/1000) #divide by thousand to get distance in km
  }
}

colnames(distanceMatrix) = 1:168
distanceMatrix = cbind(data.frame(1:168),distanceMatrix)
colnames(distanceMatrix)[1] = 'station_1'

distances = distanceMatrix %>%
  gather(station_2,distance,c(2:169))

Close_Stations = distances[which(distances$distance < Dist_between_stations),]
Close_Stations = Close_Stations[which(Close_Stations$distance != 0),]

Close_Stations$station_2 = as.numeric(Close_Stations$station_2)

Close_Stations = Close_Stations[!duplicated(Close_Stations$distance),]


for (j in 1:nrow(Close_Stations)){
  Coef2 <- rep(c(0), 168)
  Coef2[Close_Stations[j,1]] = 1
  Coef2[Close_Stations[j,2]] = 1
  add.constraint(lprog, Coef2, '<=', 1)
}



# set type
for (i in 1:nrow(Possible_stations)){
  set.type(lprog, i, "binary")
}


#show model
print(lprog)

# make model
solve(lprog)# Specify max vs min problem (default = min)

# Check final solution
# Objective value
(final_objectiveSTEP1 <- get.objective(lprog))

# variable values
(final_paramsSTEP1 <- get.variables(lprog))

# constraint values
(final_constraintsSTEP1 <- get.constraints(lprog))
# If the final constraints are equal to the LP constraints, we call these 'hard' constraints

selectedStations <- Possible_stations[final_paramsSTEP1 == 1, ]

##########4. Allocation #######

#Variables

# Make empty model
lprog2 <- make.lp(0,nrow(selectedStations)*nrow(location_terminals)) 

# Set objective function coefficients
coeffCost <- as.vector(t(selectedStations[,6:11]))
set.objfn(lprog2, coeffCost)

# Specify max vs min problem (default = min)
lp.control(lprog2, sense='min')

# Add constraints
## CONSTRAINT SELECT FOR EACH STATION 1 terminal (10 constraints van 6 DV's)
allCoefSTEP2 <- rep(c(0), nrow(location_terminals)*nrow(selectedStations))


for (j in 1:nrow(selectedStations)){
  for (i in 1:(nrow(selectedStations)*nrow(location_terminals))){
    if (i<=j*nrow(location_terminals) && i>(j-1)*nrow(location_terminals)){
      allCoefSTEP2[i] = 1 
    } else allCoefSTEP2[i] = 0
  }
  add.constraint(lprog2, allCoefSTEP2, '=', 1)
}

# Add constraints
## CONSTRAINT Capacity of each terminal (6 constraints van 10 DV's)

Coef2 <- rep(c(0), nrow(location_terminals)*nrow(selectedStations))


for (j in 0:5){
  for (i in 1:(nrow(selectedStations)*nrow(location_terminals))){
    if(i %% 6 == j){
      Coef2[i] = selectedStations[i,'Station_density....sum..'] 
    } else allCoefSTEP2[i] = 0
  }
  add.constraint(lprog2, Coef2, '<=', Capacity_constraint)
}

# set type
for (i in 1:(nrow(selectedStations)*nrow(location_terminals))){
  set.type(lprog2, i, "binary")
}

#show model
print(lprog2)

# make model
solve(lprog2)

# Check final solution
# Objective value
(final_objectiveSTEP2 <- get.objective(lprog2))

# variable values
(final_paramsSTEP2 <- get.variables(lprog2))

selectedTerminalPerStation <- matrix(nrow = nrow(selectedStations), ncol = nrow(location_terminals))

for(i in 1:nrow(selectedStations)){
  selectedTerminalPerStation[i,] <- final_paramsSTEP2[((i-1)*nrow(location_terminals)+1):(i*nrow(location_terminals))]
}

selectedTerminals <- c()
notSelectedTerminals <- c()
for (i in 1:ncol(selectedTerminalPerStation)){
  if (max(selectedTerminalPerStation[,i]) == 1){
    selectedTerminals <- c(selectedTerminals, i)
  } else {notSelectedTerminals <- c(notSelectedTerminals, i)}
}


selectedTerminals <- c()
for (i in 1:nrow(selectedStations)){
  Terminal <- location_terminals[selectedTerminalPerStation[i,] == 1,-1]
  selectedTerminals = rbind(selectedTerminals,Terminal)
}


selectedStations_Terminals = cbind(selectedStations, selectedTerminals)

# constraint values
(final_constraintsSTEP2 <- get.constraints(lprog2))
# If the final constraints are equal to the LP constraints, we call these 'hard' constraints