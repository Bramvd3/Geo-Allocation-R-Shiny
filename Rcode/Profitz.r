####Load packages and environment####
#setwd("/Users/William/Desktop/groepsWerk/Final")
for (pack in c('tidyr','lpSolve', 'lpSolveAPI', 'dplyr', 'shiny','geosphere','readxl','stringi')){if(!require(pack, character.only = TRUE)){
  install.packages(pack)}
  require(pack, character.only = TRUE, quietly = TRUE)
}
load('environment.RData')

##########5. Function for shiny #####
solveProblem<-function(updateProgress = NULL,Traffic_choice, range, Price_Per_km, Capacity_constraint, inputstations, inputnumberstations, costparameter, fueltype,Location, Dist_between_stations) {
  
  #updateProgress = NULL
  #inputstations = 'No partner'
  #Capacity_constraint = 400000
  #inputnumberstations=12
  #Traffic_choice= 'Car'
  #range=200
  #fueltype='All fuel types'
  #Location="Belgium"
  #Dist_between_stations=5
  #Price_Per_km = 2
  
  Size_Tanker = 80000 #in liters
  
  Traffic_parameter = Traffic_choice
  
  #select partner
  if(!("No partner" %in% (inputstations))){
    AllStationsnotDIRKOIL_input<-AllStationsnotDIRKOIL[AllStationsnotDIRKOIL$name_gasstation %in% inputstations,]
    Possible_stations<-rbind(AllStationsnotDIRKOIL_input,AllStationsDIRKOIL)
  }else {Possible_stations<-AllStationsDIRKOIL
  }
  
 
  
  #keep track of fuel type
  if(!("All fuel types" %in% (fueltype))){
    Possible_stations<-Possible_stations[Possible_stations$fueltype==fueltype,]
  }
  
  #keep track of all regions
  if(!("Belgium" %in% (Location))){
    Possible_stations<-Possible_stations[Possible_stations$region==Location,]
  }
  
  
  #set min and max for number of stations
  if(inputnumberstations>20){
    inputnumberstations<-20
    print("we overruled the system and used the max number, 20, as numberofstations")
  }
  if(inputnumberstations<1){
    inputnumberstations<-1
    print("we overruled the system and used the min number, 1, as numberofstations")
  }
  
  Possible_stations = cbind(Possible_stations,data.frame(1:nrow(Possible_stations)))
  colnames(Possible_stations)[15] = 'Station'
  colnames(Possible_stations)[6:11] = c(1:6)
  
  Possible_options = Possible_stations %>%
    gather(terminal,kilometers,c(6:11))
  
  Possible_options$Supply_frequency = Possible_options$Total/Size_Tanker
  
  Possible_options$Cost =Possible_options$kilometers * Price_Per_km * Possible_options$Supply_frequency
  
  Possible_options$Total = Possible_options$Total - Possible_options$Cost
  Possible_options$Truck = Possible_options$Truck - Possible_options$Cost
  Possible_options$Car = Possible_options$Car - Possible_options$Cost
  
  # VARIABLES
  
  numberOfStationsSelected = inputnumberstations
  signNumberOfStationsSelected = '<='
  coeffObjectiveFunction = Possible_options[,Traffic_parameter]
  
  # Create empty LP Model
  lprog <- make.lp(0,nrow(Possible_options))
  
  # Set objective function coefficients
  set.objfn(lprog, coeffObjectiveFunction)
  
  # Specify max vs min problem (default = min)
  lp.control(lprog,sense='max')
  
  # Add constraints
  ## CONSTRAINT SELECT MAX NUMBER OF STATIONS
  allCoefSTEP1 <- rep(c(1), nrow(Possible_options))
  
  add.constraint(lprog, allCoefSTEP1, signNumberOfStationsSelected, numberOfStationsSelected)
  
  # Add constraints
  ## CONSTRAINT At least 1 station in both regions
  Coef_Wallonia <- data.frame(rep(c(0), nrow(Possible_options)))
  Coef_Flanders <- data.frame(rep(c(0), nrow(Possible_options)))
  
  if(Location == 'All regions'){
    Coef_Wallonia[Possible_options$region == 'Wallonia',] = 1
    Coef_Flanders[Possible_options$region == 'Flanders',] = 1
    Coef_Wallonia = as.matrix(Coef_Wallonia)
    Coef_Flanders = as.matrix(Coef_Flanders)
    add.constraint(lprog, Coef_Flanders, '>', 0)
    add.constraint(lprog, Coef_Wallonia, '>', 0)
  }
  
  
  
  
  # Add constraint
  ## CONSTRAINT DISTANCE BETWEEN STATIONS IS MINIMUM 15 KM 
  
  Stations_coordinates = Possible_stations[,c('lat','lon')]
  
  distanceMatrix <- data.frame(matrix(nrow = nrow(Stations_coordinates), ncol = nrow(Stations_coordinates)))
  if (is.function(updateProgress)) {
    text <- "Looping over possibilities"
    updateProgress(message = text)
  }
  
  for (i in 1:nrow(Stations_coordinates)){
    print(i)
    if (is.function(updateProgress)) {
      text <- (i/nrow(Stations_coordinates))*10
      updateProgress(value = text)
    }
    for (j in 1:nrow(Stations_coordinates)){
      distanceMatrix[i, j] <- (distHaversine(Stations_coordinates[i,],Stations_coordinates[j,])/1000) #divide by thousand to get distance in km
    }
  }
  
  colnames(distanceMatrix) = 1:nrow(Possible_stations)
  distanceMatrix = cbind(data.frame(1:nrow(Possible_stations)),distanceMatrix)
  colnames(distanceMatrix)[1] = 'station_1'
  
  
  distances = distanceMatrix %>%
    gather(station_2,distance,c(2:as.numeric(nrow(Possible_stations)+1)))
  
  Close_Stations = distances[which(distances$distance < Dist_between_stations),]
  Close_Stations = Close_Stations[which(Close_Stations$distance != 0),]
  
  Close_Stations$station_2 = as.numeric(Close_Stations$station_2)
  
  Close_Stations = Close_Stations[!duplicated(Close_Stations$distance),]
  
  for (j in 1:nrow(Close_Stations)){
    Coef2 <- rep(c(0), nrow(Possible_stations))
    Coef2[Close_Stations[j,1]] = 1
    Coef2[Close_Stations[j,2]] = 1
    Coef2 = rep(Coef2, 6)
    add.constraint(lprog, Coef2, '<=', 1)
  }
  
  # Add constraint
  ## CONSTRAINT Range
  Coef2 <- rep(c(0), nrow(Possible_options))
  Coef2[Possible_options$kilometers>range] = 1
  add.constraint(lprog, Coef2, '<', 1)
  
  # Add constraints
  ## CONSTRAINT Each station occurs only one time (amount of stations is nr of constraints) 
  
  Coef2 <- rep(c(0), nrow(Possible_options))
  
  for (j in 0:as.numeric(nrow(Possible_stations)-1)){
    
    for (i in 1:(nrow(Possible_options))){
      if(i %% as.numeric(nrow(Possible_stations)) == j){
        Coef2[i] = 1
        
      } else Coef2[i] = 0
    }
    add.constraint(lprog, Coef2, '<=', 1)
  }
  
  # Add constraints
  ## CONSTRAINT Capacity of each terminal (6 constraints)
  
  Coef2 <- rep(c(0), nrow(Possible_stations))
  
  for (j in 1:6){
    k = 1
    for (i in 1:(nrow(Possible_options))){
      if (i<=j*nrow(Possible_stations) && i>(j-1)*nrow(Possible_stations)){
        Coef2[i] = Possible_stations[k,Traffic_parameter]
        k = k+1
      } else Coef2[i] = 0
    }
    add.constraint(lprog, Coef2, '<=', Capacity_constraint)
  }
  

  
  # set type
  for (i in 1:nrow(Possible_options)){
    set.type(lprog, i, "binary")
  }
  
  
  #show model
  print(lprog)
  
  # make model
  if (is.function(updateProgress)) {
    text <- "Solving problem"
    updateProgress(message = text)
  }
  solve(lprog)# Specify max vs min problem (default = min)
  
  # Check final solution
  # Objective value
  (final_objectiveSTEP1 <- get.objective(lprog))
  
  # variable values
  (final_paramsSTEP1 <- get.variables(lprog))
  
  # constraint values
  (final_constraintsSTEP1 <- get.constraints(lprog))
  # If the final constraints are equal to the LP constraints, we call these 'hard' constraints
  
  selectedStations <- Possible_options[final_paramsSTEP1 == 1, ]
  
  
  colnames(location_terminals)[1]
  
  selectedStations_Terminals = merge(selectedStations, location_terminals, by.x = 'terminal', by.y = "...1")
  
  selectedStations_Terminals$id = as.character(seq.int(nrow(selectedStations_Terminals)))
  
  return(selectedStations_Terminals)
  
  
}

#example
#final <- solveProblem(Price_Per_km = 2, inputstations = 'No Partner', range = 200, Capacity_constraint = 100000, inputnumberstations=4,Traffic_choice= 'Truck' , fueltype='All fuel types', Location="Belgium",  Dist_between_stations=30)
#200000 is minimum feasible capacity with 10 stations.

Total_profit = sum(final$Total)
Car_profit  = sum(final$Car)
Truck_profit = sum(final$Truck)

