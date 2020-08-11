# Script 3. Model Functions
#======================================
# model functions for dispersal simulations



# Libraries
#-------------------
library(rgeos)
library(tidyverse)
library(mgcv) # need this to predict GAM
#----


# Function Inputs
#-------------------------------------
release=1                       # Hour of release
nodes= 1                        # Nodes to call from, 1, 2, or 3 (to be averaged for current velocities)
year = 14                       # year, 14 or 95
release_size <- 180             # Start size for larvae
behavior = "onto"               # behavior for larvae. "none","photo","onto"
hour=release                    # 
name = paste0("_",year,"_",     # creates nane with variables from above
              release,"_",      # name format is _year_release_behavior_nodes
              behavior,"_",     #
              nodes,"n")        # 
print(name)
#---------------------------------



# load necessary objects
#-------------------------------------
load(here("data_processed",'restorationsites.Rdata'))
load(here("data_processed",'WA_currents_14.Rdata'))
load(here("data_processed","WA_nodes_coords.Rdata"))
load(here("data_processed",'zeta_14.Rdata'))
load(here("data_processed","siglay.Rdata"))
growthgam_final <- readRDS(here("data_raw","growthgam_finalRDS.rds"))
load(here("data_raw","growfunction.Rdata"))
load(here("data_processed","time.Rdata"))



# Function 1. Create Larvae
#------------------------------------
# this creates an initial array of larva position with 
# 19 rows (restoration sites)
# 3-5 columns (3 if using only one node for currents - 5 if using 3)
# 1201 sheets (1200 hours + 1 for end position)
create.larvae <- function(name=get('name',envir = globalenv()),nodes=get('nodes',envir = globalenv()),year=get('year',envir = globalenv()),release=get('release',envir = globalenv())){
  
  # create node points spatial reference
  nodepoints <- SpatialPoints(WA_nodes_coords[,2:3])

  # create empty array starting on release day 
  #      3 dimensions - larvae (19), columns (X,Y,Z, closest node, optional closest node 2&3, pages for model hours)
  temporary <- array(dim=c(nrow(restorationsites), # 19 sites
                      (3+nodes), # x, y, h, node 1, node 2, node 3
                      length(get(paste0("WA_currents_",year))[1,1,1,])+1)) # as long as full currrents (1201 hours)
  
  # creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
  
  temporary[,1,release] <- restorationsites$x    # X coords
  temporary[,2,release] <- restorationsites$y    # Y coords
  temporary[,3:(3+nodes-1),release] <- t(apply(gDistance(nodepoints,SpatialPoints(temporary[,1:2,release]),byid = TRUE),1,order)[1:nodes,]) # closest nodes 1-3
  temporary[,(4+nodes-1),release] <- -(WA_nodes_coords$h_smooth[temporary[,3,release]] #+ get(paste0("zeta_",year))[temporary[,3,release], release ] 
                                       ) *.75  # this releases larvae at .75 the depth at which they were released including tidal height. 
  #temporary[,,1]  # this creates first page of array with larva coordinates that I picked at sites. 
  
  assign(  paste0("larvatrack",name),  temporary, envir = .GlobalEnv) # create real array
  rm(temporary)
  
  # Create size array
  temporarysize <- array(dim=c(nrow(restorationsites),length(get(paste0("WA_currents_",year))[1,1,1,])+1))
  temporarysize[,release] <- 180
  assign(paste0("larvatrack",name,".sizes"), temporarysize, envir=.GlobalEnv)
  rm(temporarysize)
}
#--------



# Function 2. Locate Larvae
#-------------------------------------
# this function makes each larva find the closest nodes to it
loop_array <<- get(paste0("larvatrack",name))[,,hour:(hour+1)] 

locate.larvae <- function(loop_array = get('loop_array',envir=globalenv()), nodes=get('nodes',envir = globalenv()),year=get('year',envir = globalenv())){
  
  
  lastnodes <<- loop_array[,3,1] ## but the 1 will turn to "hour"
  lastdepthprop <<- loop_array[,(3+nodes),1] / -(WA_nodes_coords$h_true[lastnodes]+ get(paste0("zeta_",year))[lastnodes,hour])
  closestlay <<- sapply(-lastdepthprop,function(x) which.min(abs(siglay-x)))
  

  # find closest nodes
  if (nodes==1){
    distances <<- diag(gDistance(nodepoints[ loop_array[,3,1]],
                                SpatialPoints(loop_array[,1:2,1]),byid=T))
  }
  
  if (nodes==2) {
    
    distances <<- cbind(   diag(gDistance(nodepoints[ loop_array[,3,1]],
                                          SpatialPoints(loop_array[,1:2,1]),byid=T)),
                           diag(gDistance(nodepoints[ loop_array[,4,1]],
                                          SpatialPoints(loop_array[,1:2,1]),byid=T)))
    
  }
  
  if (nodes==3) {
    distances <<- cbind(diag(gDistance(nodepoints[ loop_array[,3,1]],
                                      SpatialPoints(loop_array[,1:2,1]),byid=T)),
                       diag(gDistance(nodepoints[ loop_array[,4,1]],
                                      SpatialPoints(loop_array[,1:2,1]),byid=T)),
                       diag(gDistance(nodepoints[ loop_array[,5,1]],
                                      SpatialPoints(loop_array[,1:2,1]),byid=T)))
  }
  
  
}
#-----




# Function 3. Grow Larvae
#-------------------------------------------
# this uses temp and sal values at last closest node to make vector of growth rates for the next step
grow.larvae <- function(year= get('year',envir=globalenv())) {
  
  growth<-  predict(growthgam_final,
                    list(Temp=get(paste0("WA_currents_",year))[cbind(lastnodes,1,closestlay,hour)],
                         Sal=get(paste0("WA_currents_",year))[cbind(lastnodes,2,closestlay,hour)])) / 24 # function output is um/day, but we want um/hour
  growth[growth<0] <- 0 # some temps are too cold so GRs are coming out negative. Change these to 0. 
  return(growth)
  
}
#----





# Function 4. Move Larvae
#---------------------------------------
# this will add new positions to the "loop_array" second page
move.larvae <- function(array = get("loop_array",envir=globalenv()), nodes=get("nodes",envir = globalenv()),year=get("year",envir=globalenv())){
  
  # condition if nodes = 1
  #-----
  if (nodes==1) {
    
    #  new coordinate     =    old coordinate    +            currents                from last closest node ,   closest layer,  last hr  converted to m/hour     
    #  --------------        -------------------      ----------------------------        -------------------      ----------     ----    ------
    loop_array[,1,2] <<- loop_array[,1,1] + get(paste0("WA_currents_",year))[cbind(  lastnodes, dim = 3,  closestlay,     hour)]   *3600  # move x
    loop_array[,2,2] <<- loop_array[,2,1] + get(paste0("WA_currents_",year))[cbind(  lastnodes, dim = 4,  closestlay,     hour)]   *3600  # move y
    loop_array[,4,2] <<- loop_array[,3+nodes,1] + get(paste0("WA_currents_",year))[cbind(  lastnodes, dim = 5,  closestlay,     hour)]   *3600  # move z
    
  #  loop_array[,1,hour+1] <<- mapply(sum,loop_array[,1,hour],get(paste0("WA_currents_",year))[cbind(loop_array[,3,hour],3,closestlay,hour)]*3600) # move x
  #  loop_array[,2,hour+1] <<- mapply(sum,loop_array[,2,hour],get(paste0("WA_currents_",year))[cbind(loop_array[,3,hour],4,closestlay,hour)]*3600) # move y
  #  loop_array[,4,hour+1] <<- mapply(sum,loop_array[,4,hour], get(paste0("WA_currents_",year))[cbind(loop_array[,3,hour],5,closestlay,hour)]*3600) # move z

  }
  #-----
  
  # condition if nodes = 3 (weighted mean)
  #-----------------
 if (nodes==3) { 
   
   movexyz <-matrix(nrow=nrow(restorationsites),ncol=3) # make matrix of weighted means of movement (prop. values from 3 closest nodes)
   movexyz[,1] <- (  get(paste0("WA_currents_",year))[cbind( loop_array[,3,1] ,3,closestlay,hour)] *(apply(distances,1,sum)-( distances[,1])) +
                      get(paste0("WA_currents_",year))[cbind( loop_array[,4,1] ,3,closestlay,hour)] *(apply(distances,1,sum)-( distances[,2])) +
                      get(paste0("WA_currents_",year))[cbind( loop_array[,5,1] ,3,closestlay,hour)] *(apply(distances,1,sum)-( distances[,3])) ) / 
     ( apply(distances,1,sum)-( distances[,1]) + (apply(distances,1,sum)-( distances[,2])) + (apply(distances,1,sum)-( distances[,3])))
   


   movexyz[,2] <- (  get(paste0("WA_currents_",year))[cbind( loop_array[,3,1] ,4,closestlay,hour)] *(apply(distances,1,sum)-( distances[,1])) +
                     get(paste0("WA_currents_",year))[cbind( loop_array[,4,1] ,4,closestlay,hour)] *(apply(distances,1,sum)-( distances[,2])) +
                     get(paste0("WA_currents_",year))[cbind( loop_array[,5,1] ,4,closestlay,hour)] *(apply(distances,1,sum)-( distances[,3])) ) / 
     ( apply(distances,1,sum)-( distances[,1]) + (apply(distances,1,sum)-( distances[,2])) + (apply(distances,1,sum)-( distances[,3])))
   
   
   movexyz[,3] <- (  get(paste0("WA_currents_",year))[cbind( loop_array[,3,1] ,5,closestlay,hour)] *(apply(distances,1,sum)-( distances[,1])) +
                       get(paste0("WA_currents_",year))[cbind( loop_array[,4,1] ,5,closestlay,hour)] *(apply(distances,1,sum)-( distances[,2])) +
                       get(paste0("WA_currents_",year))[cbind( loop_array[,5,1] ,5,closestlay,hour)] *(apply(distances,1,sum)-( distances[,3])) ) / 
     ( apply(distances,1,sum)-( distances[,1]) + (apply(distances,1,sum)-( distances[,2])) + (apply(distances,1,sum)-( distances[,3])))
   
   loop_array[,1,2] <<-  loop_array[,1,1] + (movexyz[,1]*3600)# move x
   loop_array[,2,2] <<-  loop_array[,2,1] + (movexyz[,2]*3600)# move y
   loop_array[,3+nodes,2] <<-  loop_array[,3+nodes,1] + (movexyz[,3]*3600)# move z
   
   }
  #----

  
}
#----



# Function 5. Swim Larvae
#--------------------------------------
# this adds ato the z value of larvae depending on which swim scenario you chose
swim.larvae <- function(array = get("loop_array",envir=globalenv()), behavior = get("behavior",envir=globalenv())){
  
  
  # condition if behavior == none, AKA passive transport only
  #------------------------------
  if (behavior=="none") {
    
    # do nothing. Don't really need this line, but I like to see it. 
    loop_array[,4,2] <<- loop_array[,4,2]
    
  }
  #-----
  

  
  # condition if behavior == photo, AKA up in day, down at night
  #--------------------------------
  if (behavior=="photo") {
    
    # if night time
    if(time[hour] == "N"){
      
      # swim downwards
      loop_array[,4,2] <<- loop_array[,4,2] - (.0012*3600) 
      
    } else { # otherwise, if day time,
      
      # swim upwards
      loop_array[,4,2] <<- loop_array[,4,2] + (.0012*3600)
    }
    
    
  }
  #-----
  
  
 
  
  # condition if behavior == onto, AKA up when young, down when old
  #------------------------------
  if (behavior=="onto") {
  
    # small larvae swim up
    loop_array[,4,2][ get(paste(paste0("larvatrack",name,".sizes")))[,hour]  < 220 ] <<- 
      loop_array[,4,2][ get(paste(paste0("larvatrack",name,".sizes")))[,hour]  < 220 ] + (.0012*3600) # add up swim 
    
    # big larvae swim down
    loop_array[,4,2][ get(paste(paste0("larvatrack",name,".sizes")))[,hour]  > 260 ] <<- 
      loop_array[,4,2][ get(paste(paste0("larvatrack",name,".sizes")))[,hour]  > 260 ] + (.0012*3600) # add up swim 

  }
  #-----
  
  
  
}
#-----
  
  
# Function 6. Correct Larvae
#-------------------------------------
# this function corrects larvae that have gone onto land, 
# swam above water surface or bellow the sea floor,
# and stops larvae who have skipped over bits of land. 

correct.larvae <- function(array = get("loop_array",envir=globalenv()), year=get("year",envir=globalenv())){

  # correct larvae who go on land
  #------------------------------------
  # this will correct larvae that end up further from their closest node than that node is to it's closest node,
  # then snap larvae back to their closest. This is meant to keep larvae from traveling onto land. 

  # find new closest nodes
  loop_array[,3:(3+nodes-1),2] <<- t(apply(gDistance(nodepoints,SpatialPoints(loop_array[,1:2,release]),byid = TRUE),1,order)[1:nodes,]) # closest nodes 1-3
  
 # loop_array[,3,2] <<- apply(gDistance(nodepoints,SpatialPoints(loop_array[,1:2,2]),byid=TRUE),1,which.min) # find new closest node

  # finds distance between larva and closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[loop_array[,3,2]], SpatialPoints(loop_array[,1:2,2]),byid=TRUE),2,min))


    
   # finds distance between that node and next closest node
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-loop_array[,3,2]],  nodepoints[loop_array[,3,2]],byid = TRUE),1,min)) 

  # when larvatonode is bigger than noderadius, snap coordinates of larva back to closest node
  # this will keep larvae from traveling across land
  loop_array[larvatonode > noderadius, 1:2 , 2] <<- apply(WA_nodes_coords[ loop_array[larvatonode > noderadius,3,2],2:3], 2,as.numeric)
  

  # Manually correct land jumps
  #-----------------------------
  # correct larvae that jump from fidalgo bay West onto Fidalgo Island - fix X
  loop_array[,1,2][loop_array[,1,1] >= 529902  & loop_array[,1,2] < 529902  &
                     loop_array[,2,1] >= 5368432 &  loop_array[,2,1] < 5373264] <<- 529902

  # correct larvae that jump from fidalgo bay East onto Fidalgo Island - fix X
  loop_array[,1,2][loop_array[,2,1] > 5367200  & loop_array[,2,1] < 5371500  & loop_array[,1,1] <= 532000 &
                     loop_array[,1,2] >= 532000] <<- 532300
  
  
  # keep larvae from jumping fidalgo bay into similk bay - fix Y back to fidalgo bay
  loop_array[,2,2][ loop_array[,1,1] > 529641  & loop_array[,1,1] < 531760  & loop_array[,2,1] > 5367800 &
                      loop_array[,2,2] <= 5367800] <<- 5368000
  

  # keep larvae from jumping from similk bay into fidalgo bay - fix Y back to fidalgo bay
  loop_array[,2,2][ loop_array[,1,1] > 530600  & loop_array[,1,1] < 533200  & loop_array[,2,1] < 5366300 &
                      loop_array[,2,2] > 5366300] <<- 5366000
  
  # keep larvae from crossing out of Kilisut Harbor - fix Y
  loop_array[,2,2][ loop_array[,1,1] > 521200  & loop_array[,1,1] < 523000  & loop_array[,2,1] > 5318800 &
                      loop_array[,2,2] < 5318800] <<- 5319300
  
  
  
  
  # find new closest node
  loop_array[,3:(3+nodes-1),release] <<- t(apply(gDistance(nodepoints,SpatialPoints(loop_array[,1:2,release]),byid = TRUE),1,order)[1:nodes,]) # closest nodes 1-3
  
  #loop_array[,3,2] <<- apply(gDistance(nodepoints,SpatialPoints(loop_array[,1:2,2]),byid=TRUE),1,which.min) # find new closest node
  
  
  # Correct Surface and Depth
  #------------------
  # find larvae above surface, and limit to surface
  loop_array[,3+nodes,2] [loop_array[,3+nodes,2] >  get(paste0("zeta_",year))[lastnodes,hour] ] <<-  # find larvae > tidal height
    get(paste0("zeta_",year))[lastnodes,hour] [loop_array[,3+nodes,2] >  get(paste0("zeta_",year))[lastnodes,hour] ] # make depth position tidal height
  # find larvae below bottom, and limit to bottom
  loop_array[,3+nodes,2] [ loop_array[,3+nodes,2] < (-WA_nodes_coords[lastnodes,4]) ] <<-
    (-WA_nodes_coords[lastnodes,5]) [ loop_array[,3+nodes,2] < (-WA_nodes_coords[lastnodes,4]) ]
  #----
  

}
#----





# Function 7. Convert Larvae to DF
#-----------------------------------
convert.larvae <- function(name = get("name",envir=globalenv())) {
  
  df <- data.frame(matrix(nrow=(nrow(get(paste0("larvatrack",name)))*length(get(paste0("larvatrack",name))[1,1,])),ncol=7))
  colnames(df) <- c("site","hour","x","y","d","size","day")
  
  df$site <- rep(restorationsites$site, each= length(get(paste0("larvatrack",name))[1,1,]) )
  df$hour <- rep(1:(length(get(paste0("WA_currents_",year))[1,1,1,])+1),times=nrow(restorationsites))
  df$x <- c(t(get(paste0("larvatrack",name))[1:19,1,]))
  df$y <- c(t(get(paste0("larvatrack",name))[1:19,2,]))
  df$d <- c(t(get(paste0("larvatrack",name))[1:19,3+nodes,]))
  df$size <- as.vector(t(get(paste0("larvatrack",name,".sizes"))))
  df$node <- c(t(get(paste0("larvatrack",name))[1:19,3,]))
  df <- df %>%
    group_by(site) %>%
    mutate(day = ceiling(hour/24)) %>%
    ungroup()
  
  assign(paste0("larvatrack",name,"_df"), df, envir = globalenv())
  
}
#-----



