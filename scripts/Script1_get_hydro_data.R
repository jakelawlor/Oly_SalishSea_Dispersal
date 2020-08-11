###################################################################
#           NetCDF deconstruction and reconstruction
# #################################################################

library("ncdf4")
library(ggplot2)

# after setting working directory to flash, open a file:
ncin <- nc_open("3DCurrents_ssm_00185.nc")
ncin <- nc_open(file.choose())
?nc_open
i=185
(paste("3DCurrents_ssm_00",i,".nc",sep=""))
?paste

# get info 
print(ncin)
dim(ncin)
ncinPS <- ncin[which(ncin$dim$node$vals %in% PSnodes_list)]
print(ncinPS)
rm(ncinPS)


# get name of variable id
X <- ncvar_get(ncin,"X")
Y <- ncvar_get(ncin,"Y")
Temp <- ncvar_get(ncin,"temp")   
dim(Temp) ### this shows me that "Temp" object is an array - 16012 rows (one per node), 10 cols (depths), 24 pages (time)
h <- ncvar_get(ncin,"h")
Sal <- ncvar_get(ncin,"salinity")
siglay <- ncvar_get(ncin,"siglay")
time <- ncvar_get(ncin,"time")
#node <- ncvar_get(ncin,"node")  # doesn't work - says I'm getting more than one element.?




# I don't know how to access data from the NetCDF file, so I'm just going
# to create a dataframe with nodes 1:16012, with associated coordinates,
# temp, and sal values to go along. Note that this is from Hr 1, meaning probably midnight? Is this important?
nodes <- data.frame(matrix(nrow=16012,ncol = 5))
colnames(nodes)<-c ("node","X","Y","Temp","Sal")
nodes$node <- ncin$dim$node$vals
nodes$X <- X
nodes$Y <- Y
nodes$Temp <-Temp[,1,1]
nodes$Sal <- Sal[,1,1]
dim(Temp)


# Plot points of surface temp
ggplot(data = nodes, aes(x=X,y=Y,col=Temp)) +
  geom_point() 


##### Find Washington Puget Sound Nodes Only #####
#===================================================
PSnodes <- nodes[nodes$X > 280000 & nodes$Y >5200000 & nodes$Y < 5450000,]
ggplot(data = PSnodes,aes(x=X,y=Y,color=Temp))+
  geom_point(size=.5)
PSnodes <- subset(PSnodes,!(X<450000 & Y < 5250000))
ggplot(data = PSnodes,aes(x=X,y=Y,color=Temp))+
  geom_point(size=.5)
PSnodes_list <- PSnodes$node
save(PSnodes_list,file = "PSnodes_list_Nov19.Rdata")

#save(PSnodes_list,file = "PSnodes_large.Rdata")
#save(PSnodes_list,file = "PSnodes_large2.Rdata")


rm(PSnodes2)

ggplot(data = PSnodes,aes(x=X,y=Y,color=Temp))+
  geom_point(size=.5)


min(PSnodes3$node)
PS.nodevals
nrow(PS.nodevals)
dim(PStotal95)

PSnodes_list_currents2 <- PSnodes3$node
save(PSnodes_list_currents2,file = "PSnodes_currents2.Rdata")
length(PSnodes_list_currents2)
##################################################
dim(PStotal95)

#  10.  Identify approximate restoration site coords (X,Y)
#===============================================================
restorationsites <- data.frame(matrix(nrow=19,ncol=3))
{colnames(restorationsites) <- c("site","X","Y")
restorationsites[1,] <- c("Drayton Harbor", 513619,5427610)
restorationsites[2,] <- c("Bellingham Bay", 531650,5401489)
restorationsites[3,] <- c("Samish Bay",540603, 5381721)
restorationsites[4,] <- c("Padilla Bay", 537257,5371223)
restorationsites[5,] <- c("Fidalgo Bay", 530400, 5370434)
restorationsites[6,] <- c("Similk Bay", 531804, 5365232)
restorationsites[7,] <- c("Sequim Bay", 498461,5321746)
restorationsites[8,] <- c("Discovery Bay", 511000, 5318000)
restorationsites[9,] <- c("Kilisut Harbor", 521014, 5322549)
restorationsites[10,] <- c("Quilcene Bay", 511199,5295764)
restorationsites[11,] <- c("Port Gamble Bay", 531784, 5297178)
restorationsites[12,] <- c("Liberty Bay",526342,5285798)
restorationsites[13,] <- c("Squaxin Island", 505282,5227262)
restorationsites[14,] <- c("Port Orchard Pass", 531166, 5274265)
restorationsites[15,] <- c("Dyes Inlet", 523724, 5274265)
restorationsites[16,] <- c("Sinclair Inlet", 523735, 5264650)
restorationsites[17,] <- c("Union River", 507937, 5252257)
restorationsites[18,] <- c("Henderson Bay", 512560, 5219113)
restorationsites[19,] <- c("Budd Inlet",506941, 5211139)
restorationsites <- as.data.frame(restorationsites)
restorationsites$X <- as.numeric(restorationsites$X)
restorationsites$Y <- as.numeric(restorationsites$Y)}
class(restorationsites)
head(restorationsites)
restorationsites <- restorationsites[order(-restorationsites$Y),]
restorationsites$basin <-NA
restorationsites$basin[1:5] <- c("Georgia Strait")
restorationsites$basin[6] <- c("Whidbey")
restorationsites$basin[7] <- c("North Central")
restorationsites$basin[8:9] <- c("Juan De Fuca")
restorationsites$basin[c(10:11,16)] <- c("Hood Canal")
restorationsites$basin[12:15] <- c("South Central")
restorationsites$basin[17:19] <- c("South")
restorationsites$basinnum <- NA
restorationsites$basinnum[1:5] <- 1
restorationsites$basinnum[6] <- 2
restorationsites$basinnum[7] <- 3
restorationsites$basinnum[8:9] <- 4
restorationsites$basinnum[c(10:11,16)] <- 5
restorationsites$basinnum[12:15] <- 6
restorationsites$basinnum[17:19] <- 7

save(restorationsites,file="restorationsites.Rdata")

restorationsites <- restorationsites[order(restorationsites$basinnum),]


#==============================================================

# check that spots are right
ggplot(data = PS.nodevals,aes(x=X,y=X))+
         geom_point(aes(x=X,y=Y,color=PS.array3[,1,1,1]))+
         geom_point(data=restorationsites,aes(x=X,y=Y),color="red")
# looks good. 19 sites in correct spots



#  11.  Plot movement from sites in surface currents in 24 hours 
#======================================================================
# create array of 19 sites in 24 hours
larvatrack.sites <- array(dim=c(19,3,25))  # array with 2 larvae, 3 cols (X,Y,Closestnode), 24 pages (hours)
larvatrack.sites[,1,1] <- restorationsites$X 
larvatrack.sites[,2,1] <- restorationsites$Y
larvatrack.sites[,3,1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.sites[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.sites[,,1]  # this creates first page of array with larva coordinates that I picked. 

# fill array
hour = 1
for(hour in 1:24){
  larvatrack.sites[,1,hour+1] <- mapply(sum,larvatrack.sites[,1,hour],PS.array3[larvatrack.sites[,3,hour],3,1,hour]*3600) # move x
  larvatrack.sites[,2,hour+1] <- mapply(sum,larvatrack.sites[,2,hour],PS.array3[larvatrack.sites[,3,hour],4,1,hour]*3600) # move y
  larvatrack.sites[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.sites[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  print(hour)
}
larvatrack.sites

# change array to df
sitetrack <- data.frame(matrix(nrow=(nrow(larvatrack.sites)*length(larvatrack.sites[1,1,])),ncol=4))
colnames(sitetrack) <- c("larva","hour","X","Y")
sitetrack$larva <- rep(1:nrow(larvatrack.sites),each=length(larvatrack.sites[1,1,]))
sitetrack$hour <- rep(1:25,times=nrow(larvatrack.sites))
sitetrack$X <- c(t(larvatrack.sites[1:19,1,]))
sitetrack$Y <- c(t(larvatrack.sites[1:19,2,]))


# plot tracks from sites 
ggplot(data = PS.nodevals,aes(x=X,y=X))+
  geom_point(aes(x=X,y=Y,color=PS.array3[,1,1,1])) + 
  geom_point(data=sitetrack,aes(x=X,y=Y,group=larva),color="red",size=.25)+
  geom_path(data=sitetrack,aes(x=X,y=Y,group=larva),color="red")



#  12.  Plot movement in bottom currents in 24 hours
#==========================================================
# create array of 19 sites in 24 hours
larvatrack.sites.bottom <- array(dim=c(19,3,25))  # array with 2 larvae, 3 cols (X,Y,Closestnode), 24 pages (hours)
larvatrack.sites.bottom[,1,1] <- restorationsites$X 
larvatrack.sites.bottom[,2,1] <- restorationsites$Y
larvatrack.sites.bottom[,3,1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.sites.bottom[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.sites.bottom[,,1]  # this creates first page of array with larva coordinates that I picked. 

# fill array
hour = 1
for(hour in 1:24){
  larvatrack.sites.bottom[,1,hour+1] <- mapply(sum,larvatrack.sites.bottom[,1,hour],PS.array3[larvatrack.sites.bottom[,3,hour],3,10,hour]*3600) # move x
  larvatrack.sites.bottom[,2,hour+1] <- mapply(sum,larvatrack.sites.bottom[,2,hour],PS.array3[larvatrack.sites.bottom[,3,hour],4,10,hour]*3600) # move y
  larvatrack.sites.bottom[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.sites.bottom[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  print(hour)
}
larvatrack.sites.bottom

# change array to df
sitetrack.bottom <- data.frame(matrix(nrow=(nrow(larvatrack.sites.bottom)*length(larvatrack.sites.bottom[1,1,])),ncol=4))
colnames(sitetrack.bottom) <- c("larva","hour","X","Y")
sitetrack.bottom$larva <- rep(1:nrow(larvatrack.sites.bottom),each=length(larvatrack.sites.bottom[1,1,]))
sitetrack.bottom$hour <- rep(1:25,times=nrow(larvatrack.sites.bottom))
sitetrack.bottom$X <- c(t(larvatrack.sites.bottom[1:19,1,]))
sitetrack.bottom$Y <- c(t(larvatrack.sites.bottom[1:19,2,]))


# plot tracks from sites 
ggplot(data = PS.nodevals,aes(x=X,y=X))+
  geom_point(aes(x=X,y=Y,color=PS.array3[,1,1,1])) + 
  geom_point(data=sitetrack,aes(x=X,y=Y,group=larva),color="red",size=.25)+
  geom_path(data=sitetrack,aes(x=X,y=Y,group=larva),color="red")+
  geom_point(data=sitetrack.bottom,aes(x=X,y=Y,group=larva),color="darkred",size=.25)+
  geom_path(data=sitetrack.bottom,aes(x=X,y=Y,group=larva),color="darkred")




#  13.  Add more timepoints, up to one week, from the top. 
#==========================================================================
install.packages("ncdf4")
install.packages("ggplot2")
install.packages("rgeos")
install.packages("abind")
install.packages("plotly")
install.packages("MASS")
install.
library(ncdf4)
library(ggplot2)
library(rgeos)
library(abind)
library(plotly)
library(ggforce)
library(ggrepel)
.Library


load("PSnodes_currents.RData", verbose=TRUE)
load("PSnodes_large.Rdata",verbose=TRUE)
## need to make one day first, above, and name "PStotal".
# will move code for that down here later. 
load("JulyCurrents.Rdata",verbose=TRUE)
load(file.choose(),verbose=TRUE)

rm(PSnodes_list_currents)

dim(PStotal95)


# upload June 6
ncin <- nc_open("3DCurrents_ssm_00185.nc")

# get individual variables 
X <- ncvar_get(ncin,"X")
Y <- ncvar_get(ncin,"Y")
Temp <- ncvar_get(ncin,"temp")   
dim(Temp) ### this shows me that "Temp" object is an array - 16012 rows (one per node), 10 cols (depths), 24 pages (time)
h <- ncvar_get(ncin,"h")
Sal <- ncvar_get(ncin,"salinity")
siglay <- ncvar_get(ncin,"siglay")
time <- ncvar_get(ncin,"time") # this might be concecutive between datasets?
u <- ncvar_get(ncin,"u")
v <- ncvar_get(ncin,"v")
w <- ncvar_get(ncin,"w")
zeta <- ncvar_get(ncin,"zeta")

dim(zeta)
zeta[,1]

# trim to Puget Sound nodes only
PSX <- X[PSnodes_list_currents2]
PSY <- Y[PSnodes_list_currents2]
PSTemp <- Temp[PSnodes_list_currents2,,]
dim(PSTemp)
PSh_save <- h[PSnodes_list_currents2]
PSSal <- Sal[PSnodes_list_currents2,,]
dim(PSSal)
PSsiglay <- siglay
PStime <- time
PSu <- u[PSnodes_list_currents2,,]
PSv <- v[PSnodes_list_currents2,,]
PSw <- w[PSnodes_list_currents2,,]
PSzeta <- zeta[PSnodes_list_currents2,]
dim(PSu)

nrow(PSX)
rm(X,Y,Temp,h,Sal,siglay,time,u,v,w)

#  13.2  Create node coordinates df (non-changing values)
#================================================================
PS.nodevals <- data.frame(matrix(nrow=7404,ncol=4))
colnames(PS.nodevals) <- c("node","X","Y","h")
PS.nodevals$node <- PSnodes_list_currents
PS.nodevals$X <- PSX
PS.nodevals$Y <- PSY
PS.nodevals$h <- PSh
PS.nodevals <- PS.nodevals[c(1:4,6)]
PSsiglay 
min(PS.nodevals$h_save)
dim(PSzeta)
max(PSzeta)

#  13.2  Create node coordinates df (non-changing values)  ((bigger))
#================================================================
load("/Volumes/SSM_FLASH/PS.nodevals.Rdata")
#PS.nodevals <- data.frame(matrix(nrow=7762,ncol=5))
#colnames(PS.nodevals) <- c("node","X","Y","h_distorted","h")
#PS.nodevals$node <- PSnodes_list_currents2
#PS.nodevals$X <- PSX
#PS.nodevals$Y <- PSY
#PS.nodevals$h_distorted <- PSh_save
#PS.nodevals$h <- PSnodedepths$d
#PSsiglay 

nrow(PSnodedepths)
#  13.3  Create node temp, sal, velocities list (chainging variables)
#===============================================================
#  smaller array for values that change only
PSstart <- array(dim=c(7404,5,10,24))   
# creates an array - 7963 rows (nodes), 2 columns (temp+sal), 10 pages (depths), 24 stacks (hours)
PSstart[,1,,] <-PSTemp
PSstart[,2,,] <- PSSal
PSstart[,3,,] <- PSu
PSstart[,4,,] <- PSv
PSstart[,5,,] <- PSw
PSstart[,,,1]
PStime
rm(i,PSh,PSSal,PSTemp,PSu,PSv,PSw,PSX,PSY,ncin)
dim(PSstart)

nrow(PS.nodevals)
#  13.3  Create node temp, sal, velocities list (chainging variables)  ((bigger))
#===============================================================
#  smaller array for values that change only
PSstart <- array(dim=c(7762,5,10,24))   
# creates an array - 7963 rows (nodes), 2 columns (temp+sal), 10 pages (depths), 24 stacks (hours)
PSstart[,1,,] <-PSTemp
PSstart[,2,,] <- PSSal
PSstart[,3,,] <- PSu
PSstart[,4,,] <- PSv
PSstart[,5,,] <- PSw
PSstart[,,,1]
PStime
rm(i,PSh,PSSal,PSTemp,PSu,PSv,PSw,PSX,PSY,ncin)
dim(PSstart)

PSstart[,,,1]
#  13.4  Repeat for the next day + merge
#==========================================================
dim(PSstart)
PStotal14 <- PSstart
dim(PStotal14)
PStotal14[,,1,1]
ggplot(data=PS.nodevals,aes(x=X,y=Y))+
  geom_point(aes(color=PStotal14[,1,1,16]),size=.3)
ggplotly( ggplot(data=PS.nodevals,aes(x=X,y=Y))+
            geom_point(aes(color=PStotal[,1,1,16]),size=.3))
rm(PSstart)

PStotal14[,1,1,]
mean(PStotal14[,1,1,])
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===  
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   === 

Zeta14 <- PSzeta
dim(Zeta14)
#  Loop to pull zeta vals
#=========================================================

# Create first page zeta set 
Zeta95 <- PSzeta
dim(Zeta95)

# Loop the rest
for(i in 186:234){
  # get zeta vals
  ncin <- nc_open(paste("3DCurrents_ssm_00",i,".nc",sep=""))
  zeta <- ncvar_get(ncin, "zeta")
  #trim to PSnodes
  PSzeta <- zeta[PSnodes_list_currents2,]
  
  # merge with existing arrays  
  Zeta95 <- abind(Zeta95,PSzeta,rev.along=1)
  dim(Zeta95)
  rm(PSzeta,ncin)
  print(i)
  
}

library(abind)
#  Loop to pull .nc data for 4 days
#===========================================================
dim(PStotal14)
for(i in 186:234){     # start loop
 # open data
  ncin <- nc_open(paste("3DCurrents_ssm_00",i,".nc",sep=""))
 # extract variables 
  Temp <- ncvar_get(ncin,"temp")   
  Sal <- ncvar_get(ncin,"salinity")
  u <- ncvar_get(ncin,"u")
  v <- ncvar_get(ncin,"v")
  w <- ncvar_get(ncin,"w")
  
 # trim to PS
  PSTemp <- Temp[PSnodes_list_currents2,,]
  PSSal <- Sal[PSnodes_list_currents2,,]
  PSu <- u[PSnodes_list_currents2,,]
  PSv <- v[PSnodes_list_currents2,,]
  PSw <- w[PSnodes_list_currents2,,]
 
  rm(Temp,Sal,u,v,w)
  
 # make array
  PS.array2 <- array(dim=c(length(PSnodes_list_currents2),5,10,24))   
  # creates an array - 7963 rows (nodes), 5 columns (vars), 10 pages (depths), 24 stacks (hours)
  PS.array2[,1,,] <-PSTemp
  PS.array2[,2,,] <- PSSal
  PS.array2[,3,,] <- PSu
  PS.array2[,4,,] <- PSv
  PS.array2[,5,,] <- PSw
  PS.array2[,,,1]
  rm(PSTemp,PSSal,PSu,PSv,PSw)

 # merge with existing arrays  
  PStotal14 <- abind(PStotal14,PS.array2,rev.along=1)
  rm(PS.array2)
  print(i)
} 

dim(PStotal14)
mean(PStotal14[,1,1,])
mean(PStotal95[,1,1,])
save(PStotal14,file="JulyCurrents14_Big.Rdata")
rm(i,ncin)
#  HELLLLL YEAHHHHHHHH success!
#  this yields an array 8000 rows (nodes) x 5 cols (temp,sal,u,v,w) x 10 depths x 96 hours (4 days)

save(PStotal14,file="JulyCurrentsBigVersion.Rdata")
save(PStotal,file="JulyCurrentsBigVersion95.Rdata")

dim(Zeta14)
Zeta95[1:5,1:5]
Zeta14[1:5,1:5]

min(Zeta95)
min(Zeta14)
max(Zeta95)
max(Zeta14)
#  13.5  Plot dispersal tracks for 4 days. 
#==============================================================
 # Make "restorationsites" df from code above.
 # Probably will at some point just save this
 head(restorationsites)
load("restorationsites.Rdata")

# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
larvatrack.surface <- array(dim=c(nrow(restorationsites),3,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,Closestnode), 288 pages (hours)
larvatrack.surface[,1,1] <- restorationsites$X 
larvatrack.surface[,2,1] <- restorationsites$Y
larvatrack.surface[,3,1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.surface[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.surface[,,1]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.surface)

# Fill array - move larvae with currents 
hour = 1
for(hour in 1:length(PStotal[1,1,1,])){
  larvatrack.surface[,1,hour+1] <- mapply(sum,larvatrack.surface[,1,hour],PStotal[larvatrack.surface[,3,hour],3,1,hour]*3600) # move x
  larvatrack.surface[,2,hour+1] <- mapply(sum,larvatrack.surface[,2,hour],PStotal[larvatrack.surface[,3,hour],4,1,hour]*3600) # move y
  larvatrack.surface[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.surface[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  print(hour)
}



# change array to df
larvatrack.surface.df <- data.frame(matrix(nrow=(nrow(larvatrack.surface)*length(larvatrack.surface[1,1,])),ncol=4))
colnames(larvatrack.surface.df) <- c("site","hour","X","Y")
larvatrack.surface.df$site <- rep(restorationsites$Site,each=length(larvatrack.surface[1,1,]))
larvatrack.surface.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.surface))
larvatrack.surface.df$X <- c(t(larvatrack.surface[1:19,1,]))
larvatrack.surface.df$Y <- c(t(larvatrack.surface[1:19,2,]))

head(larvatrack.surface.df)

# plot tracks from sites 
ggplot(data = PS.nodevals,aes(x=X,y=X))+
  geom_point(aes(x=X,y=Y,color=PStotal[,1,1,1])) + 
  geom_point(data=larvatrack.surface.df,aes(x=X,y=Y,group=site),color="red",size=.25)+
  geom_path(data=larvatrack.surface.df,aes(x=X,y=Y,group=site),color="red")

#  Ok, this looks good, but some larvae are moving out of the water. Need to figure out how to force off of land


head(larvatrack.surface.df)
# now add bottom currents 
#=================================================
larvatrack.bottom <- array(dim=c(nrow(restorationsites),3,length(PStotal[1,1,1,])+1))  
# array with 2 larvae, 3 cols (X,Y,Closestnode), 24 pages (hours)
larvatrack.bottom[,1,1] <- restorationsites$X 
larvatrack.bottom[,2,1] <- restorationsites$Y
larvatrack.bottom[,3,1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.bottom[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.bottom[,,1]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.bottom)

# Fill array - move larvae with currents 
hour = 1
for(hour in 1:length(PStotal[1,1,1,])){
  larvatrack.bottom[,1,hour+1] <- mapply(sum,larvatrack.bottom[,1,hour],PStotal[larvatrack.bottom[,3,hour],3,10,hour]*3600) # move x
  larvatrack.bottom[,2,hour+1] <- mapply(sum,larvatrack.bottom[,2,hour],PStotal[larvatrack.bottom[,3,hour],4,10,hour]*3600) # move y
  larvatrack.bottom[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.bottom[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  ## going to have to add line for "if distance is over x, coords = nodecoords... or something 
  print(hour)
}

dim(larvatrack.bottom)


# change array to df
larvatrack.bottom.df <- data.frame(matrix(nrow=(nrow(larvatrack.bottom)*length(larvatrack.bottom[1,1,])),ncol=4))
colnames(larvatrack.bottom.df) <- c("site","hour","X","Y")
larvatrack.bottom.df$site <- rep(restorationsites$Site,each=length(larvatrack.bottom[1,1,]))
larvatrack.bottom.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.bottom))
larvatrack.bottom.df$X <- c(t(larvatrack.bottom[1:19,1,]))
larvatrack.bottom.df$Y <- c(t(larvatrack.bottom[1:19,2,]))

rm(larvatrack.surface,larvatrack.bottom)


# plot tracks from surface and bottom 
trackplot <- ggplot(data = PS.nodevals,aes(x=X,y=X))+
  geom_point(aes(x=X,y=Y,color=PStotal[,1,1,1])) + 
  geom_point(data=larvatrack.surface.df,aes(x=X,y=Y,group=site),color="red",size=.25)+
  geom_path(data=larvatrack.surface.df,aes(x=X,y=Y,group=site),color="red")+
  geom_point(data=larvatrack.bottom.df,aes(x=X,y=Y,group=site),color="darkred",size=.25)+
  geom_path(data=larvatrack.bottom.df,aes(x=X,y=Y,group=site),color="darkred")
trackplot
ggplotly(trackplot)

rm(larvatrack.surface,larvatrack.surface.df)
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
#    ===   ===   ===   TRY WITH DEPTH DIMENSION ===   ===   ===   ===   ==    
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===  
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   === 

getwd()
setwd("/Volumes/JLDATA/SSM_2014_HYD/SSM_2014_3D_Currents_S_T  ")
load("PSnodes_currents.RData", verbose=TRUE)

## need to make one day first, above, and name "PStotal".
# will move code for that down here later. 
library(ggforce)
library(ncdf4)
library(ggplot2)
library(abind)
library(rgeos)
library(ggrepel)
# upload June 6
ncin <- nc_open("3DCurrents_ssm_00185.nc")

# get individual variables 
X <- ncvar_get(ncin,"X")
Y <- ncvar_get(ncin,"Y")
Temp <- ncvar_get(ncin,"temp")   
dim(Temp) ### this shows me that "Temp" object is an array - 16012 rows (one per node), 10 cols (depths), 24 pages (time)
h <- ncvar_get(ncin,"h")
Sal <- ncvar_get(ncin,"salinity")
siglay <- ncvar_get(ncin,"siglay")
time <- ncvar_get(ncin,"time") # this might be concecutive between datasets?
u <- ncvar_get(ncin,"u")
v <- ncvar_get(ncin,"v")
w <- ncvar_get(ncin,"w")

# trim to Puget Sound nodes only
PSX <- X[PSnodes_list_currents]
PSY <- Y[PSnodes_list_currents]
PSTemp <- Temp[PSnodes_list_currents,,]
dim(PSTemp)
PSh <- h[PSnodes_list_currents]
PSSal <- Sal[PSnodes_list_currents,,]
dim(PSSal)
PSsiglay <- siglay
PStime <- time
PSu <- u[PSnodes_list_currents,,]
PSv <- v[PSnodes_list_currents,,]
PSw <- w[PSnodes_list_currents,,]
dim(PSu)
dim(PStotal)

# also make one for the "whole sea" so it's easier to see
#======================================================
load("PSnodes_large.RData", verbose=TRUE)

wholeseaX <- X[PSnodes_list]
wholeseaY <- Y[PSnodes_list]
length(wholeseaX)
wholesea <- data.frame(matrix(nrow=length(wholeseaX),ncol=2))
colnames(wholesea)
wholesea$X1 <- wholeseaX
wholesea$X2 <- wholeseaY
rm(wholeseaX,wholeseaY)
rm(PSnodes_list)

rm(X,Y,Temp,h,Sal,siglay,u,v,w)

# also make one for the "whole sea" so it's easier to see   ((bigger version))
#======================================================
load("PSnodes_large2.RData", verbose=TRUE)

wholeseaX <- X[PSnodes_list]
wholeseaY <- Y[PSnodes_list]
length(wholeseaX)
wholesea <- data.frame(matrix(nrow=length(wholeseaX),ncol=2))
colnames(wholesea)
wholesea$X1 <- wholeseaX
wholesea$X2 <- wholeseaY
rm(wholeseaX,wholeseaY)
rm(PSnodes_list)

rm(X,Y,Temp,h,Sal,siglay,u,v,w,time)
save(wholesea,file="wholesea.Rdata")
#  13.2  Create node coordinates df (non-changing values)
#================================================================
PS.nodevals <- data.frame(matrix(nrow=nrow(PSX),ncol=4))
colnames(PS.nodevals) <- c("node","X","Y","h")
PS.nodevals$node <- PSnodes_list_currents
PS.nodevals$X <- PSX
PS.nodevals$Y <- PSY
PS.nodevals$h <- PSh
PSsiglay 


#  13.3  Create node temp, sal, velocities list (chainging variables)
#===============================================================
#  smaller array for values that change only
PSstart <- array(dim=c(nrow(PSX),5,10,24))   
# creates an array - 7963 rows (nodes), 2 columns (temp+sal), 10 pages (depths), 24 stacks (hours)
PSstart[,1,,] <-PSTemp
PSstart[,2,,] <- PSSal
PSstart[,3,,] <- PSu
PSstart[,4,,] <- PSv
PSstart[,5,,] <- PSw
PSstart[,,,1]
PStime
rm(PSh,PSSal,PSTemp,PSu,PSv,PSw,PSX,PSY,PStime,ncin)
dim(PSstart)

PStotal <- PSstart
rm(PSstart)
rm(PStotal)
load("JulyCurrents.Rdata")

#  Loop to pull .nc data for 4 days
#===========================================================
for(i in 186:195){     # start loop
  # open data
  ncin <- nc_open(paste("3DCurrents_ssm_00",i,".nc",sep=""))
  # extract variables 
  Temp <- ncvar_get(ncin,"temp")   
  Sal <- ncvar_get(ncin,"salinity")
  u <- ncvar_get(ncin,"u")
  v <- ncvar_get(ncin,"v")
  w <- ncvar_get(ncin,"w")
  
  # trim to PS
  PSTemp <- Temp[PSnodes_list_currents,,]
  PSSal <- Sal[PSnodes_list_currents,,]
  PSu <- u[PSnodes_list_currents,,]
  PSv <- v[PSnodes_list_currents,,]
  PSw <- w[PSnodes_list_currents,,]
  
  rm(Temp,Sal,u,v,w)
  
  # make array
  PS.array2 <- array(dim=c(nrow(PSTemp),5,10,24))   
  # creates an array - 7963 rows (nodes), 5 columns (vars), 10 pages (depths), 24 stacks (hours)
  PS.array2[,1,,] <-PSTemp
  PS.array2[,2,,] <- PSSal
  PS.array2[,3,,] <- PSu
  PS.array2[,4,,] <- PSv
  PS.array2[,5,,] <- PSw
  PS.array2[,,,1]
  rm(PSTemp,PSSal,PSu,PSv,PSw)
  
  # merge with existing arrays  
  PStotal <- abind(PStotal,PS.array2,rev.along=1)
  rm(PS.array2)
  print(i)
}
dim(PStotal)
rm(i,ncin)
#  this yields an array 8000 rows (nodes) x 5 cols (temp,sal,u,v,w) x 10 depths x 96 hours (4 days)


#  Make larva coordinate array  --- NO BEHAVIORS INCLUDED 
#========================================================================
# Make "restorationsites" df from code above.
# Probably will at some point just save this
head(restorationsites)


# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
larvatrack.3d <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
larvatrack.3d[,1,1] <- restorationsites$X 
larvatrack.3d[,2,1] <- restorationsites$Y
larvatrack.3d[,3,1] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.3d[,4,1] <- -PS.nodevals$h[larvatrack.3d[,3,1]]*.5 # this releases larvae at .75 the depth at which they were released. 
larvatrack.3d[,,1]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d)



# Fill array - move larvae with currents 
hour = 1
for(hour in 1:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d[,1,hour+1] <- mapply(sum,larvatrack.3d[,1,hour],PStotal[cbind(larvatrack.3d[,3,hour],3,closestlay,hour)]*3600) # move x at closest sigma layer
  larvatrack.3d[,2,hour+1] <- mapply(sum,larvatrack.3d[,2,hour],PStotal[cbind(larvatrack.3d[,3,hour],4,closestlay,hour)]*3600) # move y
  larvatrack.3d[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  larvatrack.3d[,4,hour+1] <- mapply(sum,larvatrack.3d[,4,hour], PStotal[cbind(lastnodes,5,closestlay,hour)]*3600) # move z
  rm(lastnodes,lastdepthprop,closestlay)
  print(hour)
  
}

head(restorationsites)
larvatrack.3d[1,3,1:120]

# change array to df
larvatrack.3d.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d)*length(larvatrack.3d[1,1,])),ncol=5))
colnames(larvatrack.3d.df) <- c("site","hour","X","Y","d")
larvatrack.3d.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d[1,1,]))
larvatrack.3d.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d))
larvatrack.3d.df$X <- c(t(larvatrack.3d[1:19,1,]))
larvatrack.3d.df$Y <- c(t(larvatrack.3d[1:19,2,]))
larvatrack.3d.df$d <- c(t(larvatrack.3d[1:19,4,]))
head(larvatrack.3d.df)

# plot tracks from sites 
trackplot1<- ggplot(data = wholesea,aes(x=X1,y=X2))+
  geom_point(color="slategray3",size=.3) + 
  geom_point(data=larvatrack.3d.df,aes(x=X,y=Y,group=site,color=site),size=.25)+
  geom_path(data=larvatrack.3d.df,aes(x=X,y=Y,group=site,color=site))

trackplot1
library(plotly)
ggplotly(trackplot1)
#  woohoooo!!! it works!
rm(larvatrack.3d,larvatrack.3d.df,trackplot1)

#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
#    ===   ===   ADD SWIMMING BEHAVIOR + GROW FUNCTION ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===  
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   === 



# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
release = 1
larvatrack.3d.swim <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.swim[,1,release] <- restorationsites$X 
larvatrack.3d.swim[,2,release] <- restorationsites$Y
larvatrack.3d.swim[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim[,1:2,1]),byid = TRUE),1,which.min)
larvatrack.3d.swim[,4,release] <- -PS.nodevals$h[larvatrack.3d.swim[,3,1]]*.5 # this releases larvae at .75 the depth at which they were released. 
#larvatrack.3d.swim[,4,release] <- -10 # release all at 10
dim(larvatrack.3d.swim)}
larvatrack.3d.swim[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 


larvatrack.3d.swim.sizes <- array(dim=c(nrow(restorationsites),length(PStotal[1,1,1,])+1))
dim(larvatrack.3d.swim.)

# Fill array - move larvae with currents 
  hour = release
  larvatrack.3d.swim.sizes[,hour] <- 180
  t(larvatrack.3d.swim.sizes[,750:768])

  hour=2
  #  Start Loop
#====================================
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes1 <- larvatrack.3d.swim[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop1 <- larvatrack.3d.swim[,4,hour] / PS.nodevals$h[lastnodes1]
  closestlay1 <- sapply(lastdepthprop1,function(x) which.min(abs(PSsiglay-x)))
  
  #grow larvae
  
  
  larvatrack.3d.swim.sizes[,hour+1] <- larvatrack.3d.swim.sizes[,hour] +  
                                        grow.larvae(ssm=PStotal)
  

  
  larvatrack.3d.swim[,1,hour+1] <- mapply(sum,larvatrack.3d.swim[,1,hour],PStotal[cbind(larvatrack.3d.swim[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.swim[,2,hour+1] <- mapply(sum,larvatrack.3d.swim[,2,hour],PStotal[cbind(larvatrack.3d.swim[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.swim[5,1,hour+1]<532153 &larvatrack.3d.swim[5,1,hour+1]>525030 & larvatrack.3d.swim[5,2,hour+1]<5367567 &larvatrack.3d.swim[5,2,hour+1]>5365890 ){
    larvatrack.3d.swim[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.swim[9,1,hour+1]>521956  & larvatrack.3d.swim[9,2,hour+1]>5336865 ){
    larvatrack.3d.swim[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.swim[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  

  
  larvatrack.3d.swim[,4,hour+1] <- mapply(sum,larvatrack.3d.swim[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + (.0012*3600))) # move z with up swim
  larvatrack.3d.swim[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.swim[,4,hour+1]) # stop z at surface
  larvatrack.3d.swim[,4,hour+1][  larvatrack.3d.swim[,4,hour+1] < (-PS.nodevals$h[larvatrack.3d.swim[,3,hour+1]]) ] <- 
    -PS.nodevals$h[larvatrack.3d.swim[,3,hour+1]][  larvatrack.3d.swim[,4,hour+1] < (-PS.nodevals$h[larvatrack.3d.swim[,3,hour+1]]) ]
   # ^stops z at bottom depth at closest node
  
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.swim[,3,hour+1]],nodepoints[larvatrack.3d.swim[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.swim[,3,hour+1]], SpatialPoints(larvatrack.3d.swim[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.swim[i,1:2,hour+1]
    } else {
  PS.nodevals[larvatrack.3d.swim[,3,hour+1][i],2:3]}
   }, seq_along( larvatrack.3d.swim[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.swim[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
                                            
  rm(lastnodes1,lastdepthprop1,closestlay1,noderadius,larvatonode,test,test2)
  print(hour)
  
}
  

  
# change array to df
larvatrack.3d.swim.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.swim)*length(larvatrack.3d.swim[1,1,])),ncol=7))
colnames(larvatrack.3d.swim.df) <- c("site","hour","X","Y","d","size","day")
larvatrack.3d.swim.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.swim[1,1,]))
larvatrack.3d.swim.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.swim))
larvatrack.3d.swim.df$X <- c(t(larvatrack.3d.swim[1:19,1,]))
larvatrack.3d.swim.df$Y <- c(t(larvatrack.3d.swim[1:19,2,]))
larvatrack.3d.swim.df$d <- c(t(larvatrack.3d.swim[1:19,4,]))
larvatrack.3d.swim.df$size <- as.vector(t(larvatrack.3d.swim.sizes))
larvatrack.3d.swim.df$day <- rep((rep(c(1:33),times=c(rep(24,times=32),1))),times=19)

head(larvatrack.3d.swim.df)  
  


larvatrack.3d.swim[,,2] - larvatrack.3d_2014_1[,,2]






# upload full set of node points
# plot tracks from sites 
trackplot.3d.swim.1 <- ggplot(data = PS.nodevals,aes(x=X,y=X))+
  #geom_point(aes(x=X,y=Y,color=PStotal[,1,1,1]),size=.35) + 
  #geom_point(aes(x=X,y=Y),color="slategray3",size=.6) + 
  geom_point(data=wholesea95,aes(x=X1,y=X2),color="slategray3",size=.3) +
 # geom_point(data=retorationsites,aes(x=X,y=Y))
  #geom_point(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red",size=.25)+
  #geom_path(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red")+
  #geom_point(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site),color="darkred",size=.25)+
  #geom_path(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site),color="darkred")+
  #geom_point(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site,color=site,size),size=.25)+
  geom_path(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site,color=site))+
#  geom_path(data=larvatrack.3d.swim.df[larvatrack.3d.swim.df$size>=280,],aes(x=X,y=Y,group=site,color=site),linetype="solid",size=.8)+
  #ggtitle("Larval Dispersal with Uniform Upward Swimming - 1 node")+
  theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())

trackplot.3d.swim.1 +
  geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

trackplot.3d.swim.1 <- ggplot(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site,color=site,frame=day,cumulative=TRUE),linetype="solid",size=.25)+
  geom_path()+
  geom_point(data=wholesea,aes(x=X1,y=X2),color="slategray3",size=.3) 
  
sea <- ggplot(data=wholesea,aes(x=X1,y=X2))+
  geom_point(color="slategray3",size=.3)

trackplot <- sea + geom_path(data=larvatrack.3d.swim.df,aes(x=X,y=Y,color=site))
gganimate()

ggplot(data=larvatrack.3d.swim.df,aes(frame=day,cumulative=T))+
  geom_point(data=wholesea,aes(x=X1,y=X2),color="slategray3",size=.3) +
  geom_path(data=larvatrack.3d.swim.df,aes(x=X,y=Y,color=site))


install.packages("gganimate")
library(gganimate)

gganimate(trackplot.3d.swim.1)


restorationsites
ggplotly(trackplot3d)
rm(larvatrack.3d.swim)


#==================================================
#   Add second release x hours later
#==================================================
release=8
larvatrack.3d.swim.2 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.swim.2[,1,release] <- restorationsites$X 
larvatrack.3d.swim.2[,2,release] <- restorationsites$Y
larvatrack.3d.swim.2[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim.2[,1:2,release]),byid = TRUE),1,which.min)
#larvatrack.3d.swim.2[,4,release] <- -PS.nodevals$h[larvatrack.3d.swim.2[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
larvatrack.3d.swim.2[,4,release] <- -10 # release all at 10
}
larvatrack.3d.swim.2[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.swim.2)


# Fill array - move larvae with currents 
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.swim.2[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.swim.2[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.swim.2[,1,hour+1] <- mapply(sum,larvatrack.3d.swim.2[,1,hour],PStotal[cbind(larvatrack.3d.swim.2[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.swim.2[,2,hour+1] <- mapply(sum,larvatrack.3d.swim.2[,2,hour],PStotal[cbind(larvatrack.3d.swim.2[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.swim.2[5,1,hour+1]<532153 & larvatrack.3d.swim.2[5,2,hour+1]<5367567 &larvatrack.3d.swim.2[5,2,hour+1]>5365890 ){
    larvatrack.3d.swim.2[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.swim.2[9,1,hour+1]>521956  & larvatrack.3d.swim.2[9,2,hour+1]>5336865 ){
    larvatrack.3d.swim.2[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.swim.2[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim.2[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.swim.2[,4,hour+1] <- mapply(sum,larvatrack.3d.swim.2[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + (.0012*3600))) # move z
  larvatrack.3d.swim.2[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.swim.2[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.swim.2[,3,hour+1]],nodepoints[larvatrack.3d.swim.2[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.swim.2[,3,hour+1]], SpatialPoints(larvatrack.3d.swim.2[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.swim.2[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.swim.2[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.swim.2[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.swim.2[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}

# change array to df
larvatrack.3d.swim.2.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.swim.2)*length(larvatrack.3d.swim.2[1,1,])),ncol=5))
colnames(larvatrack.3d.swim.2.df) <- c("site","hour","X","Y","d")
larvatrack.3d.swim.2.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.swim.2[1,1,]))
larvatrack.3d.swim.2.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.swim.2))
larvatrack.3d.swim.2.df$X <- c(t(larvatrack.3d.swim.2[1:19,1,]))
larvatrack.3d.swim.2.df$Y <- c(t(larvatrack.3d.swim.2[1:19,2,]))
larvatrack.3d.swim.2.df$d <- c(t(larvatrack.3d.swim.2[1:19,4,]))
head(larvatrack.3d.swim.2.df)



# plot tracks from sites 
trackplot.3d.swim.2 <- trackplot.3d.swim.1 +
  geom_path(data=larvatrack.3d.swim.2.df,aes(x=X,y=Y,group=site,color=site))

trackplot.3d.swim.2 + geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

rm(larvatrack.3d.swim.2)  

#==================================================
#  Add third release x hours later
#==================================================
release=16
larvatrack.3d.swim.3 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.swim.3[,1,release] <- restorationsites$X 
  larvatrack.3d.swim.3[,2,release] <- restorationsites$Y
  larvatrack.3d.swim.3[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim.3[,1:2,release]),byid = TRUE),1,which.min)
  #larvatrack.3d.swim.3[,4,release] <- -PS.nodevals$h[larvatrack.3d.swim.3[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
  larvatrack.3d.swim.3[,4,release] <- -10 # release all at 10
}
larvatrack.3d.swim.3[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.swim.3)




# Fill array - move larvae with currents 
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.swim.3[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.swim.3[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.swim.3[,1,hour+1] <- mapply(sum,larvatrack.3d.swim.3[,1,hour],PStotal[cbind(larvatrack.3d.swim.3[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.swim.3[,2,hour+1] <- mapply(sum,larvatrack.3d.swim.3[,2,hour],PStotal[cbind(larvatrack.3d.swim.3[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.swim.3[5,1,hour+1]<532153 & larvatrack.3d.swim.3[5,2,hour+1]<5367567 &larvatrack.3d.swim.3[5,2,hour+1]>5365890 ){
    larvatrack.3d.swim.3[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.swim.3[9,1,hour+1]>521956  & larvatrack.3d.swim.3[9,2,hour+1]>5336865 ){
    larvatrack.3d.swim.3[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
   larvatrack.3d.swim.3[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.swim.3[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.swim.3[,4,hour+1] <- mapply(sum,larvatrack.3d.swim.3[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + (.0012*3600))) # move z
  larvatrack.3d.swim.3[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.swim.3[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.swim.3[,3,hour+1]],nodepoints[larvatrack.3d.swim.3[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.swim.3[,3,hour+1]], SpatialPoints(larvatrack.3d.swim.3[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.swim.3[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.swim.3[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.swim.3[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.swim.3[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  
  
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}


# change array to df
larvatrack.3d.swim.3.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.swim.3)*length(larvatrack.3d.swim.3[1,1,])),ncol=5))
colnames(larvatrack.3d.swim.3.df) <- c("site","hour","X","Y","d")
larvatrack.3d.swim.3.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.swim.3[1,1,]))
larvatrack.3d.swim.3.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.swim.3))
larvatrack.3d.swim.3.df$X <- c(t(larvatrack.3d.swim.3[1:19,1,]))
larvatrack.3d.swim.3.df$Y <- c(t(larvatrack.3d.swim.3[1:19,2,]))
larvatrack.3d.swim.3.df$d <- c(t(larvatrack.3d.swim.3[1:19,4,]))
head(larvatrack.3d.swim.3.df)


# plot tracks from sites 
trackplot.3d.swim.3 <- trackplot.3d.swim.2 + 
  geom_path(data=larvatrack.3d.swim.3.df,aes(x=X,y=Y,group=site,color=site))
  
trackplot.3d.swim.3 +  geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

ggplotly(trackplot.3d.swim.3)

rm(larvatrack.3d.swim.3)


rm(larvatrack.3d.swim.df,larvatrack.3d.swim.3.df,larvatrack.3d.swim.2.df)

#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
#    ===   ===   ===   REPEAT WITH PHOTOTACTIC BEHAVIOR  ===   ===   ===     
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===  
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   === 


# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
larvatrack.3d.dvm <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
release = 1
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.dvm[,1,release] <- restorationsites$X 
larvatrack.3d.dvm[,2,release] <- restorationsites$Y
larvatrack.3d.dvm[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm[,1:2,release]),byid = TRUE),1,which.min)
#larvatrack.3d.dvm[,4,release] <- -PS.nodevals$h[larvatrack.3d.dvm[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
larvatrack.3d.dvm[,4,release] <- -10 # release all at 10
}
larvatrack.3d.dvm[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.dvm)



# Fill array - move larvae with currents 
time <- rep(c("N","N","N","N",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "N","N","N","N"),length.out=length(PStotal[1,1,1,]))


larvatrack.3d.dvm[,,1]
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.dvm[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.dvm[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.dvm[,1,hour+1] <- mapply(sum,larvatrack.3d.dvm[,1,hour],PStotal[cbind(larvatrack.3d.dvm[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.dvm[,2,hour+1] <- mapply(sum,larvatrack.3d.dvm[,2,hour],PStotal[cbind(larvatrack.3d.dvm[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.dvm[5,1,hour+1]<532153 &larvatrack.3d.dvm[5,1,hour+1]>525030 & larvatrack.3d.dvm[5,2,hour+1]<5367567 &larvatrack.3d.dvm[5,2,hour+1]>5365890 ){
    larvatrack.3d.dvm[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.dvm[9,1,hour+1]>521956  & larvatrack.3d.dvm[9,2,hour+1]>5336865 ){
    larvatrack.3d.dvm[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.dvm[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.dvm[,4,hour+1] <- mapply(sum,larvatrack.3d.dvm[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + 
                  if(time[hour]=="N") (-.0012*3600) else (.0012*3600))) # move z - up in day, down at night
  
  larvatrack.3d.dvm[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.dvm[,4,hour+1]) # stop z at surface

  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.dvm[,3,hour+1]],nodepoints[larvatrack.3d.dvm[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.dvm[,3,hour+1]], SpatialPoints(larvatrack.3d.dvm[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.dvm[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.dvm[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.dvm[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.dvm[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}


larvatrack.3d.dvm[,,]

min(larvatrack.3d.dvm[,4,])


# change array to df
larvatrack.3d.dvm.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.dvm)*length(larvatrack.3d.dvm[1,1,])),ncol=5))
colnames(larvatrack.3d.dvm.df) <- c("site","hour","X","Y","d")
larvatrack.3d.dvm.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.dvm[1,1,]))
larvatrack.3d.dvm.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.dvm))
larvatrack.3d.dvm.df$X <- c(t(larvatrack.3d.dvm[1:19,1,]))
larvatrack.3d.dvm.df$Y <- c(t(larvatrack.3d.dvm[1:19,2,]))
larvatrack.3d.dvm.df$d <- c(t(larvatrack.3d.dvm[1:19,4,]))
head(larvatrack.3d.dvm.df)

# upload full set of node points
# plot tracks from sites 
trackplot.3d.dvm.1 <- ggplot(data = PS.nodevals,aes(x=X,y=X))+
  #geom_point(aes(x=X,y=Y,color=PStotal[,1,1,1]),size=.35) + 
  #geom_point(aes(x=X,y=Y),color="slategray3",size=.6) + 
  geom_point(data=wholesea,aes(x=X1,y=X2),color="slategray3",size=.3) +
  # geom_point(data=retorationsites,aes(x=X,y=Y))
  #geom_point(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red",size=.25)+
  #geom_path(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red")+
  #geom_point(data=larvatrack.3d.dvm.df,aes(x=X,y=Y,group=site),color="darkred",size=.25)+
  #geom_path(data=larvatrack.3d.dvm.df,aes(x=X,y=Y,group=site),color="darkred")+
  geom_point(data=larvatrack.3d.dvm.df,aes(x=X,y=Y,group=site,color=site),size=.25)+
  geom_path(data=larvatrack.3d.dvm.df,aes(x=X,y=Y,group=site,color=site))+
  ggtitle("Larval Dispersal with Phototactic Swimming - 1 node")+
  theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())

trackplot.3d.dvm.1 +   geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

ggplotly(trackplot3d.dvm.1)
rm(larvatrack.3d.dvm)


#======================================================
#       Add second release at hour x
#======================================================

# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
larvatrack.3d.dvm.2 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
release=8
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.dvm.2[,1,release] <- restorationsites$X 
larvatrack.3d.dvm.2[,2,release] <- restorationsites$Y
larvatrack.3d.dvm.2[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm.2[,1:2,release]),byid = TRUE),1,which.min)
#larvatrack.3d.dvm.2[,4,release] <- -PS.nodevals$h[larvatrack.3d.dvm.2[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
larvatrack.3d.dvm.2[,4,release] <- -10 # release all at 10
}
larvatrack.3d.dvm.2[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.dvm.2)


# Fill array - move larvae with currents 
time <- rep(c("N","N","N","N",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "N","N","N","N"),length.out=length(PStotal[1,1,1,]))

dim(larvatrack.3d.dvm.2)
hour = release
larvatrack.3d.dvm.2[,,hour]


for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.dvm.2[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.dvm.2[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  

  larvatrack.3d.dvm.2[,1,hour+1] <- mapply(sum,larvatrack.3d.dvm.2[,1,hour],PStotal[cbind(larvatrack.3d.dvm.2[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.dvm.2[,2,hour+1] <- mapply(sum,larvatrack.3d.dvm.2[,2,hour],PStotal[cbind(larvatrack.3d.dvm.2[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.dvm.2[5,1,hour+1]<532153 & larvatrack.3d.dvm.2[5,1,hour+1]>525030 & larvatrack.3d.dvm.2[5,2,hour+1]<5367567 &larvatrack.3d.dvm.2[5,2,hour+1]>5365890 ){
    larvatrack.3d.dvm.2[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.dvm.2[9,1,hour+1]>521956  & larvatrack.3d.dvm.2[9,2,hour+1]>5336865 ){
    larvatrack.3d.dvm.2[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.dvm.2[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm.2[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.dvm.2[,4,hour+1] <- mapply(sum,larvatrack.3d.dvm.2[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + 
                                                                            if(time[hour]=="N") (-.0012*3600) else (.0012*3600))) # move z - up in day, down at night
  
  larvatrack.3d.dvm.2[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.dvm.2[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.dvm.2[,3,hour+1]],nodepoints[larvatrack.3d.dvm.2[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.dvm.2[,3,hour+1]], SpatialPoints(larvatrack.3d.dvm.2[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if((noderadius[i]) >= larvatonode[i]){
      larvatrack.3d.dvm.2[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.dvm.2[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.dvm.2[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.dvm.2[,1:2,hour+1] <- test2     
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}

larvatrack.3d.dvm.2[,,]


# change array to df
larvatrack.3d.dvm.2.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.dvm.2)*length(larvatrack.3d.dvm.2[1,1,])),ncol=5))
colnames(larvatrack.3d.dvm.2.df) <- c("site","hour","X","Y","d")
larvatrack.3d.dvm.2.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.dvm.2[1,1,]))
larvatrack.3d.dvm.2.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.dvm.2))
larvatrack.3d.dvm.2.df$X <- c(t(larvatrack.3d.dvm.2[1:19,1,]))
larvatrack.3d.dvm.2.df$Y <- c(t(larvatrack.3d.dvm.2[1:19,2,]))
larvatrack.3d.dvm.2.df$d <- c(t(larvatrack.3d.dvm.2[1:19,4,]))
head(larvatrack.3d.dvm.2.df)
rm(larvatrack.3d.dvm.2)

# upload full set of node points
# plot tracks from sites 
trackplot.3d.dvm.2 <- trackplot.3d.dvm.1 +
  geom_point(data=larvatrack.3d.dvm.2.df,aes(x=X,y=Y,group=site,color=site),size=.25)+
  geom_path(data=larvatrack.3d.dvm.2.df,aes(x=X,y=Y,group=site,color=site))

trackplot.3d.dvm.2 +   geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

ggplotly(trackplot.3d.dvm.2)

#======================================================
#      Add third release
#======================================================
release = 16
# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
larvatrack.3d.dvm.3 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.dvm.3[,1,release] <- restorationsites$X 
larvatrack.3d.dvm.3[,2,release] <- restorationsites$Y
larvatrack.3d.dvm.3[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm.3[,1:2,release]),byid = TRUE),1,which.min)
#larvatrack.3d.dvm.3[,4,release] <- -PS.nodevals$h[larvatrack.3d.dvm.3[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
larvatrack.3d.dvm.3[,4,release] <- -10 # release all at 10
}
larvatrack.3d.dvm.3[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.dvm.3)



# Fill array - move larvae with currents 
time <- rep(c("N","N","N","N",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "N","N","N","N"),length.out=length(PStotal[1,1,1,]))
# creates an array for time, so the first 4 hours are "night" (1am-4am)
# then 16 hours of "day" (5am-8pm), then 4 night (9pm12am) 

hour=release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.dvm.3[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.dvm.3[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.dvm.3[,1,hour+1] <- mapply(sum,larvatrack.3d.dvm.3[,1,hour],PStotal[cbind(larvatrack.3d.dvm.3[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.dvm.3[,2,hour+1] <- mapply(sum,larvatrack.3d.dvm.3[,2,hour],PStotal[cbind(larvatrack.3d.dvm.3[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.dvm.3[5,1,hour+1]<532153 & larvatrack.3d.dvm.3[5,1,hour+1]>525030 & larvatrack.3d.dvm.3[5,2,hour+1]<5367567 &larvatrack.3d.dvm.3[5,2,hour+1]>5365890 ){
    larvatrack.3d.dvm.3[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.dvm.3[9,1,hour+1]>521956  & larvatrack.3d.dvm.3[9,2,hour+1]>5336865 ){
    larvatrack.3d.dvm.3[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.dvm.3[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.dvm.3[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.dvm.3[,4,hour+1] <- mapply(sum,larvatrack.3d.dvm.3[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600 + 
                                                                                  if(time[hour]=="N") (-.0012*3600) else (.0012*3600))) # move z - up in day, down at night
  
  larvatrack.3d.dvm.3[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.dvm.3[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.dvm.3[,3,hour+1]],nodepoints[larvatrack.3d.dvm.3[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.dvm.3[,3,hour+1]], SpatialPoints(larvatrack.3d.dvm.3[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.dvm.3[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.dvm.3[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.dvm.3[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.dvm.3[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}


# change array to df
larvatrack.3d.dvm.3.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.dvm.3)*length(larvatrack.3d.dvm.3[1,1,])),ncol=5))
colnames(larvatrack.3d.dvm.3.df) <- c("site","hour","X","Y","d")
larvatrack.3d.dvm.3.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.dvm.3[1,1,]))
larvatrack.3d.dvm.3.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.dvm.3))
larvatrack.3d.dvm.3.df$X <- c(t(larvatrack.3d.dvm.3[1:19,1,]))
larvatrack.3d.dvm.3.df$Y <- c(t(larvatrack.3d.dvm.3[1:19,2,]))
larvatrack.3d.dvm.3.df$d <- c(t(larvatrack.3d.dvm.3[1:19,4,]))
head(larvatrack.3d.dvm.3.df)


# upload full set of node points
# plot tracks from sites 
trackplot.3d.dvm.3 <- trackplot.3d.dvm.2 +
  geom_point(data=larvatrack.3d.dvm.3.df,aes(x=X,y=Y,group=site,color=site),size=.25)+ # release 3
  geom_path(data=larvatrack.3d.dvm.3.df,aes(x=X,y=Y,group=site,color=site))

trackplot.3d.dvm.3 +geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)

ggplotly(trackplot.3d.dvm.3)
rm(larvatrack.3d.dvm.3)




#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
#    ===   ===   ===   DO WITHOUT SWIMMING BEHAVIOR ===   ===   ===   ===    
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===  
#    ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   
# ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   ===   === 



# Create array to fill with larva coordinates 
# to do this, we'll need rgeos package
nodepoints <- SpatialPoints(PS.nodevals[,2:3])
# back to array
release = 1
larvatrack.3d.noswim <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
larvatrack.3d.noswim[,1,release] <- restorationsites$X 
  larvatrack.3d.noswim[,2,release] <- restorationsites$Y
  larvatrack.3d.noswim[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim[,1:2,1]),byid = TRUE),1,which.min)
  #larvatrack.3d.noswim[,4,release] <- -PS.nodevals$h[larvatrack.3d.noswim[,3,1]]*.5 # this releases larvae at .5 the depth at which they were released. 
  larvatrack.3d.noswim[,4,release] <- -10 # release all at 10
   dim(larvatrack.3d.noswim)
larvatrack.3d.noswim[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 

restorationsites$Site

# Fill array - move larvae with currents 
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.noswim[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.noswim[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.noswim[,1,hour+1] <- mapply(sum,larvatrack.3d.noswim[,1,hour],PStotal[cbind(larvatrack.3d.noswim[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.noswim[,2,hour+1] <- mapply(sum,larvatrack.3d.noswim[,2,hour],PStotal[cbind(larvatrack.3d.noswim[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.noswim[5,1,hour+1]<532153 & larvatrack.3d.noswim[5,1,hour+1]>525030 & larvatrack.3d.noswim[5,2,hour+1]<5367567 &larvatrack.3d.noswim[5,2,hour+1]>5365890 ){
    larvatrack.3d.noswim[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.noswim[9,1,hour+1]>521956  & larvatrack.3d.noswim[9,2,hour+1]>5336865 ){
    larvatrack.3d.noswim[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  
  larvatrack.3d.noswim[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.noswim[,4,hour+1] <- mapply(sum,larvatrack.3d.noswim[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600)) # move z
  larvatrack.3d.noswim[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.noswim[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.noswim[,3,hour+1]],nodepoints[larvatrack.3d.noswim[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.noswim[,3,hour+1]], SpatialPoints(larvatrack.3d.noswim[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.noswim[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.noswim[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.noswim[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.noswim[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}

dim(larvatrack.3d.noswim)
larvatrack.3d.noswim[,,2]

# change array to df
larvatrack.3d.noswim.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.noswim)*length(larvatrack.3d.noswim[1,1,])),ncol=5))
colnames(larvatrack.3d.noswim.df) <- c("site","hour","X","Y","d")
larvatrack.3d.noswim.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.noswim[1,1,]))
larvatrack.3d.noswim.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.noswim))
larvatrack.3d.noswim.df$X <- c(t(larvatrack.3d.noswim[1:19,1,]))
larvatrack.3d.noswim.df$Y <- c(t(larvatrack.3d.noswim[1:19,2,]))
larvatrack.3d.noswim.df$d <- c(t(larvatrack.3d.noswim[1:19,4,]))
head(larvatrack.3d.noswim.df)



# upload full set of node points
# plot tracks from sites 
trackplot.3d.noswim.1 <- ggplot(data = PS.nodevals,aes(x=X,y=X))+
  #geom_point(aes(x=X,y=Y,color=PStotal[,1,1,1]),size=.35) + 
  #geom_point(aes(x=X,y=Y),color="slategray3",size=.6) + 
  geom_point(data=wholesea,aes(x=X1,y=X2),color="slategray3",size=.3) +
  # geom_point(data=retorationsites,aes(x=X,y=Y))
  #geom_point(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red",size=.25)+
  #geom_path(data=larvatrack.3d.df,aes(x=X,y=Y,group=site),color="red")+
  #geom_point(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site),color="darkred",size=.25)+
  #geom_path(data=larvatrack.3d.swim.df,aes(x=X,y=Y,group=site),color="darkred")+
  geom_point(data=larvatrack.3d.noswim.df,aes(x=X,y=Y,group=site,color=site),size=.25)+
  geom_path(data=larvatrack.3d.noswim.df,aes(x=X,y=Y,group=site,color=site))+
  ggtitle("Larval Dispersal, Neutrally Bouyant with -10m Start")+
  theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())

trackplot.3d.noswim.1 +
  geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)


ggplotly(trackplot3d)
rm(larvatrack.3d.noswim)


#==================================================
#  Add second release x hours later without errors
#==================================================
release=8
larvatrack.3d.noswim.2 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.noswim.2[,1,release] <- restorationsites$X 
  larvatrack.3d.noswim.2[,2,release] <- restorationsites$Y
  larvatrack.3d.noswim.2[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim.2[,1:2,release]),byid = TRUE),1,which.min)
  #larvatrack.3d.noswim.2[,4,release] <- -PS.nodevals$h[larvatrack.3d.noswim.2[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
  larvatrack.3d.noswim.2[,4,release] <- -10 # release all at 10
}
larvatrack.3d.noswim.2[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.noswim.2)




# Fill array - move larvae with currents 
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.noswim.2[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.noswim.2[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.noswim.2[,1,hour+1] <- mapply(sum,larvatrack.3d.noswim.2[,1,hour],PStotal[cbind(larvatrack.3d.noswim.2[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.noswim.2[,2,hour+1] <- mapply(sum,larvatrack.3d.noswim.2[,2,hour],PStotal[cbind(larvatrack.3d.noswim.2[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.noswim.2[5,1,hour+1]<532153 & larvatrack.3d.noswim.2[5,1,hour+1]>525030 & larvatrack.3d.noswim.2[5,2,hour+1]<5367567 &larvatrack.3d.noswim.2[5,2,hour+1]>5365890 ){
    larvatrack.3d.noswim.2[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.noswim.2[9,1,hour+1]>521956  & larvatrack.3d.noswim.2[9,2,hour+1]>5336865 ){
    larvatrack.3d.noswim.2[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.noswim.2[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim.2[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.noswim.2[,4,hour+1] <- mapply(sum,larvatrack.3d.noswim.2[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600)) # move z
  larvatrack.3d.noswim.2[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.noswim.2[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.noswim.2[,3,hour+1]],nodepoints[larvatrack.3d.noswim.2[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.noswim.2[,3,hour+1]], SpatialPoints(larvatrack.3d.noswim.2[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.noswim.2[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.noswim.2[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.noswim.2[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.noswim.2[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  
  
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}
dim(larvatrack.3d.noswim.2)
larvatrack.3d.noswim.2[,4,265]


# change array to df
larvatrack.3d.noswim.2.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.noswim.2)*length(larvatrack.3d.noswim.2[1,1,])),ncol=5))
colnames(larvatrack.3d.noswim.2.df) <- c("site","hour","X","Y","d")
larvatrack.3d.noswim.2.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.noswim.2[1,1,]))
larvatrack.3d.noswim.2.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.noswim.2))
larvatrack.3d.noswim.2.df$X <- c(t(larvatrack.3d.noswim.2[1:19,1,]))
larvatrack.3d.noswim.2.df$Y <- c(t(larvatrack.3d.noswim.2[1:19,2,]))
larvatrack.3d.noswim.2.df$d <- c(t(larvatrack.3d.noswim.2[1:19,4,]))
head(larvatrack.3d.noswim.2.df)
dim(larvatrack.3d.noswim.2.df)

# plot tracks from sites 
trackplot.3d.noswim.2 <- trackplot.3d.noswim.1 + 
  geom_path(data=larvatrack.3d.noswim.2.df,aes(x=X,y=Y,group=site,color=site))

trackplot.3d.noswim.2 +  geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)


#==================================================
#  Add third release x hours later
#==================================================
release=16
larvatrack.3d.noswim.3 <- array(dim=c(nrow(restorationsites),4,length(PStotal[1,1,1,])+1))  
# creates array with 19 larvae, 3 cols (X,Y,depth,Closestnode), 288 pages (hours)
{larvatrack.3d.noswim.3[,1,release] <- restorationsites$X 
  larvatrack.3d.noswim.3[,2,release] <- restorationsites$Y
  larvatrack.3d.noswim.3[,3,release] <-apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim.3[,1:2,release]),byid = TRUE),1,which.min)
  #larvatrack.3d.noswim.3[,4,release] <- -PS.nodevals$h[larvatrack.3d.noswim.3[,3,release]]*.5 # this releases larvae at .75 the depth at which they were released. 
  larvatrack.3d.noswim.3[,4,release] <- -10 # release all at 10
}
larvatrack.3d.noswim.3[,,release]  # this creates first page of array with larva coordinates that I picked at sites. 
dim(larvatrack.3d.noswim.3)




# Fill array - move larvae with currents 
hour = release
for(hour in release:length(PStotal[1,1,1,])){
  lastnodes <- larvatrack.3d.noswim.3[,3,hour] ## but the 1 will turn to "hour"
  lastdepthprop <- larvatrack.3d.noswim.3[,4,hour] / PS.nodevals$h[lastnodes]
  closestlay <- sapply(lastdepthprop,function(x) which.min(abs(PSsiglay-x)))
  
  
  larvatrack.3d.noswim.3[,1,hour+1] <- mapply(sum,larvatrack.3d.noswim.3[,1,hour],PStotal[cbind(larvatrack.3d.noswim.3[,3,hour],3,closestlay,hour)]*3600) # move x
  larvatrack.3d.noswim.3[,2,hour+1] <- mapply(sum,larvatrack.3d.noswim.3[,2,hour],PStotal[cbind(larvatrack.3d.noswim.3[,3,hour],4,closestlay,hour)]*3600) # move y
  if(larvatrack.3d.noswim.3[5,1,hour+1]<532153 & larvatrack.3d.noswim.3[5,1,hour+1]>525030 & larvatrack.3d.noswim.3[5,2,hour+1]<5367567 &larvatrack.3d.noswim.3[5,2,hour+1]>5365890 ){
    larvatrack.3d.noswim.3[5,1:2,hour+1] <- c(531454.5,5367567)
  }  # keeps fidalgo bay larva from jumping over land
  if(larvatrack.3d.noswim.3[9,1,hour+1]>521956  & larvatrack.3d.noswim.3[9,2,hour+1]>5336865 ){
    larvatrack.3d.noswim.3[9,1:2,hour+1] <- c(522464,5336598)
  }  # keeps kilisut harbor larva from jumping over land
  larvatrack.3d.noswim.3[,3,hour+1] <- apply(gDistance(nodepoints,SpatialPoints(larvatrack.3d.noswim.3[,1:2,hour+1]),byid=TRUE),1,which.min) # find new closest node
  
  
  larvatrack.3d.noswim.3[,4,hour+1] <- mapply(sum,larvatrack.3d.noswim.3[,4,hour], (PStotal[cbind(lastnodes,5,closestlay,hour)]*3600)) # move z
  larvatrack.3d.noswim.3[,4,hour+1] <- mapply(function(x) if (x>0) 0 else x, larvatrack.3d.noswim.3[,4,hour+1]) # stop z at surface
  
  noderadius <- as.vector( apply ( gDistance ( nodepoints[-larvatrack.3d.noswim.3[,3,hour+1]],nodepoints[larvatrack.3d.noswim.3[,3,hour+1]],byid = TRUE),1,min)) 
  # ^ finds distance between node and next closest node
  larvatonode <- as.vector(apply ( gDistance ( nodepoints[larvatrack.3d.noswim.3[,3,hour+1]], SpatialPoints(larvatrack.3d.noswim.3[,1:2,hour+1]),byid=TRUE),1,min))
  # ^ finds distance between larva and node
  test<-as.array(t(mapply(function(i) {
    if(noderadius[i] >= larvatonode[i]){
      larvatrack.3d.noswim.3[i,1:2,hour+1]
    } else {
      PS.nodevals[larvatrack.3d.noswim.3[,3,hour+1][i],2:3]}
  }, seq_along( larvatrack.3d.noswim.3[,3,hour+1]))))
  test2 <- as.matrix(matrix(nrow=19,ncol=2))
  test2[,1] <- as.numeric(test[,1])
  test2[,2] <- as.numeric(test[,2])
  larvatrack.3d.noswim.3[,1:2,hour+1] <- test2    
  # ^ this takes larvae that are further from the nearest node 
  #   than that node is to its nearest neighbor, and snaps it back
  #   to nearest node point. This is so that larvae aren't 
  #   dispersing out of the boundaries of the water. 
  
  
  
  
  rm(lastnodes,lastdepthprop,closestlay,noderadius,larvatonode,test,test2)
  print(hour)
  
}
dim(larvatrack.3d.noswim.3)
larvatrack.3d.noswim.3[,4,265]


# change array to df
larvatrack.3d.noswim.3.df <- data.frame(matrix(nrow=(nrow(larvatrack.3d.noswim.3)*length(larvatrack.3d.noswim.3[1,1,])),ncol=5))
colnames(larvatrack.3d.noswim.3.df) <- c("site","hour","X","Y","d")
larvatrack.3d.noswim.3.df$site <- rep(restorationsites$Site,each=length(larvatrack.3d.noswim.3[1,1,]))
larvatrack.3d.noswim.3.df$hour <- rep(1:(length(PStotal[1,1,1,])+1),times=nrow(larvatrack.3d.noswim.3))
larvatrack.3d.noswim.3.df$X <- c(t(larvatrack.3d.noswim.3[1:19,1,]))
larvatrack.3d.noswim.3.df$Y <- c(t(larvatrack.3d.noswim.3[1:19,2,]))
larvatrack.3d.noswim.3.df$d <- c(t(larvatrack.3d.noswim.3[1:19,4,]))
head(larvatrack.3d.noswim.3.df)
dim(larvatrack.3d.noswim.3.df)

# plot tracks from sites 
trackplot.3d.noswim.3 <- trackplot.3d.noswim.2 + 
  geom_path(data=larvatrack.3d.noswim.3.df,aes(x=X,y=Y,group=site,color=site))

trackplot.3d.noswim.3 +  geom_text_repel(data=restorationsites,aes(x=X,y=Y,label=Site),size=3,hjust=0,vjust=0,)


rm(larvatrack.3d.noswim.3)

larvatrack.3d.noswim[,,101] -larvatrack.3d.noswim[,,108]
larvatrack.3d.noswim.2[,,8]
rm(larvatrack.3d.noswim.df,larvatrack.3d.noswim.3.df,larvatrack.3d.noswim.2.df)
as.vector(restorationsites$Site)
#[1] "Drayton Harbor"    "Bellingham Bay"    "Samish Bay"        "Padilla Bay"       "Fidalgo Bay"      
#[6] "Similk Bay"        "Sequim Bay"        "Discovery Bay"     "Kilisut Harbor"    "Quilcene Bay"     
#[11] "Port Gamble Bay"   "Liberty Bay"       "Squaxin Island"    "Port Orchard Pass" "Dyes Inlet"       






## Try dividing into Basins #####




#[16] "Sinclair Inlet"    "Union River"       "Henderson Bay"     "Budd Inlet"    
