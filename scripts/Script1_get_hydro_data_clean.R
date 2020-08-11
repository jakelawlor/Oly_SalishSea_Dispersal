# Script 1. 
# get NedCDF Salish Sea Model data and convert to array


# install libraries
#--------------------------
library(ncdf4)
library(dplyr)
library(here)
library(ggplot2)
library(abind)
#----



# start with just one day of data
#------------------------------------

day1 <- nc_open(here("data_raw","SSM2095","3DCurrents_ssm_00185.nc"))

# get info 
print(day1)

# separate individual variables
# get name of variable id
X <- ncvar_get(day1,"X")
length(X)
Y <- ncvar_get(day1,"Y")
Temp <- ncvar_get(day1,"temp")   
dim(Temp) ### this shows me that "Temp" object is an array - 16012 rows (one per node), 10 cols (depths), 24 pages (time)
h <- ncvar_get(day1,"h")
u <- ncvar_get(day1,"u") # n/s velocity
v <- ncvar_get(day1,"v") # e/w velocity
w <- ncvar_get(day1,"w") # up/down velocity
Sal <- ncvar_get(day1,"salinity")
siglay <- ncvar_get(day1,"siglay")
time <- ncvar_get(day1,"time")



# merge back into dataframe and view
day1_nodes <- data.frame(matrix(nrow=16012,ncol = 5))
colnames(day1_nodes)<-c ("node","X","Y","Temp","Sal")
day1_nodes$node <- day1$dim$node$vals
day1_nodes$X <- X
day1_nodes$Y <- Y
day1_nodes$Temp <-Temp[,1,1] #this gives just surface temp in the first hour
day1_nodes$Sal <- Sal[,1,1] # surface sal in the first hour
day1_nodes$h_smooth <- h
day1_nodes %>% head()

# save out siglay as these will always be constant
save(siglay,file = here("data_processed","siglay.Rdata"))
#-----




# filter to washington nodes only and save
#-------------------------------
# plot, then cut node list to only washington
day1_nodes %>%
  ggplot(aes(x=X,y=Y,color=Temp)) +
  geom_point() +
  coord_equal()

# now try filtered
day1_nodes %>%
  filter((X > 280000 & Y >5200000 & Y < 5450000 ),
         !(X<450000 & Y < 5250000)) %>%
  ggplot(aes(x=X,y=Y,color=Temp)) +
  geom_point(size=.5) +
  coord_equal()

# get node numbers of washington nodes
WA_nodes_list <- day1_nodes %>%
  filter((X > 280000 & Y >5200000 & Y < 5450000 ),
         !(X<450000 & Y < 5250000)) %>%
  pull(node)



# now try filtered
day1_nodes[WA_nodes_list,] %>%
  ggplot(aes(x=X,y=Y,color=Temp)) +
  geom_point(size=.5) +
  coord_equal()

length(WA_nodes_list)

# save as r object
save(WA_nodes_list,
     file = here("data_processed","WA_nodes_list.Rdata"))
#----


# make coords df
#------------------------------------
WA_nodes_coords <- 
  data.frame(node = WA_nodes_list,
             x = X[WA_nodes_list],
             y = Y[WA_nodes_list],
             h_smooth = h[WA_nodes_list]) 

h_true[WA_nodes_list,] %>% 
  ggplot(aes(x=x,y=y,color=h_true)) +
  geom_point()

WA_nodes_coords %>%
  ggplot(aes(x=x,y=y,color=h_smooth))+
  geom_point(size=.6) 


WA_nodes_coords %>% dim()

# save as r object
save(WA_nodes_coords,
     file = here("data_processed","WA_nodes_coords.Rdata"))

#rm(WA_nodes_coords,WA_nodes_list,h,
#   Sal, Temp, u,v,w,time,siglay,X,Y,day1,day1_nodes)
#----





# add true height
#------------------------------
load(here("data_processed","WA_nodes_coords.Rdata"))
load(here("data_processed","WA_nodes_list.Rdata"))


# upload true depths and attach
h_true <- read.table(here("data_raw","+SSM_Original_Bathy_UTM_Zone10_NAVD88.txt"),
                     header = T)
colnames(h_true) <- c("x","y","h_true")

nrow(h_true)

h_true[WA_nodes_list,3] %>% 
  cbind(WA_nodes_coords) %>%
  mutate(diff = abs(h_smooth - .)) %>%
  mutate(bigdiff = case_when(diff > 100 ~ "yes")) %>%
  ggplot(aes(x=x,y=y,color=.)) +
  geom_point()



# so it seems like the h_true coords are very slightly different than
# the SSM node coords, and that specifically 2 nodes are in a different order. 


# for now, we're just gonna save wa_nodes_coords binded to the filtered h_true.


WA_nodes_coords <- WA_nodes_coords %>% cbind(h_true = h_true[WA_nodes_list,3])

save(WA_nodes_coords,file=here("data_processed","WA_nodes_coords.Rdata"))
#----

# here is some script of me trying to figure out the differences between h_true coords and wa nodes coords
#---------------
WA_nodes_list == WA_nodes_list2

unique(WA_nodes_list[! WA_nodes_list %in% WA_nodes_list2])
unique(WA_nodes_list2[! WA_nodes_list2 %in% WA_nodes_list])

day1_nodes[c(5764,5879,5695,5810),]
h_true[c(5764,5879,5695,5810),]

length(WA_nodes_list)


day1_nodes %>% 
  mutate(color = case_when(node %in% c(5695, 5810) ~ "day1",
                           node %in% c(5764, 5879) ~ "h_true",
                           TRUE ~ "normal")) %>%
  ggplot(aes(x=X,y=Y,color=color,alpha=color)) +
  geom_point() +
  scale_alpha_manual(breaks = c("day1","h_true","normal"),
                     values = c(1,1,.01))


# see how far coords are off
day1_nodes %>% 
  cbind(x2 = h_true[,1]) %>%
  mutate(xdiff = X-x2) %>%
  mutate(bigxdiff = case_when(xdiff > 6000 ~ "true",
                              TRUE ~ "false")) %>%
  ggplot(aes(x=X,y=Y,color=bigxdiff,alpha=bigxdiff)) +
  geom_point(size=.7) +
  scale_alpha_manual(breaks = c("true","false"),values=c(1,.2))

# 
day1_nodes %>% 
  cbind(h_true = h_true[,1:3]) %>%
  mutate(diff = X - h_true.x) %>% arrange(desc(diff)) %>% head()
  mutate(xdiff = X-x2) %>%
  mutate(bigxdiff = case_when(xdiff > 6000 ~ "true",
                              TRUE ~ "false")) %>%
  ggplot(aes(x=X,y=Y,color=bigxdiff,alpha=bigxdiff)) +
  geom_point(size=.7) +
  scale_alpha_manual(breaks = c("true","false"),values=c(1,.2))


day1_nodes %>% 
  cbind(h_true = h_true[,3]) %>%
  mutate(h_diff = h_smooth-h_true) %>%
  mutate(bigh_diff = case_when(h_diff > 200 ~ "true",
                              TRUE ~ "false")) %>%
  ggplot(aes(x=X,y=Y,color=bigh_diff)) +
  geom_point() 


  

WA_nodes_coords <- 
  h_true[WA_nodes_list,] %>% 
  right_join(WA_nodes_coords,by=c("x","y")) 

WA_nodes_coords %>%
  mutate(diff = h_true - h_smooth)  %>%
  ggplot(aes(x=x,y=y,color=diff))+
  geom_point(size=1) +
  theme_void() +
  scale_color_gradient(low="red",high="turquoise")
#----


# subset day1 to just washington nodes
#-------------------------
load( here("data_processed","WA_nodes_list.Rdata"))
load( here("data_processed","WA_nodes_coords.Rdata"))

# make a smaller array for values that change only
day1_changing <- array(dim=c(length(WA_nodes_list),5,10,24))  

# creates an array - 7963 rows (nodes), 2 columns (temp+sal), 10 pages (depths), 24 stacks (hours)
day1_changing[,1,,] <-Temp[WA_nodes_list,,] #temps
day1_changing[,2,,] <- Sal[WA_nodes_list,,] # sals
day1_changing[,3,,] <- u[WA_nodes_list,,] # 
day1_changing[,4,,] <- v[WA_nodes_list,,]
day1_changing[,5,,] <- w[WA_nodes_list,,]



# 8490 rows (nodes), 
# 5 columns (temp, sal, x,y,z velocities), 
# 10 sigma layers, 
# 24 hours
i <- 215
# view first hour of array
day1_changing[,,,1] 
#-----

currentslist <- list()
# put in a loop to extract all summer days
#--------------------------------------------
# create what will become the full array (WA_currents), and 
# start it out with values of day 1, because we will bind the rest together after that
WA_currents <- day1_changing
dim(WA_currents)

rm(Temp,Sal,u,v,w,h,time)
# import, bind, and sort in a loop
for(i in 185:234){     # start loop
  
  # import data
#{
  dayx <- nc_open(  here("data_raw","SSM2014",paste("3DCurrents_ssm_00",i,".nc",sep="")) )
 
   # extract variables 
  Temp <- ncvar_get(dayx,"temp")   
  Sal <- ncvar_get(dayx,"salinity")
  u <- ncvar_get(dayx,"u")
  v <- ncvar_get(dayx,"v")
  w <- ncvar_get(dayx,"w")
  
  # make array
  dayx_array <- array(dim=c(length(WA_nodes_list),5,10,24))   
  # creates an array - 7963 rows (nodes), 5 columns (vars), 10 pages (depths), 24 stacks (hours)
  dayx_array[,1,,] <- Temp[WA_nodes_list,,]
  dayx_array[,2,,] <- Sal[WA_nodes_list,,]
  dayx_array[,3,,] <- u[WA_nodes_list,,]
  dayx_array[,4,,] <- v[WA_nodes_list,,]
  dayx_array[,5,,] <- w[WA_nodes_list,,]

  # merge with existing arrays  
  # save objects in a list
  currentslist[[i]] <- dayx_array
#  WA_currents <- abind(WA_currents,dayx_array,rev.along=1)
  # rm(Temp,Sal,u,v,w,dayx_array,dayx)
 # gc()
  print(i)
#}
}


WA_currents_14 <- do.call(abind,currentslist)
rm(currentslist)
dim(WA_currents_14)
rm(h,i,siglay,time,X,Y,u,v,w)
WA_currents_14[100,2,1,] %>% plot(type="l")
gc()
save(WA_currents_14,file=here::here("data_processed","WA_currents_14.Rdata"))
#----
rm(WA_currents_14)

# save object as 14 to distinguish it
#-------------------------
WA_currents_95 <- WA_currents

save(WA_currents_95, 
     file = here("data_processed","WA_currents_95.Rdata"))
#----


# repeat this whole process to extract 2014 and 2095 zeta values.
#----------------------------
load( here("data_processed","WA_nodes_list.Rdata"))

# get first day
day1 <- nc_open(here("data_raw","SSM2095","3DCurrents_ssm_00185.nc"))
# extract zeta
day1_zeta <- ncvar_get(day1,"zeta")[WA_nodes_list,]



# assin
zeta_95 <- day1_zeta


for(i in 186:234){
  # get zeta vals
  dayx <- nc_open(  here("data_raw","SSM2095",paste("3DCurrents_ssm_00",i,".nc",sep="")) )
  zetax <- ncvar_get(dayx,"zeta")[WA_nodes_list,]


  # merge with existing arrays  
  zeta_95 <- abind(zeta_95,zetax,rev.along=1)
  rm(zetax,dayx)
  print(i)
  
}
#----


# save zetas
#----------------------------
dim(zeta_95)
save(zeta_95,file = here("data_processed","zeta_95.Rdata"))
#----


