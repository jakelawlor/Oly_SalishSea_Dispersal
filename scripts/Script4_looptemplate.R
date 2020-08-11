# Script 4. particle dispersal template





#---------------------------------------------------
####           ANNOTATED   TEMPLATE           #####
#---------------------------------------------------




############ ** INPUTS ###############
#----------------------------------
{release=2                       # Hour of release
nodes=3                         # Nodes to call from, 1, 2, or 3
year = 14                       # year, 14 or 95
release_size <- 180             # Start size for larvae
behavior = "onto"               # behavior for larvae. "up","none","photo","onto"
hour=release                    # 
name = paste0("_",year,"_",     # creates nane with variables from above
              release,"_",      # name format is _year_release_behavior_nodes
              behavior,"_",     #
              nodes,"n")        # 
print(name)}
#-------------------------------------



############# Load Objects ##############
#----
load(here("data_processed",paste0("WA_currents_",year,".Rdata")))
load(here("data_processed",paste0("zeta_",year,".Rdata")))
#----


WA_currents_14[1:10,,1,1]
########### Make Blank Array ############
#----
# this creates 2 arrays: 
# - one array for dispersal position, "larvatrack_name"
#   (19 larvae x 3+nodes columns(X,Y,depth,closestnodes1:3) x 1201 hours) 
#   larvae are released at coordinates from restorationsites dataset, at .5 depth of site
# - one for larval sizes (19 larvae x 1201 ) "larvatrack3d_name.sizes"
create.larvae(name=name,nodes = nodes,year=year,release=release)
#----


############## View Arrays ###################
#----
get(paste0("larvatrack",name))[,,release:(release+1)]
get(paste0("larvatrack",name,".sizes"))[,release:(release+1)]
# this will show the position tracking array, from release time to 2 hours after
# should be 1 page of (X, Y, Node1, (Node2), (Node3), Depth), followed by 2 pages of NAs
#----




#############  Start Loop  ###################
# * Make sure to check inputs - some require manual change!
#-----------
#  * first need to manually change the name before Grow function, and at very end. 
#for (hour in release:(dim(get(paste0("larvatrack.3d",name)))[3]-1)){ # for hours 1 through end (hour 1200)
for(hour in release:600 ){ 
  
  #** 1. Create temporary array ####
  #----------------------------------
  loop_array <<- get(paste0("larvatrack",name))[,,hour:(hour+1)] 
  #print("created")
  #----
  # creates mini array using most recently filled out page and next NA page
  #----

  #**  2. Locate Larvae ####
  #---------------------------------
  locate.larvae(loop_array,nodes=nodes,year=year) 
  #print("located")
  #----
  # --- creates a "distances" matrix, containing distances from 19 larvae to the 'n' closest nodes to them (1 to 3)
  # --- these distances will be used for weighing current velocities of nodes
  # --- also "lastnodes", "lastdepthprop", and "closestlay"
  #----
  
  #** 3. Grow Larvae ####      (needs manual input)
  #-------------------------------------------
  larvatrack_14_2_onto_3n.sizes[,hour+1] <- larvatrack_14_2_onto_3n.sizes[,hour] + grow.larvae(year=year) #      <---***needs manual change           
  #print("sized")
  #----
  # --- this updates the .sizes array growing larvae from release size 
  # --- according to GAM model from Lawlor & Arellano 2020
  #----
  
  #**  4. Move Larvae ####
  #-------------------------
  move.larvae(loop_array,nodes=nodes,year=year)
  #print("moved")
  #----
  # this moves larvae in x, y, and z direction 
  # based on weighted mean vectors of currents from n closest nodes
  # if n > 1, or just based on value of node if n = 1
  #----
  
  #**  5. Swim Larvae ####
  #---------------------------
  swim.larvae(loop_array,behavior = behavior)
  #print("swim")


  #**  6. Correct Larvae ####
  #---------------------------
  correct.larvae(loop_array,year = year)
  
  
  #**  7. Clock ####
  print(hour)
  
  #** 8. Fill array page with temp array     (needs manual change)
  #------------------------------
  larvatrack_14_2_onto_3n[,,hour+1] <- loop_array[,,2]   #      <---***needs manual change 
  
  
  # remove variables
  #-------------------------------
  rm(lastnodes,closestlay,lastdepthprop,loop_array)
  
}#end loop 
##################################

# convert to df
#------------------------
convert.larvae()
#----


larvatrack_14_2_onto_3n[,,hour:(hour+1)]

larvatrack_14_2_onto_3n_df

# plot
#------------------------
basemap <- WA_nodes_coords %>%
  ggplot(aes(x=x,y=y)) +
  geom_point(color="slategrey", size=.5) +
  theme_void() +
  theme(panel.background = element_rect(color="grey20"),
        plot.margin = margin(10,10,10,10,unit="pt")) +
  coord_equal()




p <- basemap + 
  geom_path(data = larvatrack_14_2_onto_3n_df,
            aes(x=x,y=y,color=site,label=hour),
            size = .3)


p2 <- basemap + 
  geom_path(data = onto14.2 %>% filter(hour < 100),
            aes(x=X,y=Y,color=site,label=hour),
            size = .7)

plotly::ggplotly(p)



test <- larvatrack_14_1_onto_1n_df %>%
  filter(site == "Fidalgo Bay",
         complete.cases(.))


larvatrack_14_1_onto_1n_df %>%
  filter(site =="Fidalgo Bay",
         complete.cases(.)) 


loop_array[,,2] <- NA

# plot depths
larvatrack_14_2_onto_3n_df %>%
  filter(complete.cases(.)) %>%
  
  # filter out anything outside of the strait
  filter(!x < 387400) %>%
  ggplot(aes(color=site,x=hour,y=d)) + 
  geom_path(size=.5) + 
  theme_classic()



basemap +
  geom_point(data = WA_nodes_coords[c(2172,2096),],
             color="red") +
  geom_point(data = WA_nodes_coords[c(2029, 2028, 1964),],
             color="purple") 
  








