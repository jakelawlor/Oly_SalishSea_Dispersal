# Script 4. particle dispersal template





#---------------------------------------------------
####           ANNOTATED   TEMPLATE           #####
#---------------------------------------------------




############ ** INPUTS ###############
#----------------------------------
{release=10                       # Hour of release
nodes=3                         # Nodes to call from, 1, 2, or 3
year = 14                       # year, 14 or 95
release_size <- 180             # Start size for larvae
behavior = "none"               # behavior for larvae. "up","none","photo","onto"
hour=release                    # 
name = paste0("_",year,"_",     # creates nane with variables from above
              release,"_",      # name format is _year_release_behavior_nodes
              behavior,"_",     #
              nodes,"n")        # 
print(name)}
#-------------------------------------



############# Load Objects ##############
#----
# load(here("data_processed",paste0("WA_currents_",year,".Rdata")))
# load(here("data_processed",paste0("zeta_",year,".Rdata")))
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
for(hour in release:1200 ){ 
  
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
  larvatrack_14_10_none_3n.sizes[,hour+1] <- larvatrack_14_10_none_3n.sizes[,hour] + grow.larvae(year=year) #      <---***needs manual change           
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
  larvatrack_14_10_none_3n[,,hour+1] <- loop_array[,,2]   #      <---***needs manual change 
  
  
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
  ggplot(aes(x=x,y=y,label=node)) +
  geom_point(color="slategrey", size=.3) +
  theme_void() +
  theme(panel.background = element_rect(color="grey20"),
        plot.margin = margin(10,10,10,10,unit="pt")) +
  coord_equal()



WA_nodes_coords %>%
  st_as_sf( coords = c("x","y"),
           crs = "+proj=utm +zone=10") %>% 
  sf::st_transform(crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  ggplot() +
  geom_sf(color="slategrey",size=.3) + 
  geom_sf(inherit.aes = F,
          data = coasts_transformed,
          size=.3)

  
  
?st_transform
?st_as_sf

name
p <- basemap + 
  geom_path(data = get(paste0("larvatrack",name,"_df")) %>% filter(complete.cases(.)),
            aes(x=x,y=y,color=site,label=hour),
            size = .3, alpha= .5) + 
  geom_point(data = get(paste0("larvatrack",name,"_df")) %>% 
               filter(size > 260 &
                      d < (-WA_nodes_coords$h_true[.$node])*.75),
             shape=23,color="black", size=.75, stroke=.5) +
  geom_sf(inherit.aes = F,
          data = coasts_transformed,
          size=.3)



plotly::ggplotly(p)

# plot depths colored by site
#=========================================
get(paste0("larvatrack",name,"_df")) %>%
  filter(complete.cases(.)) %>%
  
  # filter out anything outside of the strait
  filter(!x < 387400) %>%
  
  # plot
  ggplot(aes(color=site,fill=site,x=hour,y=d)) + 
  
  # add depth line
  geom_path(size=.5,alpha=.3) + 
  
  # add depth path of big larvae
  geom_path(data = get(paste0("larvatrack",name,"_df")) %>%
              filter(size > 260), size=1.25) + 
  
  # add settlement points
  geom_point(data =  get(paste0("larvatrack",name,"_df")) %>%
                        filter(size > 260 &
                               d < (-WA_nodes_coords$h_true[.$node])*.75),
             shape=23,color="black", size=.75) +
  
  # theme stuff
  labs(x="Hour",y="Depth (m)") +
  theme_classic() 
#====


# plot depths colored by X/Y velocity
#========================================

get(paste0("larvatrack",name,"_df")) %>%
  filter(complete.cases(.)) %>%
  
  # calculate delta x/y 
  group_by(site) %>%
  mutate(deltax = abs(x - lag(x,1)), # calculate change in x
         deltay = abs(y - lag(y,1)), # calculate change in y
         ) %>%
  mutate(deltadist = sqrt(deltax^2 + deltay^2)) %>% # add total x/y distance with pythagorean theorem

 # arrange(desc(deltadist)) %>%
  
  # filter out anything outside of the strait
  filter(!x < 387400) %>%
  
  # plot
  ggplot(aes(group=site,x=hour,y=d,color=deltadist)) + 
  
  # add depth line
  geom_path(size=.25,alpha=.3) + 
  
  # add depth path of big larvae
 # geom_path(data = . %>%
#              filter(size > 260), size=1.25) +
  
  # add settlement points
  #geom_point(data =  . %>%
  #             filter(size > 260 &
  #                      d < (-WA_nodes_coords$h_true[.$node])*.75),
  #           shape=23,color="black", size=.75) +
  
  # theme stuff
  labs(x="Hour",y="Depth (m)") +
  viridis::scale_color_viridis(name = "Larval Velocity\n(m/hr)") +
  theme_classic() 
#-----




# plot map colored by x/y velocity
#--------------------------------

basemap +
  geom_path(data=
              get(paste0("larvatrack",name,"_df")) %>%
              filter(complete.cases(.)) %>%
              
              # calculate delta x/y 
              group_by(site) %>%
              mutate(deltax = abs(x - lag(x,1)), # calculate change in x
                     deltay = abs(y - lag(y,1)), # calculate change in y
              ) %>%
              mutate(deltadist = sqrt(deltax^2 + deltay^2)),
            aes(x=x,y=y,group=site,color=deltadist)# add total x/y distance with pythagorean theorem
  ) +
  viridis::scale_color_viridis(name = "Larval Velocity\n(m/hr)") +
  
  geom_sf(inherit.aes = F,
          data = coasts_transformed,
          size=.3)


ggsave(filename = here("velocity.png"),dpi=400)
#------------

max(WA_currents_14[,3,1,])


min_currents <- apply(t(WA_currents_14[,3,1,]), MARGIN = 1, FUN = min)
min(min_currents)
  
dim(WA_currents_14)
max(WA_currents_14[,4,1,1:1200])
  

### find fastest trajectory
fastnodes <- get(paste0("larvatrack",name,"_df")) %>%
  filter(complete.cases(.)) %>%
  
  # calculate delta x/y 
  group_by(site) %>%
  mutate(deltax = abs(x - lag(x,1)), # calculate change in x
         deltay = abs(y - lag(y,1)), # calculate change in y
  ) %>%
  mutate(deltadist = sqrt(deltax^2 + deltay^2)) %>% 
  ungroup() %>%
  
  filter(!x<365500,
         !(x<400000 & y < 5350000)) %>%
  ungroup() %>% 
  arrange(desc(deltadist)) %>%
  
  dplyr::select(node,deltadist) %>% slice(1:200) 


WA_nodes_coords %>%
  ggplot(aes(x=x,y=y,label=node)) +
  geom_point(color="grey80",alpha=.7) +
  
  geom_point(data = WA_nodes_coords[fastnodes$node,],aes(color=fastnodes$deltadist))


WA_currents_14[c(86,1709,6581,6528),3:4,1,865:867]











plotly::ggplotly(p)



larvatrack_14_4_photo_3n_df %>%
  mutate(growth = size -lag(size,1)) %>%
  group_by(site) %>%
  summarize(meangrow = mean(growth,na.rm = T)*24) %>%
  arrange(desc(meangrow))

WA_nodes_coords %>% filter(node %in% c(11600, 11507, 11506, 11508, 11601, 11602))

pal <- PNWColors::pnw_palette("Bay",100)
dplot <- WA_nodes_coords %>% 
  ggplot(aes(x=x,y=y,color=h_smooth,label=node)) +
  geom_point(size=.3,alpha=.5) +
  scale_color_gradientn(colors = pal)

WA_nodes_coords %>% 
  mutate(h_diff = h_smooth - h_true) %>%
  mutate(bigdiff = case_when(h_diff > 50 ~ "y",
                             TRUE ~ "n")) %>%
  ggplot(aes(x=x,y=y,color=bigdiff,label=node)) +
  geom_point(size=.3,alpha=.5) 


test <- larvatrack_14_1_onto_1n_df %>%
  filter(site == "Fidalgo Bay",
         complete.cases(.))


larvatrack_14_1_onto_1n_df %>%
  filter(site =="Fidalgo Bay",
         complete.cases(.)) 


loop_array[,,2] <- NA



basemap +
  geom_path(data=larvatrack_14_4_onto_3n_df %>%
              filter(complete.cases(.)),
            aes(x=x,y=y,color=site), size=.5, alpha=.3) +
  geom_path(data=larvatrack_14_4_onto_3n_df %>%
              filter(complete.cases(.),
                     size > 260),
            aes(x=x,y=y,color=site), size=.75, alpha=1) +
  geom_point(data = larvatrack_14_4_onto_3n_df %>%
               filter(size > 260 &
                        d < (-WA_nodes_coords$h_true[.$node])*.75),
             shape=23,color="black", size=.75)








larvatrack_14_2_onto_3n_df %>%
  filter(hour ==2) %>%
  arrange(d)
