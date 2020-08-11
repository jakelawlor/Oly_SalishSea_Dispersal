# script 2: make constants
#=====================================



# libraries
#------------------------
library(tidyverse)
library(here)
#------

dim(PStotal14)
dim(WA_currents_14)

WA_currents_14[400,3,1,1:100] %>%
  plot()
  

# Make restoration site coordinates 
#---------------------------------------

# I did this from a map and with my best knowledge of restoration sites in those areas
# coordinates need to be converted to meters from 
restorationsites <- data.frame(matrix(nrow=19,ncol=3))
{colnames(restorationsites) <- c("site","x","y")
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
  restorationsites[19,] <- c("Budd Inlet",506941, 5211139) }


# fix df - add basins and order
restorationsites <- restorationsites %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y)) %>%
  arrange(desc(y)) %>%
  mutate(site = fct_inorder(site)) %>%
  as_tibble() %>%
  mutate(basin = case_when(site %in% c("Drayton Harbor","Bellingham Bay", "Samish Bay","Padilla Bay","Fidalgo Bay") ~ "Georgia Straight",
             site %in% c("Similk Bay") ~ "Whidbey",
             site %in% c("Kilisut Harbor") ~ "North Central",
             site %in% c("Sequim Bay","Discovery Bay") ~ "Juan de Fuca",
             site %in% c("Port Gamble Bay","Quilcene Bay",  "Union River") ~"Hood Canal",
             site %in% c("Liberty Bay", "Port Orchard Pass", "Dyes Inlet", "Sinclair Inlet") ~ "South Central",
             site %in% c("Squaxin Island", "Henderson Bay","Budd Inlet") ~ "South")) %>%
  group_by(basin) %>%
  nest() %>%
  ungroup() %>%
  mutate(basin_num = c(1:7)) %>%
  unnest(cols = c(data)) %>%
  relocate(site,x,y)


#save
save(restorationsites,
     file=here("data_processed","restorationsites.Rdata"))
#-----





# Time
#-------------------------------
# vector of time telling when it is night and day
# this shows 8/16 N/D where night (N) is from 10ppm-5am and day (D) is 5am-9pm,
# which seems like a reasonable range for washington in the summertime. 
time <- rep(c("N","N","N","N",
              "N","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "D","D","D","D",
              "D","N","N","N"),length.out=1200)

save(time,
     file=here("data_processed","time.Rdata"))
#-----


