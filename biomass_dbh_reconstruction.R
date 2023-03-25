### biomass reconstruction from tree diameter estimates at different locations ###

### LinkAGES method, Chojnacky equations, Lambert equations ###

### can use the output.R function for LinkAGES method of estimating biomass, now need to implement the Lambert and
### Chojnacky methods

### id: individual identifier for each tree
### year: year corresponding to the diameter relative to 1900 (1 = 1900)
### incr: diameter increment
### plot:
### stat_id: individual tree number, combined with id for unique identifier
### taxon: species abbreviation
### D: diameter (cm) for the given tree during the given year

rm(list=ls())

library(tidyverse)

sites <- list('Sylvania', 'HMC', 'Rooster', 'HF', 'Goose', 'NRP')

# creates new directory for each of the different reconstruction sites

for (j in sites){
  print(j)
  new_dir = paste0('biomass_dbh_reconstruction/',j)
  if (!dir.exists(new_dir)){
    dir.create(new_dir, recursive = T)
  }
}
### Lambert function found in Lambert et al. 2005
# Alyssa and I made the decision not to include the error terms in the final calculation here


Lambert <- function(species_params, D){
  b_wood1 = species_params[1,2]
  b_wood2 = species_params[2,2]
  e_wood = species_params[3,2]
  b_bark1 = species_params[4,2]
  b_bark2 = species_params[5,2]
  e_bark = species_params[6,2]
  b_foliage1 = species_params[7,2]
  b_foliage2 = species_params[8,2]
  e_foliage = species_params[9,2]
  b_branches1 = species_params[10,2]
  b_branches2 = species_params[11,2]
  e_branches = species_params[12,2]
  e_crown = e_branches + e_foliage
  e_stem = e_wood + e_bark


  y_wood = b_wood1*(D^b_wood2)
  #print(y_wood)
  y_bark = b_bark1*(D^b_bark2)
  #print(y_bark)
  y_stem = y_wood + y_bark
  y_foliage = b_foliage1*(D^b_foliage2)
  print(y_foliage)
  y_branches = b_branches1*(D^b_branches2)
  print(y_branches)
  y_crown = y_foliage + y_branches
  y_total = y_wood + y_bark + y_foliage + y_branches #+ e_total
  #print(y_total)

  return (y_total)
}

### Chojnacky function found in Chojnacky et al. 2014 ###

Chojnacky <- function(species_params, dbh){

  b_0 = species_params[1,2]
  b_1 = species_params[2,2]
  biomass = exp(b_0)*(dbh^b_1)
  print(biomass)

  return(biomass)
}

### LINKAGES function ###

Linkages_allometry <- function(species_params, dbh){

  frt = species_params[1,2]#frt[i]
  slta = species_params[2,2]
  sltb = species_params[3,2]
  fwt = species_params[4,2]

  ret = frt

  #calculate leaf biomass (kg/tree)
  folw = ((slta+sltb*dbh)/2)^2 * 3.14 * fwt * ret * .001

  #calculate total biomass per tree (kg/tree)
  t_ind_tree_agb = .1193 * dbh^2.393 + folw


  return(t_ind_tree_agb)
}


### Sylvania
# Sylvania species
# ACSA = sugar maple
# BEAL = yellow birch
# THOC = northern white-cedar
# TSCA = eastern hemlock
#unique(SYL_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

SYL_diam_data <- read.csv('SYL_diam_data.csv')

SYL_diam_data_no_inc <- subset(SYL_diam_data, select = c(-incr,-X))
SYL_diam_data_no_inc <-SYL_diam_data_no_inc %>% distinct()

### Lambert species params Sylvania

sugar_maple_Lamb <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                            Param_vals = c(0.1315,2.3129,0.0084 + 0.0182,0.0631,1.9241,0.0103 + 0.0464,0.0393,1.6930,0.0024 + 0.0196,0.0330,2.3741,0.0038 + 0.0341 ))

yellow_birch_Lamb <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                              Param_vals = c(0.1932,2.1569,0.0193 + 0.0269,0.0192,2.2475,0.0017 + 0.0243,0.1119,1.3973,0.0121 + 0.0313,0.0305,2.4044,0.0024 + 0.0230 ))

north_white_cedar_Lamb <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.0654,2.2121,0.0037 + 0.0170,0.0114,2.1432,0.0013 + 0.0348,0.0499,1.7278,0.0038 + 0.0265,0.0335,1.9367,0.0034 + 0.0331 ))

east_hemlock_Lamb <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                          Param_vals = c(0.0619,2.3821,0.0030 + 0.0150,0.0139,2.3282,0.0010 + 0.0210,0.0776,1.6995,0.0069 + 0.0292,0.0217,2.2653,0.0031 + 0.0420 ))

#syl_maple <- filter(SYL_diam_data_no_inc, taxon == 'ACSA')
#filter(SYL_diam_data_no_inc, taxon == 'BEAL')
#filter(SYL_diam_data_no_inc, taxon == 'THOC')
#filter(SYL_diam_data_no_inc, taxon == 'TSCA')

max_year <- max(SYL_diam_data_no_inc$year)

sugar_maple_biomass_Lamb <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
yellow_birch_biomass_Lamb <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
north_white_cedar_biomass_Lamb <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
east_hemlock_biomass_Lamb <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))

# calculating biomass from dbh using Lambert

 for(i in 1:nrow(SYL_diam_data_no_inc)){
   year = SYL_diam_data_no_inc[i,2]
   dbh = SYL_diam_data_no_inc[i,6]
  if (SYL_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Lambert(species_params = sugar_maple_Lamb, D = dbh)
    sugar_maple_biomass_Lamb[year,2] = sugar_maple_biomass_Lamb[year,2] + temp

  }
  else if (SYL_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Lambert(species_params = yellow_birch_Lamb, D = dbh)
    yellow_birch_biomass_Lamb[year,2] = yellow_birch_biomass_Lamb[year,2] + temp
  }
  else if (SYL_diam_data_no_inc[i,5] == 'THOC'){
    temp = Lambert(species_params = north_white_cedar_Lamb, D = dbh)
    north_white_cedar_biomass_Lamb[year,2] = north_white_cedar_biomass_Lamb[year,2] + temp
  }
  else {return
    temp = Lambert(species_params = east_hemlock_Lamb, D = dbh)
    east_hemlock_biomass_Lamb[year,2] = east_hemlock_biomass_Lamb[year,2] + temp
  }
 }

### Chojnacky species params Sylvania

sugar_maple_Choj <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852))
yellow_birch_Choj <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8096, 2.3480)) ### Betulaceae .5-.59 spg
north_white_cedar_Choj <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.9615, 2.1063)) ### Cupressaceae  <.3 spg
east_hemlock_Choj <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.3480,2.3876)) ### Tsuga <.4 spg

sugar_maple_biomass_Choj <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
yellow_birch_biomass_Choj <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
north_white_cedar_biomass_Choj <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
east_hemlock_biomass_Choj <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))

for(i in 1:nrow(SYL_diam_data_no_inc)){
  year = SYL_diam_data_no_inc[i,2]
  #print(year)
  dbh = SYL_diam_data_no_inc[i,6]
  #print(dbh)
  if (SYL_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Chojnacky(species_params = sugar_maple_Choj, dbh = dbh)
    sugar_maple_biomass_Choj[year,2] = sugar_maple_biomass_Choj[year,2] + temp
    #print(temp)
    temp = 0
  }
  else if (SYL_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Chojnacky(species_params = yellow_birch_Choj, dbh = dbh)
    yellow_birch_biomass_Choj[year,2] = yellow_birch_biomass_Choj[year,2] + temp
  }
  else if (SYL_diam_data_no_inc[i,5] == 'THOC'){
    temp = Chojnacky(species_params = north_white_cedar_Choj, dbh = dbh)
    north_white_cedar_biomass_Choj[year,2] = north_white_cedar_biomass_Choj[year,2] + temp
  }
  else {
    temp = Chojnacky(species_params = east_hemlock_Choj, dbh = dbh)
    east_hemlock_biomass_Choj[year,2] = east_hemlock_biomass_Choj[year,2] + temp
  }
}


### LinkAGES species params Sylvania

sugar_maple_Link <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6463, 0.1425, 440))
yellow_birch_Link <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6026, 0.1493, 248))
north_white_cedar_Link <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 1.3127, 0.0989, 248))
east_hemlock_Link <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 2.6284, 0.1218, 440))

sugar_maple_biomass_Link <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
yellow_birch_biomass_Link <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
north_white_cedar_biomass_Link <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
east_hemlock_biomass_Link <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))

for(i in 1:nrow(SYL_diam_data_no_inc)){
  year = SYL_diam_data_no_inc[i,2]
  dbh = SYL_diam_data_no_inc[i,6]
  if (SYL_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Linkages_allometry(species_params = sugar_maple_Link, dbh = dbh)
    sugar_maple_biomass_Link[year,2] = sugar_maple_biomass_Link[year,2] + temp
    print(temp)
    temp = 0
  }
  else if (SYL_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Linkages_allometry(species_params = yellow_birch_Link, dbh = dbh)
    yellow_birch_biomass_Link[year,2] = yellow_birch_biomass_Link[year,2] + temp
  }
  else if (SYL_diam_data_no_inc[i,5] == 'THOC'){
    temp = Linkages_allometry(species_params = north_white_cedar_Link, dbh = dbh)
    north_white_cedar_biomass_Link[year,2] = north_white_cedar_biomass_Link[year,2] + temp
  }
  else {
    temp = Linkages_allometry(species_params = east_hemlock_Link, dbh = dbh)
    east_hemlock_biomass_Link[year,2] = east_hemlock_biomass_Link[year,2] + temp
  }
}


### Sylvania biomass visualizations ###

par(mfrow=c(2,2))
plot(sugar_maple_biomass_Lamb$Year, sugar_maple_biomass_Lamb$Biomass, type = 'l', main = "Total biomass Sugar Maple Sylvania", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,5500))
lines(sugar_maple_biomass_Choj$Year, sugar_maple_biomass_Choj$Biomass, type = 'l', col = 'green')
lines(sugar_maple_biomass_Link$Year, sugar_maple_biomass_Link$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(yellow_birch_biomass_Lamb$Year, yellow_birch_biomass_Lamb$Biomass, type = 'l', main = "Total biomass Yellow Birch Sylvania", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,25000))
lines(yellow_birch_biomass_Choj$Year, yellow_birch_biomass_Choj$Biomass, type = 'l', col = 'green')
lines(yellow_birch_biomass_Link$Year, yellow_birch_biomass_Link$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(north_white_cedar_biomass_Lamb$Year, north_white_cedar_biomass_Lamb$Biomass, type = 'l', main = "Total biomass Norther White Cedar Sylvania", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,10000))
lines(north_white_cedar_biomass_Choj$Year, north_white_cedar_biomass_Choj$Biomass, type = 'l', col = 'green')
lines(north_white_cedar_biomass_Link$Year, north_white_cedar_biomass_Link$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(east_hemlock_biomass_Lamb$Year, east_hemlock_biomass_Lamb$Biomass, type = 'l', main = "Total biomass Eastern Hemlock Sylvania", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,80000))
lines(east_hemlock_biomass_Choj$Year, east_hemlock_biomass_Choj$Biomass, type = 'l', col = 'green')
lines(east_hemlock_biomass_Link$Year, east_hemlock_biomass_Link$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at Sylvania by Equation
t_agb_Lamb_Sylvania <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
t_agb_Choj_Sylvania <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))
t_agb_Link_Sylvania <- data.frame('Year' = c(1:max_year), "Biomass" =c(0))

t_agb_Lamb_Sylvania$Biomass <- sugar_maple_biomass_Lamb$Biomass + yellow_birch_biomass_Lamb$Biomass + north_white_cedar_biomass_Lamb$Biomass + east_hemlock_biomass_Lamb$Biomass
t_agb_Choj_Sylvania$Biomass <- sugar_maple_biomass_Choj$Biomass + yellow_birch_biomass_Choj$Biomass + north_white_cedar_biomass_Choj$Biomass + east_hemlock_biomass_Choj$Biomass
t_agb_Link_Sylvania$Biomass <- sugar_maple_biomass_Link$Biomass + yellow_birch_biomass_Link$Biomass + north_white_cedar_biomass_Link$Biomass + east_hemlock_biomass_Link$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_Sylvania$Year, t_agb_Lamb_Sylvania$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method Sylvania', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_Sylvania$Year, t_agb_Choj_Sylvania$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_Sylvania$Year, t_agb_Link_Sylvania$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/Sylvania")
dev.off()

#maple.other.biomass = data.frame('Biomass' = c(0))

#for (i in syl_maple$D){
#  #print(i)
#  bio = Chojnacky(species_params = sugar_maple_Choj, dbh = i)
#  maple.other.biomass[nrow(maple.other.biomass) + 1,] = bio
#  bio = 0
#  print(bio)
#}

#maple.other.biomass <- maple.other.biomass[-1,]
#maple.other.biomass <- data.frame(maple.other.biomass)

#syl_maple <- cbind(syl_maple, maple.other.biomass)


### HMC ###
# TSCA eastern hemlock
# ACSA sugar maple
# POGR bigtooth aspen
# BEAL yellow birch
# THOC northern white-cedar
# ASCA ?
# TIAM american basswood
# ACRU red maple

HMC_diam_data <- read.csv('HMC_diam_data.csv')


table(HMC_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

HMC_diam_data_no_inc <- subset(HMC_diam_data, select = c(-incr,-X))
HMC_diam_data_no_inc <-HMC_diam_data_no_inc %>% distinct()
max_year_HMC <- max(HMC_diam_data$year)

### Lambert species params HMC

east_hemlock_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                               Param_vals = c(0.0619,2.3821,0.0030 + 0.0150,0.0139,2.3282,0.0010 + 0.0210,0.0776,1.6995,0.0069 + 0.0292,0.0217,2.2653,0.0031 + 0.0420 ))

sugar_maple_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.1315,2.3129,0.0084 + 0.0182,0.0631,1.9241,0.0103 + 0.0464,0.0393,1.6930,0.0024 + 0.0196,0.0330,2.3741,0.0038 + 0.0341 ))

bigtooth_aspen_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.0959,2.3430,0.0144 + 0.0483,0.0308,2.2240,0.0035 + 0.0388,0.0080,2.0149,0.0016 + 0.0629,0.0047,2.6530,0.0006 + 0.0435 ))

yellow_birch_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.1932,2.1569,0.0193 + 0.0269,0.0192,2.2475,0.0017 + 0.0243,0.1119,1.3973,0.0121 + 0.0313,0.0305,2.4044,0.0024 + 0.0230 ))

north_white_cedar_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                               Param_vals = c(0.0654,2.2121,0.0037 + 0.0170,0.0114,2.1432,0.0013 + 0.0348,0.0499,1.7278,0.0038 + 0.0265,0.0335,1.9367,0.0034 + 0.0331 ))

#???????????????? <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                Param_vals = c(0.0562,2.4102,0.0045 + 0.0227,0.0302,2.0976,0.0068 + 0.0652,0.0288,1.6378,0.0095 + 0.0966,0.0230,2.2382,0.0069 + 0.0888 ))

american_basswood_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.0562,2.4102,0.0045 + 0.0227,0.0302,2.0976,0.0068 + 0.0652,0.0288,1.6378,0.0095 + 0.0966,0.0230,2.2382,0.0069 + 0.0888))

red_maple_Lamb_HMC <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.1014,2.3448,0.0052 + 0.0173,0.0291,2.0893,0.0027 + 0.0320,0.0515,1.5198,0.0065 + 0.0400,0.0175,2.4846,0.0034 + 0.0603 ))


east_hemlock_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
sugar_maple_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
bigtooth_aspen_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
yellow_birch_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
north_white_cedar_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
#????????_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
american_basswood_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
red_maple_biomass_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))


for(i in 1:nrow(HMC_diam_data_no_inc)){
  year = HMC_diam_data_no_inc[i,2]
  dbh = HMC_diam_data_no_inc[i,6]
  if (HMC_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Lambert(species_params = east_hemlock_Lamb_HMC, D = dbh)
    east_hemlock_biomass_Lamb_HMC[year,2] = east_hemlock_biomass_Lamb_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Lambert(species_params = sugar_maple_Lamb_HMC, D = dbh)
    sugar_maple_biomass_Lamb_HMC[year,2] = sugar_maple_biomass_Lamb_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'POGR'){
    temp = Lambert(species_params = bigtooth_aspen_Lamb_HMC, D = dbh)
    bigtooth_aspen_biomass_Lamb_HMC[year,2] = bigtooth_aspen_biomass_Lamb_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Lambert(species_params = yellow_birch_Lamb_HMC, D = dbh)
    yellow_birch_biomass_Lamb_HMC[year,2] = yellow_birch_biomass_Lamb_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'THOC'){
    temp = Lambert(species_params = north_white_cedar_Lamb_HMC, D = dbh)
    north_white_cedar_biomass_Lamb_HMC[year,2] = north_white_cedar_biomass_Lamb_HMC[year,2] + temp
  }
  #else if (SYL_diam_data_no_inc[i,5] == 'ASCA'){
   # temp = Lambert(species_params = north_white_cedar_Lamb, D = dbh)
    #north_white_cedar_biomass_Lamb[year,2] = north_white_cedar_biomass_Lamb[year,2] + temp
  #}
  else if (HMC_diam_data_no_inc[i,5] == 'TIAM'){
    temp = Lambert(species_params = american_basswood_Lamb_HMC, D = dbh)
    american_basswood_biomass_Lamb_HMC[year,2] = american_basswood_biomass_Lamb_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Lambert(species_params = red_maple_Lamb_HMC, D = dbh)
    red_maple_biomass_Lamb_HMC[year,2] = red_maple_biomass_Lamb_HMC[year,2] + temp
  }
  else{return}
}

### Chojnacky species params HMC

east_hemlock_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.3480,2.3876)) ### Tsuga <.4 spg
sugar_maple_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852)) ### Aceraceae >/.50 spg
bigtooth_aspen_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.4441, 2.4561)) ### Salicaceae >/ .35 spg
yellow_birch_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8096, 2.3480)) ### Betulaceae .5-.59 spg
north_white_cedar_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2271, 2.4513)) ### Betulaceae 0.40-0.49 psg
#_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8096, 2.3480)) ###
american_basswood_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.4108, 2.4177)) ### Hippocastanaceae/Tiliaceae
red_maple_Choj_HMC <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0470,2.3852)) ### Aceraceae < 0.50 spg

east_hemlock_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
sugar_maple_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
bigtooth_aspen_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
yellow_birch_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
north_white_cedar_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
#????????_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
american_basswood_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
red_maple_biomass_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))



for(i in 1:nrow(HMC_diam_data_no_inc)){
  year = HMC_diam_data_no_inc[i,2]
  dbh = HMC_diam_data_no_inc[i,6]
  if (HMC_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Chojnacky(species_params = east_hemlock_Choj_HMC, dbh = dbh)
    east_hemlock_biomass_Choj_HMC[year,2] = east_hemlock_biomass_Choj_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Chojnacky(species_params = sugar_maple_Choj_HMC, dbh = dbh)
    sugar_maple_biomass_Choj_HMC[year,2] = sugar_maple_biomass_Choj_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'POGR'){
    temp = Chojnacky(species_params = bigtooth_aspen_Choj_HMC, dbh = dbh)
    bigtooth_aspen_biomass_Choj_HMC[year,2] = bigtooth_aspen_biomass_Choj_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Chojnacky(species_params = yellow_birch_Choj_HMC, dbh = dbh)
    yellow_birch_biomass_Choj_HMC[year,2] = yellow_birch_biomass_Choj_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'THOC'){
    temp = Chojnacky(species_params = north_white_cedar_Choj_HMC, dbh = dbh)
    north_white_cedar_biomass_Choj_HMC[year,2] = north_white_cedar_biomass_Choj_HMC[year,2] + temp
  }
  #else if (SYL_diam_data_no_inc[i,5] == 'ASCA'){
  # temp = Lambert(species_params = north_white_cedar_Lamb, D = dbh)
  #north_white_cedar_biomass_Lamb[year,2] = north_white_cedar_biomass_Lamb[year,2] + temp
  #}
  else if (HMC_diam_data_no_inc[i,5] == 'TIAM'){
    temp = Chojnacky(species_params = american_basswood_Choj_HMC, dbh = dbh)
    american_basswood_biomass_Choj_HMC[year,2] = american_basswood_biomass_Choj_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Chojnacky(species_params = red_maple_Choj_HMC, dbh = dbh)
    red_maple_biomass_Choj_HMC[year,2] = red_maple_biomass_Choj_HMC[year,2] + temp
  }
  else{return}
}


### LinkAGES species params HMC

east_hemlock_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 2.6284, 0.1218, 440))
sugar_maple_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6463, 0.1425, 440))
bigtooth_aspen_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.8250, 0.1155, 248))
yellow_birch_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6026, 0.1493, 248))
north_white_cedar_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 1.3127, 0.0989, 248))
#_Link <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6026, 0.1493, 248))
american_basswood_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.2409, 0.1276, 440))
red_maple_Link_HMC <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.4075, 0.1396, 440))

east_hemlock_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
sugar_maple_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
bigtooth_aspen_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
yellow_birch_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
north_white_cedar_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
#????????_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
american_basswood_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
red_maple_biomass_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))



for(i in 1:nrow(HMC_diam_data_no_inc)){
  year = HMC_diam_data_no_inc[i,2]
  dbh = HMC_diam_data_no_inc[i,6]
  if (HMC_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Linkages_allometry(species_params = east_hemlock_Link_HMC, dbh = dbh)
    east_hemlock_biomass_Link_HMC[year,2] = east_hemlock_biomass_Link_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Linkages_allometry(species_params = sugar_maple_Link_HMC, dbh = dbh)
    sugar_maple_biomass_Link_HMC[year,2] = sugar_maple_biomass_Link_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'POGR'){
    temp = Linkages_allometry(species_params = bigtooth_aspen_Link_HMC,dbh = dbh)
    bigtooth_aspen_biomass_Link_HMC[year,2] = bigtooth_aspen_biomass_Link_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Linkages_allometry(species_params = yellow_birch_Link_HMC, dbh = dbh)
    yellow_birch_biomass_Link_HMC[year,2] = yellow_birch_biomass_Link_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'THOC'){
    temp = Linkages_allometry(species_params = north_white_cedar_Link_HMC, dbh = dbh)
    north_white_cedar_biomass_Link_HMC[year,2] = north_white_cedar_biomass_Link_HMC[year,2] + temp
  }
  #else if (SYL_diam_data_no_inc[i,5] == 'ASCA'){
  # temp = Lambert(species_params = north_white_cedar_Lamb, dbh = dbh)
  #north_white_cedar_biomass_Lamb[year,2] = north_white_cedar_biomass_Lamb[year,2] + temp
  #}
  else if (HMC_diam_data_no_inc[i,5] == 'TIAM'){
    temp = Linkages_allometry(species_params = american_basswood_Link_HMC, dbh = dbh)
    american_basswood_biomass_Link_HMC[year,2] = american_basswood_biomass_Link_HMC[year,2] + temp
  }
  else if (HMC_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Linkages_allometry(species_params = red_maple_Link_HMC, dbh = dbh)
    red_maple_biomass_Link_HMC[year,2] = red_maple_biomass_Link_HMC[year,2] + temp
  }
  else{return}
}

### HMC biomass visualizations ###

par(mfrow=c(2,2))
plot(east_hemlock_biomass_Lamb_HMC$Year, east_hemlock_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Eastern Hemlock HMC", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_hemlock_biomass_Choj_HMC$Year, east_hemlock_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(east_hemlock_biomass_Link_HMC$Year, east_hemlock_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(sugar_maple_biomass_Lamb_HMC$Year, sugar_maple_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Sugar Maple HMC", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,100000))
lines(sugar_maple_biomass_Choj_HMC$Year, sugar_maple_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(sugar_maple_biomass_Link_HMC$Year, sugar_maple_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(bigtooth_aspen_biomass_Lamb_HMC$Year, bigtooth_aspen_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Bigtooth Aspen HMC", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(bigtooth_aspen_biomass_Choj_HMC$Year, bigtooth_aspen_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(bigtooth_aspen_biomass_Link_HMC$Year, bigtooth_aspen_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(yellow_birch_biomass_Lamb_HMC$Year, yellow_birch_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Yellow Birch HMC", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(yellow_birch_biomass_Choj_HMC$Year, yellow_birch_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(yellow_birch_biomass_Link_HMC$Year, yellow_birch_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(north_white_cedar_biomass_Lamb_HMC$Year, north_white_cedar_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Norther White Cedar HMC", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,1500))
lines(north_white_cedar_biomass_Choj_HMC$Year, north_white_cedar_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(north_white_cedar_biomass_Link_HMC$Year, north_white_cedar_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

#plot(east_hemlock_biomass_Lamb$Year, east_hemlock_biomass_Lamb$Biomass, type = 'l', main = "Total biomass Eastern Hemlock Sylvania", col="red", xlab = 'year', ylab = 'agb (kg)', ylim = c(0,80000))
#lines(east_hemlock_biomass_Choj$Year, east_hemlock_biomass_Choj$Biomass, type = 'l', col = 'green')
#lines(east_hemlock_biomass_Link$Year, east_hemlock_biomass_Link$Biomass, type = 'l', col = 'blue')
#legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(american_basswood_biomass_Lamb_HMC$Year, american_basswood_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass American Basswood HMC", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(american_basswood_biomass_Choj_HMC$Year, american_basswood_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(american_basswood_biomass_Link_HMC$Year, american_basswood_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(red_maple_biomass_Lamb_HMC$Year, red_maple_biomass_Lamb_HMC$Biomass, type = 'l', main = "Total biomass Red Maple HMC", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_maple_biomass_Choj_HMC$Year, red_maple_biomass_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(red_maple_biomass_Link_HMC$Year, red_maple_biomass_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at HMC by Equation
t_agb_Lamb_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
t_agb_Choj_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))
t_agb_Link_HMC <- data.frame('Year' = c(1:max_year_HMC), "Biomass" =c(0))

t_agb_Lamb_HMC$Biomass <- east_hemlock_biomass_Lamb_HMC$Biomass + sugar_maple_biomass_Lamb_HMC$Biomass + bigtooth_aspen_biomass_Lamb_HMC$Biomass + yellow_birch_biomass_Lamb_HMC$Biomass + north_white_cedar_biomass_Lamb_HMC$Biomass + american_basswood_biomass_Lamb_HMC$Biomass + red_maple_biomass_Lamb_HMC$Biomass
t_agb_Choj_HMC$Biomass <- east_hemlock_biomass_Choj_HMC$Biomass + sugar_maple_biomass_Choj_HMC$Biomass + bigtooth_aspen_biomass_Choj_HMC$Biomass + yellow_birch_biomass_Choj_HMC$Biomass + north_white_cedar_biomass_Choj_HMC$Biomass + american_basswood_biomass_Choj_HMC$Biomass + red_maple_biomass_Choj_HMC$Biomass
t_agb_Link_HMC$Biomass <- east_hemlock_biomass_Link_HMC$Biomass + sugar_maple_biomass_Link_HMC$Biomass + bigtooth_aspen_biomass_Link_HMC$Biomass + yellow_birch_biomass_Link_HMC$Biomass + north_white_cedar_biomass_Link_HMC$Biomass + american_basswood_biomass_Link_HMC$Biomass + red_maple_biomass_Link_HMC$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_HMC$Year, t_agb_Lamb_HMC$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method HMC', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_HMC$Year, t_agb_Choj_HMC$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_HMC$Year, t_agb_Link_HMC$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/HMC")
dev.off()


### Rooster ###

# ACRU: red maple
# BEPA: paper birch
# FAGR: american beech
# PCRU: red spruce
# PIST: eastern white pine
# PRSE: black cherry
# QURU: northern red oak

Rooster_diam_data <- read.csv('Rooster_diam_data.csv')


table(Rooster_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

Rooster_diam_data_no_inc <- subset(Rooster_diam_data, select = c(-incr,-X))
Rooster_diam_data_no_inc <-Rooster_diam_data_no_inc %>% distinct()
max_year_Rooster <- max(Rooster_diam_data$year)

### Lambert species params Rooster

red_maple_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.1014,2.3448,0.0052 + 0.0173,0.0291,2.0893,0.0027 + 0.0320,0.0515,1.5198,0.0065 + 0.0400,0.0175,2.4846,0.0034 + 0.0603 ))

paper_birch_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                       Param_vals = c(0.0593, 2.5026, 0.0020 + 0.0114, 0.0135, 2.4053, 0.0008 + 0.0193, 0.0546, 1.6351, 0.0028 + 0.0189, 0.0135, 2.5532, 0.0009 + 0.0231))

american_beech_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                          Param_vals = c(0.1478, 2.2986, 0.0202 + 0.0401, 0.0120, 2.2388, 0.0010 + 0.0256, 0.0376, 1.6164, 0.0055 + 0.0445, 0.0370, 2.3680, 0.0042 + 0.0353))

red_spruce_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                   Param_vals = c(0.0989, 2.2814, 0.0068 + 0.0212, 0.0220, 2.0908, 0.0017 + 0.0257, 0.0066, 2.4213, 0.0012 + 0.0545, 0.0005, 3.2750, 0.000 + 0.0591))

east_white_pine_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                      Param_vals = c(0.0997, 2.2709, 0.0129 + 0.0350, 0.0192, 2.2038, 0.0016 + 0.0237, 0.0284, 1.9375,0.0024 + 0.0248, 0.0056, 2.6011, 0.0008 + 0.0381))

black_cherry_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                    Param_vals = c(0.3743, 1.9406, 0.0573 + 0.0472, 0.0679, 1.8377, 0.0204 + 0.0883, 0.0840, 1.2319, 0.0053 + 0.0248, 0.0796, 2.0103, 0.0108 + 0.0483))

north_red_oak_Lamb_Rooster <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                         Param_vals = c(0.1754, 2.1616, 0.0149 + 0.0267, 0.0381, 2.0991, 0.0051 + 0.0403, 0.0373, 1.6740, 0.0029 + 0.0268, 0.0085, 2.7790, 0.0028 + 0.0979))



red_maple_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
paper_birch_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
american_beech_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
red_spruce_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
east_white_pine_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
black_cherry_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
north_red_oak_biomass_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))

for(i in 1:nrow(Rooster_diam_data_no_inc)){
  year = Rooster_diam_data_no_inc[i,2]
  dbh = Rooster_diam_data_no_inc[i,6]
  if (Rooster_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Lambert(species_params = red_maple_Lamb_Rooster, D = dbh)
    red_maple_biomass_Lamb_Rooster[year,2] = red_maple_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Lambert(species_params = paper_birch_Lamb_Rooster, D = dbh)
    paper_birch_biomass_Lamb_Rooster[year,2] = paper_birch_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Lambert(species_params = american_beech_Lamb_Rooster, D = dbh)
    american_beech_biomass_Lamb_Rooster[year,2] = american_beech_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PCRU'){
    temp = Lambert(species_params = red_spruce_Lamb_Rooster, D = dbh)
    red_spruce_biomass_Lamb_Rooster[year,2] = red_spruce_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PIST'){
    temp = Lambert(species_params = east_white_pine_Lamb_Rooster, D = dbh)
    east_white_pine_biomass_Lamb_Rooster[year,2] = east_white_pine_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Lambert(species_params = black_cherry_Lamb_Rooster, D = dbh)
    black_cherry_biomass_Lamb_Rooster[year,2] = black_cherry_biomass_Lamb_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'QURU'){
    temp = Lambert(species_params = north_red_oak_Lamb_Rooster, D = dbh)
    north_red_oak_biomass_Lamb_Rooster[year,2] = north_red_oak_biomass_Lamb_Rooster[year,2] + temp
  }
  else{return}
}

### Chojnacky species params Rooster

red_maple_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852)) ### Aceraceae >/.50 spg
paper_birch_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2271, 2.4513)) ### Betulaceae 0.40-0.49 psg
american_beech_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
red_spruce_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.1364, 2.3233)) ### Picea >/ 0.35 spg
east_white_pine_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.6177, 2.4638)) ### Pinus < 0.45 spg
black_cherry_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2118, 2.4133)) ### Cornaceae/Ericaceae/Lauraceae
north_red_oak_Choj_Rooster <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous

red_maple_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
paper_birch_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
american_beech_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
red_spruce_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
east_white_pine_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
black_cherry_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
north_red_oak_biomass_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))

for(i in 1:nrow(Rooster_diam_data_no_inc)){
  year = Rooster_diam_data_no_inc[i,2]
  dbh = Rooster_diam_data_no_inc[i,6]
  if (Rooster_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Chojnacky(species_params = red_maple_Choj_Rooster, dbh = dbh)
    red_maple_biomass_Choj_Rooster[year,2] = red_maple_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Chojnacky(species_params = paper_birch_Choj_Rooster, dbh = dbh)
    paper_birch_biomass_Choj_Rooster[year,2] = paper_birch_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Chojnacky(species_params = american_beech_Choj_Rooster, dbh = dbh)
    american_beech_biomass_Choj_Rooster[year,2] = american_beech_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PCRU'){
    temp = Chojnacky(species_params = red_spruce_Choj_Rooster, dbh = dbh)
    red_spruce_biomass_Choj_Rooster[year,2] = red_spruce_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PIST'){
    temp = Chojnacky(species_params = east_white_pine_Choj_Rooster, dbh = dbh)
    east_white_pine_biomass_Choj_Rooster[year,2] = east_white_pine_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Chojnacky(species_params = black_cherry_Choj_Rooster, dbh = dbh)
    black_cherry_biomass_Choj_Rooster[year,2] = black_cherry_biomass_Choj_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'QURU'){
    temp = Chojnacky(species_params = north_red_oak_Choj_Rooster, dbh = dbh)
    north_red_oak_biomass_Choj_Rooster[year,2] = north_red_oak_biomass_Choj_Rooster[year,2] + temp
  }
  else{return}
}

### LinkAGES species params Rooster

red_maple_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.4075, 0.1396, 440))
paper_birch_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.409, 0.171, 248))
american_beech_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 3.4849, 0.1294, 440))
red_spruce_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 1.084, 0.1368, 440))
east_white_pine_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(2, 1.7076, 0.1228, 440))
black_cherry_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.0239, 0.1424, 173))
north_red_oak_Link_Rooster <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3967, 0.1508, 440))

red_maple_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
paper_birch_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
american_beech_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
red_spruce_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
east_white_pine_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
black_cherry_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
north_red_oak_biomass_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))

for(i in 1:nrow(Rooster_diam_data_no_inc)){
  year = Rooster_diam_data_no_inc[i,2]
  dbh = Rooster_diam_data_no_inc[i,6]
  if (Rooster_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Linkages_allometry(species_params = red_maple_Link_Rooster, dbh = dbh)
    red_maple_biomass_Link_Rooster[year,2] = red_maple_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Linkages_allometry(species_params = paper_birch_Link_Rooster, dbh = dbh)
    paper_birch_biomass_Link_Rooster[year,2] = paper_birch_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Linkages_allometry(species_params = american_beech_Link_Rooster, dbh = dbh)
    american_beech_biomass_Link_Rooster[year,2] = american_beech_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PCRU'){
    temp = Linkages_allometry(species_params = red_spruce_Link_Rooster, dbh = dbh)
    red_spruce_biomass_Link_Rooster[year,2] = red_spruce_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PIST'){
    temp = Linkages_allometry(species_params = east_white_pine_Link_Rooster, dbh = dbh)
    east_white_pine_biomass_Link_Rooster[year,2] = east_white_pine_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Linkages_allometry(species_params = black_cherry_Link_Rooster, dbh = dbh)
    black_cherry_biomass_Link_Rooster[year,2] = black_cherry_biomass_Link_Rooster[year,2] + temp
  }
  else if (Rooster_diam_data_no_inc[i,5] == 'QURU'){
    temp = Linkages_allometry(species_params = north_red_oak_Link_Rooster, dbh = dbh)
    north_red_oak_biomass_Link_Rooster[year,2] = north_red_oak_biomass_Link_Rooster[year,2] + temp
  }
  else{return}
}

### Rooster biomass visualizations ###

par(mfrow=c(2,2))
plot(red_maple_biomass_Lamb_Rooster$Year, red_maple_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Red Maple Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_maple_biomass_Choj_Rooster$Year, red_maple_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(red_maple_biomass_Link_Rooster$Year, red_maple_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(paper_birch_biomass_Lamb_Rooster$Year, paper_birch_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Paper Birch Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(paper_birch_biomass_Choj_Rooster$Year, paper_birch_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(paper_birch_biomass_Link_Rooster$Year, paper_birch_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(american_beech_biomass_Lamb_Rooster$Year, american_beech_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass American Beech Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(american_beech_biomass_Choj_Rooster$Year, american_beech_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(american_beech_biomass_Link_Rooster$Year, american_beech_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(red_spruce_biomass_Lamb_Rooster$Year, red_spruce_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Red Spruce Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_spruce_biomass_Choj_Rooster$Year, red_spruce_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(red_spruce_biomass_Link_Rooster$Year, red_spruce_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(east_white_pine_biomass_Lamb_Rooster$Year, east_white_pine_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Eastern White Pine Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_white_pine_biomass_Choj_Rooster$Year, east_white_pine_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(east_white_pine_biomass_Link_Rooster$Year, east_white_pine_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(black_cherry_biomass_Lamb_Rooster$Year, black_cherry_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Black Cherry Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(black_cherry_biomass_Choj_Rooster$Year, black_cherry_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(black_cherry_biomass_Link_Rooster$Year, black_cherry_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(north_red_oak_biomass_Lamb_Rooster$Year, north_red_oak_biomass_Lamb_Rooster$Biomass, type = 'l', main = "Total biomass Northern Red Oak Rooster", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(north_red_oak_biomass_Choj_Rooster$Year, north_red_oak_biomass_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(north_red_oak_biomass_Link_Rooster$Year, north_red_oak_biomass_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at Rooster by Equation
t_agb_Lamb_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
t_agb_Choj_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))
t_agb_Link_Rooster <- data.frame('Year' = c(1:max_year_Rooster), "Biomass" =c(0))

t_agb_Lamb_Rooster$Biomass <- red_maple_biomass_Lamb_Rooster$Biomass + paper_birch_biomass_Lamb_Rooster$Biomass + american_beech_biomass_Lamb_Rooster$Biomass + red_spruce_biomass_Lamb_Rooster$Biomass + east_white_pine_biomass_Lamb_Rooster$Biomass + black_cherry_biomass_Lamb_Rooster$Biomass + north_red_oak_biomass_Lamb_Rooster$Biomass
t_agb_Choj_Rooster$Biomass <- red_maple_biomass_Choj_Rooster$Biomass + paper_birch_biomass_Choj_Rooster$Biomass + american_beech_biomass_Choj_Rooster$Biomass + red_spruce_biomass_Choj_Rooster$Biomass + east_white_pine_biomass_Choj_Rooster$Biomass + black_cherry_biomass_Choj_Rooster$Biomass + north_red_oak_biomass_Choj_Rooster$Biomass
t_agb_Link_Rooster$Biomass <- red_maple_biomass_Link_Rooster$Biomass + paper_birch_biomass_Link_Rooster$Biomass + american_beech_biomass_Link_Rooster$Biomass + red_spruce_biomass_Link_Rooster$Biomass + east_white_pine_biomass_Link_Rooster$Biomass + black_cherry_biomass_Link_Rooster$Biomass + north_red_oak_biomass_Link_Rooster$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_Rooster$Year, t_agb_Lamb_Rooster$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method Rooster', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_Rooster$Year, t_agb_Choj_Rooster$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_Rooster$Year, t_agb_Link_Rooster$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/Rooster")
dev.off()

### HF ###
# ACRU: red maple
# BEAL: yellow birch
# BELE: sweet birch
# FAGR: american beech
# HAVI: ?
# PIST: eastern white pine
# QURU: northern red oak
# QUVE: black oak
# TSCA: eastern hemlock



HF_diam_data <- read.csv('HF_diam_data.csv')


table(HF_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

HF_diam_data_no_inc <- subset(HF_diam_data, select = c(-incr,-X))
HF_diam_data_no_inc <-HF_diam_data_no_inc %>% distinct()
max_year_HF <- max(HF_diam_data$year)

### found negative dbh values for north_red_oak in HF from years 23-27 ish, changed to absolute value of those dbh values
#north_red_oak_hf <- filter(HF_diam_data_no_inc, taxon == 'QURU')
#north_red_oak_hf$D <- abs(north_red_oak_hf$D)

HF_diam_data_no_inc$D <- abs(HF_diam_data_no_inc$D)

### Lambert species params HF

red_maple_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.1014,2.3448,0.0052 + 0.0173,0.0291,2.0893,0.0027 + 0.0320,0.0515,1.5198,0.0065 + 0.0400,0.0175,2.4846,0.0034 + 0.0603 ))

yellow_birch_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                    Param_vals = c(0.1932,2.1569,0.0193 + 0.0269,0.0192,2.2475,0.0017 + 0.0243,0.1119,1.3973,0.0121 + 0.0313,0.0305,2.4044,0.0024 + 0.0230 ))

#sweet_birch_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                     Param_vals = c()) does not seem to be present in Lambert paper

american_beech_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                          Param_vals = c(0.1478, 2.2986, 0.0202 + 0.0401, 0.0120, 2.2388, 0.0010 + 0.0256, 0.0376, 1.6164, 0.0055 + 0.0445, 0.0370, 2.3680, 0.0042 + 0.0353))

#?_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                      Param_vals = c(0.0989, 2.2814, 0.0068 + 0.0212, 0.0220, 2.0908, 0.0017 + 0.0257, 0.0066, 2.4213, 0.0012 + 0.0545, 0.0005, 3.2750, 0.000 + 0.0591))

east_white_pine_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                           Param_vals = c(0.0997, 2.2709, 0.0129 + 0.0350, 0.0192, 2.2038, 0.0016 + 0.0237, 0.0284, 1.9375,0.0024 + 0.0248, 0.0056, 2.6011, 0.0008 + 0.0381))

north_red_oak_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                    Param_vals = c(0.1754, 2.1616, 0.0149 + 0.0267, 0.0381, 2.0991, 0.0051 + 0.0403, 0.0373, 1.6740, 0.0029 + 0.0268, 0.0085, 2.7790, 0.0028 + 0.0979))

#black_oak_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                        Param_vals = c()) does not seem to be present in Lambert paper

east_hemlock_Lamb_HF <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                       Param_vals = c(0.0619,2.3821,0.0030 + 0.0150,0.0139,2.3282,0.0010 + 0.0210,0.0776,1.6995,0.0069 + 0.0292,0.0217,2.2653,0.0031 + 0.0420 ))

red_maple_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
yellow_birch_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
#sweet_birch_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
american_beech_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
#_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_white_pine_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
north_red_oak_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
#black_oak_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_hemlock_biomass_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))


for(i in 1:nrow(HF_diam_data_no_inc)){
  year = HF_diam_data_no_inc[i,2]
  dbh = HF_diam_data_no_inc[i,6]
  if (HF_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Lambert(species_params = red_maple_Lamb_HF, D = dbh)
    red_maple_biomass_Lamb_HF[year,2] = red_maple_biomass_Lamb_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Lambert(species_params = yellow_birch_Lamb_HF, D = dbh)
    yellow_birch_biomass_Lamb_HF[year,2] = yellow_birch_biomass_Lamb_HF[year,2] + temp
  }
  #else if (HF_diam_data_no_inc[i,5] == 'BELE'){
   # temp = Lambert(species_params = sweet_birch_Lamb_HF, D = dbh)
    #sweet_birch_biomass_Lamb_HF[year,2] = sweet_birch_biomass_Lamb_HF[year,2] + temp
  #}
  else if (HF_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Lambert(species_params = american_beech_Lamb_HF, D = dbh)
    american_beech_biomass_Lamb_HF[year,2] = american_beech_biomass_Lamb_HF[year,2] + temp
  }
  #else if (HF_diam_data_no_inc[i,5] == ''){
   # temp = Lambert(species_params = _pine_Lamb_HF, D = dbh)
    #_pine_biomass_Lamb_HF[year,2] = _pine_biomass_Lamb_HF[year,2] + temp
  #}
  else if (HF_diam_data_no_inc[i,5] == 'PIST'){
   temp = Lambert(species_params = east_white_pine_Lamb_HF, D = dbh)
   east_white_pine_biomass_Lamb_HF[year,2] = east_white_pine_biomass_Lamb_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'QURU'){
    temp = Lambert(species_params = north_red_oak_Lamb_HF, D = dbh)
    north_red_oak_biomass_Lamb_HF[year,2] = north_red_oak_biomass_Lamb_HF[year,2] + temp
  }
  #else if (HF_diam_data_no_inc[i,5] == 'QUVE'){
   # temp = Lambert(species_params = black_oak_Lamb_HF, D = dbh)
    #black_oak_biomass_Lamb_HF[year,2] = black_oak_biomass_Lamb_HF[year,2] + temp
  #}
  else if (HF_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Lambert(species_params = east_hemlock_Lamb_HF, D = dbh)
    east_hemlock_biomass_Lamb_HF[year,2] = east_hemlock_biomass_Lamb_HF[year,2] + temp
  }
  else{return}
}

### Chojnacky species params HF

red_maple_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852)) ### Aceraceae >/.50 spg
yellow_birch_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8096, 2.3480)) ### Betulaceae 0.50 –0.59 spg
sweet_birch_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2652, 2.5349)) ### Betulaceae ≥ 0.60 spg
american_beech_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
#_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c()) ###
east_white_pine_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.6177, 2.4638)) ### Pinus < 0.45 spg
north_red_oak_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
black_oak_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
east_hemlock_Choj_HF <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.3480,2.3876)) ### Tsuga <.4 spg

red_maple_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
yellow_birch_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
sweet_birch_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
american_beech_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
#_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_white_pine_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
north_red_oak_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
black_oak_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_hemlock_biomass_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))

for(i in 1:nrow(HF_diam_data_no_inc)){
  year = HF_diam_data_no_inc[i,2]
  dbh = HF_diam_data_no_inc[i,6]
  if (HF_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Chojnacky(species_params = red_maple_Choj_HF, dbh = dbh)
    red_maple_biomass_Choj_HF[year,2] = red_maple_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Chojnacky(species_params = yellow_birch_Choj_HF, dbh = dbh)
    yellow_birch_biomass_Choj_HF[year,2] = yellow_birch_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'BELE'){
   temp = Chojnacky(species_params = sweet_birch_Choj_HF, dbh = dbh)
   sweet_birch_biomass_Choj_HF[year,2] = sweet_birch_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Chojnacky(species_params = american_beech_Choj_HF,dbh = dbh)
    american_beech_biomass_Choj_HF[year,2] = american_beech_biomass_Choj_HF[year,2] + temp
  }
  #else if (HF_diam_data_no_inc[i,5] == ''){
  # temp = Chojnacky(species_params = _pine_Choj_HF, dbh = dbh)
  #_pine_biomass_Choj_HF[year,2] = _pine_biomass_Choj_HF[year,2] + temp
  #}
  else if (HF_diam_data_no_inc[i,5] == 'PIST'){
    temp = Chojnacky(species_params = east_white_pine_Choj_HF, dbh = dbh)
    east_white_pine_biomass_Choj_HF[year,2] = east_white_pine_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'QURU'){
    temp = Chojnacky(species_params = north_red_oak_Choj_HF, dbh = dbh)
    north_red_oak_biomass_Choj_HF[year,2] = north_red_oak_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'QUVE'){
   temp = Chojnacky(species_params = black_oak_Choj_HF, dbh = dbh)
   black_oak_biomass_Choj_HF[year,2] = black_oak_biomass_Choj_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Chojnacky(species_params = east_hemlock_Choj_HF, dbh = dbh)
    east_hemlock_biomass_Choj_HF[year,2] = east_hemlock_biomass_Choj_HF[year,2] + temp
  }
  else{return}
}

### LinkAGES species params HF

red_maple_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.4075, 0.1396, 440))
yellow_birch_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6026, 0.1493, 248))
sweet_birch_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3766, 0.1624, 248))
american_beech_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 3.4849, 0.1294, 440))
#_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c())
east_white_pine_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(2, 1.7076, 0.1228, 440))
north_red_oak_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3967, 0.1508, 440))
black_oak_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.2350, 0.1597, 440))
east_hemlock_Link_HF <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 2.6284, 0.1218, 440))

red_maple_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
yellow_birch_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
sweet_birch_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
american_beech_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
#_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_white_pine_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
north_red_oak_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
black_oak_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
east_hemlock_biomass_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))

for(i in 1:nrow(HF_diam_data_no_inc)){
  year = HF_diam_data_no_inc[i,2]
  dbh = HF_diam_data_no_inc[i,6]
  if (HF_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Linkages_allometry(species_params = red_maple_Link_HF, dbh = dbh)
    red_maple_biomass_Link_HF[year,2] = red_maple_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Linkages_allometry(species_params = yellow_birch_Link_HF, dbh = dbh)
    yellow_birch_biomass_Link_HF[year,2] = yellow_birch_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'BELE'){
    temp = Linkages_allometry(species_params = sweet_birch_Link_HF, dbh = dbh)
    sweet_birch_biomass_Link_HF[year,2] = sweet_birch_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Linkages_allometry(species_params = american_beech_Link_HF, dbh = dbh)
    american_beech_biomass_Link_HF[year,2] = american_beech_biomass_Link_HF[year,2] + temp
  }
  #else if (HF_diam_data_no_inc[i,5] == ''){
  # temp = Linkages_allometry(species_params = _pine_Link_HF, dbh = dbh)
  #_pine_biomass_Link_HF[year,2] = _pine_biomass_Link_HF[year,2] + temp
  #}
  else if (HF_diam_data_no_inc[i,5] == 'PIST'){
    temp = Linkages_allometry(species_params = east_white_pine_Link_HF, dbh = dbh)
    east_white_pine_biomass_Link_HF[year,2] = east_white_pine_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'QURU'){
    temp = Linkages_allometry(species_params = north_red_oak_Link_HF, dbh = dbh)
    north_red_oak_biomass_Link_HF[year,2] = north_red_oak_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'QUVE'){
    temp = Linkages_allometry(species_params = black_oak_Link_HF, dbh = dbh)
    black_oak_biomass_Link_HF[year,2] = black_oak_biomass_Link_HF[year,2] + temp
  }
  else if (HF_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Linkages_allometry(species_params = east_hemlock_Link_HF, dbh = dbh)
    east_hemlock_biomass_Link_HF[year,2] = east_hemlock_biomass_Link_HF[year,2] + temp
  }
  else{return}
}

### HF biomass visualizations ###

par(mfrow=c(2,2))
plot(red_maple_biomass_Lamb_HF$Year, red_maple_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass Red Maple HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_maple_biomass_Choj_HF$Year, red_maple_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(red_maple_biomass_Link_HF$Year, red_maple_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(yellow_birch_biomass_Lamb_HF$Year, yellow_birch_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass Yellow Birch HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(yellow_birch_biomass_Choj_HF$Year, yellow_birch_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(yellow_birch_biomass_Link_HF$Year, yellow_birch_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(sweet_birch_biomass_Choj_HF$Year, sweet_birch_biomass_Choj_HF$Biomass, type = 'l', main = "Total biomass Sweet Birch HF", col="green", xlab = 'year', ylab = 'agb (kg)')
lines(sweet_birch_biomass_Link_HF$Year, sweet_birch_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
#lines(sweet_birch_biomass_Lamb_HF$Year, sweet_birch_biomass_Lamb_HF$Biomass, type = 'l', col = 'red')
legend(x = 'topleft', legend = c('Chojnacky', 'LinkAGES'), fill = c('green', 'blue'))

plot(american_beech_biomass_Lamb_HF$Year, american_beech_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass American Beech HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(american_beech_biomass_Choj_HF$Year, american_beech_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(american_beech_biomass_Link_HF$Year, american_beech_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c('Lambert','Chojnacky', 'LinkAGES'), fill = c('red','green', 'blue'))

#plot(_biomass_Lamb_HF$Year, _biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass  HF", col="red", xlab = 'year', ylab = 'agb (kg)')
#lines(_biomass_Choj_HF$Year, _biomass_Choj_HF$Biomass, type = 'l', col = 'green')
#lines(_biomass_Link_HF$Year, _biomass_Link_HF$Biomass, type = 'l', col = 'blue')
#legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(east_white_pine_biomass_Lamb_HF$Year, east_white_pine_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass Eastern White Pine HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_white_pine_biomass_Choj_HF$Year, east_white_pine_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(east_white_pine_biomass_Link_HF$Year, east_white_pine_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(north_red_oak_biomass_Lamb_HF$Year, north_red_oak_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass Northern Red Oak HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(north_red_oak_biomass_Choj_HF$Year, north_red_oak_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(north_red_oak_biomass_Link_HF$Year, north_red_oak_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(black_oak_biomass_Choj_HF$Year, black_oak_biomass_Choj_HF$Biomass, type = 'l', main = "Total biomass Black Oak HF", col="green", xlab = 'year', ylab = 'agb (kg)')
lines(black_oak_biomass_Link_HF$Year, black_oak_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
#lines(black_oak_biomass_Lamb_HF$Year, black_oak_biomass_Lamb_HF$Biomass, type = 'l', col = 'red')
legend(x = 'topleft', legend = c( 'Chojnacky', 'LinkAGES'), fill = c('green', 'blue'))

plot(east_hemlock_biomass_Lamb_HF$Year, east_hemlock_biomass_Lamb_HF$Biomass, type = 'l', main = "Total biomass Eastern Hemlock HF", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_hemlock_biomass_Choj_HF$Year, east_hemlock_biomass_Choj_HF$Biomass, type = 'l', col = 'green')
lines(east_hemlock_biomass_Link_HF$Year, east_hemlock_biomass_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at HF by Equation
t_agb_Lamb_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
t_agb_Choj_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))
t_agb_Link_HF <- data.frame('Year' = c(1:max_year_HF), "Biomass" =c(0))

t_agb_Lamb_HF$Biomass <- red_maple_biomass_Lamb_HF$Biomass + yellow_birch_biomass_Lamb_HF$Biomass + #sweet_birch_biomass_Lamb_HF$Biomass
  + american_beech_biomass_Lamb_HF$Biomass + east_white_pine_biomass_Lamb_HF$Biomass + north_red_oak_biomass_Lamb_HF$Biomass + east_hemlock_biomass_Lamb_HF$Biomass # + black_oak_biomass_Lamb_HF$Biomass
t_agb_Choj_HF$Biomass <- red_maple_biomass_Choj_HF$Biomass + yellow_birch_biomass_Choj_HF$Biomass + #sweet_birch_biomass_Choj_HF$Biomass
  + american_beech_biomass_Choj_HF$Biomass + east_white_pine_biomass_Choj_HF$Biomass + north_red_oak_biomass_Choj_HF$Biomass + east_hemlock_biomass_Lamb_HF$Biomass # + black_oak_biomass_Choj_HF$Biomass
t_agb_Link_HF$Biomass <- red_maple_biomass_Link_HF$Biomass + yellow_birch_biomass_Link_HF$Biomass + #sweet_birch_biomass_Link_HF$Biomass
  + american_beech_biomass_Link_HF$Biomass + east_white_pine_biomass_Link_HF$Biomass + north_red_oak_biomass_Link_HF$Biomass + east_hemlock_biomass_Lamb_HF$Biomass # + black_oak_biomass_Link_HF$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_HF$Year, t_agb_Lamb_HF$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method HF', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_HF$Year, t_agb_Choj_HF$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_HF$Year, t_agb_Link_HF$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/HF")
dev.off()

### Goose ###
# ACRU: red maple
# ACSA: sugar maple
# AMAR:
# BELE: sweet birch
# BEPA: paper birch
# FAGR: american beech
# OSVI: eastern hophornbeam
# PIST: eastern white pine
# QUAL: white oak
# QUMO: chestnut oak
# QURU: northern red oak


Goose_diam_data <- read.csv('GOOSE_diam_data.csv')


table(Goose_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

Goose_diam_data_no_inc <- subset(Goose_diam_data, select = c(-incr,-X))
Goose_diam_data_no_inc <-Goose_diam_data_no_inc %>% distinct()
max_year_Goose <- max(Goose_diam_data$year)

### Lambert species params Goose

red_maple_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.1014,2.3448,0.0052 + 0.0173,0.0291,2.0893,0.0027 + 0.0320,0.0515,1.5198,0.0065 + 0.0400,0.0175,2.4846,0.0034 + 0.0603 ))

sugar_maple_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                               Param_vals = c(0.1315,2.3129,0.0084 + 0.0182,0.0631,1.9241,0.0103 + 0.0464,0.0393,1.6930,0.0024 + 0.0196,0.0330,2.3741,0.0038 + 0.0341 ))

#_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                #   Param_vals = c())

#sweet_birch_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                     Param_vals = c()) does not seem to be present in Lambert paper

paper_birch_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                       Param_vals = c(0.0593, 2.5026, 0.0020 + 0.0114, 0.0135, 2.4053, 0.0008 + 0.0193, 0.0546, 1.6351, 0.0028 + 0.0189, 0.0135, 2.5532, 0.0009 + 0.0231))

american_beech_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.1478, 2.2986, 0.0202 + 0.0401, 0.0120, 2.2388, 0.0010 + 0.0256, 0.0376, 1.6164, 0.0055 + 0.0445, 0.0370, 2.3680, 0.0042 + 0.0353))

east_hophornbeam_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                      Param_vals = c(0.1929, 1.9672, 0.0431 + 0.0833, 0.0671, 1.5911, 0.0201 + 0.1317, 0.0293, 1.9502, 0.0081 + 0.1018, 0.0278, 2.1336, 0.0110 + 0.1417))

east_white_pine_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                      Param_vals = c(0.0997, 2.2709, 0.0129 + 0.0350, 0.0192, 2.2038, 0.0016 + 0.0237, 0.0284, 1.9375,0.0024 + 0.0248, 0.0056, 2.6011, 0.0008 + 0.0381))

white_oak_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                        Param_vals = c(0.0762, 2.3335, 0.0082 + 0.0373, 0.0338, 1.9845, 0.0019 + 0.0181, 0.0188, 1.7881, 0.0041 + 0.0671, 0.0113, 2.6211, 0.0023 + 0.0583))

#chestnut_oak_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                   Param_vals = c()) does not seem to e present in Lambert paper

north_red_oak_Lamb_Goose <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                    Param_vals = c(0.1754, 2.1616, 0.0149 + 0.0267, 0.0381, 2.0991, 0.0051 + 0.0403, 0.0373, 1.6740, 0.0029 + 0.0268, 0.0085, 2.7790, 0.0028 + 0.0979))

red_maple_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
sugar_maple_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#sweet_birch_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
paper_birch_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
american_beech_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
east_hophornbeam_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
east_white_pine_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
white_oak_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#chestnut_oak_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
north_red_oak_biomass_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))

for(i in 1:nrow(Goose_diam_data_no_inc)){
  year = Goose_diam_data_no_inc[i,2]
  dbh = Goose_diam_data_no_inc[i,6]
  if (Goose_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Lambert(species_params = red_maple_Lamb_Goose, D = dbh)
    red_maple_biomass_Lamb_Goose[year,2] = red_maple_biomass_Lamb_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Lambert(species_params = sugar_maple_Lamb_Goose, D = dbh)
    sugar_maple_biomass_Lamb_Goose[year,2] = sugar_maple_biomass_Lamb_Goose[year,2] + temp
  }
  #else if (Goose_diam_data_no_inc[i,5] == ''){
  # temp = Lambert(species_params = _Lamb_Goose, D = dbh)
  #_biomass_Lamb_Goose[year,2] = _biomass_Lamb_Goose[year,2] + temp
  #}
  #else if (Goose_diam_data_no_inc[i,5] == 'BELE'){
  # temp = Lambert(species_params = sweet_birch_Lamb_Goose, D = dbh)
  #sweet_birch_biomass_Lamb_Goose[year,2] = sweet_birch_biomass_Lamb_Goose[year,2] + temp
  #}
  else if (Goose_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Lambert(species_params = paper_birch_Lamb_Goose, D = dbh)
    paper_birch_biomass_Lamb_Goose[year,2] = paper_birch_biomass_Lamb_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Lambert(species_params = american_beech_Lamb_Goose, D = dbh)
    american_beech_biomass_Lamb_Goose[year,2] = american_beech_biomass_Lamb_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'OSVI'){
   temp = Lambert(species_params = east_hophornbeam_Lamb_Goose, D = dbh)
  east_hophornbeam_biomass_Lamb_Goose[year,2] = east_hophornbeam_biomass_Lamb_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'PIST'){
    temp = Lambert(species_params = east_white_pine_Lamb_Goose, D = dbh)
    east_white_pine_biomass_Lamb_Goose[year,2] = east_white_pine_biomass_Lamb_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QUAL'){
   temp = Lambert(species_params = white_oak_Lamb_Goose, D = dbh)
  white_oak_biomass_Lamb_Goose[year,2] = white_oak_biomass_Lamb_Goose[year,2] + temp
  }
  #else if (Goose_diam_data_no_inc[i,5] == 'QUMO'){
  # temp = Lambert(species_params = chestnut_oak_Lamb_Goose, D = dbh)
  #chestnut_oak_biomass_Lamb_Goose[year,2] = chestnut_oak_biomass_Lamb_Goose[year,2] + temp
  #}
  else if (Goose_diam_data_no_inc[i,5] == 'QURU'){
    temp = Lambert(species_params = north_red_oak_Lamb_Goose, D = dbh)
    north_red_oak_biomass_Lamb_Goose[year,2] = north_red_oak_biomass_Lamb_Goose[year,2] + temp
  }
  else{return}
}

### Chojnacky species params Goose

red_maple_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852)) ### Aceraceae >/.50 spg
sugar_maple_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852))
#_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c()) ###
sweet_birch_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2652, 2.5349)) ### Betulaceae ≥ 0.60 spg
paper_birch_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2271, 2.4513)) ### Betulaceae 0.40-0.49 psg
american_beech_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
east_hophornbeam_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2652, 2.5349)) ### Betulaceae ≥ 0.60 spg
east_white_pine_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.6177, 2.4638)) ### Pinus < 0.45 spg
white_oak_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
north_red_oak_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
chestnut_oak_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
east_hemlock_Choj_Goose <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.3480,2.3876)) ### Tsuga <.4 spg

red_maple_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
sugar_maple_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
sweet_birch_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
paper_birch_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
american_beech_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
east_hophornbeam_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
east_white_pine_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
white_oak_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
chestnut_oak_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
north_red_oak_biomass_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))

for(i in 1:nrow(Goose_diam_data_no_inc)){
  year = Goose_diam_data_no_inc[i,2]
  dbh = Goose_diam_data_no_inc[i,6]
  if (Goose_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Chojnacky(species_params = red_maple_Choj_Goose, dbh = dbh)
    red_maple_biomass_Choj_Goose[year,2] = red_maple_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Chojnacky(species_params = sugar_maple_Choj_Goose, dbh = dbh)
    sugar_maple_biomass_Choj_Goose[year,2] = sugar_maple_biomass_Choj_Goose[year,2] + temp
  }
  #else if (Goose_diam_data_no_inc[i,5] == ''){
  # temp = Chojnacky(species_params = _Choj_Goose, dbh = dbh)
  #_biomass_Choj_Goose[year,2] = _biomass_Choj_Goose[year,2] + temp
  #}
  else if (Goose_diam_data_no_inc[i,5] == 'BELE'){
   temp = Chojnacky(species_params = sweet_birch_Choj_Goose, dbh = dbh)
   sweet_birch_biomass_Choj_Goose[year,2] = sweet_birch_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Chojnacky(species_params = paper_birch_Choj_Goose, dbh = dbh)
    paper_birch_biomass_Choj_Goose[year,2] = paper_birch_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Chojnacky(species_params = american_beech_Choj_Goose, dbh = dbh)
    american_beech_biomass_Choj_Goose[year,2] = american_beech_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'OSVI'){
    temp = Chojnacky(species_params = east_hophornbeam_Choj_Goose, dbh = dbh)
    east_hophornbeam_biomass_Choj_Goose[year,2] = east_hophornbeam_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'PIST'){
    temp = Chojnacky(species_params = east_white_pine_Choj_Goose, dbh = dbh)
    east_white_pine_biomass_Choj_Goose[year,2] = east_white_pine_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QUAL'){
    temp = Chojnacky(species_params = white_oak_Choj_Goose, dbh = dbh)
    white_oak_biomass_Choj_Goose[year,2] = white_oak_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QUMO'){
   temp = Chojnacky(species_params = chestnut_oak_Choj_Goose, dbh = dbh)
   chestnut_oak_biomass_Choj_Goose[year,2] = chestnut_oak_biomass_Choj_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QURU'){
    temp = Chojnacky(species_params = north_red_oak_Choj_Goose, dbh = dbh)
    north_red_oak_biomass_Choj_Goose[year,2] = north_red_oak_biomass_Choj_Goose[year,2] + temp
  }
  else{return}
}

### LinkAGES species params Goose

red_maple_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.4075, 0.1396, 440))
sugar_maple_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6463, 0.1425, 440))
#_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c())
sweet_birch_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3766, 0.1624, 248))
paper_birch_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.409, 0.171, 248))
american_beech_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 3.4849, 0.1294, 440))
#east_hophornbeam_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(NA, 3.3085, 0.1141, NA))
east_white_pine_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(2, 1.7076, 0.1228, 440))
white_oak_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.3129, 0.1854, 440))
chestnut_oak_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.187, 0.185, 440))
north_red_oak_Link_Goose <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3967, 0.1508, 440))

red_maple_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
sugar_maple_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
sweet_birch_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
paper_birch_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
american_beech_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
#east_hophornbeam_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
east_white_pine_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
white_oak_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
chestnut_oak_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
north_red_oak_biomass_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))

for(i in 1:nrow(Goose_diam_data_no_inc)){
  year = Goose_diam_data_no_inc[i,2]
  dbh = Goose_diam_data_no_inc[i,6]
  if (Goose_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Linkages_allometry(species_params = red_maple_Link_Goose, dbh = dbh)
    red_maple_biomass_Link_Goose[year,2] = red_maple_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Linkages_allometry(species_params = sugar_maple_Link_Goose, dbh = dbh)
    sugar_maple_biomass_Link_Goose[year,2] = sugar_maple_biomass_Link_Goose[year,2] + temp
  }
  #else if (Goose_diam_data_no_inc[i,5] == ''){
  # temp = Linkages_allometry(species_params = _Link_Goose, dbh = dbh)
  #_biomass_Link_Goose[year,2] = _biomass_Link_Goose[year,2] + temp
  #}
  else if (Goose_diam_data_no_inc[i,5] == 'BELE'){
    temp = Linkages_allometry(species_params = sweet_birch_Link_Goose, dbh = dbh)
    sweet_birch_biomass_Link_Goose[year,2] = sweet_birch_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Linkages_allometry(species_params = paper_birch_Link_Goose, dbh = dbh)
    paper_birch_biomass_Link_Goose[year,2] = paper_birch_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Linkages_allometry(species_params = american_beech_Link_Goose, dbh = dbh)
    american_beech_biomass_Link_Goose[year,2] = american_beech_biomass_Link_Goose[year,2] + temp
  }
  #else if (Goose_diam_data_no_inc[i,5] == 'OSVI'){
   # temp = Linkages_allometry(species_params = east_hophornbeam_Link_Goose, dbh = dbh)
    #east_hophornbeam_biomass_Link_Goose[year,2] = east_hophornbeam_biomass_Link_Goose[year,2] + temp
  #}
  else if (Goose_diam_data_no_inc[i,5] == 'PIST'){
    temp = Linkages_allometry(species_params = east_white_pine_Link_Goose, dbh = dbh)
    east_white_pine_biomass_Link_Goose[year,2] = east_white_pine_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QUAL'){
    temp = Linkages_allometry(species_params = white_oak_Link_Goose, dbh = dbh)
    white_oak_biomass_Link_Goose[year,2] = white_oak_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QUMO'){
    temp = Linkages_allometry(species_params = chestnut_oak_Link_Goose, dbh = dbh)
    chestnut_oak_biomass_Link_Goose[year,2] = chestnut_oak_biomass_Link_Goose[year,2] + temp
  }
  else if (Goose_diam_data_no_inc[i,5] == 'QURU'){
    temp = Linkages_allometry(species_params = north_red_oak_Link_Goose, dbh = dbh)
    north_red_oak_biomass_Link_Goose[year,2] = north_red_oak_biomass_Link_Goose[year,2] + temp
  }
  else{return}
}

### Goose biomass visualizations ###

par(mfrow=c(2,2))
plot(red_maple_biomass_Lamb_Goose$Year, red_maple_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass Red Maple Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_maple_biomass_Choj_Goose$Year, red_maple_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(red_maple_biomass_Link_Goose$Year, red_maple_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(sugar_maple_biomass_Lamb_Goose$Year, sugar_maple_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass Sugar Maple Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(sugar_maple_biomass_Choj_Goose$Year, sugar_maple_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(sugar_maple_biomass_Link_Goose$Year, sugar_maple_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

#plot(_biomass_Lamb_Goose$Year, _biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
#lines(_biomass_Choj_Goose$Year, _biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
#lines(_biomass_Link_Goose$Year, _biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
#legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(sweet_birch_biomass_Choj_Goose$Year, paper_birch_biomass_Choj_Goose$Biomass, type = 'l', main = "Total biomass Sweet Birch Goose", col="green", xlab = 'year', ylab = 'agb (kg)')
lines(sweet_birch_biomass_Link_Goose$Year, paper_birch_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
#lines(sweet_birch_biomass_Lamb_Goose$Year, paper_birch_biomass_Lamb_Goose$Biomass, type = 'l', col = 'red')
legend(x = 'topleft', legend = c('Chojnacky', 'LinkAGES'), fill = c('green', 'blue'))

plot(american_beech_biomass_Lamb_Goose$Year, american_beech_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass American Beech Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(american_beech_biomass_Choj_Goose$Year, american_beech_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(american_beech_biomass_Link_Goose$Year, american_beech_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c('Lambert','Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(east_hophornbeam_biomass_Lamb_Goose$Year, east_hophornbeam_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass East Hophornbeam Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_hophornbeam_biomass_Choj_Goose$Year, east_hophornbeam_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
#lines(_biomass_Link_Goose$Year, _biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky' ), fill = c('red', 'green'))

plot(east_white_pine_biomass_Lamb_Goose$Year, east_white_pine_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass Eastern White Pine Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_white_pine_biomass_Choj_Goose$Year, east_white_pine_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(east_white_pine_biomass_Link_Goose$Year, east_white_pine_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(white_oak_biomass_Lamb_Goose$Year, white_oak_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass White Oak Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(white_oak_biomass_Choj_Goose$Year, white_oak_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(white_oak_biomass_Link_Goose$Year, white_oak_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

plot(chestnut_oak_biomass_Choj_Goose$Year, chestnut_oak_biomass_Choj_Goose$Biomass, type = 'l', main = "Total biomass Chestnut Oak Goose", col="green", xlab = 'year', ylab = 'agb (kg)')
lines(chestnut_oak_biomass_Link_Goose$Year, chestnut_oak_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
#lines(chestnut_oak_biomass_Lamb_Goose$Year, chestnut_oak_biomass_Lamb_Goose$Biomass, type = 'l', col = 'red')
legend(x = 'topleft', legend = c('Chojnacky', 'LinkAGES'), fill = c('green', 'blue'))

plot(north_red_oak_biomass_Lamb_Goose$Year, north_red_oak_biomass_Lamb_Goose$Biomass, type = 'l', main = "Total biomass Northern Red Oak Goose", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(north_red_oak_biomass_Choj_Goose$Year, north_red_oak_biomass_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(north_red_oak_biomass_Link_Goose$Year, north_red_oak_biomass_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at Goose by Equation
t_agb_Lamb_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
t_agb_Choj_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))
t_agb_Link_Goose <- data.frame('Year' = c(1:max_year_Goose), "Biomass" =c(0))

t_agb_Lamb_Goose$Biomass <- red_maple_biomass_Lamb_Goose$Biomass + sugar_maple_biomass_Lamb_Goose$Biomass + #sweet_birch_biomass_Lamb_Goose$Biomass
  + american_beech_biomass_Lamb_Goose$Biomass + east_white_pine_biomass_Lamb_Goose$Biomass + white_oak_biomass_Lamb_Goose$Biomass + north_red_oak_biomass_Lamb_Goose$Biomass # + east_hophornbeam_biomass_Lamb_Goose$Biomass + chestnut_oak_biomass_Lamb_Goose$Biomass
t_agb_Choj_Goose$Biomass <- red_maple_biomass_Choj_Goose$Biomass + sugar_maple_biomass_Choj_Goose$Biomass + #sweet_birch_biomass_Choj_Goose$Biomass
  + american_beech_biomass_Choj_Goose$Biomass + east_white_pine_biomass_Choj_Goose$Biomass + white_oak_biomass_Choj_Goose$Biomass + north_red_oak_biomass_Lamb_Goose$Biomass # + east_hophornbeam_biomass_Choj_Goose$Biomass + chestnut_oak_biomass_Choj_Goose$Biomass
t_agb_Link_Goose$Biomass <- red_maple_biomass_Link_Goose$Biomass + sugar_maple_biomass_Link_Goose$Biomass + #sweet_birch_biomass_Link_Goose$Biomass
  + american_beech_biomass_Link_Goose$Biomass + east_white_pine_biomass_Link_Goose$Biomass + white_oak_biomass_Link_Goose$Biomass + north_red_oak_biomass_Lamb_Goose$Biomass # + east_hophornbeam_biomass_Link_Goose$Biomass + chestnut_oak_biomass_Link_Goose$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_Goose$Year, t_agb_Lamb_Goose$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method Goose', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_Goose$Year, t_agb_Choj_Goose$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_Goose$Year, t_agb_Link_Goose$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/Goose")
dev.off()

### NRP ###
# ACRU: red maple
# ACSA: sugar maple
# BEAL: yellow birch
# BELE: sweet birch
# BEPA: paper birch
# FAGR: american beech
# FRAM: white ash
# PIST: eastern white pine
# PRSE: black cherry
# QURU: northern red oak
# TSCA: eastern hemlock


NRP_diam_data <- read.csv('NRP_diam_data.csv')


table(NRP_diam_data$taxon)
#species <- unique(SYL_diam_data$taxon)

NRP_diam_data_no_inc <- subset(NRP_diam_data, select = c(-incr,-X))
NRP_diam_data_no_inc <-NRP_diam_data_no_inc %>% distinct()
max_year_NRP <- max(NRP_diam_data$year)

### Lambert species params NRP

red_maple_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                   Param_vals = c(0.1014,2.3448,0.0052 + 0.0173,0.0291,2.0893,0.0027 + 0.0320,0.0515,1.5198,0.0065 + 0.0400,0.0175,2.4846,0.0034 + 0.0603 ))

sugar_maple_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.1315,2.3129,0.0084 + 0.0182,0.0631,1.9241,0.0103 + 0.0464,0.0393,1.6930,0.0024 + 0.0196,0.0330,2.3741,0.0038 + 0.0341 ))

yellow_birch_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.1932,2.1569,0.0193 + 0.0269,0.0192,2.2475,0.0017 + 0.0243,0.1119,1.3973,0.0121 + 0.0313,0.0305,2.4044,0.0024 + 0.0230 ))

#sweet_birch_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
#                                     Param_vals = c()) does not seem to be present in Lambert paper

paper_birch_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                     Param_vals = c(0.0593, 2.5026, 0.0020 + 0.0114, 0.0135, 2.4053, 0.0008 + 0.0193, 0.0546, 1.6351, 0.0028 + 0.0189, 0.0135, 2.5532, 0.0009 + 0.0231))

american_beech_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                        Param_vals = c(0.1478, 2.2986, 0.0202 + 0.0401, 0.0120, 2.2388, 0.0010 + 0.0256, 0.0376, 1.6164, 0.0055 + 0.0445, 0.0370, 2.3680, 0.0042 + 0.0353))

white_ash_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                   Param_vals = c(0.1861, 2.1665, 0.0142 + 0.0251, 0.0406, 1.9946, 0.0040 + 0.0308, 0.1106, 1.2277, 0.0276 + 0.0753, 0.0461, 2.2291, 0.0102 + 0.0695))

east_white_pine_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                         Param_vals = c(0.0997, 2.2709, 0.0129 + 0.0350, 0.0192, 2.2038, 0.0016 + 0.0237, 0.0284, 1.9375,0.0024 + 0.0248, 0.0056, 2.6011, 0.0008 + 0.0381))

black_cherry_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                       Param_vals = c(0.3743, 1.9406, 0.0573 + 0.0472, 0.0679, 1.8377, 0.0204 + 0.0883, 0.0840, 1.2319, 0.0053 + 0.0248, 0.0796, 2.0103, 0.0108 + 0.0483))

north_red_oak_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                       Param_vals = c(0.1754, 2.1616, 0.0149 + 0.0267, 0.0381, 2.0991, 0.0051 + 0.0403, 0.0373, 1.6740, 0.0029 + 0.0268, 0.0085, 2.7790, 0.0028 + 0.0979))

east_hemlock_Lamb_NRP <- data.frame(Parameter = c('b_wood1', 'b_wood2', 'e_wood', 'b_bark1', 'b_bark2', 'e_bark', 'b_foliage1', 'b_foliage2', 'e_foliage', 'b_branches1', 'b_branches2', 'e_branches'),
                                Param_vals = c(0.0619, 2.3821, 0.0030 + 0.0150, 0.0139, 2.3282, 0.0010 + 0.0210, 0.0776, 1.6995, 0.0069 + 0.0292, 0.0217, 2.2653, 0.0031 + 0.0420 ))

red_maple_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
sugar_maple_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
yellow_birch_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
#sweet_birch_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
paper_birch_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
american_beech_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
white_ash_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_white_pine_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
black_cherry_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
north_red_oak_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_hemlock_biomass_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))

for(i in 1:nrow(NRP_diam_data_no_inc)){
  year = NRP_diam_data_no_inc[i,2]
  dbh = NRP_diam_data_no_inc[i,6]
  if (NRP_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Lambert(species_params = red_maple_Lamb_NRP, D = dbh)
    red_maple_biomass_Lamb_NRP[year,2] = red_maple_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Lambert(species_params = sugar_maple_Lamb_NRP, D = dbh)
    sugar_maple_biomass_Lamb_NRP[year,2] = sugar_maple_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BEAL'){
   temp = Lambert(species_params = yellow_birch_Lamb_NRP, D = dbh)
   yellow_birch_biomass_Lamb_NRP[year,2] = yellow_birch_biomass_Lamb_NRP[year,2] + temp
  }
  #else if (NRP_diam_data_no_inc[i,5] == 'BELE'){
  # temp = Lambert(species_params = sweet_birch_Lamb_NRP, D = dbh)
  #sweet_birch_biomass_Lamb_NRP[year,2] = sweet_birch_biomass_Lamb_NRP[year,2] + temp
  #}
  else if (NRP_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Lambert(species_params = paper_birch_Lamb_NRP, D = dbh)
    paper_birch_biomass_Lamb_NRP[year,2] = paper_birch_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Lambert(species_params = american_beech_Lamb_NRP, D = dbh)
    american_beech_biomass_Lamb_NRP[year,2] = american_beech_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FRAM'){
    temp = Lambert(species_params = white_ash_Lamb_NRP, D = dbh)
    white_ash_biomass_Lamb_NRP[year,2] = white_ash_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PIST'){
    temp = Lambert(species_params = east_white_pine_Lamb_NRP, D = dbh)
    east_white_pine_biomass_Lamb_NRP[year,2] = east_white_pine_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Lambert(species_params = black_cherry_Lamb_NRP, D = dbh)
    black_cherry_biomass_Lamb_NRP[year,2] = black_cherry_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'QURU'){
    temp = Lambert(species_params = north_red_oak_Lamb_NRP, D = dbh)
    north_red_oak_biomass_Lamb_NRP[year,2] = north_red_oak_biomass_Lamb_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'TSCA'){
   temp = Lambert(species_params = east_hemlock_Lamb_NRP, D = dbh)
   east_hemlock_biomass_Lamb_NRP[year,2] = east_hemlock_biomass_Lamb_NRP[year,2] + temp
  }
  else{return}
}

### Chojnacky species params NRP

red_maple_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852)) ### Aceraceae >/.50 spg
sugar_maple_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8011, 2.3852))
yellow_birch_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8096, 2.3480)) ### Betulaceae 0.50-0.59 spg
sweet_birch_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2652, 2.5349)) ### Betulaceae ≥ 0.60 spg
paper_birch_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2271, 2.4513)) ### Betulaceae 0.40-0.49 psg
american_beech_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
white_ash_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-1.8384, 2.3524)) ### Oleaceae >/ 0.55 spg
east_white_pine_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.6177, 2.4638)) ### Pinus < 0.45 spg
black_cherry_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.2118, 2.4133)) ### Cornaceae/Ericaceae/Lauraceae/Patanaceae...
north_red_oak_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.0705, 2.4410)) ### Fagaceae, deciduous
east_hemlock_Choj_NRP <- data.frame(Parameter = c("b_0", 'b_1'), Value = c(-2.3480,2.3876)) ### Tsuga <.4 spg

red_maple_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
sugar_maple_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
yellow_birch_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
sweet_birch_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
paper_birch_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
american_beech_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
white_ash_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_white_pine_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
black_cherry_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
north_red_oak_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_hemlock_biomass_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))

for(i in 1:nrow(NRP_diam_data_no_inc)){
  year = NRP_diam_data_no_inc[i,2]
  dbh = NRP_diam_data_no_inc[i,6]
  if (NRP_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Chojnacky(species_params = red_maple_Choj_NRP, dbh = dbh)
    red_maple_biomass_Choj_NRP[year,2] = red_maple_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Chojnacky(species_params = sugar_maple_Choj_NRP, dbh = dbh)
    sugar_maple_biomass_Choj_NRP[year,2] = sugar_maple_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Chojnacky(species_params = yellow_birch_Choj_NRP, dbh = dbh)
    yellow_birch_biomass_Choj_NRP[year,2] = yellow_birch_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BELE'){
   temp = Chojnacky(species_params = sweet_birch_Choj_NRP, dbh = dbh)
   sweet_birch_biomass_Choj_NRP[year,2] = sweet_birch_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Chojnacky(species_params = paper_birch_Choj_NRP, dbh = dbh)
    paper_birch_biomass_Choj_NRP[year,2] = paper_birch_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Chojnacky(species_params = american_beech_Choj_NRP, dbh = dbh)
    american_beech_biomass_Choj_NRP[year,2] = american_beech_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FRAM'){
    temp = Chojnacky(species_params = white_ash_Choj_NRP, dbh = dbh)
    white_ash_biomass_Choj_NRP[year,2] = white_ash_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PIST'){
    temp = Chojnacky(species_params = east_white_pine_Choj_NRP, dbh = dbh)
    east_white_pine_biomass_Choj_NRP[year,2] = east_white_pine_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Chojnacky(species_params = black_cherry_Choj_NRP, dbh = dbh)
    black_cherry_biomass_Choj_NRP[year,2] = black_cherry_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'QURU'){
    temp = Chojnacky(species_params = north_red_oak_Choj_NRP, dbh = dbh)
    north_red_oak_biomass_Choj_NRP[year,2] = north_red_oak_biomass_Choj_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Chojnacky(species_params = east_hemlock_Choj_NRP, dbh = dbh)
    east_hemlock_biomass_Choj_NRP[year,2] = east_hemlock_biomass_Choj_NRP[year,2] + temp
  }
  else{return}
}

### LinkAGES species params NRP

red_maple_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.4075, 0.1396, 440))
sugar_maple_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6463, 0.1425, 440))
yellow_birch_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.6026, 0.1493, 248))
sweet_birch_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3766, 0.1624, 248))
paper_birch_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.409, 0.171, 248))
american_beech_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 3.4849, 0.1294, 440))
white_ash_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 1.7457, 0.1573, 440))
east_white_pine_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(2, 1.7076, 0.1228, 440))
black_cherry_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.0239, 0.1424, 173))
north_red_oak_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(1, 2.3967, 0.1508, 440))
east_hemlock_Link_NRP <- data.frame(Parameter = c("frt", 'slta', 'sltb', 'fwt'), Value = c(3, 2.6284, 0.1218, 440))

red_maple_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
sugar_maple_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
yellow_birch_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
sweet_birch_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
paper_birch_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
american_beech_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
white_ash_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_white_pine_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
black_cherry_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
north_red_oak_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
east_hemlock_biomass_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))

for(i in 1:nrow(NRP_diam_data_no_inc)){
  year = NRP_diam_data_no_inc[i,2]
  dbh = NRP_diam_data_no_inc[i,6]
  if (NRP_diam_data_no_inc[i,5] == 'ACRU'){
    temp = Linkages_allometry(species_params = red_maple_Link_NRP, dbh = dbh)
    red_maple_biomass_Link_NRP[year,2] = red_maple_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'ACSA'){
    temp = Linkages_allometry(species_params = sugar_maple_Link_NRP, dbh = dbh)
    sugar_maple_biomass_Link_NRP[year,2] = sugar_maple_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BEAL'){
    temp = Linkages_allometry(species_params = yellow_birch_Link_NRP, dbh = dbh)
    yellow_birch_biomass_Link_NRP[year,2] = yellow_birch_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BELE'){
    temp = Linkages_allometry(species_params = sweet_birch_Link_NRP, dbh = dbh)
    sweet_birch_biomass_Link_NRP[year,2] = sweet_birch_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'BEPA'){
    temp = Linkages_allometry(species_params = paper_birch_Link_NRP, dbh = dbh)
    paper_birch_biomass_Link_NRP[year,2] = paper_birch_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FAGR'){
    temp = Linkages_allometry(species_params = american_beech_Link_NRP, dbh = dbh)
    american_beech_biomass_Link_NRP[year,2] = american_beech_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'FRAM'){
    temp = Linkages_allometry(species_params = white_ash_Link_NRP, dbh = dbh)
    white_ash_biomass_Link_NRP[year,2] = white_ash_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PIST'){
    temp = Linkages_allometry(species_params = east_white_pine_Link_NRP, dbh = dbh)
    east_white_pine_biomass_Link_NRP[year,2] = east_white_pine_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'PRSE'){
    temp = Linkages_allometry(species_params = black_cherry_Link_NRP, dbh = dbh)
    black_cherry_biomass_Link_NRP[year,2] = black_cherry_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'QURU'){
    temp = Linkages_allometry(species_params = north_red_oak_Link_NRP, dbh = dbh)
    north_red_oak_biomass_Link_NRP[year,2] = north_red_oak_biomass_Link_NRP[year,2] + temp
  }
  else if (NRP_diam_data_no_inc[i,5] == 'TSCA'){
    temp = Linkages_allometry(species_params = east_hemlock_Link_NRP, dbh = dbh)
    east_hemlock_biomass_Link_NRP[year,2] = east_hemlock_biomass_Link_NRP[year,2] + temp
  }
  else{return}
}

### NRP biomass visualizations ###

par(mfrow=c(2,2))
plot(red_maple_biomass_Lamb_NRP$Year, red_maple_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Red Maple NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(red_maple_biomass_Choj_NRP$Year, red_maple_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(red_maple_biomass_Link_NRP$Year, red_maple_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(sugar_maple_biomass_Lamb_NRP$Year, sugar_maple_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Sugar Maple NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(sugar_maple_biomass_Choj_NRP$Year, sugar_maple_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(sugar_maple_biomass_Link_NRP$Year, sugar_maple_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(yellow_birch_biomass_Lamb_NRP$Year, yellow_birch_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Yellow Birch NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(yellow_birch_biomass_Choj_NRP$Year, yellow_birch_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(yellow_birch_biomass_Link_NRP$Year, yellow_birch_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(sweet_birch_biomass_Choj_NRP$Year, paper_birch_biomass_Choj_NRP$Biomass, type = 'l', main = "Total biomass Sweet Birch NRP", col="green", xlab = 'year', ylab = 'agb (kg)')
lines(sweet_birch_biomass_Link_NRP$Year, paper_birch_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
#lines(sweet_birch_biomass_Lamb_NRP$Year, paper_birch_biomass_Lamb_NRP$Biomass, type = 'l', col = 'red')
legend(x = 'topleft', legend = c('Chojnacky', 'LINKAGES'), fill = c('green', 'blue'))

plot(paper_birch_biomass_Lamb_NRP$Year, paper_birch_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Paper Birch NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(paper_birch_biomass_Choj_NRP$Year, paper_birch_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(paper_birch_biomass_Link_NRP$Year, paper_birch_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c('Lambert','Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(american_beech_biomass_Lamb_NRP$Year, american_beech_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass American Beech NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(american_beech_biomass_Choj_NRP$Year, american_beech_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(american_beech_biomass_Link_NRP$Year, american_beech_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c('Lambert','Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(white_ash_biomass_Lamb_NRP$Year, white_ash_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass White Ash NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(white_ash_biomass_Choj_NRP$Year, white_ash_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(white_ash_biomass_Link_NRP$Year, white_ash_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES' ), fill = c('red', 'green', 'blue'))

plot(east_white_pine_biomass_Lamb_NRP$Year, east_white_pine_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Eastern White Pine NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_white_pine_biomass_Choj_NRP$Year, east_white_pine_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(east_white_pine_biomass_Link_NRP$Year, east_white_pine_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(black_cherry_biomass_Lamb_NRP$Year, black_cherry_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Black Cherry NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(black_cherry_biomass_Choj_NRP$Year, black_cherry_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(black_cherry_biomass_Link_NRP$Year, black_cherry_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(north_red_oak_biomass_Lamb_NRP$Year, north_red_oak_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Northern Red Oak NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(north_red_oak_biomass_Choj_NRP$Year, north_red_oak_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(north_red_oak_biomass_Link_NRP$Year, north_red_oak_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

plot(east_hemlock_biomass_Lamb_NRP$Year, east_hemlock_biomass_Lamb_NRP$Biomass, type = 'l', main = "Total biomass Eastern Hemlock NRP", col="red", xlab = 'year', ylab = 'agb (kg)')
lines(east_hemlock_biomass_Choj_NRP$Year, east_hemlock_biomass_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(east_hemlock_biomass_Link_NRP$Year, east_hemlock_biomass_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LINKAGES'), fill = c('red', 'green', 'blue'))

# Total Biomass at NRP by Equation
t_agb_Lamb_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
t_agb_Choj_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))
t_agb_Link_NRP <- data.frame('Year' = c(1:max_year_NRP), "Biomass" =c(0))

t_agb_Lamb_NRP$Biomass <- red_maple_biomass_Lamb_NRP$Biomass + sugar_maple_biomass_Lamb_NRP$Biomass + yellow_birch_biomass_Lamb_NRP$Biomass + paper_birch_biomass_Lamb_NRP$Biomass + american_beech_biomass_Lamb_NRP$Biomass + white_ash_biomass_Lamb_NRP$Biomass + east_white_pine_biomass_Lamb_NRP$Biomass + black_cherry_biomass_Lamb_NRP$Biomass + north_red_oak_biomass_Lamb_NRP$Biomass  + east_hemlock_biomass_Lamb_NRP$Biomass # + sweet_birch_biomass_Lamb_NRP$Biomass
t_agb_Choj_NRP$Biomass <- red_maple_biomass_Choj_NRP$Biomass + sugar_maple_biomass_Choj_NRP$Biomass + yellow_birch_biomass_Choj_NRP$Biomass + paper_birch_biomass_Choj_NRP$Biomass + american_beech_biomass_Choj_NRP$Biomass + white_ash_biomass_Choj_NRP$Biomass + east_white_pine_biomass_Choj_NRP$Biomass + black_cherry_biomass_Choj_NRP$Biomass + north_red_oak_biomass_Lamb_NRP$Biomass  + east_hemlock_biomass_Choj_NRP$Biomass # + sweet_birch_biomass_Choj_NRP$Biomass
t_agb_Link_NRP$Biomass <- red_maple_biomass_Link_NRP$Biomass + sugar_maple_biomass_Link_NRP$Biomass + yellow_birch_biomass_Link_NRP$Biomass + paper_birch_biomass_Link_NRP$Biomass + american_beech_biomass_Link_NRP$Biomass + white_ash_biomass_Link_NRP$Biomass + east_white_pine_biomass_Link_NRP$Biomass + black_cherry_biomass_Link_NRP$Biomass + north_red_oak_biomass_Lamb_NRP$Biomass  + east_hemlock_biomass_Link_NRP$Biomass # + sweet_birch_biomass_Link_NRP$Biomass

par(mfrow=c(1,1))
plot(t_agb_Lamb_NRP$Year, t_agb_Lamb_NRP$Biomass, type = 'l', main = 'Total Aboveground Biomass by Allometry Method NRP', col = 'red', xlab = 'year', ylab = 'agb (kg)')
lines(t_agb_Choj_NRP$Year, t_agb_Choj_NRP$Biomass, type = 'l', col = 'green')
lines(t_agb_Link_NRP$Year, t_agb_Link_NRP$Biomass, type = 'l', col = 'blue')
legend(x = 'topleft', legend = c("Lambert", 'Chojnacky', 'LinkAGES'), fill = c('red', 'green', 'blue'))


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="biomass_dbh_reconstruction/NRP")
dev.off()

