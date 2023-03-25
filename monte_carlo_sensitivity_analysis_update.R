### Monte Carlo Sensitivity Analysis ###
rm(list=ls())
library(linkages)
library(Rmisc)
library(readr)
library(ggplot2)

### input the sampling distribution of parameter
### randomly pick from this distribution
### run the model with random parameter value,
### then run model ~200-300 times and determine the distribution of the agb
### must supply user inputs
### depending on site selected, must uncomment the correct site specific Input Adjustments
### must also decide whether you want to vary the met data or stick with the met with the highest weight

### User Inputs

set.seed(8)
site = "HF"
numruns = 2
#parameter = 'D3'

###Site specific inputs, uncomment correct site

###HF Input Adjustments


input = 'Harvard.input.Rdata'
load(input)

input = 'Harvard_Met_Input.Rdata'
load(input)
#
end.year = 2015
start.year = 1815
nyear <- 200
precip.mat <- tail(precip.mat, n = 201)
temp.mat <- tail(temp.mat, n = 201)
# ### only needed if doing climate scenarios
## use next 2 lines if you want to vary met data, randomly samples 10 out of ~200 possible mets
met_runs_HF <- read_csv('Met_updated/HF_met/weights/ensemble-weights-HARVARD-prism.csv')
met_runs_sample <- sample(met_runs_HF$climate_model, 10)
#met_runs_sample <- c('bcc.csm1.1_007.01', 'bcc.csm1.1_020.02', 'CCSM4_013.01', 'CCSM4_035.01', 'MIROC.ESM_020.01', 'MIROC.ESM_020.02', 'MIROC.ESM_024.01', 'MIROC.ESM_024.02', 'MIROC.ESM_028.01', 'MPI.ESM.P_018.01')


###NRP Input Adjustments

# input = 'NRP.linkages.input.Rdata'
# load(input)
#
# input = 'NRP_met_data_bcc.csm1.1_032.01.Rdata'
# load(input)
#
# end.year = 2015
# start.year = 1815
# nyear <- 200
# precip.mat <- tail(precip.mat, n = 201)
# temp.mat <- tail(temp.mat, n = 201)
# ### only needed if doing climate scenarios
## use next 2 lines if you want to vary met data, randomly samples 10 out of ~200 possible mets
# #  met_runs_NRP <- read_csv('Met_updated/NRP_met/weights/ensemble-weights-NRP-prism.csv')
# #  met_runs_sample <- sample(met_runs_NRP$climate_model, 10, replace = FALSE)

###Rooster Input Adjustments

# input = 'Rooster.linkages.input.Rdata'
# load(input)
#
# input = 'Rooster_met_data_MPI.ESM.P_032.01.Rdata'
# load(input)
#
# end.year = 2015
# start.year = 1815
# nyear <- 200
# precip.mat <- tail(precip.mat, n = 201)
# temp.mat <- tail(temp.mat, n = 201)
# ### only needed if doing climate scenarios
## use next 2 lines if you want to vary met data, randomly samples 10 out of ~200 possible mets
# met_runs_Rooster <- read_csv('Met_updated/Rooster_met/weights/ensemble-weights-ROOSTER-prism.csv')
# met_runs_sample <- sample(met_runs_Rooster$climate_model, 10, replace = FALSE)

###Goose Input Adjustments

# input = 'Goose.linkages.input.Rdata'
# load(input)
#
# input = 'Goose_Met_MPI.ESM.P_027.01.RData'
# load(input)
# #
# end.year = 2015
# start.year = 1715
# nyear <- 300
# precip.mat <- tail(precip.mat, n = 301)
# temp.mat <- tail(temp.mat, n = 301)

### only needed if doing climate scenarios
## use next 2 lines if you want to vary met data, randomly samples 10 out of ~200 possible mets
# met_runs_Goose <- read_csv('Met_updated/Goose_met/weights/ensemble-weights-GOOSE-prism.csv')
# met_runs_sample <- sample(met_runs_Goose$climate_model, 10, replace = FALSE)

###Sylvania Input Adjustments

# input = 'Sylvania.linkages.input.Rdata'
# load(input)
#
# input = 'Sylvania_met_data_bcc.csm1.1_024.02.Rdata'
# load(input)
#
# end.year = 2015
# start.year = 1715
# nyear <- 300
# precip.mat <- tail(precip.mat, n = 301)
# temp.mat <- tail(temp.mat, n = 301)
## use next 2 lines if you want to vary met data, randomly samples 10 out of ~200 possible mets
#met_runs_Sylvania <- read_csv('Met_updated/Sylvania_met/weights/ensemble-weights-SYLVANIA-prism.csv')
#met_runs_sample <- sample(met_runs_Sylvania$climate_model, 10, replace = FALSE)
#met_runs_sample <- c('bcc.csm1.1_033.02', 'MIROC.ESM_001.01', 'MPI.ESM.P_002.02', 'MIROC.ESM_024.02', 'CCSM4_013.02', 'MIROC.ESM_031.01', 'CCSM4_007.01', 'MPI.ESM.P_024.01', 'MIROC.ESM_035.02', 'MIROC.ESM_020.01')


#TSCA, FAGR, QURU, FRAM, BEAL, PIST, ACSA, ACRU

### Parameters to be varied for each species ###

red_oak_params <- data.frame('Spp_name' = c('red oak'), 'D_3_a' = c(249.392), 'D_3_b' = c(1185.544), 'MPLANT_a' = c(30), 'MPLANT_b' = c(50), 'DMAX_a' = c(6500), 'DMAX_b' = c(50), 'Frost_a' = c(-18), 'Frost_b' = c(2), "AGEMX_a" = c(301), 'AGEMX_b' = c(351), 'DMIN_a' = c(1100), 'DMIN_b' = c(50), 'G_a' = c(120), 'G_b' = c(20), 'SPRTND_a' = c(19), 'SPRTND_b' = c(1))
red_maple_params <- data.frame('Spp_name' = c('red maple'), 'D_3_a' = c(9.341), 'D_3_b' = c(107.419), 'MPLANT_a' = c(90), 'MPLANT_b' = c(110), 'DMAX_a' = c(8725), 'DMAX_b' = c(50), 'Frost_a' = c(-20), 'Frost_b' = c(2), "AGEMX_a" = c(275), 'AGEMX_b' = c(325), 'DMIN_a' = c(1000), 'DMIN_b' = c(50), 'G_a' = c(107), 'G_b' = c(1), 'SPRTND_a' = c(21), 'SPRTND_b' = c(1))
american_beech_params <- data.frame('Spp_name' = c('american beech'), 'D_3_a' = c(17.492), 'D_3_b' = c(251.619), 'MPLANT_a' = c(30), 'MPLANT_b' = c(50), 'DMAX_a' = c(7000), 'DMAX_b' = c(50), 'Frost_a' = c(-16.4), 'Frost_b' = c(2), "AGEMX_a" = c(325), 'AGEMX_b' = c(375), 'DMIN_a' = c(1250), 'DMIN_b' = c(50), 'G_a' = c(100), 'G_b' = c(10), 'SPRTND_a' = c(52), 'SPRTND_b' = c(1))
yellow_birch_params <- data.frame('Spp_name' = c('yellow birch'), 'D_3_a' = c(297.942), 'D_3_b' = c(1253.033), 'MPLANT_a' = c(110), 'MPLANT_b' = c(130), 'DMAX_a' = c(4250), 'DMAX_b' = c(50), 'Frost_a' = c(-16.4), 'Frost_b' = c(2), "AGEMX_a" = c(362), 'AGEMX_b' = c(412), 'DMIN_a' = c(1000), 'DMIN_b' = c(50), 'G_a' = c(70), 'G_b' = c(1), 'SPRTND_a' = c(43), 'SPRTND_b' = c(1))
east_hemlock_params <- data.frame('Spp_name' = c('east hemlock'), 'D_3_a' = c(9.341), 'D_3_b' = c(107.419), 'MPLANT_a' = c(5), 'MPLANT_b' = c(18), 'DMAX_a' = c(5500), 'DMAX_b' = c(50), 'Frost_a' = c(-17.6), 'Frost_b' = c(2), "AGEMX_a" = c(530), 'AGEMX_b' = c(580), 'DMIN_a' = c(1250), 'DMIN_b' = c(50), 'G_a' = c(50), 'G_b' = c(5), 'SPRTND_a' = c(0), 'SPRTND_b' = c(0))
white_ash_params <- data.frame('Spp_name' = c('white ash'), 'D_3_a' = c(109.238), 'D_3_b' = c(875.774), 'MPLANT_a' = c(40), 'MPLANT_b' = c(80), 'DMAX_a' = c(7250), 'DMAX_b' = c(50), 'Frost_a' = c(-16.4), 'Frost_b' = c(2), "AGEMX_a" = c(173), 'AGEMX_b' = c(223), 'DMIN_a' = c(1250), 'DMIN_b' = c(50), 'G_a' = c(150), 'G_b' = c(10), 'SPRTND_a' = c(52), 'SPRTND_b' = c(1))
east_white_pine_params <- data.frame('Spp_name' = c('eastern white pine'), 'D_3_a' = c(93.153), 'D_3_b' = c(821.011), 'MPLANT_a' = c(130), 'MPLANT_b' = c(150), 'DMAX_a' = c(5500), 'DMAX_b' = c(50), 'Frost_a' = c(-20), 'Frost_b' = c(2), "AGEMX_a" = c(383), 'AGEMX_b' = c(433), 'DMIN_a' = c(1000), 'DMIN_b' = c(50), 'G_a' = c(65), 'G_b' = c(1), 'SPRTND_a' = c(0), 'SPRTND_b' = c(0))
sugar_maple_params <- data.frame('Spp_name' = c('sugar maple'), 'D_3_a' = c(86.697), 'D_3_b' = c(797.067), 'MPLANT_a' = c(130), 'MPLANT_b' = c(150), 'DMAX_a' = c(5000), 'DMAX_b' = c(50), 'Frost_a' = c(-20), 'Frost_b' = c(2), "AGEMX_a" = c(290), 'AGEMX_b' = c(340), 'DMIN_a' = c(1000), 'DMIN_b' = c(50), 'G_a' = c(110), 'G_b' = c(1), 'SPRTND_a' = c(10), 'SPRTND_b' = c(1))
black_birch_params <- data.frame('Spp_name' = c('black birch'), 'D_3_a' = c(0.17), 'D_3_b' = c(0.277), 'MPLANT_a' = c(10), 'MPLANT_b' = c(30), 'DMAX_a' = c(5500), 'DMAX_b' = c(50), 'Frost_a' = c(-30), 'Frost_b' = c(7), "AGEMX_a" = c(335), 'AGEMX_b' = c(385), 'DMIN_a' = c(1250), 'DMIN_b' = c(100), 'G_a' = c(60), 'G_b' = c(10), 'SPRTND_a' = c(8), 'SPRTND_b' = c(1))
red_spruce_params <- data.frame('Spp_name' = c('red spruce'), 'D_3_a' = c(134.57), 'D_3_b' = c(950.67), 'MPLANT_a' = c(5), 'MPLANT_b' = c(18), 'DMAX_a' = c(2750), 'DMAX_b' = c(50), 'Frost_a' = c(-14), 'Frost_b' = c(2), "AGEMX_a" = c(420), 'AGEMX_b' = c(470), 'DMIN_a' = c(1247), 'DMIN_b' = c(50), 'G_a' = c(37), 'G_b' = c(1), 'SPRTND_a' = c(0), 'SPRTND_b' = c(0))
white_oak_params <- data.frame('Spp_name' = c('white oak'), 'D_3_a' = c(610.93), 'D_3_b' = c(1464.945), 'MPLANT_a' = c(30), 'MPLANT_b' = c(50), 'DMAX_a' = c(7250), 'DMAX_b' = c(50), 'Frost_a' = c(-14), 'Frost_b' = c(2), "AGEMX_a" = c(439), 'AGEMX_b' = c(489), 'DMIN_a' = c(1400), 'DMIN_b' = c(100), 'G_a' = c(90), 'G_b' = c(10), 'SPRTND_a' = c(16), 'SPRTND_b' = c(1))
chestnut_oak_params <- data.frame('Spp_name' = c('chestnut oak'), 'D_3_a' = c(571.026), 'D_3_b' = c(1453.71), 'MPLANT_a' = c(10), 'MPLANT_b' = c(30), 'DMAX_a' = c(5500), 'DMAX_b' = c(50), 'Frost_a' = c(-14), 'Frost_b' = c(2), "AGEMX_a" = c(400), 'AGEMX_b' = c(450), 'DMIN_a' = c(1500), 'DMIN_b' = c(100), 'G_a' = c(63), 'G_b' = c(10), 'SPRTND_a' = c(22), 'SPRTND_b' = c(1))
north_white_cedar_params <- data.frame('Spp_name' = c('northern white cedar'), 'D_3_a' = c(190.9284), 'D_3_b' = c(1082.555), 'MPLANT_a' = c(5), 'MPLANT_b' = c(18), 'DMAX_a' = c(3500), 'DMAX_b' = c(50), 'Frost_a' = c(-22), 'Frost_b' = c(3), "AGEMX_a" = c(975), 'AGEMX_b' = c(1025), 'DMIN_a' = c(900), 'DMIN_b' = c(65), 'G_a' = c(30.8864328), 'G_b' = c(3), 'SPRTND_a' = c(0), 'SPRTND_b' = c(0))
black_oak_params <- data.frame('Spp_name' = c('black oak'), 'D_3_a' = c(297.942), 'D_3_b' = c(1253.033), 'MPLANT_a' = c(10), 'MPLANT_b' = c(30), 'DMAX_a' = c(6750), 'DMAX_b' = c(50), 'Frost_a' = c(-12.8), 'Frost_b' = c(2), "AGEMX_a" = c(175), 'AGEMX_b' = c(225), 'DMIN_a' = c(1400), 'DMIN_b' = c(50), 'G_a' = c(100), 'G_b' = c(20), 'SPRTND_a' = c(8), 'SPRTND_b' = c(1))

# list of all parameters that are varied
all_parameters <- list('D3', 'MPLANT', 'DMAX', 'Frost', 'AGEMX', 'DMIN', 'G', 'SPRTND')

### Main monte carlo loop, takes in the parameter and the number of runs

monte_carlo_simulation <- function(parameter, numruns){
  start.time <<- Sys.time()
  if (site == 'HF'){
    individual_species_params <- rbind(red_maple_params, american_beech_params, red_oak_params, black_oak_params, east_hemlock_params)
  }
  else if (site == 'Goose'){
    individual_species_params <- rbind(american_beech_params, east_white_pine_params, white_oak_params, chestnut_oak_params, red_oak_params)
  }
  else if (site == 'NRP'){
    individual_species_params <- rbind(black_birch_params, east_white_pine_params, red_oak_params, east_hemlock_params)
  }
  else if (site == 'Rooster'){
    individual_species_params <- rbind(red_maple_params, american_beech_params, red_spruce_params, east_white_pine_params, red_oak_params)
  }
  else if (site == 'Sylvania'){
    individual_species_params <- rbind(sugar_maple_params, yellow_birch_params, north_white_cedar_params, east_hemlock_params)
  }
  ### need to come back to and improve speed, far too many loops, takes way too long to run efficiently
  ### only use outer for loop if varying met data lines ~159-167
  for (k in met_runs_sample){
    met = k
    input = paste0('Met_updated','/',site,'_met','/linkages/',met,'.Rdata')
    load(input)
    end.year <<- 2015
    start.year <<- 1815
    nyear <<- 200
    precip.mat <<- tail(precip.mat, n = 201)
    temp.mat <<- tail(temp.mat, n = 201)
    for (l in all_parameters){
      parameter = l
      for (j in 1:numruns){
        # pick one of the next two lines, 1st if varying met, 2nd if not
        ### if you choose to vary met, you then must comment out
        new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_no_update_',site,'/species_update/',met,'/',parameter)
        #new_dir = paste0('monte_carlo_sensitivity_analysis/',site,'/species_update/',parameter)
        if (!dir.exists(new_dir)){
          dir.create(new_dir, recursive = T)
        }
        if (parameter == 'D3'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,14] <<- rbeta(1, individual_species_params[i,2], individual_species_params[i,3])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'MPLANT'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,13] <<- runif(1, individual_species_params[i,4], individual_species_params[i,5])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'DMAX'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,3] <<- rnorm(1, individual_species_params[i,6], individual_species_params[i,7])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'Frost'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,15] <<- rnorm(1, individual_species_params[i,8], individual_species_params[i,9])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'AGEMX'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,8] <<- runif(1, individual_species_params[i,10], individual_species_params[i,11])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'DMIN'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,4] <<- rnorm(1, individual_species_params[i,12], individual_species_params[i,13])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'G'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,9] <<- rnorm(1, individual_species_params[i,14], individual_species_params[i,15])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)
        }
        else if (parameter == 'SPRTND'){
          for (i in 1:nrow(individual_species_params)){
            spp.params[i,10] <<- rnorm(1, individual_species_params[i,16], individual_species_params[i,17])
          }
          running_linkages(spp.params, parameter = parameter, runnum = j, met = met)

        }
      }
    }
  }
  end.time <<- Sys.time()
}

### running_linkages function is for saving each run in the correct directory

running_linkages <- function(spp.params, parameter, runnum, met){

  # create new run folder
  # choose correct new_dir based on whether or not you have chosen to vary met
  new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_no_update_',site,'/species_update/',met,'/',parameter,'/',runnum)
  #new_dir = paste0('monte_carlo_sensitivity_analysis/',site,'/species_update/',parameter,'/',runnum)
  if (!dir.exists(new_dir)){
    dir.create(new_dir, recursive = T)
  }

  # save new input file with new parameters
  save(clat=clat,fdat=fdat,precip.mat=precip.mat,spp.params=spp.params,switch.mat=switch.mat,
       temp.mat=temp.mat,basesc=basesc,basesn=basesn,bgs=bgs,dry=dry,egs=egs,end.year=end.year,fc=fc,
       iplot=iplot,max.ind=max.ind,nspec=nspec,nyear=nyear,plat=plat,start.year=start.year,
       file=paste0(new_dir,'/','linkages.input.Rdata'))

  # run model and save output in ensemble run directory
  linkages(linkages.input = paste0(new_dir,'/','linkages.input.Rdata'),
           outdir = new_dir)
}
#calling monte carlo simulation function and running
LAI_method = 'normal'
monte_carlo_simulation(parameter = parameter, numruns = numruns)
run_time = end.time - start.time
run_time
# final_biomass_redoak_D3 <- data.frame(spp_agbD3[1,nyear,])
# mean(final_biomass_redoak_D3$spp_agbD3.1..nyear...)
# CI(final_biomass_redoak_D3$spp_agbD3.1..nyear..., ci = 0.95)

# create save matrices
avg_age <- array(0,c(nspec,nyear,numruns))
avg_diam <- array(0,c(nspec,nyear,numruns))
tot_agb <- matrix(0,nyear,numruns)
spp_agb <- array(0,c(nspec,nyear,numruns))
spp_trees <- array(0,c(nspec,nyear,numruns))
spp_fcomp <- array(0,c(nspec,nyear,numruns))
spp_lgfval <- array(NA,c(nspec,nyear,numruns))
spp_lgfname <- array(NA,c(nspec,nyear,numruns))
spp_birth <- array(0,c(nspec,nyear,numruns))
spp_death <- array(0,c(nspec,nyear,numruns))
params <- array(0,c(nspec,26,numruns))
varied_param <- matrix(0,nspec,numruns)

### this loop, runs through all of the save folders for each parameter at the selected site and aggregates
### the data into matrices
### need this first for loop if you have varied climate data
### remember to select correct directory and correct save matrices before aggregating data
### uncomment next 2 lines if varying met data
for (m in met_runs_sample){
  met = m


  for (l in all_parameters){
    if (l == 'D3'){position = 14}
    else if (l == 'MPLANT'){position = 13}
    else if (l == 'DMAX'){position = 3}
    else if (l == 'Frost'){position = 15}
    else if (l == 'AGEMX'){position = 8}
    else if (l == 'DMIN'){position = 4}
    else if (l == 'G'){position = 9}
    else{position = 10}


    for (i in 1:numruns){

      # find directory with input and output file
      # this directory for use with changing met
      thisdir <- paste0('monte_carlo_sensitivity_analysis/',site,'/species_update/',met,'/',l,'/',toString(i))
      #this directory for use with constant met
      #thisdir <- paste0('monte_carlo_sensitivity_analysis/',site,'/species_update/',l,'/',toString(i))
      load(paste0(thisdir,'/','linkages.out.Rdata'))
      load(paste0(thisdir,'/','linkages.input.Rdata'))

      # collect parameter information

      params[,,i] <- as.double(as.matrix(spp.params))
      varied_param[,i] <- spp.params[,position]

      # collect annual data
      for (j in 1:nyear){
        st <- 1
        tot_agb[,i] <- ag.biomass[,1]

        # collect species-specific annual data
        for (k in 1:nspec){

          # determine indices of correct trees
          if (ntrees.kill[k,j,1]==0) next
          end <- st + ntrees.kill[k,j,1] - 1

          # store data
          spp_trees[k,j,i] <- ntrees.kill[k,j,1]
          spp_agb[k,j,i] <- agb.pft[k,j,1]
          spp_fcomp[k,j,i] <- f.comp[k,j]
          spp_lgfval[k,j,i] <- min(gf.vec.save[k,,j,1],na.rm=TRUE)
          spp_lgfname[k,j,i] <- which.min(gf.vec.save[k,,j,1])
          if (j != 1){spp_birth[k,j,i] <- ntrees.birth[k,j,1]-ntrees.kill[k,(j-1),1]} else {spp_birth[k,j,i] = ntrees.birth[k,j,1]}
          spp_death[k,j,i] <- ntrees.birth[k,j,1]-ntrees.kill[k,j,1]
          avg_age[k,j,i] <- mean(iage.save[(st:end),j,1])
          avg_diam[k,j,i] <- mean(dbh.save[(st:end),j,1])
          st <- end + 1
        }
      }
    }

    ### renames generic matrix to parameter specific matrix
    ## use these matrices with changing met
    assign(paste0("avg_age",l,m),avg_age)
    assign(paste0("avg_diam",l,m),avg_diam)
    assign(paste0("tot_agb",l,m),tot_agb)
    assign(paste0("spp_agb",l,m),spp_agb)
    assign(paste0("spp_trees",l,m),spp_trees)
    assign(paste0("spp_fcomp",l,m),spp_fcomp)
    assign(paste0("spp_lgfval",l,m),spp_lgfval)
    assign(paste0("spp_lgfname",l,m),spp_lgfname)
    assign(paste0("spp_birth",l,m),spp_birth)
    assign(paste0("spp_death",l,m),spp_death)
    assign(paste0("params",l,m),params)
    assign(paste0(l,m),varied_param)

    ## use these matrices with constant met
    # assign(paste0("avg_age",l),avg_age)
    # assign(paste0("avg_diam",l),avg_diam)
    # assign(paste0("tot_agb",l),tot_agb)
    # assign(paste0("spp_agb",l),spp_agb)
    # assign(paste0("spp_trees",l),spp_trees)
    # assign(paste0("spp_fcomp",l),spp_fcomp)
    # assign(paste0("spp_lgfval",l),spp_lgfval)
    # assign(paste0("spp_lgfname",l),spp_lgfname)
    # assign(paste0("spp_birth",l),spp_birth)
    # assign(paste0("spp_death",l),spp_death)
    # assign(paste0("params",l),params)
    # assign(paste0(l),varied_param)

  }
}

###This section is only if one climate scenario used goes to around line ~1120
###need to unhighlight correct site
###HF###

# final_biomass_redmaple_D3 <- data.frame(spp_agbD3[1,nyear,])
# final_biomass_americanbeech_D3 <- data.frame(spp_agbD3[2,nyear,])
# final_biomass_redoak_D3 <- data.frame(spp_agbD3[3,nyear,])
# final_biomass_blackoak_D3 <- data.frame(spp_agbD3[4,nyear,])
# final_biomass_easthemlock_D3 <- data.frame(spp_agbD3[5,nyear,])
#
# final_biomass_redmaple_MPLANT <- data.frame(spp_agbMPLANT[1,nyear,])
# final_biomass_americanbeech_MPLANT <- data.frame(spp_agbMPLANT[2,nyear,])
# final_biomass_redoak_MPLANT <- data.frame(spp_agbMPLANT[3,nyear,])
# final_biomass_blackoak_MPLANT <- data.frame(spp_agbMPLANT[4,nyear,])
# final_biomass_easthemlock_MPLANT <- data.frame(spp_agbMPLANT[5,nyear,])
#
# final_biomass_redmaple_DMAX <- data.frame(spp_agbDMAX[1,nyear,])
# final_biomass_americanbeech_DMAX <- data.frame(spp_agbDMAX[2,nyear,])
# final_biomass_redoak_DMAX <- data.frame(spp_agbDMAX[3,nyear,])
# final_biomass_blackoak_DMAX <- data.frame(spp_agbDMAX[4,nyear,])
# final_biomass_easthemlock_DMAX <- data.frame(spp_agbDMAX[5,nyear,])
#
# final_biomass_redmaple_DMIN <- data.frame(spp_agbDMIN[1,nyear,])
# final_biomass_americanbeech_DMIN <- data.frame(spp_agbDMIN[2,nyear,])
# final_biomass_redoak_DMIN <- data.frame(spp_agbDMIN[3,nyear,])
# final_biomass_blackoak_DMIN <- data.frame(spp_agbDMIN[4,nyear,])
# final_biomass_easthemlock_DMIN <- data.frame(spp_agbDMIN[5,nyear,])
#
# final_biomass_redmaple_Frost <- data.frame(spp_agbFrost[1,nyear,])
# final_biomass_americanbeech_Frost <- data.frame(spp_agbFrost[2,nyear,])
# final_biomass_redoak_Frost <- data.frame(spp_agbFrost[3,nyear,])
# final_biomass_blackoak_Frost <- data.frame(spp_agbFrost[4,nyear,])
# final_biomass_easthemlock_Frost <- data.frame(spp_agbFrost[5,nyear,])
#
# final_biomass_redmaple_G <- data.frame(spp_agbG[1,nyear,])
# final_biomass_americanbeech_G <- data.frame(spp_agbG[2,nyear,])
# final_biomass_redoak_G <- data.frame(spp_agbG[3,nyear,])
# final_biomass_blackoak_G <- data.frame(spp_agbG[4,nyear,])
# final_biomass_easthemlock_G <- data.frame(spp_agbG[5,nyear,])
#
# final_biomass_redmaple_AGEMX <- data.frame(spp_agbAGEMX[1,nyear,])
# final_biomass_americanbeech_AGEMX <- data.frame(spp_agbAGEMX[2,nyear,])
# final_biomass_redoak_AGEMX <- data.frame(spp_agbAGEMX[3,nyear,])
# final_biomass_blackoak_AGEMX <- data.frame(spp_agbAGEMX[4,nyear,])
# final_biomass_easthemlock_AGEMX <- data.frame(spp_agbAGEMX[5,nyear,])
#
# final_biomass_redmaple_SPRTND <- data.frame(spp_agbSPRTND[1,nyear,])
# final_biomass_americanbeech_SPRTND <- data.frame(spp_agbSPRTND[2,nyear,])
# final_biomass_redoak_SPRTND <- data.frame(spp_agbSPRTND[3,nyear,])
# final_biomass_blackoak_SPRTND<- data.frame(spp_agbSPRTND[4,nyear,])
# final_biomass_easthemlock_SPRTND <- data.frame(spp_agbSPRTND[5,nyear,])

###NRP###

# final_biomass_blackbirch_D3 <- data.frame(spp_agbD3[1,nyear,])
# final_biomass_eastwhitepine_D3 <- data.frame(spp_agbD3[2,nyear,])
# final_biomass_redoak_D3 <- data.frame(spp_agbD3[3,nyear,])
# final_biomass_easthemlock_D3 <- data.frame(spp_agbD3[4,nyear,])
#
# final_biomass_blackbirch_MPLANT <- data.frame(spp_agbMPLANT[1,nyear,])
# final_biomass_eastwhitepine_MPLANT <- data.frame(spp_agbMPLANT[2,nyear,])
# final_biomass_redoak_MPLANT <- data.frame(spp_agbMPLANT[3,nyear,])
# final_biomass_easthemlock_MPLANT <- data.frame(spp_agbMPLANT[4,nyear,])
#
# final_biomass_blackbirch_DMAX <- data.frame(spp_agbDMAX[1,nyear,])
# final_biomass_eastwhitepine_DMAX <- data.frame(spp_agbDMAX[2,nyear,])
# final_biomass_redoak_DMAX <- data.frame(spp_agbDMAX[3,nyear,])
# final_biomass_easthemlock_DMAX <- data.frame(spp_agbDMAX[4,nyear,])
#
# final_biomass_blackbirch_DMIN <- data.frame(spp_agbDMIN[1,nyear,])
# final_biomass_eastwhitepine_DMIN <- data.frame(spp_agbDMIN[2,nyear,])
# final_biomass_redoak_DMIN <- data.frame(spp_agbDMIN[3,nyear,])
# final_biomass_easthemlock_DMIN <- data.frame(spp_agbDMIN[4,nyear,])
#
# final_biomass_blackbirch_Frost <- data.frame(spp_agbFrost[1,nyear,])
# final_biomass_eastwhitepine_Frost <- data.frame(spp_agbFrost[2,nyear,])
# final_biomass_redoak_Frost <- data.frame(spp_agbFrost[3,nyear,])
# final_biomass_easthemlock_Frost <- data.frame(spp_agbFrost[4,nyear,])
#
# final_biomass_blackbirch_G <- data.frame(spp_agbG[1,nyear,])
# final_biomass_eastwhitepine_G <- data.frame(spp_agbG[2,nyear,])
# final_biomass_redoak_G <- data.frame(spp_agbG[3,nyear,])
# final_biomass_easthemlock_G <- data.frame(spp_agbG[4,nyear,])
#
# final_biomass_blackbirch_AGEMX <- data.frame(spp_agbAGEMX[1,nyear,])
# final_biomass_eastwhitepine_AGEMX <- data.frame(spp_agbAGEMX[2,nyear,])
# final_biomass_redoak_AGEMX <- data.frame(spp_agbAGEMX[3,nyear,])
# final_biomass_easthemlock_AGEMX <- data.frame(spp_agbAGEMX[4,nyear,])
#
# final_biomass_blackbirch_SPRTND <- data.frame(spp_agbSPRTND[1,nyear,])
# final_biomass_eastwhitepine_SPRTND <- data.frame(spp_agbSPRTND[2,nyear,])
# final_biomass_redoak_SPRTND <- data.frame(spp_agbSPRTND[3,nyear,])
# final_biomass_easthemlock_SPRTND <- data.frame(spp_agbSPRTND[4,nyear,])

###Rooster###

# final_biomass_redmaple_D3 <- data.frame(spp_agbD3[1,nyear,])
# final_biomass_americanbeech_D3 <- data.frame(spp_agbD3[2,nyear,])
# final_biomass_redspruce_D3 <- data.frame(spp_agbD3[3,nyear,])
# final_biomass_whitepine_D3 <- data.frame(spp_agbD3[4,nyear,])
# final_biomass_redoak_D3 <- data.frame(spp_agbD3[5,nyear,])
#
# final_biomass_redmaple_MPLANT <- data.frame(spp_agbMPLANT[1,nyear,])
# final_biomass_americanbeech_MPLANT <- data.frame(spp_agbMPLANT[2,nyear,])
# final_biomass_redspruce_MPLANT <- data.frame(spp_agbMPLANT[3,nyear,])
# final_biomass_whitepine_MPLANT <- data.frame(spp_agbMPLANT[4,nyear,])
# final_biomass_redoak_MPLANT <- data.frame(spp_agbMPLANT[5,nyear,])
#
# final_biomass_redmaple_DMAX <- data.frame(spp_agbDMAX[1,nyear,])
# final_biomass_americanbeech_DMAX <- data.frame(spp_agbDMAX[2,nyear,])
# final_biomass_redspruce_DMAX <- data.frame(spp_agbDMAX[3,nyear,])
# final_biomass_whitepine_DMAX <- data.frame(spp_agbDMAX[4,nyear,])
# final_biomass_redoak_DMAX <- data.frame(spp_agbDMAX[5,nyear,])
#
# final_biomass_redmaple_DMIN <- data.frame(spp_agbDMIN[1,nyear,])
# final_biomass_americanbeech_DMIN <- data.frame(spp_agbDMIN[2,nyear,])
# final_biomass_redspruce_DMIN <- data.frame(spp_agbDMIN[3,nyear,])
# final_biomass_whitepine_DMIN <- data.frame(spp_agbDMIN[4,nyear,])
# final_biomass_redoak_DMIN <- data.frame(spp_agbDMIN[5,nyear,])
#
# final_biomass_redmaple_Frost <- data.frame(spp_agbFrost[1,nyear,])
# final_biomass_americanbeech_Frost <- data.frame(spp_agbFrost[2,nyear,])
# final_biomass_redspruce_Frost <- data.frame(spp_agbFrost[3,nyear,])
# final_biomass_whitepine_Frost <- data.frame(spp_agbFrost[4,nyear,])
# final_biomass_redoak_Frost <- data.frame(spp_agbFrost[5,nyear,])
#
# final_biomass_redmaple_G <- data.frame(spp_agbG[1,nyear,])
# final_biomass_americanbeech_G <- data.frame(spp_agbG[2,nyear,])
# final_biomass_redspruce_G <- data.frame(spp_agbG[3,nyear,])
# final_biomass_whitepine_G <- data.frame(spp_agbG[4,nyear,])
# final_biomass_redoak_G <- data.frame(spp_agbG[5,nyear,])
#
# final_biomass_redmaple_AGEMX <- data.frame(spp_agbAGEMX[1,nyear,])
# final_biomass_americanbeech_AGEMX <- data.frame(spp_agbAGEMX[2,nyear,])
# final_biomass_redspruce_AGEMX <- data.frame(spp_agbAGEMX[3,nyear,])
# final_biomass_whitepine_AGEMX <- data.frame(spp_agbAGEMX[4,nyear,])
# final_biomass_redoak_AGEMX <- data.frame(spp_agbAGEMX[5,nyear,])
#
# final_biomass_redmaple_SPRTND <- data.frame(spp_agbSPRTND[1,nyear,])
# final_biomass_americanbeech_SPRTND <- data.frame(spp_agbSPRTND[2,nyear,])
# final_biomass_redspruce_SPRTND <- data.frame(spp_agbSPRTND[3,nyear,])
# final_biomass_whitepine_SPRTND <- data.frame(spp_agbSPRTND[4,nyear,])
# final_biomass_redoak_SPRTND <- data.frame(spp_agbSPRTND[5,nyear,])

###Goose###

# final_biomass_americanbeech_D3 <- data.frame(spp_agbD3[1,nyear,])
# final_biomass_whitepine_D3 <- data.frame(spp_agbD3[2,nyear,])
# final_biomass_whiteoak_D3 <- data.frame(spp_agbD3[3,nyear,])
# final_biomass_chestnutoak_D3 <- data.frame(spp_agbD3[4,nyear,])
# final_biomass_redoak_D3 <- data.frame(spp_agbD3[5,nyear,])
#
# final_biomass_americanbeech_MPLANT <- data.frame(spp_agbMPLANT[1,nyear,])
# final_biomass_whitepine_MPLANT <- data.frame(spp_agbMPLANT[2,nyear,])
# final_biomass_whiteoak_MPLANT <- data.frame(spp_agbMPLANT[3,nyear,])
# final_biomass_chestnutoak_MPLANT <- data.frame(spp_agbMPLANT[4,nyear,])
# final_biomass_redoak_MPLANT <- data.frame(spp_agbMPLANT[5,nyear,])
#
# final_biomass_americanbeech_DMAX <- data.frame(spp_agbDMAX[1,nyear,])
# final_biomass_whitepine_DMAX <- data.frame(spp_agbDMAX[2,nyear,])
# final_biomass_whiteoak_DMAX <- data.frame(spp_agbDMAX[3,nyear,])
# final_biomass_chestnutoak_DMAX <- data.frame(spp_agbDMAX[4,nyear,])
# final_biomass_redoak_DMAX <- data.frame(spp_agbDMAX[5,nyear,])
#
# final_biomass_americanbeech_DMIN <- data.frame(spp_agbDMIN[1,nyear,])
# final_biomass_whitepine_DMIN <- data.frame(spp_agbDMIN[2,nyear,])
# final_biomass_whiteoak_DMIN <- data.frame(spp_agbDMIN[3,nyear,])
# final_biomass_chestnutoak_DMIN <- data.frame(spp_agbDMIN[4,nyear,])
# final_biomass_redoak_DMIN <- data.frame(spp_agbDMIN[5,nyear,])
#
# final_biomass_americanbeech_Frost <- data.frame(spp_agbFrost[1,nyear,])
# final_biomass_whitepine_Frost <- data.frame(spp_agbFrost[2,nyear,])
# final_biomass_whiteoak_Frost <- data.frame(spp_agbFrost[3,nyear,])
# final_biomass_chestnutoak_Frost <- data.frame(spp_agbFrost[4,nyear,])
# final_biomass_redoak_Frost <- data.frame(spp_agbFrost[5,nyear,])
#
# final_biomass_americanbeech_G <- data.frame(spp_agbG[1,nyear,])
# final_biomass_whitepine_G <- data.frame(spp_agbG[2,nyear,])
# final_biomass_whiteoak_G <- data.frame(spp_agbG[3,nyear,])
# final_biomass_chestnutoak_G <- data.frame(spp_agbG[4,nyear,])
# final_biomass_redoak_G <- data.frame(spp_agbG[5,nyear,])
#
# final_biomass_americanbeech_AGEMX <- data.frame(spp_agbAGEMX[1,nyear,])
# final_biomass_whitepine_AGEMX <- data.frame(spp_agbAGEMX[2,nyear,])
# final_biomass_whiteoak_AGEMX <- data.frame(spp_agbAGEMX[3,nyear,])
# final_biomass_chestnutoak_AGEMX <- data.frame(spp_agbAGEMX[4,nyear,])
# final_biomass_redoak_AGEMX <- data.frame(spp_agbAGEMX[5,nyear,])
#
# final_biomass_americanbeech_SPRTND <- data.frame(spp_agbSPRTND[1,nyear,])
# final_biomass_whitepine_SPRTND <- data.frame(spp_agbSPRTND[2,nyear,])
# final_biomass_whiteoak_SPRTND <- data.frame(spp_agbSPRTND[3,nyear,])
# final_biomass_chestnutoak_SPRTND <- data.frame(spp_agbSPRTND[4,nyear,])
# final_biomass_redoak_SPRTND <- data.frame(spp_agbSPRTND[5,nyear,])

###Sylvania###

# final_biomass_sugarmaple_D3 <- data.frame(spp_agbD3[1,nyear,])
# final_biomass_yellowbirch_D3 <- data.frame(spp_agbD3[2,nyear,])
# final_biomass_northwhitecedar_D3 <- data.frame(spp_agbD3[3,nyear,])
# final_biomass_easthemlock_D3 <- data.frame(spp_agbD3[4,nyear,])
#
# final_biomass_sugarmaple_MPLANT <- data.frame(spp_agbMPLANT[1,nyear,])
# final_biomass_yellowbirch_MPLANT <- data.frame(spp_agbMPLANT[2,nyear,])
# final_biomass_northwhitecedar_MPLANT <- data.frame(spp_agbMPLANT[3,nyear,])
# final_biomass_easthemlock_MPLANT <- data.frame(spp_agbMPLANT[4,nyear,])
#
# final_biomass_sugarmaple_DMAX <- data.frame(spp_agbDMAX[1,nyear,])
# final_biomass_yellowbirch_DMAX <- data.frame(spp_agbDMAX[2,nyear,])
# final_biomass_northwhitecedar_DMAX <- data.frame(spp_agbDMAX[3,nyear,])
# final_biomass_easthemlock_DMAX <- data.frame(spp_agbDMAX[4,nyear,])
#
# final_biomass_sugarmaple_DMIN <- data.frame(spp_agbDMIN[1,nyear,])
# final_biomass_yellowbirch_DMIN <- data.frame(spp_agbDMIN[2,nyear,])
# final_biomass_northwhitecedar_DMIN <- data.frame(spp_agbDMIN[3,nyear,])
# final_biomass_easthemlock_DMIN <- data.frame(spp_agbDMIN[4,nyear,])
#
# final_biomass_sugarmaple_Frost <- data.frame(spp_agbFrost[1,nyear,])
# final_biomass_yellowbirch_Frost <- data.frame(spp_agbFrost[2,nyear,])
# final_biomass_northwhitecedar_Frost <- data.frame(spp_agbFrost[3,nyear,])
# final_biomass_easthemlock_Frost <- data.frame(spp_agbFrost[4,nyear,])
#
# final_biomass_sugarmaple_G <- data.frame(spp_agbG[1,nyear,])
# final_biomass_yellowbirch_G <- data.frame(spp_agbG[2,nyear,])
# final_biomass_northwhitecedar_G <- data.frame(spp_agbG[3,nyear,])
# final_biomass_easthemlock_G <- data.frame(spp_agbG[4,nyear,])
#
# final_biomass_sugarmaple_AGEMX <- data.frame(spp_agbAGEMX[1,nyear,])
# final_biomass_yellowbirch_AGEMX <- data.frame(spp_agbAGEMX[2,nyear,])
# final_biomass_northwhitecedar_AGEMX <- data.frame(spp_agbAGEMX[3,nyear,])
# final_biomass_easthemlock_AGEMX <- data.frame(spp_agbAGEMX[4,nyear,])
#
# final_biomass_sugarmaple_SPRTND <- data.frame(spp_agbSPRTND[1,nyear,])
# final_biomass_yellowbirch_SPRTND <- data.frame(spp_agbSPRTND[2,nyear,])
# final_biomass_northwhitecedar_SPRTND <- data.frame(spp_agbSPRTND[3,nyear,])
# final_biomass_easthemlock_SPRTND <- data.frame(spp_agbSPRTND[4,nyear,])

#mean(final_biomass_blackbirch_D3$spp_agbD3.1..nyear...)
# CI(final_biomass_redoak_D3$spp_agbD3.1..nyear..., ci = 0.95)
# aggregates final year agb
D3_final_agb <- data.frame(tot_agbD3[nyear,])
DMAX_final_agb <- data.frame(tot_agbDMAX[nyear,])
DMIN_final_agb <- data.frame(tot_agbDMIN[nyear,])
AGEMX_final_agb <- data.frame(tot_agbAGEMX[nyear,])
Frost_final_agb <- data.frame(tot_agbFrost[nyear,])
G_final_agb <- data.frame(tot_agbG[nyear,])
SPRTND_final_agb <- data.frame(tot_agbSPRTND[nyear,])
MPLANT_final_agb <- data.frame(tot_agbMPLANT[nyear,])

# # calculate the confidence interval for the final agb for each parameter
#
# D3_final_agb_CI <- CI(D3_final_agb$tot_agbD3.nyear..., ci = 0.95)
# DMAX_final_agb_CI <- CI(DMAX_final_agb$tot_agbDMAX.nyear...,ci = 0.95)
# DMIN_final_agb_CI <- CI(DMIN_final_agb$tot_agbDMIN.nyear..., ci = 0.95)
# AGEMX_final_agb_CI <- CI(AGEMX_final_agb$tot_agbAGEMX.nyear..., ci = 0.95)
# Frost_final_agb_CI <- CI(Frost_final_agb$tot_agbFrost.nyear..., ci = 0.95)
# G_final_agb_CI <- CI(G_final_agb$tot_agbG.nyear..., ci = 0.95)
# SPRTND_final_agb_CI <- CI(SPRTND_final_agb$tot_agbSPRTND.nyear..., ci = 0.95)
# MPLANT_final_agb_CI <- CI(MPLANT_final_agb$tot_agbMPLANT.nyear..., ci = 0.95)
#
# all_parameters <- data.frame('D3', 'MPLANT', 'DMAX', 'Frost', 'AGEMX', 'DMIN', 'G', 'SPRTND')
# all_parameters <- t(all_parameters)
#
# all_parameters_CI <- rbind(D3_final_agb_CI, MPLANT_final_agb_CI, DMAX_final_agb_CI, Frost_final_agb_CI, AGEMX_final_agb_CI, DMIN_final_agb_CI, G_final_agb_CI, SPRTND_final_agb_CI)
# all_parameters_CI <- data.frame(all_parameters_CI)
# all_parameters_CI <- cbind(all_parameters_CI, all_parameters)
#
# # Visualization Parameter CI #
#
#
# par(mfrow = c(1,1))
# ggplot(all_parameters_CI, aes(x = all_parameters_CI$all_parameters, y = all_parameters_CI$mean)) + geom_point(position = position_dodge(width = 0.4), size = 4) + geom_errorbar(aes(ymax = all_parameters_CI$upper, ymin = all_parameters_CI$lower)) + labs(title = paste0('Confidence Intervals for Parameter Estimates ',site), x = ('parameter'), y = 'agb')

#uncomment correct site visuals
## HF visuals
#D3
# par(mfrow = c(2,3))
# plot(D3[1,], final_biomass_redmaple_D3$spp_agbD3.1..nyear..., title(main = 'D3 Red Maple HF'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redmaple_D3$spp_agbD3.1..nyear...~D3[1,]), col = 'red')
# lines(lowess(D3[1,],final_biomass_redmaple_D3$spp_agbD3.1..nyear...), col = 'blue')
# plot(D3[2,], final_biomass_americanbeech_D3$spp_agbD3.2..nyear..., title(main = 'D3 American Beech HF'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_americanbeech_D3$spp_agbD3.2..nyear...~D3[2,]), col = 'red')
# lines(lowess(D3[2,],final_biomass_americanbeech_D3$spp_agbD3.2..nyear...), col = 'blue')
# plot(D3[3,], final_biomass_redoak_D3$spp_agbD3.3..nyear..., title(main = 'D3 Red Oak HF'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redoak_D3$spp_agbD3.3..nyear...~D3[3,]), col = 'red')
# lines(lowess(D3[3,],final_biomass_redoak_D3$spp_agbD3.3..nyear...), col = 'blue')
# plot(D3[4,], final_biomass_blackoak_D3$spp_agbD3.4..nyear..., title(main = 'D3 Black Oak HF'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_blackoak_D3$spp_agbD3.4..nyear...~D3[4,]), col = 'red')
# lines(lowess(D3[4,],final_biomass_blackoak_D3$spp_agbD3.4..nyear...), col = 'blue')
# plot(D3[5,], final_biomass_easthemlock_D3$spp_agbD3.5..nyear..., title(main = 'D3 Eastern Hemlock HF'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_easthemlock_D3$spp_agbD3.5..nyear...~D3[5,]), col = 'red')
# lines(lowess(D3[5,],final_biomass_easthemlock_D3$spp_agbD3.5..nyear...), col = 'blue')
#
#
# #DMAX
# par(mfrow = c(2,3))
# plot(DMAX[1,], final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear..., title(main = 'DMAX Red Maple HF'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear...~DMAX[1,]), col = 'red')
# lines(lowess(DMAX[1,],final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear...), col = 'blue')
# plot(DMAX[2,], final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear..., title(main = 'DMAX American Beech HF'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear...~DMAX[2,]), col = 'red')
# lines(lowess(DMAX[2,],final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear...), col = 'blue')
# plot(DMAX[3,], final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear..., title(main = 'DMAX Red Oak HF'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear...~DMAX[3,]), col = 'red')
# lines(lowess(DMAX[3,],final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear...), col = 'blue')
# plot(DMAX[4,], final_biomass_blackoak_DMAX$spp_agbDMAX.4..nyear..., title(main = 'DMAX Black Oak HF'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_blackoak_DMAX$spp_agbDMAX.4..nyear...~DMAX[4,]), col = 'red')
# lines(lowess(DMAX[4,],final_biomass_blackoak_DMAX$spp_agbDMAX.4..nyear...), col = 'blue')
# plot(DMAX[5,], final_biomass_easthemlock_DMAX$spp_agbDMAX.5..nyear..., title(main = 'DMAX Eastern Hemlock HF'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_easthemlock_DMAX$spp_agbDMAX.5..nyear...~DMAX[5,]), col = 'red')
# lines(lowess(DMAX[5,],final_biomass_easthemlock_DMAX$spp_agbDMAX.5..nyear...), col = 'blue')
#
#
# #DMIN
# par(mfrow = c(2,3))
# plot(DMIN[1,], final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear..., title(main = 'DMIN Red Maple HF'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[1,],final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear...), col = 'blue')
# abline(lm(final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear...~DMIN[1,]), col = 'red')
# plot(DMIN[2,], final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear..., title(main = 'DMIN American Beech HF'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[2,],final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear...), col = 'blue')
# abline(lm(final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear...~DMIN[2,]), col = 'red')
# plot(DMIN[3,], final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear..., title(main = 'DMIN Red Oak HF'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[3,], final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear...), col = 'blue')
# abline(lm(final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear...~DMIN[3,]), col = 'red')
# plot(DMIN[4,], final_biomass_blackoak_DMIN$spp_agbDMIN.4..nyear..., title(main = 'DMIN Black Oak HF'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[4,],final_biomass_blackoak_DMIN$spp_agbDMIN.4..nyear...), col = 'blue')
# abline(lm(final_biomass_blackoak_DMIN$spp_agbDMIN.4..nyear...~DMIN[4,]), col = 'red')
# plot(DMIN[5,], final_biomass_easthemlock_DMIN$spp_agbDMIN.5..nyear..., title(main = 'DMIN Eastern Hemlock HF'), ylab = 'agb', xlab = 'DMIN')
# abline(lm(final_biomass_easthemlock_DMIN$spp_agbDMIN.5..nyear...~DMIN[5,]), col = 'red')
# lines(lowess(DMIN[5,],final_biomass_easthemlock_DMIN$spp_agbDMIN.5..nyear...), col = 'blue')
#
#
# #Frost
# par(mfrow = c(2,3))
# plot(Frost[1,], final_biomass_redmaple_Frost$spp_agbFrost.1..nyear..., title(main = 'Frost Red Maple HF'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redmaple_Frost$spp_agbFrost.1..nyear...~Frost[1,]), col = 'red')
# lines(lowess(Frost[1,],final_biomass_redmaple_Frost$spp_agbFrost.1..nyear...), col = 'blue')
# plot(Frost[2,], final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear..., title(main = 'Frost American Beech HF'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear...~Frost[2,]), col = 'red')
# lines(lowess(Frost[2,],final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear...), col = 'blue')
# plot(Frost[3,], final_biomass_redoak_Frost$spp_agbFrost.3..nyear..., title(main = 'Frost Red Oak HF'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redoak_Frost$spp_agbFrost.3..nyear...~Frost[3,]), col = 'red')
# lines(lowess(Frost[3,],final_biomass_redoak_Frost$spp_agbFrost.3..nyear...), col = 'blue')
# plot(Frost[4,], final_biomass_blackoak_Frost$spp_agbFrost.4..nyear..., title(main = 'Frost Black Oak HF'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_blackoak_Frost$spp_agbFrost.4..nyear...~Frost[4,]), col = 'red')
# lines(lowess(Frost[4,],final_biomass_blackoak_Frost$spp_agbFrost.4..nyear...), col = 'blue')
# plot(Frost[5,], final_biomass_easthemlock_Frost$spp_agbFrost.5..nyear..., title(main = 'Frost Eastern Hemlock HF'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_easthemlock_Frost$spp_agbFrost.5..nyear...~Frost[5,]), col = 'red')
# lines(lowess(Frost[5,],final_biomass_easthemlock_Frost$spp_agbFrost.5..nyear...), col = 'blue')
#
#
# #G
# par(mfrow = c(2,3))
#
# plot(G[1,], final_biomass_redmaple_G$spp_agbG.1..nyear..., title(main = 'G Red Maple HF'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redmaple_G$spp_agbG.1..nyear...~G[1,]), col = 'red')
# lines(lowess(G[1,],final_biomass_redmaple_G$spp_agbG.1..nyear...), col = 'blue')
# plot(G[2,], final_biomass_americanbeech_G$spp_agbG.2..nyear..., title(main = 'G American Beech HF'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_americanbeech_G$spp_agbG.2..nyear...~G[2,]), col = 'red')
# lines(lowess(G[2,],final_biomass_americanbeech_G$spp_agbG.2..nyear...), col = 'blue')
# plot(G[3,], final_biomass_redoak_G$spp_agbG.3..nyear..., title(main = 'G Red Oak HF'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redoak_G$spp_agbG.3..nyear...~G[3,]), col = 'red')
# lines(lowess(G[3,],final_biomass_redoak_G$spp_agbG.3..nyear...), col = 'blue')
# plot(G[4,], final_biomass_blackoak_G$spp_agbG.4..nyear..., title(main = 'G Black Oak HF'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_blackoak_G$spp_agbG.4..nyear...~G[4,]), col = 'red')
# lines(lowess(G[4,],final_biomass_blackoak_G$spp_agbG.4..nyear...), col = 'blue')
# plot(G[5,], final_biomass_easthemlock_G$spp_agbG.5..nyear..., title(main = 'G Eastern Hemlock HF'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_easthemlock_G$spp_agbG.5..nyear...~G[5,]), col = 'red')
# lines(lowess(G[5,],final_biomass_easthemlock_G$spp_agbG.5..nyear...), col = 'blue')
#
#
#
# #SPRTND
# par(mfrow = c(2,3))
# plot(SPRTND[1,], final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear..., title(main = 'SPRTND Red Maple HF'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear...~SPRTND[1,]), col = 'red')
# lines(lowess(SPRTND[1,],final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear...), col = 'blue')
# plot(SPRTND[2,], final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear..., title(main = 'SPRTND American Beech HF'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear...~SPRTND[2,]), col = 'red')
# lines(lowess(SPRTND[2,],final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear...), col = 'blue')
# plot(SPRTND[3,], final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear..., title(main = 'SPRTND Red Oak HF'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear...~SPRTND[3,]), col = 'red')
# lines(lowess(SPRTND[3,],final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear...), col = 'blue')
# plot(SPRTND[4,], final_biomass_blackoak_SPRTND$spp_agbSPRTND.4..nyear..., title(main = 'SPRTND Black Oak HF'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_blackoak_SPRTND$spp_agbSPRTND.4..nyear...~SPRTND[4,]), col = 'red')
# lines(lowess(SPRTND[4,],final_biomass_blackoak_SPRTND$spp_agbSPRTND.4..nyear...), col = 'blue')
# plot(SPRTND[5,], final_biomass_easthemlock_SPRTND$spp_agbSPRTND.5..nyear..., title(main = 'SPRTND Eastern Hemlock HF'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_easthemlock_SPRTND$spp_agbSPRTND.5..nyear...~SPRTND[5,]), col = 'red')
# lines(lowess(SPRTND[5,],final_biomass_easthemlock_SPRTND$spp_agbSPRTND.5..nyear...), col = 'blue')
#
#
#
# #MPLANT
# par(mfrow = c(2,3))
# plot(MPLANT[1,], final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear..., title(main = 'MPLANT Red Maple HF'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear...~MPLANT[1,]), col = 'red')
# lines(lowess(MPLANT[1,],final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear...), col = 'blue')
# plot(MPLANT[2,], final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear..., title(main = 'MPLANT American Beech HF'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear...~MPLANT[2,]), col = 'red')
# lines(lowess(MPLANT[2,],final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear...), col = 'blue')
# plot(MPLANT[3,], final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear..., title(main = 'MPLANT Red Oak HF'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear...~MPLANT[3,]), col = 'red')
# lines(lowess(MPLANT[3,],final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear...), col = 'blue')
# plot(MPLANT[4,], final_biomass_blackoak_MPLANT$spp_agbMPLANT.4..nyear..., title(main = 'MPLANT Black Oak HF'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_blackoak_MPLANT$spp_agbMPLANT.4..nyear...~MPLANT[4,]), col = 'red')
# lines(lowess(MPLANT[4,],final_biomass_blackoak_MPLANT$spp_agbMPLANT.4..nyear...), col = 'blue')
# plot(MPLANT[5,], final_biomass_easthemlock_MPLANT$spp_agbMPLANT.5..nyear..., title(main = 'MPLANT Eastern Hemlock HF'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_easthemlock_MPLANT$spp_agbMPLANT.5..nyear...~MPLANT[5,]), col = 'red')
# lines(lowess(MPLANT[5,],final_biomass_easthemlock_MPLANT$spp_agbMPLANT.5..nyear...), col = 'blue')
#
#
#
# #AGEMX
# par(mfrow = c(2,3))
# plot(AGEMX[1,], final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear..., title(main = 'AGEMX Red Maple HF'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear...~AGEMX[1,]), col = 'red')
# lines(lowess(AGEMX[1,],final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear...), col = 'blue')
# plot(AGEMX[2,], final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear..., title(main = 'AGEMX American Beech HF'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear...~AGEMX[2,]), col = 'red')
# lines(lowess(AGEMX[2,],final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear...), col = 'blue')
# plot(AGEMX[3,], final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear..., title(main = 'AGEMX Red Oak HF'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear...~AGEMX[3,]), col = 'red')
# lines(lowess(AGEMX[3,],final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear...), col = 'blue')
# plot(AGEMX[4,], final_biomass_blackoak_AGEMX$spp_agbAGEMX.4..nyear..., title(main = 'AGEMX Black Oak HF'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_blackoak_AGEMX$spp_agbAGEMX.4..nyear...~AGEMX[4,]), col = 'red')
# lines(lowess(AGEMX[4,],final_biomass_blackoak_AGEMX$spp_agbAGEMX.4..nyear...), col = 'blue')
# plot(AGEMX[5,], final_biomass_easthemlock_AGEMX$spp_agbAGEMX.5..nyear..., title(main = 'AGEMX Eastern Hemlock HF'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_easthemlock_AGEMX$spp_agbAGEMX.5..nyear...~AGEMX[5,]), col = 'red')
# lines(lowess(AGEMX[5,],final_biomass_easthemlock_AGEMX$spp_agbAGEMX.5..nyear...), col = 'blue')
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="monte_carlo_sensitivity_analysis/HF/Plots/New_species")

## NRP visuals
#D3
# par(mfrow = c(2,2))
# plot(D3[1,], final_biomass_blackbirch_D3$spp_agbD3.1..nyear..., title(main = 'D3 Black Birch NRP'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_blackbirch_D3$spp_agbD3.1..nyear...~D3[1,]), col = 'red')
# lines(lowess(D3[1,],final_biomass_blackbirch_D3$spp_agbD3.1..nyear...), col = 'blue')
# plot(D3[2,], final_biomass_eastwhitepine_D3$spp_agbD3.2..nyear..., title(main = 'D3 Eastern White Pine NRP'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_eastwhitepine_D3$spp_agbD3.2..nyear...~D3[2,]), col = 'red')
# lines(lowess(D3[2,],final_biomass_eastwhitepine_D3$spp_agbD3.2..nyear...), col = 'blue')
# plot(D3[3,], final_biomass_redoak_D3$spp_agbD3.3..nyear..., title(main = 'D3 Red Oak NRP'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redoak_D3$spp_agbD3.3..nyear...~D3[3,]), col = 'red')
# lines(lowess(D3[3,],final_biomass_redoak_D3$spp_agbD3.3..nyear...), col = 'blue')
# plot(D3[4,], final_biomass_easthemlock_D3$spp_agbD3.4..nyear..., title(main = 'D3 Eastern Hemlock NRP'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_easthemlock_D3$spp_agbD3.4..nyear...~D3[4,]), col = 'red')
# lines(lowess(D3[4,],final_biomass_easthemlock_D3$spp_agbD3.4..nyear...), col = 'blue')
#
# #DMAX
# par(mfrow = c(2,2))
# plot(DMAX[1,], final_biomass_blackbirch_DMAX$spp_agbDMAX.1..nyear..., title(main = 'DMAX Black Birch NRP'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_blackbirch_DMAX$spp_agbDMAX.1..nyear...~DMAX[1,]), col = 'red')
# lines(lowess(DMAX[1,],final_biomass_blackbirch_DMAX$spp_agbDMAX.1..nyear...), col = 'blue')
# plot(DMAX[2,], final_biomass_eastwhitepine_DMAX$spp_agbDMAX.2..nyear..., title(main = 'DMAX Eastern White Pine NRP'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_eastwhitepine_DMAX$spp_agbDMAX.2..nyear...~DMAX[2,]), col = 'red')
# lines(lowess(DMAX[2,],final_biomass_eastwhitepine_DMAX$spp_agbDMAX.2..nyear...), col = 'blue')
# plot(DMAX[3,], final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear..., title(main = 'DMAX Red Oak NRP'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear...~DMAX[3,]), col = 'red')
# lines(lowess(DMAX[3,],final_biomass_redoak_DMAX$spp_agbDMAX.3..nyear...), col = 'blue')
# plot(DMAX[4,], final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear..., title(main = 'DMAX Eastern Hemlock NRP'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear...~DMAX[4,]), col = 'red')
# lines(lowess(DMAX[4,],final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear...), col = 'blue')
#
# #DMIN
# par(mfrow = c(2,2))
# plot(DMIN[1,], final_biomass_blackbirch_DMIN$spp_agbDMIN.1..nyear..., title(main = 'DMIN Black Birch NRP'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[1,], final_biomass_blackbirch_DMIN$spp_agbDMIN.1..nyear...), col = 'blue')
# abline(lm(final_biomass_blackbirch_DMIN$spp_agbDMIN.1..nyear...~DMIN[1,]), col = 'red')
# plot(DMIN[2,], final_biomass_eastwhitepine_DMIN$spp_agbDMIN.2..nyear..., title(main = 'DMIN Eastern White Pine NRP'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[2,],final_biomass_eastwhitepine_DMIN$spp_agbDMIN.2..nyear...), col = 'blue')
# abline(lm(final_biomass_eastwhitepine_DMIN$spp_agbDMIN.2..nyear...~DMIN[2,]), col = 'red')
# plot(DMIN[3,], final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear..., title(main = 'DMIN Red Oak NRP'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[3,],final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear...), col = 'blue')
# abline(lm(final_biomass_redoak_DMIN$spp_agbDMIN.3..nyear...~DMIN[3,]), col = 'red')
# plot(DMIN[4,], final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear..., title(main = 'DMIN Eastern Hemlock NRP'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[4,],final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear...), col = 'blue')
# abline(lm(final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear...~DMIN[4,]), col = 'red')
#
# #Frost
# par(mfrow = c(2,2))
# plot(Frost[1,], final_biomass_blackbirch_Frost$spp_agbFrost.1..nyear..., title(main = 'Frost Black Birch NRP'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_blackbirch_Frost$spp_agbFrost.1..nyear...~Frost[1,]), col = 'red')
# lines(lowess(Frost[1,],final_biomass_blackbirch_Frost$spp_agbFrost.1..nyear...), col = 'blue')
# plot(Frost[2,], final_biomass_eastwhitepine_Frost$spp_agbFrost.2..nyear..., title(main = 'Frost Eastern White Pine NRP'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_eastwhitepine_Frost$spp_agbFrost.2..nyear...~Frost[2,]), col = 'red')
# lines(lowess(Frost[2,],final_biomass_eastwhitepine_Frost$spp_agbFrost.2..nyear...), col = 'blue')
# plot(Frost[3,], final_biomass_redoak_Frost$spp_agbFrost.3..nyear..., title(main = 'Frost Red Oak NRP'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redoak_Frost$spp_agbFrost.3..nyear...~Frost[3,]), col = 'red')
# lines(lowess(Frost[3,],final_biomass_redoak_Frost$spp_agbFrost.3..nyear...), col = 'blue')
# plot(Frost[4,], final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear..., title(main = 'Frost Eastern Hemlock NRP'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear...~Frost[4,]), col = 'red')
# lines(lowess(Frost[4,],final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear...), col = 'blue')
#
# #G
# par(mfrow = c(2,2))
# plot(G[1,], final_biomass_blackbirch_G$spp_agbG.1..nyear..., title(main = 'G Black Birch NRP'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_blackbirch_G$spp_agbG.1..nyear...~G[1,]), col = 'red')
# lines(lowess(G[1,],final_biomass_blackbirch_G$spp_agbG.1..nyear...), col = 'blue')
# plot(G[2,], final_biomass_eastwhitepine_G$spp_agbG.2..nyear..., title(main = 'G Eastern White Pine NRP'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_eastwhitepine_G$spp_agbG.2..nyear...~G[2,]), col = 'red')
# lines(lowess(G[2,],final_biomass_eastwhitepine_G$spp_agbG.2..nyear...), col = 'blue')
# plot(G[3,], final_biomass_redoak_G$spp_agbG.3..nyear..., title(main = 'G Red Oak NRP'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redoak_G$spp_agbG.3..nyear...~G[3,]), col = 'red')
# lines(lowess(G[3,],final_biomass_redoak_G$spp_agbG.3..nyear...), col = 'blue')
# plot(G[4,], final_biomass_easthemlock_G$spp_agbG.4..nyear..., title(main = 'G Eastern Hemlock NRP'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_easthemlock_G$spp_agbG.4..nyear...~G[4,]), col = 'red')
# lines(lowess(G[4,],final_biomass_easthemlock_G$spp_agbG.4..nyear...), col = 'blue')
#
#
# #SPRTND
# par(mfrow = c(2,2))
# plot(SPRTND[1,], final_biomass_blackbirch_SPRTND$spp_agbSPRTND.1..nyear..., title(main = 'SPRTND Black Birch NRP'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_blackbirch_SPRTND$spp_agbSPRTND.1..nyear...~SPRTND[1,]), col = 'red')
# lines(lowess(SPRTND[1,],final_biomass_blackbirch_SPRTND$spp_agbSPRTND.1..nyear...), col = 'blue')
# plot(SPRTND[2,], final_biomass_eastwhitepine_SPRTND$spp_agbSPRTND.2..nyear..., title(main = 'SPRTND Eastern White Pine NRP'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_eastwhitepine_SPRTND$spp_agbSPRTND.2..nyear...~SPRTND[2,]), col = 'red')
# lines(lowess(SPRTND[2,],final_biomass_eastwhitepine_SPRTND$spp_agbSPRTND.2..nyear...), col = 'blue')
# plot(SPRTND[3,], final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear..., title(main = 'SPRTND Red Oak NRP'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear...~SPRTND[3,]), col = 'red')
# lines(lowess(SPRTND[3,],final_biomass_redoak_SPRTND$spp_agbSPRTND.3..nyear...), col = 'blue')
# plot(SPRTND[4,], final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear..., title(main = 'SPRTND Eastern Hemlock NRP'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear...~SPRTND[4,]), col = 'red')
# lines(lowess(SPRTND[4,],final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear...), col = 'blue')
#
#
# #MPLANT
# par(mfrow = c(2,2))
# plot(MPLANT[1,], final_biomass_blackbirch_MPLANT$spp_agbMPLANT.1..nyear..., title(main = 'MPLANT Black Birch NRP'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_blackbirch_MPLANT$spp_agbMPLANT.1..nyear...~MPLANT[1,]), col = 'red')
# lines(lowess(MPLANT[1,],final_biomass_blackbirch_MPLANT$spp_agbMPLANT.1..nyear...), col = 'blue')
# plot(MPLANT[2,], final_biomass_eastwhitepine_MPLANT$spp_agbMPLANT.2..nyear..., title(main = 'MPLANT Eastern White Pine NRP'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_eastwhitepine_MPLANT$spp_agbMPLANT.2..nyear...~MPLANT[2,]), col = 'red')
# lines(lowess(MPLANT[2,],final_biomass_eastwhitepine_MPLANT$spp_agbMPLANT.2..nyear...), col = 'blue')
# plot(MPLANT[3,], final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear..., title(main = 'MPLANT Red Oak NRP'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear...~MPLANT[3,]), col = 'red')
# lines(lowess(MPLANT[3,],final_biomass_redoak_MPLANT$spp_agbMPLANT.3..nyear...), col = 'blue')
# plot(MPLANT[4,], final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear..., title(main = 'MPLANT Eastern Hemlock NRP'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear...~MPLANT[4,]), col = 'red')
# lines(lowess(MPLANT[4,],final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear...), col = 'blue')
#
#
# #AGEMX
# par(mfrow = c(2,2))
# plot(AGEMX[1,], final_biomass_blackbirch_AGEMX$spp_agbAGEMX.1..nyear..., title(main = 'AGEMX Black Birch NRP'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_blackbirch_AGEMX$spp_agbAGEMX.1..nyear...~AGEMX[1,]), col = 'red')
# lines(lowess(AGEMX[1,],final_biomass_blackbirch_AGEMX$spp_agbAGEMX.1..nyear...), col = 'blue')
# plot(AGEMX[2,], final_biomass_eastwhitepine_AGEMX$spp_agbAGEMX.2..nyear..., title(main = 'AGEMX Eastern White Pine NRP'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_eastwhitepine_AGEMX$spp_agbAGEMX.2..nyear...~AGEMX[2,]), col = 'red')
# lines(lowess(AGEMX[2,],final_biomass_eastwhitepine_AGEMX$spp_agbAGEMX.2..nyear...), col = 'blue')
# plot(AGEMX[3,], final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear..., title(main = 'AGEMX Red Oak NRP'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear...~AGEMX[3,]), col = 'red')
# lines(lowess(AGEMX[3,],final_biomass_redoak_AGEMX$spp_agbAGEMX.3..nyear...), col = 'blue')
# plot(AGEMX[4,], final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear..., title(main = 'AGEMX Eastern Hemlock NRP'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear...~AGEMX[4,]), col = 'red')
# lines(lowess(AGEMX[4,],final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear...), col = 'blue')
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="monte_carlo_sensitivity_analysis/NRP/Plots")


## Rooster visuals
#D3
# par(mfrow = c(2,3))
# plot(D3[1,], final_biomass_redmaple_D3$spp_agbD3.1..nyear..., title(main = 'D3 Red Maple Rooster'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redmaple_D3$spp_agbD3.1..nyear...~D3[1,]), col = 'red')
# lines(lowess(D3[1,],final_biomass_redmaple_D3$spp_agbD3.1..nyear...), col = 'blue')
# plot(D3[2,], final_biomass_americanbeech_D3$spp_agbD3.2..nyear..., title(main = 'D3 American Beech Rooster'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_americanbeech_D3$spp_agbD3.2..nyear...~D3[2,]), col = 'red')
# lines(lowess(D3[2,],final_biomass_americanbeech_D3$spp_agbD3.2..nyear...), col = 'blue')
# plot(D3[3,], final_biomass_redspruce_D3$spp_agbD3.3..nyear..., title(main = 'D3 Red Spruce Rooster'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redspruce_D3$spp_agbD3.3..nyear...~D3[3,]), col = 'red')
# lines(lowess(D3[3,],final_biomass_redspruce_D3$spp_agbD3.3..nyear...), col = 'blue')
# plot(D3[4,], final_biomass_whitepine_D3$spp_agbD3.4..nyear..., title(main = 'D3 Eastern White Pine Rooster'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_whitepine_D3$spp_agbD3.4..nyear...~D3[4,]), col = 'red')
# lines(lowess(D3[4,],final_biomass_whitepine_D3$spp_agbD3.4..nyear...), col = 'blue')
# plot(D3[5,], final_biomass_redoak_D3$spp_agbD3.5..nyear..., title(main = 'D3 Red Oak Rooster'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redoak_D3$spp_agbD3.5..nyear...~D3[5,]), col = 'red')
# lines(lowess(D3[5,],final_biomass_redoak_D3$spp_agbD3.5..nyear...), col = 'blue')
#
#
# #DMAX
# par(mfrow = c(2,3))
# plot(DMAX[1,], final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear..., title(main = 'DMAX Red Maple Rooster'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear...~DMAX[1,]), col = 'red')
# lines(lowess(DMAX[1,],final_biomass_redmaple_DMAX$spp_agbDMAX.1..nyear...), col = 'blue')
# plot(DMAX[2,], final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear..., title(main = 'DMAX American Beech Rooster'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear...~DMAX[2,]), col = 'red')
# lines(lowess(DMAX[2,],final_biomass_americanbeech_DMAX$spp_agbDMAX.2..nyear...), col = 'blue')
# plot(DMAX[3,], final_biomass_redspruce_DMAX$spp_agbDMAX.3..nyear..., title(main = 'DMAX Red Spruce Rooster'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redspruce_DMAX$spp_agbDMAX.3..nyear...~DMAX[3,]), col = 'red')
# lines(lowess(DMAX[3,],final_biomass_redspruce_DMAX$spp_agbDMAX.3..nyear...), col = 'blue')
# plot(DMAX[4,], final_biomass_whitepine_DMAX$spp_agbDMAX.4..nyear..., title(main = 'DMAX Eastern White Pine Rooster'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_whitepine_DMAX$spp_agbDMAX.4..nyear...~DMAX[4,]), col = 'red')
# lines(lowess(DMAX[4,],final_biomass_whitepine_DMAX$spp_agbDMAX.4..nyear...), col = 'blue')
# plot(DMAX[5,], final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear..., title(main = 'DMAX Red Oak Rooster'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear...~DMAX[5,]), col = 'red')
# lines(lowess(DMAX[5,],final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear...), col = 'blue')
#
#
# #DMIN
# par(mfrow = c(2,3))
# plot(DMIN[1,], final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear..., title(main = 'DMIN Red Maple Rooster'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[1,], final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear...), col = 'blue')
# abline(lm(final_biomass_redmaple_DMIN$spp_agbDMIN.1..nyear...~DMIN[1,]), col = 'red')
# plot(DMIN[2,], final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear..., title(main = 'DMIN American Beech Rooster'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[2,],final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear...), col = 'blue')
# abline(lm(final_biomass_americanbeech_DMIN$spp_agbDMIN.2..nyear...~DMIN[2,]), col = 'red')
# plot(DMIN[3,], final_biomass_redspruce_DMIN$spp_agbDMIN.3..nyear..., title(main = 'DMIN Red Spruce Rooster'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[3,],final_biomass_redspruce_DMIN$spp_agbDMIN.3..nyear...), col = 'blue')
# abline(lm(final_biomass_redspruce_DMIN$spp_agbDMIN.3..nyear...~DMIN[3,]), col = 'red')
# plot(DMIN[4,], final_biomass_whitepine_DMIN$spp_agbDMIN.4..nyear..., title(main = 'DMIN Eastern White Pine Rooster'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[4,],final_biomass_whitepine_DMIN$spp_agbDMIN.4..nyear...), col = 'blue')
# abline(lm(final_biomass_whitepine_DMIN$spp_agbDMIN.4..nyear...~DMIN[4,]), col = 'red')
# plot(DMIN[5,], final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear..., title(main = 'DMIN Red Oak Rooster'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[5,],final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear...), col = 'blue')
# abline(lm(final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear...~DMIN[5,]), col = 'red')
#
# #Frost
# par(mfrow = c(2,3))
# plot(Frost[1,], final_biomass_redmaple_Frost$spp_agbFrost.1..nyear..., title(main = 'Frost Red Maple Rooster'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redmaple_Frost$spp_agbFrost.1..nyear...~Frost[1,]), col = 'red')
# lines(lowess(Frost[1,],final_biomass_redmaple_Frost$spp_agbFrost.1..nyear...), col = 'blue')
# plot(Frost[2,], final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear..., title(main = 'Frost American Beech Rooster'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear...~Frost[2,]), col = 'red')
# lines(lowess(Frost[2,],final_biomass_americanbeech_Frost$spp_agbFrost.2..nyear...), col = 'blue')
# plot(Frost[3,], final_biomass_redspruce_Frost$spp_agbFrost.3..nyear..., title(main = 'Frost Red Spruce Rooster'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redspruce_Frost$spp_agbFrost.3..nyear...~Frost[3,]), col = 'red')
# lines(lowess(Frost[3,],final_biomass_redspruce_Frost$spp_agbFrost.3..nyear...), col = 'blue')
# plot(Frost[4,], final_biomass_whitepine_Frost$spp_agbFrost.4..nyear..., title(main = 'Frost Eastern White Pine Rooster'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_whitepine_Frost$spp_agbFrost.4..nyear...~Frost[4,]), col = 'red')
# lines(lowess(Frost[4,],final_biomass_whitepine_Frost$spp_agbFrost.4..nyear...), col = 'blue')
# plot(Frost[5,], final_biomass_redoak_Frost$spp_agbFrost.5..nyear..., title(main = 'Frost Red Oak Rooster'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redoak_Frost$spp_agbFrost.5..nyear...~Frost[5,]), col = 'red')
# lines(lowess(Frost[5,],final_biomass_redoak_Frost$spp_agbFrost.5..nyear...), col = 'blue')
#
#
# #G
# par(mfrow = c(2,3))
# plot(G[1,], final_biomass_redmaple_G$spp_agbG.1..nyear..., title(main = 'G Red Maple Rooster'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redmaple_G$spp_agbG.1..nyear...~G[1,]), col = 'red')
# lines(lowess(G[1,],final_biomass_redmaple_G$spp_agbG.1..nyear...), col = 'blue')
# plot(G[2,], final_biomass_americanbeech_G$spp_agbG.2..nyear..., title(main = 'G American Beech Rooster'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_americanbeech_G$spp_agbG.2..nyear...~G[2,]), col = 'red')
# lines(lowess(G[2,],final_biomass_americanbeech_G$spp_agbG.2..nyear...), col = 'blue')
# plot(G[3,], final_biomass_redspruce_G$spp_agbG.3..nyear..., title(main = 'G Red Spruce Rooster'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redspruce_G$spp_agbG.3..nyear...~G[3,]), col = 'red')
# lines(lowess(G[3,],final_biomass_redspruce_G$spp_agbG.3..nyear...), col = 'blue')
# plot(G[4,], final_biomass_whitepine_G$spp_agbG.4..nyear..., title(main = 'G Eastern White Pine Rooster'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_whitepine_G$spp_agbG.4..nyear...~G[4,]), col = 'red')
# lines(lowess(G[4,],final_biomass_whitepine_G$spp_agbG.4..nyear...), col = 'blue')
# plot(G[5,], final_biomass_redoak_G$spp_agbG.5..nyear..., title(main = 'G Red Oak Rooster'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redoak_G$spp_agbG.5..nyear...~G[5,]), col = 'red')
# lines(lowess(G[5,],final_biomass_redoak_G$spp_agbG.5..nyear...), col = 'blue')
#
#
# #SPRTND
# par(mfrow = c(2,3))
# plot(SPRTND[1,], final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear..., title(main = 'SPRTND Red Maple Rooster'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear...~SPRTND[1,]), col = 'red')
# lines(lowess(SPRTND[1,],final_biomass_redmaple_SPRTND$spp_agbSPRTND.1..nyear...), col = 'blue')
# plot(SPRTND[2,], final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear..., title(main = 'SPRTND American Beech Rooster'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear...~SPRTND[2,]), col = 'red')
# lines(lowess(SPRTND[2,],final_biomass_americanbeech_SPRTND$spp_agbSPRTND.2..nyear...), col = 'blue')
# plot(SPRTND[3,], final_biomass_redspruce_SPRTND$spp_agbSPRTND.3..nyear..., title(main = 'SPRTND Red Spruce Rooster'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redspruce_SPRTND$spp_agbSPRTND.3..nyear...~SPRTND[3,]), col = 'red')
# lines(lowess(SPRTND[3,],final_biomass_redspruce_SPRTND$spp_agbSPRTND.3..nyear...), col = 'blue')
# plot(SPRTND[4,], final_biomass_whitepine_SPRTND$spp_agbSPRTND.4..nyear..., title(main = 'SPRTND Eastern White Pine Rooster'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_whitepine_SPRTND$spp_agbSPRTND.4..nyear...~SPRTND[4,]), col = 'red')
# lines(lowess(SPRTND[4,],final_biomass_whitepine_SPRTND$spp_agbSPRTND.4..nyear...), col = 'blue')
# plot(SPRTND[5,], final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear..., title(main = 'SPRTND Red Oak Rooster'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear...~SPRTND[5,]), col = 'red')
# lines(lowess(SPRTND[5,],final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear...), col = 'blue')
#
#
# #MPLANT
# par(mfrow = c(2,3))
# plot(MPLANT[1,], final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear..., title(main = 'MPLANT Red Maple Rooster'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear...~MPLANT[1,]), col = 'red')
# lines(lowess(MPLANT[1,],final_biomass_redmaple_MPLANT$spp_agbMPLANT.1..nyear...), col = 'blue')
# plot(MPLANT[2,], final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear..., title(main = 'MPLANT American Beech Rooster'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear...~MPLANT[2,]), col = 'red')
# lines(lowess(MPLANT[2,],final_biomass_americanbeech_MPLANT$spp_agbMPLANT.2..nyear...), col = 'blue')
# plot(MPLANT[3,], final_biomass_redspruce_MPLANT$spp_agbMPLANT.3..nyear..., title(main = 'MPLANT Red Spruce Rooster'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redspruce_MPLANT$spp_agbMPLANT.3..nyear...~MPLANT[3,]), col = 'red')
# lines(lowess(MPLANT[3,],final_biomass_redspruce_MPLANT$spp_agbMPLANT.3..nyear...), col = 'blue')
# plot(MPLANT[4,], final_biomass_whitepine_MPLANT$spp_agbMPLANT.4..nyear..., title(main = 'MPLANT Eastern White Pine Rooster'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_whitepine_MPLANT$spp_agbMPLANT.4..nyear...~MPLANT[4,]), col = 'red')
# lines(lowess(MPLANT[4,],final_biomass_whitepine_MPLANT$spp_agbMPLANT.4..nyear...), col = 'blue')
# plot(MPLANT[5,], final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear..., title(main = 'MPLANT Red Oak Rooster'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear...~MPLANT[5,]), col = 'red')
# lines(lowess(MPLANT[5,],final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear...), col = 'blue')
#
# #AGEMX
# par(mfrow = c(2,3))
# plot(AGEMX[1,], final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear..., title(main = 'AGEMX Red Maple Rooster'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear...~AGEMX[1,]), col = 'red')
# lines(lowess(AGEMX[1,],final_biomass_redmaple_AGEMX$spp_agbAGEMX.1..nyear...), col = 'blue')
# plot(AGEMX[2,], final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear..., title(main = 'AGEMX American Beech Pine Rooster'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear...~AGEMX[2,]), col = 'red')
# lines(lowess(AGEMX[2,],final_biomass_americanbeech_AGEMX$spp_agbAGEMX.2..nyear...), col = 'blue')
# plot(AGEMX[3,], final_biomass_redspruce_AGEMX$spp_agbAGEMX.3..nyear..., title(main = 'AGEMX Red Spruce Rooster'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redspruce_AGEMX$spp_agbAGEMX.3..nyear...~AGEMX[3,]), col = 'red')
# lines(lowess(AGEMX[3,],final_biomass_redspruce_AGEMX$spp_agbAGEMX.3..nyear...), col = 'blue')
# plot(AGEMX[4,], final_biomass_whitepine_AGEMX$spp_agbAGEMX.4..nyear..., title(main = 'AGEMX Eastern White Pine Rooster'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_whitepine_AGEMX$spp_agbAGEMX.4..nyear...~AGEMX[4,]), col = 'red')
# lines(lowess(AGEMX[4,],final_biomass_whitepine_AGEMX$spp_agbAGEMX.4..nyear...), col = 'blue')
# plot(AGEMX[5,], final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear..., title(main = 'AGEMX Red Oak Rooster'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear...~AGEMX[5,]), col = 'red')
# lines(lowess(AGEMX[5,],final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear...), col = 'blue')
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="monte_carlo_sensitivity_analysis/Rooster/Plots")

## Goose visuals
#D3
# par(mfrow = c(2,3))
# plot(D3[1,], final_biomass_americanbeech_D3$spp_agbD3.1..nyear..., title(main = 'D3 American Beech Goose'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_americanbeech_D3$spp_agbD3.1..nyear...~D3[1,]), col = 'red')
# lines(lowess(D3[1,],final_biomass_americanbeech_D3$spp_agbD3.1..nyear...), col = 'blue')
# plot(D3[2,], final_biomass_whitepine_D3$spp_agbD3.2..nyear..., title(main = 'D3 White Pine Goose'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_whitepine_D3$spp_agbD3.2..nyear...~D3[2,]), col = 'red')
# lines(lowess(D3[2,],final_biomass_whitepine_D3$spp_agbD3.2..nyear...), col = 'blue')
# plot(D3[3,], final_biomass_whiteoak_D3$spp_agbD3.3..nyear..., title(main = 'D3 White Oak Goose'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_whiteoak_D3$spp_agbD3.3..nyear...~D3[3,]), col = 'red')
# lines(lowess(D3[3,],final_biomass_whiteoak_D3$spp_agbD3.3..nyear...), col = 'blue')
# plot(D3[4,], final_biomass_chestnutoak_D3$spp_agbD3.4..nyear..., title(main = 'D3 Chestnut Oak Goose'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_chestnutoak_D3$spp_agbD3.4..nyear...~D3[4,]), col = 'red')
# lines(lowess(D3[4,],final_biomass_chestnutoak_D3$spp_agbD3.4..nyear...), col = 'blue')
# plot(D3[5,], final_biomass_redoak_D3$spp_agbD3.5..nyear..., title(main = 'D3 Red Oak Goose'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_redoak_D3$spp_agbD3.5..nyear...~D3[5,]), col = 'red')
# lines(lowess(D3[5,],final_biomass_redoak_D3$spp_agbD3.5..nyear...), col = 'blue')
#
#
# #DMAX
# par(mfrow = c(2,3))
# plot(DMAX[1,], final_biomass_americanbeech_DMAX$spp_agbDMAX.1..nyear..., title(main = 'DMAX American Beech Goose'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_americanbeech_DMAX$spp_agbDMAX.1..nyear...~DMAX[1,]), col = 'red')
# lines(lowess(DMAX[1,],final_biomass_americanbeech_DMAX$spp_agbDMAX.1..nyear...), col = 'blue')
# plot(DMAX[2,], final_biomass_whitepine_DMAX$spp_agbDMAX.2..nyear..., title(main = 'DMAX White Pine Goose'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_whitepine_DMAX$spp_agbDMAX.2..nyear...~DMAX[2,]), col = 'red')
# lines(lowess(DMAX[2,],final_biomass_whitepine_DMAX$spp_agbDMAX.2..nyear...), col = 'blue')
# plot(DMAX[3,], final_biomass_whiteoak_DMAX$spp_agbDMAX.3..nyear..., title(main = 'DMAX White Oak Goose'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_whiteoak_DMAX$spp_agbDMAX.3..nyear...~DMAX[3,]), col = 'red')
# lines(lowess(DMAX[3,],final_biomass_whiteoak_DMAX$spp_agbDMAX.3..nyear...), col = 'blue')
# plot(DMAX[4,], final_biomass_chestnutoak_DMAX$spp_agbDMAX.4..nyear..., title(main = 'DMAX Chestnut Oak Goose'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_chestnutoak_DMAX$spp_agbDMAX.4..nyear...~DMAX[4,]), col = 'red')
# lines(lowess(DMAX[4,],final_biomass_chestnutoak_DMAX$spp_agbDMAX.4..nyear...), col = 'blue')
# plot(DMAX[5,], final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear..., title(main = 'DMAX Red Oak Goose'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear...~DMAX[5,]), col = 'red')
# lines(lowess(DMAX[5,],final_biomass_redoak_DMAX$spp_agbDMAX.5..nyear...), col = 'blue')
#
#
# #DMIN
# par(mfrow = c(2,3))
# plot(DMIN[1,], final_biomass_americanbeech_DMIN$spp_agbDMIN.1..nyear..., title(main = 'DMIN American Beech Goose'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[1,], final_biomass_americanbeech_DMIN$spp_agbDMIN.1..nyear...), col = 'blue')
# abline(lm(final_biomass_americanbeech_DMIN$spp_agbDMIN.1..nyear...~DMIN[1,]), col = 'red')
# plot(DMIN[2,], final_biomass_whitepine_DMIN$spp_agbDMIN.2..nyear..., title(main = 'DMIN White Pine Goose'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[2,],final_biomass_whitepine_DMIN$spp_agbDMIN.2..nyear...), col = 'blue')
# abline(lm(final_biomass_whitepine_DMIN$spp_agbDMIN.2..nyear...~DMIN[2,]), col = 'red')
# plot(DMIN[3,], final_biomass_whiteoak_DMIN$spp_agbDMIN.3..nyear..., title(main = 'DMIN White Oak Goose'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[3,],final_biomass_whiteoak_DMIN$spp_agbDMIN.3..nyear...), col = 'blue')
# abline(lm(final_biomass_whiteoak_DMIN$spp_agbDMIN.3..nyear...~DMIN[3,]), col = 'red')
# plot(DMIN[4,], final_biomass_chestnutoak_DMIN$spp_agbDMIN.4..nyear..., title(main = 'DMIN Chestnut Oak Goose'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[4,],final_biomass_chestnutoak_DMIN$spp_agbDMIN.4..nyear...), col = 'blue')
# abline(lm(final_biomass_chestnutoak_DMIN$spp_agbDMIN.4..nyear...~DMIN[4,]), col = 'red')
# plot(DMIN[5,], final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear..., title(main = 'DMIN Red Oak Goose'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[5,],final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear...), col = 'blue')
# abline(lm(final_biomass_redoak_DMIN$spp_agbDMIN.5..nyear...~DMIN[5,]), col = 'red')
#
# #Frost
# par(mfrow = c(2,3))
# plot(Frost[1,], final_biomass_americanbeech_Frost$spp_agbFrost.1..nyear..., title(main = 'Frost American Beech Goose'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_americanbeech_Frost$spp_agbFrost.1..nyear...~Frost[1,]), col = 'red')
# lines(lowess(Frost[1,],final_biomass_americanbeech_Frost$spp_agbFrost.1..nyear...), col = 'blue')
# plot(Frost[2,], final_biomass_whitepine_Frost$spp_agbFrost.2..nyear..., title(main = 'Frost White Pine Goose'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_whitepine_Frost$spp_agbFrost.2..nyear...~Frost[2,]), col = 'red')
# lines(lowess(Frost[2,],final_biomass_whitepine_Frost$spp_agbFrost.2..nyear...), col = 'blue')
# plot(Frost[3,], final_biomass_whiteoak_Frost$spp_agbFrost.3..nyear..., title(main = 'Frost White Oak Goose'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_whiteoak_Frost$spp_agbFrost.3..nyear...~Frost[3,]), col = 'red')
# lines(lowess(Frost[3,],final_biomass_whiteoak_Frost$spp_agbFrost.3..nyear...), col = 'blue')
# plot(Frost[4,], final_biomass_chestnutoak_Frost$spp_agbFrost.4..nyear..., title(main = 'Frost Chestnut Oak Goose'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_chestnutoak_Frost$spp_agbFrost.4..nyear...~Frost[4,]), col = 'red')
# lines(lowess(Frost[4,],final_biomass_chestnutoak_Frost$spp_agbFrost.4..nyear...), col = 'blue')
# plot(Frost[5,], final_biomass_redoak_Frost$spp_agbFrost.5..nyear..., title(main = 'Frost Red Oak Goose'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_redoak_Frost$spp_agbFrost.5..nyear...~Frost[5,]), col = 'red')
# lines(lowess(Frost[5,],final_biomass_redoak_Frost$spp_agbFrost.5..nyear...), col = 'blue')
#
#
# #G
# par(mfrow = c(2,3))
# plot(G[1,], final_biomass_americanbeech_G$spp_agbG.1..nyear..., title(main = 'G American Beech Goose'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_americanbeech_G$spp_agbG.1..nyear...~G[1,]), col = 'red')
# lines(lowess(G[1,],final_biomass_americanbeech_G$spp_agbG.1..nyear...), col = 'blue')
# plot(G[2,], final_biomass_whitepine_G$spp_agbG.2..nyear..., title(main = 'G White Pine Goose'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_whitepine_G$spp_agbG.2..nyear...~G[2,]), col = 'red')
# lines(lowess(G[2,],final_biomass_whitepine_G$spp_agbG.2..nyear...), col = 'blue')
# plot(G[3,], final_biomass_whiteoak_G$spp_agbG.3..nyear..., title(main = 'G White Oak Goose'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_whiteoak_G$spp_agbG.3..nyear...~G[3,]), col = 'red')
# lines(lowess(G[3,],final_biomass_whiteoak_G$spp_agbG.3..nyear...), col = 'blue')
# plot(G[4,], final_biomass_chestnutoak_G$spp_agbG.4..nyear..., title(main = 'G Chestnut Oak Goose'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_chestnutoak_G$spp_agbG.4..nyear...~G[4,]), col = 'red')
# lines(lowess(G[4,],final_biomass_chestnutoak_G$spp_agbG.4..nyear...), col = 'blue')
# plot(G[5,], final_biomass_redoak_G$spp_agbG.5..nyear..., title(main = 'G Red Oak Goose'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_redoak_G$spp_agbG.5..nyear...~G[5,]), col = 'red')
# lines(lowess(G[5,],final_biomass_redoak_G$spp_agbG.5..nyear...), col = 'blue')
#
#
# #SPRTND
# par(mfrow = c(2,3))
# plot(SPRTND[1,], final_biomass_americanbeech_SPRTND$spp_agbSPRTND.1..nyear..., title(main = 'SPRTND American Beech Goose'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_americanbeech_SPRTND$spp_agbSPRTND.1..nyear...~SPRTND[1,]), col = 'red')
# lines(lowess(SPRTND[1,],final_biomass_americanbeech_SPRTND$spp_agbSPRTND.1..nyear...), col = 'blue')
# plot(SPRTND[2,], final_biomass_whitepine_SPRTND$spp_agbSPRTND.2..nyear..., title(main = 'SPRTND White Pine Goose'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_whitepine_SPRTND$spp_agbSPRTND.2..nyear...~SPRTND[2,]), col = 'red')
# lines(lowess(SPRTND[2,],final_biomass_whitepine_SPRTND$spp_agbSPRTND.2..nyear...), col = 'blue')
# plot(SPRTND[3,], final_biomass_whiteoak_SPRTND$spp_agbSPRTND.3..nyear..., title(main = 'SPRTND White Oak Goose'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_whiteoak_SPRTND$spp_agbSPRTND.3..nyear...~SPRTND[3,]), col = 'red')
# lines(lowess(SPRTND[3,],final_biomass_whiteoak_SPRTND$spp_agbSPRTND.3..nyear...), col = 'blue')
# plot(SPRTND[4,], final_biomass_chestnutoak_SPRTND$spp_agbSPRTND.4..nyear..., title(main = 'SPRTND Chestnut Oak Goose'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_chestnutoak_SPRTND$spp_agbSPRTND.4..nyear...~SPRTND[4,]), col = 'red')
# lines(lowess(SPRTND[4,],final_biomass_chestnutoak_SPRTND$spp_agbSPRTND.4..nyear...), col = 'blue')
# plot(SPRTND[5,], final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear..., title(main = 'SPRTND Red Oak Goose'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear...~SPRTND[5,]), col = 'red')
# lines(lowess(SPRTND[5,],final_biomass_redoak_SPRTND$spp_agbSPRTND.5..nyear...), col = 'blue')
#
#
# #MPLANT
# par(mfrow = c(2,3))
# plot(MPLANT[1,], final_biomass_americanbeech_MPLANT$spp_agbMPLANT.1..nyear..., title(main = 'MPLANT American Beech Goose'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_americanbeech_MPLANT$spp_agbMPLANT.1..nyear...~MPLANT[1,]), col = 'red')
# lines(lowess(MPLANT[1,],final_biomass_americanbeech_MPLANT$spp_agbMPLANT.1..nyear...), col = 'blue')
# plot(MPLANT[2,], final_biomass_whitepine_MPLANT$spp_agbMPLANT.2..nyear..., title(main = 'MPLANT White Pine Goose'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_whitepine_MPLANT$spp_agbMPLANT.2..nyear...~MPLANT[2,]), col = 'red')
# lines(lowess(MPLANT[2,],final_biomass_whitepine_MPLANT$spp_agbMPLANT.2..nyear...), col = 'blue')
# plot(MPLANT[3,], final_biomass_whiteoak_MPLANT$spp_agbMPLANT.3..nyear..., title(main = 'MPLANT White Oak Goose'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_whiteoak_MPLANT$spp_agbMPLANT.3..nyear...~MPLANT[3,]), col = 'red')
# lines(lowess(MPLANT[3,],final_biomass_whiteoak_MPLANT$spp_agbMPLANT.3..nyear...), col = 'blue')
# plot(MPLANT[4,], final_biomass_chestnutoak_MPLANT$spp_agbMPLANT.4..nyear..., title(main = 'MPLANT Chestnut Oak Goose'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_chestnutoak_MPLANT$spp_agbMPLANT.4..nyear...~MPLANT[4,]), col = 'red')
# lines(lowess(MPLANT[4,],final_biomass_chestnutoak_MPLANT$spp_agbMPLANT.4..nyear...), col = 'blue')
# plot(MPLANT[5,], final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear..., title(main = 'MPLANT Red Oak Goose'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear...~MPLANT[5,]), col = 'red')
# lines(lowess(MPLANT[5,],final_biomass_redoak_MPLANT$spp_agbMPLANT.5..nyear...), col = 'blue')
#
# #AGEMX
# par(mfrow = c(2,3))
# plot(AGEMX[1,], final_biomass_americanbeech_AGEMX$spp_agbAGEMX.1..nyear..., title(main = 'AGEMX American Beech Goose'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_americanbeech_AGEMX$spp_agbAGEMX.1..nyear...~AGEMX[1,]), col = 'red')
# lines(lowess(AGEMX[1,],final_biomass_americanbeech_AGEMX$spp_agbAGEMX.1..nyear...), col = 'blue')
# plot(AGEMX[2,], final_biomass_whitepine_AGEMX$spp_agbAGEMX.2..nyear..., title(main = 'AGEMX White Pine Goose'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_whitepine_AGEMX$spp_agbAGEMX.2..nyear...~AGEMX[2,]), col = 'red')
# lines(lowess(AGEMX[2,],final_biomass_whitepine_AGEMX$spp_agbAGEMX.2..nyear...), col = 'blue')
# plot(AGEMX[3,], final_biomass_whiteoak_AGEMX$spp_agbAGEMX.3..nyear..., title(main = 'AGEMX White Oak Goose'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_whiteoak_AGEMX$spp_agbAGEMX.3..nyear...~AGEMX[3,]), col = 'red')
# lines(lowess(AGEMX[3,],final_biomass_whiteoak_AGEMX$spp_agbAGEMX.3..nyear...), col = 'blue')
# plot(AGEMX[4,], final_biomass_chestnutoak_AGEMX$spp_agbAGEMX.4..nyear..., title(main = 'AGEMX Chestnut Oak Goose'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_chestnutoak_AGEMX$spp_agbAGEMX.4..nyear...~AGEMX[4,]), col = 'red')
# lines(lowess(AGEMX[4,],final_biomass_chestnutoak_AGEMX$spp_agbAGEMX.4..nyear...), col = 'blue')
# plot(AGEMX[5,], final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear..., title(main = 'AGEMX Red Oak Goose'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear...~AGEMX[5,]), col = 'red')
# lines(lowess(AGEMX[5,],final_biomass_redoak_AGEMX$spp_agbAGEMX.5..nyear...), col = 'blue')
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="monte_carlo_sensitivity_analysis/Goose/Plots")

## Sylvania visuals
# par(mfrow = c(2,2))
# plot(D3[1,], final_biomass_sugarmaple_D3$spp_agbD3.1..nyear..., title(main = 'D3 Sugar Maple Sylvania'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_sugarmaple_D3$spp_agbD3.1..nyear...~D3[1,]), col = 'red')
# lines(lowess(D3[1,],final_biomass_sugarmaple_D3$spp_agbD3.1..nyear...), col = 'blue')
# plot(D3[2,], final_biomass_yellowbirch_D3$spp_agbD3.2..nyear..., title(main = 'D3 Yellow Birch Sylvania'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_yellowbirch_D3$spp_agbD3.2..nyear...~D3[2,]), col = 'red')
# lines(lowess(D3[2,],final_biomass_yellowbirch_D3$spp_agbD3.2..nyear...), col = 'blue')
# plot(D3[3,], final_biomass_northwhitecedar_D3$spp_agbD3.3..nyear..., title(main = 'D3 Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_northwhitecedar_D3$spp_agbD3.3..nyear...~D3[3,]), col = 'red')
# lines(lowess(D3[3,],final_biomass_northwhitecedar_D3$spp_agbD3.3..nyear...), col = 'blue')
# plot(D3[4,], final_biomass_easthemlock_D3$spp_agbD3.4..nyear..., title(main = 'D3 Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'D3')
# abline(lm(final_biomass_easthemlock_D3$spp_agbD3.4..nyear...~D3[4,]), col = 'red')
# lines(lowess(D3[4,],final_biomass_easthemlock_D3$spp_agbD3.4..nyear...), col = 'blue')
#
# #DMAX
# par(mfrow = c(2,2))
# plot(DMAX[1,], final_biomass_sugarmaple_DMAX$spp_agbDMAX.1..nyear..., title(main = 'DMAX Sugar Maple Sylvania'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_sugarmaple_DMAX$spp_agbDMAX.1..nyear...~DMAX[1,]), col = 'red')
# lines(lowess(DMAX[1,],final_biomass_sugarmaple_DMAX$spp_agbDMAX.1..nyear...), col = 'blue')
# plot(DMAX[2,], final_biomass_yellowbirch_DMAX$spp_agbDMAX.2..nyear..., title(main = 'DMAX Yellow Birch Sylvania'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_yellowbirch_DMAX$spp_agbDMAX.2..nyear...~DMAX[2,]), col = 'red')
# lines(lowess(DMAX[2,],final_biomass_yellowbirch_DMAX$spp_agbDMAX.2..nyear...), col = 'blue')
# plot(DMAX[3,], final_biomass_northwhitecedar_DMAX$spp_agbDMAX.3..nyear..., title(main = 'DMAX Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_northwhitecedar_DMAX$spp_agbDMAX.3..nyear...~DMAX[3,]), col = 'red')
# lines(lowess(DMAX[3,],final_biomass_northwhitecedar_DMAX$spp_agbDMAX.3..nyear...), col = 'blue')
# plot(DMAX[4,], final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear..., title(main = 'DMAX Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'DMAX')
# abline(lm(final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear...~DMAX[4,]), col = 'red')
# lines(lowess(DMAX[4,],final_biomass_easthemlock_DMAX$spp_agbDMAX.4..nyear...), col = 'blue')
#
# #DMIN
# par(mfrow = c(2,2))
# plot(DMIN[1,], final_biomass_sugarmaple_DMIN$spp_agbDMIN.1..nyear..., title(main = 'DMIN Sugar Maple Sylvania'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[1,], final_biomass_sugarmaple_DMIN$spp_agbDMIN.1..nyear...), col = 'blue')
# abline(lm(final_biomass_sugarmaple_DMIN$spp_agbDMIN.1..nyear...~DMIN[1,]), col = 'red')
# plot(DMIN[2,], final_biomass_yellowbirch_DMIN$spp_agbDMIN.2..nyear..., title(main = 'DMIN Yellow Birch Sylvania'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[2,],final_biomass_yellowbirch_DMIN$spp_agbDMIN.2..nyear...), col = 'blue')
# abline(lm(final_biomass_yellowbirch_DMIN$spp_agbDMIN.2..nyear...~DMIN[2,]), col = 'red')
# plot(DMIN[3,], final_biomass_northwhitecedar_DMIN$spp_agbDMIN.3..nyear..., title(main = 'DMIN Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[3,],final_biomass_northwhitecedar_DMIN$spp_agbDMIN.3..nyear...), col = 'blue')
# abline(lm(final_biomass_northwhitecedar_DMIN$spp_agbDMIN.3..nyear...~DMIN[3,]), col = 'red')
# plot(DMIN[4,], final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear..., title(main = 'DMIN Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'DMIN')
# lines(lowess(DMIN[4,],final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear...), col = 'blue')
# abline(lm(final_biomass_easthemlock_DMIN$spp_agbDMIN.4..nyear...~DMIN[4,]), col = 'red')
#
# #Frost
# par(mfrow = c(2,2))
# plot(Frost[1,], final_biomass_sugarmaple_Frost$spp_agbFrost.1..nyear..., title(main = 'Frost Sugar Maple Sylvania'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_sugarmaple_Frost$spp_agbFrost.1..nyear...~Frost[1,]), col = 'red')
# lines(lowess(Frost[1,],final_biomass_sugarmaple_Frost$spp_agbFrost.1..nyear...), col = 'blue')
# plot(Frost[2,], final_biomass_yellowbirch_Frost$spp_agbFrost.2..nyear..., title(main = 'Frost Yellow Birch Sylvania'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_yellowbirch_Frost$spp_agbFrost.2..nyear...~Frost[2,]), col = 'red')
# lines(lowess(Frost[2,],final_biomass_yellowbirch_Frost$spp_agbFrost.2..nyear...), col = 'blue')
# plot(Frost[3,], final_biomass_northwhitecedar_Frost$spp_agbFrost.3..nyear..., title(main = 'Frost Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_northwhitecedar_Frost$spp_agbFrost.3..nyear...~Frost[3,]), col = 'red')
# lines(lowess(Frost[3,],final_biomass_northwhitecedar_Frost$spp_agbFrost.3..nyear...), col = 'blue')
# plot(Frost[4,], final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear..., title(main = 'Frost Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'Frost')
# abline(lm(final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear...~Frost[4,]), col = 'red')
# lines(lowess(Frost[4,],final_biomass_easthemlock_Frost$spp_agbFrost.4..nyear...), col = 'blue')
#
# #G
# par(mfrow = c(2,2))
# plot(G[1,], final_biomass_sugarmaple_G$spp_agbG.1..nyear..., title(main = 'G Sugar Maple Sylvania'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_sugarmaple_G$spp_agbG.1..nyear...~G[1,]), col = 'red')
# lines(lowess(G[1,],final_biomass_sugarmaple_G$spp_agbG.1..nyear...), col = 'blue')
# plot(G[2,], final_biomass_yellowbirch_G$spp_agbG.2..nyear..., title(main = 'G Yellow Birch Sylvania'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_yellowbirch_G$spp_agbG.2..nyear...~G[2,]), col = 'red')
# lines(lowess(G[2,],final_biomass_yellowbirch_G$spp_agbG.2..nyear...), col = 'blue')
# plot(G[3,], final_biomass_northwhitecedar_G$spp_agbG.3..nyear..., title(main = 'G Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_northwhitecedar_G$spp_agbG.3..nyear...~G[3,]), col = 'red')
# lines(lowess(G[3,],final_biomass_northwhitecedar_G$spp_agbG.3..nyear...), col = 'blue')
# plot(G[4,], final_biomass_easthemlock_G$spp_agbG.4..nyear..., title(main = 'G Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'G')
# abline(lm(final_biomass_easthemlock_G$spp_agbG.4..nyear...~G[4,]), col = 'red')
# lines(lowess(G[4,],final_biomass_easthemlock_G$spp_agbG.4..nyear...), col = 'blue')
#
#
# #SPRTND
# par(mfrow = c(2,2))
# plot(SPRTND[1,], final_biomass_sugarmaple_SPRTND$spp_agbSPRTND.1..nyear..., title(main = 'SPRTND Sugar Maple Sylvania'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_sugarmaple_SPRTND$spp_agbSPRTND.1..nyear...~SPRTND[1,]), col = 'red')
# lines(lowess(SPRTND[1,],final_biomass_sugarmaple_SPRTND$spp_agbSPRTND.1..nyear...), col = 'blue')
# plot(SPRTND[2,], final_biomass_yellowbirch_SPRTND$spp_agbSPRTND.2..nyear..., title(main = 'SPRTND Yellow Birch Sylvania'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_yellowbirch_SPRTND$spp_agbSPRTND.2..nyear...~SPRTND[2,]), col = 'red')
# lines(lowess(SPRTND[2,],final_biomass_yellowbirch_SPRTND$spp_agbSPRTND.2..nyear...), col = 'blue')
# plot(SPRTND[3,], final_biomass_northwhitecedar_SPRTND$spp_agbSPRTND.3..nyear..., title(main = 'SPRTND Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_northwhitecedar_SPRTND$spp_agbSPRTND.3..nyear...~SPRTND[3,]), col = 'red')
# lines(lowess(SPRTND[3,],final_biomass_northwhitecedar_SPRTND$spp_agbSPRTND.3..nyear...), col = 'blue')
# plot(SPRTND[4,], final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear..., title(main = 'SPRTND Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'SPRTND')
# abline(lm(final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear...~SPRTND[4,]), col = 'red')
# lines(lowess(SPRTND[4,],final_biomass_easthemlock_SPRTND$spp_agbSPRTND.4..nyear...), col = 'blue')
#
#
# #MPLANT
# par(mfrow = c(2,2))
# plot(MPLANT[1,], final_biomass_sugarmaple_MPLANT$spp_agbMPLANT.1..nyear..., title(main = 'MPLANT Sugar Maple Sylvania'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_sugarmaple_MPLANT$spp_agbMPLANT.1..nyear...~MPLANT[1,]), col = 'red')
# lines(lowess(MPLANT[1,],final_biomass_sugarmaple_MPLANT$spp_agbMPLANT.1..nyear...), col = 'blue')
# plot(MPLANT[2,], final_biomass_yellowbirch_MPLANT$spp_agbMPLANT.2..nyear..., title(main = 'MPLANT Yellow Birch Sylvania'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_yellowbirch_MPLANT$spp_agbMPLANT.2..nyear...~MPLANT[2,]), col = 'red')
# lines(lowess(MPLANT[2,],final_biomass_yellowbirch_MPLANT$spp_agbMPLANT.2..nyear...), col = 'blue')
# plot(MPLANT[3,], final_biomass_northwhitecedar_MPLANT$spp_agbMPLANT.3..nyear..., title(main = 'MPLANT Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_northwhitecedar_MPLANT$spp_agbMPLANT.3..nyear...~MPLANT[3,]), col = 'red')
# lines(lowess(MPLANT[3,],final_biomass_northwhitecedar_MPLANT$spp_agbMPLANT.3..nyear...), col = 'blue')
# plot(MPLANT[4,], final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear..., title(main = 'MPLANT Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'MPLANT')
# abline(lm(final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear...~MPLANT[4,]), col = 'red')
# lines(lowess(MPLANT[4,],final_biomass_easthemlock_MPLANT$spp_agbMPLANT.4..nyear...), col = 'blue')
#
#
# #AGEMX
# par(mfrow = c(2,2))
# plot(AGEMX[1,], final_biomass_sugarmaple_AGEMX$spp_agbAGEMX.1..nyear..., title(main = 'AGEMX Sugar Maple Sylvania'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_sugarmaple_AGEMX$spp_agbAGEMX.1..nyear...~AGEMX[1,]), col = 'red')
# lines(lowess(AGEMX[1,],final_biomass_sugarmaple_AGEMX$spp_agbAGEMX.1..nyear...), col = 'blue')
# plot(AGEMX[2,], final_biomass_yellowbirch_AGEMX$spp_agbAGEMX.2..nyear..., title(main = 'AGEMX Yellow Birch Sylvania'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_yellowbirch_AGEMX$spp_agbAGEMX.2..nyear...~AGEMX[2,]), col = 'red')
# lines(lowess(AGEMX[2,],final_biomass_yellowbirch_AGEMX$spp_agbAGEMX.2..nyear...), col = 'blue')
# plot(AGEMX[3,], final_biomass_northwhitecedar_AGEMX$spp_agbAGEMX.3..nyear..., title(main = 'AGEMX Northern White Cedar Sylvania'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_northwhitecedar_AGEMX$spp_agbAGEMX.3..nyear...~AGEMX[3,]), col = 'red')
# lines(lowess(AGEMX[3,],final_biomass_northwhitecedar_AGEMX$spp_agbAGEMX.3..nyear...), col = 'blue')
# plot(AGEMX[4,], final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear..., title(main = 'AGEMX Eastern Hemlock Sylvania'), ylab = 'agb', xlab = 'AGEMX')
# abline(lm(final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear...~AGEMX[4,]), col = 'red')
# lines(lowess(AGEMX[4,],final_biomass_easthemlock_AGEMX$spp_agbAGEMX.4..nyear...), col = 'blue')
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="monte_carlo_sensitivity_analysis/Sylvania/Plots")


### Climate Visualizations ###
### These visualizations are for multiple climate scenarios
###need to include the correct weights in the same order as your met sample names for this to work correctly
### depending on your met runs sampled, will have to change met_runs_sample names
### Goose met runs
# met_runs_sample <- c('bcc.csm1.1_030.01', 'CCSM4_003.02', 'CCSM4_021.01', 'CCSM4_025.01', 'CCSM4_033.02', 'CCSM4_034.01', 'CCSM4_040.02', 'MIROC.ESM_034.01', 'MIROC.ESM_035.02', 'MPI.ESM.P_040.02')
# weights <- c( 0.001923811, 0.001795128,0.002178503, 0.003361148,0.002671322,0.001909697,0.001641673,0.001686571,0.001463229,0.002864214)
# ### HF met runs
#weights <- c(0.008330371, 0.002673155, 0.002429977, 0.003723038, 0.002806883, 0.003047707, 0.002728893, 0.002412901, 0.002726198, 0.003948004)
# met_runs_sample <- c('bcc.csm1.1_001.02', 'bcc.csm1.1_018.02', 'bcc.csm1.1_028.02','bcc.csm1.1_033.02','CCSM4_002.01','CCSM4_015.01','CCSM4_021.02','MIROC.ESM_035.02', 'MIROC.ESM_036.02', 'MIROC.ESM_036.02')
### Rooster met runs
#weights <- c(0.001701156,0.001790012,0.001879725,0.004614861,0.002847479,0.001426843,0.002129376,0.00554181,0.002076193,0.004147599)
# met_runs_sample <- c('bcc.csm1.1_008.01','bcc.csm1.1_014.01','CCSM4_004.01','CCSM4_007.01','CCSM4_011.02','CCSM4_013.02','CCSM4_028.01','MIROC.ESM_030.01','MIROC.ESM_033.02','MPI.ESM.P_020.01')
### `NRP` met runs
#weights <- c(0.00481159,0.002675446,0.024667743, 0.003623449,0.003855094,0.003503017,0.003412477,0.006339748,0.004847587,0.00710015)
#  met_runs_sample <- c('bcc.csm1.1_003.01','bcc.csm1.1_012.02', 'bcc.csm1.1_032.01','CCSM4_015.01','MIROC.ESM_002.01','MIROC.ESM_005.02','MIROC.ESM_015.02','MIROC.ESM_018.02','MIROC.ESM_030.01','MPI.ESM.P_011.01')
### Sylvania met runs
# met_runs_sample <- c('bcc.csm1.1_033.02', 'MIROC.ESM_001.01', 'MPI.ESM.P_002.02', 'MIROC.ESM_024.02', 'CCSM4_013.02', 'MIROC.ESM_031.01', 'CCSM4_007.01', 'MPI.ESM.P_024.01', 'MIROC.ESM_035.02', 'MIROC.ESM_020.01')
# weights <- c(0.004765123,0.002696855,0.002975437,0.015185098,0.002758079,0.002335665,0.00271624,0.032109081,0.002220669,0.002213662)
# HF update species met runs
met_runs_sample <- c('bcc.csm1.1_007.01', 'bcc.csm1.1_020.02', 'CCSM4_013.01', 'CCSM4_035.01', 'MIROC.ESM_020.01', 'MIROC.ESM_020.02', 'MIROC.ESM_024.01', 'MIROC.ESM_024.02', 'MIROC.ESM_028.01', 'MPI.ESM.P_018.01')
weights <- c(0.002394563,0.002701909,0.002875138,0.002607054,0.002763361,0.002913151,0.005425503,0.004292597,0.004631772,0.003425436)

### aggregation of all parameter runs for one climate scenario
for (i in met_runs_sample){
  save_tot =c()
  for (j in all_parameters){
    parameter = j
    save <- get(paste0('tot_agb',parameter,i))
    save <- save[nyear,]
    save_tot <- rbind(save_tot,save)
  }
  rownames(save_tot) <- all_parameters
  assign(paste0('tot_agb',i),save_tot)
  save = c()
}


### aggregation of all climate runs for one parameter

for (i in all_parameters){
  save_tot =c()
  for (j in met_runs_sample){
    save.param <- get(paste0('tot_agb',i,j))
    save.param <- save.param[nyear,]
    save_tot <- rbind(save_tot,save.param)
  }
  rownames(save_tot) <- met_runs_sample
  assign(paste0('tot_agb',i),save_tot)
  save.param = c()
}

total_list <- c('tot_agbAGEMX', 'tot_agbD3', 'tot_agbDMAX', 'tot_agbDMIN', 'tot_agbFrost','tot_agbG', 'tot_agbMPLANT', 'tot_agbSPRTND')

for (i in total_list){
  temp_CI = get(i)
  #print(temp_CI)
  tot_CI <- matrix(0,length(met_runs_sample),3)
  rownames(tot_CI) <- met_runs_sample
  colnames(tot_CI) <- c('Upper', 'Mean', 'Lower')
  tot_CI <- data.frame(tot_CI)
  for (j in 1:nrow(temp_CI)){
    tot_CI[j,] <- CI(temp_CI[j,], ci = 0.95)
    #print( CI(temp_CI[j,], ci = 0.95))
  }
  assign(paste0(i,'_CI'),tot_CI)
}

total_list_CI <- c('tot_agbAGEMX_CI', 'tot_agbD3_CI', 'tot_agbDMAX_CI', 'tot_agbDMIN_CI', 'tot_agbFrost_CI','tot_agbG_CI', 'tot_agbMPLANT_CI', 'tot_agbSPRTND_CI')

for (i in total_list_CI){
  par(mfrow = c(1,1))
  temp <- get(i)
  temp_plot <- ggplot(temp, aes(x = weights, y = temp[,2])) + geom_point(position = position_dodge(width = 0.4), size = 2) + geom_errorbar(aes(ymax = temp[,1]), ymin = temp[,3]) + labs(title = paste0(i,'_',site), x = ('weight of met run'), y = 'agb') + xlim(0,.004)
  print(temp_plot)
}


rowMeans(tot_agbAGEMX)
plot(weights, rowMeans(tot_agbAGEMX),pch =21, bg = 'red', ylim = c(0.1,1.2), ylab ='agb', main = paste0(site,' Parameter Distribution by Climate Scenario'))
points(weights, rowMeans(tot_agbD3), pch =21, bg = 'green')
points(weights, rowMeans(tot_agbDMAX), pch =21, bg ='blue')
points(weights, rowMeans(tot_agbDMIN), pch =21, bg ='orange')
points(weights, rowMeans(tot_agbFrost), pch =21, bg = 'pink')
points(weights, rowMeans(tot_agbG), pch =21, bg = 'yellow')
points(weights, rowMeans(tot_agbMPLANT), pch = 21, bg = 'black')
points(weights, rowMeans(tot_agbSPRTND), pch =21, bg = 'dark green')
legend('topright', legend = c('AGEMX', 'D3', 'DMAX', 'DMIN', 'Frost', 'G', 'MPLANT', 'SPRTND'), fill = c('red', 'green', 'blue', 'orange', 'pink', 'yellow', ' black', 'dark green'))

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
dir = paste0('monte_carlo_sensitivity_analysis/',site,'/','Plots/New_species')
file.copy(from=plots.png.paths, to=dir)

