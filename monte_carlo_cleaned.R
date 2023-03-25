### updated script for monte carlo sensitivity analysis to increase efficiences for running on other machines
### Remember to change the save path in both the running_linkages function and the monte_carlo_simulation function to your specifications

rm(list=ls())
library(linkages)
library(Rmisc)
library(readr)
library(ggplot2)

set.seed(8)
### User initial conditions
### Vary Met True or False
vary_met = 'True'
num_mets = 10
### pick site either HF, NRP, Sylvania, Rooster, Goose
site = "HF"
numruns = 2
nyear_user = 200

if (site == 'HF'){
  site.alt = 'HARVARD'

  input = 'Harvard.input.Rdata'
  load(input)

  input = 'Harvard_Met_Input.Rdata'
  load(input)

} else if (site == 'Goose'){
  site.alt = 'GOOSE'

  input = 'Goose.linkages.input.Rdata'
  load(input)

  input = 'Goose_Met_MPI.ESM.P_027.01.RData'
  load(input)

}  else if (site == 'Rooster'){
  site.alt = 'ROOSTER'

  input = 'Rooster.linkages.input.Rdata'
  load(input)

  input = 'Rooster_met_data_MPI.ESM.P_032.01.Rdata'
  load(input)

} else if (site == 'Sylvania'){
  site.alt = 'SYLVANIA'

  input = 'Sylvania.linkages.input.Rdata'
  load(input)

  input = 'Sylvania_met_data_bcc.csm1.1_024.02.Rdata'
  load(input)

} else if (site == 'NRP'){
  site.alt = site

  input = 'NRP.linkages.input.Rdata'
  load(input)

  input = 'NRP_met_data_bcc.csm1.1_032.01.Rdata'
  load(input)
}

if (vary_met == 'True'){
met_runs <- read_csv(paste0('Met_updated/',site,'_met/weights/ensemble-weights-',site.alt,'-prism.csv'))
met_runs_sample <- sample(met_runs$climate_model,num_mets)}
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

HF <- rbind(red_maple_params, american_beech_params, red_oak_params, black_oak_params, east_hemlock_params)
Goose <- rbind(american_beech_params, east_white_pine_params, white_oak_params, chestnut_oak_params, red_oak_params)
NRP <- rbind(black_birch_params, east_white_pine_params, red_oak_params, east_hemlock_params)
Rooster <- rbind(red_maple_params, american_beech_params, red_spruce_params, east_white_pine_params, red_oak_params)
Sylvania <- rbind(sugar_maple_params, yellow_birch_params, north_white_cedar_params, east_hemlock_params)

individual_species_params_list = list(HF,Goose,NRP,Rooster,Sylvania)
names(individual_species_params_list) = c('HF','Goose','NRP','Rooster','Sylvania')

D3_samples <- matrix(0,nspec,numruns)
MPLANT_samples <- matrix(0,nspec,numruns)
DMAX_samples <- matrix(0,nspec,numruns)
Frost_samples <- matrix(0,nspec,numruns)
AGEMX_samples <- matrix(0,nspec,numruns)
DMIN_samples<- matrix(0,nspec,numruns)
G_samples<- matrix(0,nspec,numruns)
SPRTND_samples<- matrix(0,nspec,numruns)


individual_species_params <- data.frame(individual_species_params_list[site])

for (i in 1:nrow(individual_species_params)){D3_samples[i,] <- rbeta(numruns, individual_species_params[i,2], individual_species_params[i,3])}
for (i in 1:nrow(individual_species_params)){MPLANT_samples[i,]<- runif(numruns, individual_species_params[i,4], individual_species_params[i,5])}
for (i in 1:nrow(individual_species_params)){DMAX_samples[i,]<- rnorm(numruns, individual_species_params[i,6], individual_species_params[i,7])}
for (i in 1:nrow(individual_species_params)){Frost_samples[i,] <- rnorm(numruns, individual_species_params[i,8], individual_species_params[i,9])}
for (i in 1:nrow(individual_species_params)){AGEMX_samples[i,] <- runif(numruns, individual_species_params[i,10], individual_species_params[i,11])}
for (i in 1:nrow(individual_species_params)){DMIN_samples[i,]  <- rnorm(numruns, individual_species_params[i,12], individual_species_params[i,13])}
for (i in 1:nrow(individual_species_params)){G_samples[i,] <- rnorm(numruns, individual_species_params[i,14], individual_species_params[i,15])}
for (i in 1:nrow(individual_species_params)){SPRTND_samples[i,] <- rnorm(numruns, individual_species_params[i,16], individual_species_params[i,17])}

# list of all parameters that are varied
all_parameters <- list('D3', 'MPLANT', 'DMAX', 'Frost', 'AGEMX', 'DMIN', 'G', 'SPRTND')


running_linkages <- function(spp.params, parameter, runnum, met){

  # create new run folder
  # choose correct new_dir based on whether or not you have chosen to vary met
  if (vary_met == 'True'){new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_updated_',site,'/species_update/',met,'/',parameter,'/',runnum)}
  else{new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_updated_',site,'/species_update/',parameter,'/',runnum)}
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

monte_carlo_simulation <- function(parameter, numruns){
  start.time <<- Sys.time()
  count = 0
  for (k in met_runs_sample){
    met = k
    input = paste0('Met_updated','/',site,'_met','/linkages/',met,'.Rdata')
    load(input)
    nyear <<- nyear_user
    end.year <<- 2015
    start.year <<- end.year-nyear_user+1
    precip.mat <<- tail(precip.mat, n = nyear_user)
    temp.mat <<- tail(temp.mat, n = nyear_user)
    for (l in all_parameters){
      parameter = l
      count = count + 1
      for (j in 1:numruns){
        if (vary_met == 'True'){new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_updated_',site,'/species_update/',met,'/',parameter)}
        else{new_dir = paste0('monte_carlo_sensitivity_analysis/efficiency_testing_updated_',site,'/species_update/',parameter)}
        if (!dir.exists(new_dir)){dir.create(new_dir, recursive = T)}
        temp_param <- get(paste0(all_parameters[count],'_samples'))
        spp.params[,paste0(all_parameters[parameter])] <- temp_param[,j]
        if (vary_met == 'True'){running_linkages(spp.params, parameter = parameter, runnum = j, met = met)}
        else {running_linkages(spp.params, parameter = parameter, runnum = j)}
      }
      count = 0
    }
    }
  end.time <<- Sys.time()
  }
LAI_method = 'normal'
monte_carlo_simulation(parameter = parameter, numruns = numruns)
run_time = end.time - start.time
run_time

