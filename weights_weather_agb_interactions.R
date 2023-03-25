### Looking at the interactions between weather, climate weights, and agb

rm(list=ls())
library(linkages)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)
setwd("~/Documents/linkages_package")

### user inputs, select site, number of years for previous monte carlo runs and whether you want to look at temp or precip matrix
site = 'HF'
nyear = 200
#precip or temp
met_data = 'temp'

#if (site == 'HF'){species_names <- c('red oak', 'red maple', 'american beech', 'yellow birch')} #HF
if (site == 'HF'){species_names <- c('red maple', 'american beech', 'red oak', 'black oak', 'eastern hemlock')} #HF
if (site == 'Goose'){species_names <- c('american beech', 'eastern white pine', 'white oak', 'chestnut oak', 'red oak')} #Goose
if (site == 'Rooster'){species_names <- c('red maple', 'american beech', 'red spruce', 'eastern white pine', 'red oak')} #Rooster
if (site == 'NRP'){species_names <- c('black birch', 'eastern white pine', 'red oak', 'eastern hemlock')} #NRP
if (site == 'Sylvania'){species_names <- c('sugar maple', 'yellow birch', 'northern white cedar', 'eastern hemlock')} #Sylvania

nspec <- length(species_names)

if (site == 'HF'){
  site.alt = 'HARVARD'
} else if (site == 'Goose'){
  site.alt = 'GOOSE'
}  else if (site == 'Rooster'){
  site.alt = 'ROOSTER'
} else if (site == 'Sylvania'){
  site.alt = 'SYLVANIA'
} else (site.alt = site)


## this just loads correct met data for selected site
csv_name <- paste0('Met_updated/',site,'_met/weights/ensemble-weights-',site.alt,'-prism.csv')

met_runs <- read_csv(csv_name)
met_runs <- met_runs[-4:-ncol(met_runs)]

average_monthly_climate_data <- matrix(0,nrow(met_runs),12)
rownames(average_monthly_climate_data) <- met_runs$climate_model
colnames(average_monthly_climate_data) <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

### this loop averages met data across each month for a specific met matrix
count = 0
for (i in met_runs$climate_model){
  met = i
  count = count + 1
  input = paste0('Met_updated','/',site,'_met','/linkages/',met,'.Rdata')
  load(input)
  average_monthly_climate_data[count,] <- colMeans(precip.mat)

}

average_monthly_climate_data_plotting <- matrix(0,nrow(met_runs),4)
average_monthly_climate_data_plotting <- data.frame(average_monthly_climate_data_plotting)
average_monthly_climate_data_plotting$X1 <- met_runs$climate_model
average_monthly_climate_data_plotting$X3 <- c(met_runs$wts)

### these next few loops transforms data for easier plotting and adds additional info like
### met run name, weight of that run, name of each month and average value

count = 0
for (j in months){
  count = count + 1
  met_count = 0
  month = j
for (i in met_runs$climate_model){
  met = i
  met_count = met_count +1
  input = paste0('Met_updated','/',site,'_met','/linkages/',met,'.Rdata')
  load(input)
  average_monthly_climate_data_plotting[met_count,2] <- mean(precip.mat[,count])
}
  average_monthly_climate_data_plotting$X4 <- month
  colnames(average_monthly_climate_data_plotting) <- c('Met Run', 'Average', 'Weight', 'Month')
  assign(paste0(j,'_average'), average_monthly_climate_data_plotting)
  average_monthly_climate_data_plotting <- matrix(0,nrow(met_runs),4)
  average_monthly_climate_data_plotting <- data.frame(average_monthly_climate_data_plotting)
  average_monthly_climate_data_plotting$X1 <- met_runs$climate_model
  average_monthly_climate_data_plotting$X3 <- c(met_runs$wts)
}

met_data_plotting <- rbind(January_average, February_average, March_average, April_average, May_average, June_average, July_average, August_average, September_average, October_average, November_average, December_average)

ggplot(met_data_plotting, aes(x = met_data_plotting$Weight, y = met_data_plotting$Average, color = Month)) + geom_point() +
xlab('met weight') + ylab('average monthly precip') + labs(title = paste0('Average ',met_data,' vs. met weight ',site))
average_monthly_climate_data <- data.frame(average_monthly_climate_data)
average_monthly_climate_data$weights <- c(met_runs$wts)
#average_monthly_climate_data <- data.frame(average_monthly_climate_data)


# ggplot(average_monthly_climate_data, aes(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,1])) + geom_point() + ylim(0,15) +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,2], color = 'red') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,3], color = 'green') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,4], color = 'orange') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,5], color = 'purple') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,6], color = 'pink') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,7], color = 'blue') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,8], color = 'light blue') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,9], color = 'dark green') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,10], color = 'brown') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,11], color = 'slategray') +
#   geom_point(x = average_monthly_climate_data[,13], y = average_monthly_climate_data[,12], color = 'sienna') +
#   xlab('met weight') + ylab('average monthly precip') + labs(title = paste0('Average ',met_data,' vs. met weight ',site))
#
# plot(average_monthly_climate_data[,13], average_monthly_climate_data[,1])



### agb vs. weather and weights
### cannot proceed to this step unless you have run the varied met monte carlo simulation runs for the specificed site

#if (site == 'HF'){ met_runs_sample <- c('bcc.csm1.1_001.02', 'bcc.csm1.1_018.02', 'bcc.csm1.1_028.02','bcc.csm1.1_033.02','CCSM4_002.01','CCSM4_015.01','CCSM4_021.02','MIROC.ESM_035.02', 'MIROC.ESM_036.02', 'MPI.ESM.P_035.01')}
if (site == 'HF'){met_runs_sample <- c('bcc.csm1.1_007.01', 'bcc.csm1.1_020.02', 'CCSM4_013.01', 'CCSM4_035.01', 'MIROC.ESM_020.01', 'MIROC.ESM_020.02', 'MIROC.ESM_024.01', 'MIROC.ESM_024.02', 'MIROC.ESM_028.01', 'MPI.ESM.P_018.01')}
if (site == 'Goose'){ met_runs_sample <- c('bcc.csm1.1_030.01', 'CCSM4_003.02', 'CCSM4_021.01', 'CCSM4_025.01', 'CCSM4_033.02', 'CCSM4_034.01', 'CCSM4_040.02', 'MIROC.ESM_034.01', 'MIROC.ESM_035.02', 'MPI.ESM.P_040.02')}
if (site == 'Rooster'){ met_runs_sample <- c('bcc.csm1.1_008.01', 'bcc.csm1.1_014.01', 'CCSM4_004.01', 'CCSM4_007.01', 'CCSM4_011.02', 'CCSM4_013.02', 'CCSM4_028.01', 'MIROC.ESM_030.01', 'MIROC.ESM_033.02', 'MPI.ESM.P_020.01')}
if (site == 'NRP'){ met_runs_sample <- c('bcc.csm1.1_003.01', 'bcc.csm1.1_012.02', 'bcc.csm1.1_032.01', 'CCSM4_015.01', 'MIROC.ESM_002.01', 'MIROC.ESM_005.02', 'MIROC.ESM_015.02', "MIROC.ESM_018.02", 'MIROC.ESM_030.01', 'MPI.ESM.P_011.01')}
if (site == 'Sylvania'){met_runs_sample <- c('bcc.csm1.1_033.02', 'MIROC.ESM_001.01', 'MPI.ESM.P_002.02', 'MIROC.ESM_024.02', 'CCSM4_013.02', 'MIROC.ESM_031.01', 'CCSM4_007.01', 'MPI.ESM.P_024.01', 'MIROC.ESM_035.02', 'MIROC.ESM_020.01')}


all_parameters <- list('D3', 'MPLANT', 'DMAX', 'Frost', 'AGEMX', 'DMIN', 'G', 'SPRTND')
numruns = 200

### collecting met data

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
      #thisdir <- paste0('monte_carlo_sensitivity_analysis/',site,'/',met,'/',l,'/',toString(i))
      thisdir <- paste0('monte_carlo_sensitivity_analysis/',site,'/species_update/',met,'/',l,'/',toString(i))
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

  }
}

temp_species_avg <- matrix(0, nspec, length(met_runs_sample))
colnames(temp_species_avg) <- met_runs_sample

for (i in all_parameters){
  param = i
  count = 0
  for (k in met_runs_sample){
    count = count + 1
    met <<- k
    temp <- (get(paste0('spp_agb',i,k)))
    for (j in  1:nspec){
    temp_species_avg[j,count] <- (mean(temp[j,200,]))
    }
  }
  assign(paste0(param,met,'_species_average'), temp_species_avg)
}

temp_species_yearly_avg <- matrix(0,nyear,6)
temp_species_yearly_avg <- data.frame(temp_species_yearly_avg)
colnames(temp_species_yearly_avg) <- c('Average', 'Met', 'Weight', 'Average AGB', 'Species', 'Parameter Value')

### these loops grabs correct agb matrix for correct site and also correct met data
### then it takes the mean met data for each year of the run and then averages species biomass for each year
### then the data is collated for easier plotting, data is collected into matrices based on parameter

for (i in all_parameters){
  param = i
  count = 0
  for (k in met_runs_sample){
    count = count + 1
    met <<- k
    temp <- (get(paste0('spp_agb',i,k)))
    input = paste0('Met_updated','/',site,'_met','/linkages/',met,'.Rdata')
    load(input)
    end.year = 2015
    start.year = 1815
    nyear <- 200
    if (met_data == 'precip'){
      precip.mat <- tail(precip.mat, n = 201)
      precip.mat <- head(precip.mat, -1)}
    if (met_data == 'temp'){
      temp.mat <- tail(temp.mat, n = 201)
      temp.mat <- head(temp.mat, -1)}
    param_vals <- get(paste0(i,k))
    for (j in  1:nspec){
      ## for the 4 sites I have specified only would have 4 or 5 species, but if another site is added might have to revisit this line
      if (j == 1 ){species = species_names[1]}
      else if (j == 2 ) {species = species_names[2]}
      else if (j == 3 ) {species = species_names[3]}
      else if (j == 4) {species = species_names[4]}
      else if (j == 5) {species = species_names[5]}
      if (met_data == 'precip'){temp_species_yearly_avg[,1] <- rowMeans(precip.mat)}
      if (met_data == 'temp'){temp_species_yearly_avg[,1] <- rowMeans(temp.mat)}
      temp_species_yearly_avg[,2] <- met
      temp_species_yearly_avg[,4] <- rowMeans(temp[j,,])
      temp_species_yearly_avg[,5] <- species
      temp_species_yearly_avg[,6] <- param_vals[j,] #need to double check and make sure it is pulling correct param values, pretty sure it is but would be good to double check

      assign(paste0(met,j), temp_species_yearly_avg)
    }
    ## for the 4 sites I have specified only would have 4 or 5 species, but if another site is added might have to revisit this line
    if (nspec == 4){met_species_year_avg <- rbind(get(paste0(met,'1')), get(paste0(met,'2')), get(paste0(met,'3')), get(paste0(met,'4')))}
    if (nspec == 5){met_species_year_avg <- rbind(get(paste0(met,'1')), get(paste0(met,'2')), get(paste0(met,'3')), get(paste0(met,'4')), get(paste0(met,'5')))}
    assign(paste0(met,param,'_species_yearly_avg'), met_species_year_avg)
  }
}

for (i in all_parameters){
  param = i
  starter_frame <- matrix(0,1,6)
  starter_frame <- data.frame(starter_frame)
  colnames(starter_frame) <- c('Average', 'Met', 'Weight', 'Average AGB', 'Species', 'Parameter Value')
  for (k in met_runs_sample){
    met = k
    temp_frame <-get(paste0(met,param,'_species_yearly_avg'))
    starter_frame <- rbind(starter_frame,temp_frame)
  }
  starter_frame <- starter_frame[-1,]
  assign(paste0(i,"_weights_averages_mets"),starter_frame)
}

library(dplyr)
colnames(met_runs) <- c('number', 'Met', 'Weight')
### adds weight to matrices based on met run

AGEMX_weights_averages_mets <- inner_join(met_runs, AGEMX_weights_averages_mets, by = 'Met')
D3_weights_averages_mets <- inner_join(met_runs, D3_weights_averages_mets, by = 'Met')
DMAX_weights_averages_mets <- inner_join(met_runs, DMAX_weights_averages_mets, by = 'Met')
DMIN_weights_averages_mets <- inner_join(met_runs, DMIN_weights_averages_mets, by = 'Met')
Frost_weights_averages_mets <- inner_join(met_runs, Frost_weights_averages_mets, by = 'Met')
G_weights_averages_mets <- inner_join(met_runs, G_weights_averages_mets, by = 'Met')
MPLANT_weights_averages_mets <- inner_join(met_runs, MPLANT_weights_averages_mets, by = 'Met')
SPRTND_weights_averages_mets <- inner_join(met_runs, SPRTND_weights_averages_mets, by = 'Met')

all_averages <- c('AGEMX_weights_averages_mets', 'D3_weights_averages_mets', 'DMAX_weights_averages_mets','DMIN_weights_averages_mets', 'Frost_weights_averages_mets','G_weights_averages_mets','MPLANT_weights_averages_mets','SPRTND_weights_averages_mets')

# for (i in all_averages){
#   current <- get(i)
#   plot<- ggplot(current, aes(x = Average, y = 'Average AGB', color = Species, size = Weight.x)) + geom_point() +
#     xlab(paste0('Average',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, current))
#   print(plot)
# }

# ggplot(AGEMX_weights_averages_mets, aes(x= Average, y = AGEMX_weights_averages_mets$`Average AGB`, size = Weight.x, color = species)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' AGEMX'))
#
# ggplot(D3_weights_averages_mets, aes(x= Average, y = D3_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' D3'))
#
# ggplot(DMAX_weights_averages_mets, aes(x= Average, y = DMAX_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' DMAX'))
#
# ggplot(DMIN_weights_averages_mets, aes(x= Average, y = DMIN_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' DMIN'))
#
# ggplot(Frost_weights_averages_mets, aes(x= Average, y = Frost_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' Frost'))
#
# ggplot(G_weights_averages_mets, aes(x= Average, y = G_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' G'))
#
# ggplot(MPLANT_weights_averages_mets, aes(x= Average, y = MPLANT_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' MPLANT'))
#
# ggplot(SPRTND_weights_averages_mets, aes(x= Average, y = SPRTND_weights_averages_mets$`Average AGB`, color = Species, size = Weight.x)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' SPRTND'))

### scaled by parameter values

# ggplot(AGEMX_weights_averages_mets, aes(x= Average, y = AGEMX_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' AGEMX')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(D3_weights_averages_mets, aes(x= Average, y = D3_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' D3')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(DMAX_weights_averages_mets, aes(x= Average, y = DMAX_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' DMAX')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(DMIN_weights_averages_mets, aes(x= Average, y = DMIN_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' DMIN')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(Frost_weights_averages_mets, aes(x= Average, y = Frost_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' Frost')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(G_weights_averages_mets, aes(x= Average, y = G_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' G')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(MPLANT_weights_averages_mets, aes(x= Average, y = MPLANT_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' MPLANT')) + facet_wrap( ~Species , ncol=2)
#
# ggplot(SPRTND_weights_averages_mets, aes(x= Average, y = SPRTND_weights_averages_mets$`Average AGB`, size = Weight.x, color = `Parameter Value`)) + geom_point() +
#   xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0('Average ',met_data,' vs. AGB ',site, ' SPRTND')) + facet_wrap( ~Species , ncol=2, scales = 'free')

### Separate plots by species

out <- by(data = AGEMX_weights_averages_mets, INDICES = AGEMX_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' AGEMX'))

out <- by(data = D3_weights_averages_mets, INDICES = D3_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' D3'))

out <- by(data = DMAX_weights_averages_mets, INDICES = DMAX_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' DMAX'))

out <- by(data = DMIN_weights_averages_mets, INDICES = DMIN_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' DMIN'))

out <- by(data = Frost_weights_averages_mets, INDICES = Frost_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' Frost'))

out <- by(data = G_weights_averages_mets, INDICES = G_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' G'))

out <- by(data = MPLANT_weights_averages_mets, INDICES = MPLANT_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' MPLANT'))

out <- by(data = SPRTND_weights_averages_mets, INDICES = SPRTND_weights_averages_mets$Species, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(Average, `Average AGB`, color = `Parameter Value`, size = Weight.x)) +
    geom_point() +
    xlab(paste0('Average ',met_data)) + ylab('Average Yearly AGB') + labs(title = paste0(m$Species))
})

grid.arrange(grobs = out, top = paste0('Average ',met_data,' vs. AGB ',site, ' SPRTND'))

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
dir = paste0('monte_carlo_sensitivity_analysis/',site,'/','Plots/New_species')
file.copy(from=plots.png.paths, to=dir)

