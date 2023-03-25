### LAI implementation in LINKAGES
### following procedure from Hall and Hollinger 2000
### Species Specific Data from: https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1231
### Species Specific Data actually from:

rm(list=ls())
library(linkages)
library(ggplot2)
library(data.table)
library(dplyr)
dir = '/Users/haydengallo/Documents/linkages_package'
setwd(dir)

sites <- c('Goose', 'HF', 'NRP', 'Rooster', 'Sylvania')
#sites <- c('Sylvania')

for (i in sites){

site = i
numruns = 100
nyear_user = 200

### must specify a method for LAI, either:
  # 1. 'normal'
  # 2. 'species_specific_LAI

Method = 'species_specific_LAI'

if (site == 'HF'){

###HF Input Adjustments

input = 'HF.linkages.input.RData'
load(input)

input = 'Harvard_Met_Input.Rdata'
load(input)

nyear = nyear_user
end.year = 2015
start.year = end.year-nyear_user+1
precip.mat <- tail(precip.mat, n = nyear_user)
temp.mat <- tail(temp.mat, n = nyear_user)
}

if (site == 'NRP'){

###NRP Input Adjustments

input = 'NRP.linkages.input.Rdata'
load(input)

input = 'NRP_met_data_bcc.csm1.1_032.01.Rdata'
load(input)

nyear = nyear_user
end.year = 2015
start.year = end.year-nyear_user+1
precip.mat <- tail(precip.mat, n = nyear_user)
temp.mat <- tail(temp.mat, n = nyear_user)
}

###Rooster Input Adjustments

if (site == 'Rooster'){

input = 'Rooster.linkages.input.Rdata'
load(input)

input = 'Rooster_met_data_MPI.ESM.P_032.01.Rdata'
load(input)

nyear = nyear_user
end.year = 2015
start.year = end.year-nyear_user+1
precip.mat <- tail(precip.mat, n = nyear_user)
temp.mat <- tail(temp.mat, n = nyear_user)

}

if (site == 'Goose'){

###Goose Input Adjustments

input = 'Goose.linkages.input.Rdata'
load(input)

input = 'Goose_Met_MPI.ESM.P_027.01.RData'
load(input)
nyear = nyear_user
end.year = 2015
start.year = end.year-nyear_user+1
precip.mat <- tail(precip.mat, n = nyear_user)
temp.mat <- tail(temp.mat, n = nyear_user)

}

if (site == 'Sylvania'){

###Sylvania Input Adjustments

input = 'Sylvania.linkages.input.Rdata'
load(input)

input = 'Sylvania_met_data_bcc.csm1.1_024.02.Rdata'
load(input)

nyear = nyear_user
end.year = 2015
start.year = end.year-nyear_user+1
precip.mat <- tail(precip.mat, n = nyear_user)
temp.mat <- tail(temp.mat, n = nyear_user)

}

LAI.data <- fread("LAI_data.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)

filter(LAI.data, )

LAI.data <- subset(LAI.data, LAI.data$TraitID == 3115)
unique(LAI.data$SpeciesName)
LAI.data <- LAI.data[!is.na(LAI.data$StdValue)]
LAI.data <- split(LAI.data, f = LAI.data$SpeciesName)



LAI.data$`Betula alleghaniensis` <- rbind(LAI.data$`Betula alleghaniensis`,LAI.data$`Betula lutea`)
LAI.data$`Fagus grandifolia` <- rbind(LAI.data$`Fagus grandifolia`, LAI.data$`Fagus americana`)
LAI.data$`Quercus rubra` <- rbind(LAI.data$`Quercus rubra`, LAI.data$`Quercus rubra I`)

LAI.data <- LAI.data[-c(5,6,12)]
LAI.names <-names(LAI.data)


mean_SLA <- matrix(0,length(LAI.names),1)
rownames(mean_SLA) <- LAI.names
colnames(mean_SLA) <- 'Mean SLA mm2/mg'

for (i in 1:length(LAI.data)){
  temp = LAI.data[i]
  temp <- data.frame(temp)
  mean_SLA[i,1] <- colMeans(temp[,21, drop=FALSE])
  temp_name = LAI.names[i]
  assign(temp_name,temp)
}

mean_SLA <- data.frame(mean_SLA)
#mm2/mg -> g/m^2
#(mg/mm^2)*1000

mean_SLA$SLM_g_m2 <- (1/(mean_SLA$Mean.SLA.mm2.mg))*1000

mean_SLA$Spp_Name <- c('Red Maple', 'Sugar Maple', 'Yellow Birch', 'Sweet Birch','American Beech', 'White Ash', 'Eastern White Pine', 'White Oak', 'Red Oak', 'Northern White Cedar', 'Eastern Hemlock')
if (site == 'Goose'){spp.params$Spp_Name <- c('American Beech','Eastern White Pine','White Oak','Chestnut Oak', 'Red Oak')}
if (site == 'Rooster'){spp.params$Spp_Name <- c('Red Maple','American Beech','Red Spruce','Eastern White Pine', 'Red Oak')}
if (site == 'NRP'){spp.params$Spp_Name <- c('Sweet Birch','Eastern White Pine','Red Oak','Eastern Hemlock')}
if (site == 'Sylvania'){spp.params$Spp_Name <- c('Sugar Maple','Yellow Birch','Northern White Cedar','Eastern Hemlock')}


spp.params <- inner_join(spp.params,mean_SLA, by = 'Spp_Name')
if (site == 'Rooster'){switch.mat <- switch.mat[-c(3),]}
if (site == 'Rooster'){rownames(switch.mat) <- c('American Beech','Eastern White Pine','White Oak', 'Red Oak')}
if (site == 'Goose'){switch.mat <- switch.mat[-c(4),]}
if (site == 'Goose'){rownames(switch.mat) <- c('American Beech','Eastern White Pine','White Oak', 'Red Oak')}
if (site == 'Goose'){nspec = 4}
if (site == 'Rooster'){nspec = 4}
#
for (i in 1:numruns){
  #print(i)

  # create new run folder
  new_dir = paste0(Method,'/',site,'/',toString(i))
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
}

write.csv(mean_SLA,"mean_SLA_table.csv", row.names = TRUE)


### Visualizing runs
# pick site to visualize
site = 'Goose'
nyear  = 300
numruns = 300
nspec = 4

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
params <- array(0,c(nspec,28,numruns))
varied_param <- matrix(0,nspec,numruns)

different_methods <- c('Original', 'Original_Fixed', 'LAI')

for (l in different_methods){

for (i in 1:numruns){

  # find directory with input and output file
  # this directory for use with changing met
  thisdir <- paste0(l,'/',site,'/',toString(i))
  load(paste0(thisdir,'/','linkages.out.Rdata'))
  load(paste0(thisdir,'/','linkages.input.Rdata'))

  # collect parameter information

  params[,,i] <- as.double(as.matrix(spp.params))
  #varied_param[,i] <- spp.params[,position]

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


  assign(paste0("avg_age_",l),avg_age)
  assign(paste0("avg_diam_",l),avg_diam)
  assign(paste0("tot_agb_",l),tot_agb)
  assign(paste0("spp_agb_",l),spp_agb)
  assign(paste0("spp_trees_",l),spp_trees)
  assign(paste0("spp_fcomp_",l),spp_fcomp)
  assign(paste0("spp_lgfval_",l),spp_lgfval)
  assign(paste0("spp_lgfname_",l),spp_lgfname)
  assign(paste0("spp_birth_",l),spp_birth)
  assign(paste0("spp_death_",l),spp_death)
  assign(paste0("params_",l),params)
}

spp_agb_LAI
spp_agb_Original

mean_agb_LAI <- rowMeans(tot_agb_LAI)
mean_agb_Original_Fixed <- rowMeans(tot_agb_Original_Fixed)
mean_agb_Original <- rowMeans(tot_agb_Original)
mean_agb_LAI <- data.frame(mean_agb_LAI)
mean_agb_Original_Fixed <- data.frame(mean_agb_Original_Fixed)
mean_agb_original <- data.frame(mean_agb_Original)
mean_agb_LAI$year <- 1:nyear
mean_agb_Original_Fixed$year <- 1:nyear
#mean_agb_Original$year <- 1:nyear

par(mfrow=c(1,1))
plot(mean_agb_Original_Fixed$year, mean_agb_Original_Fixed$mean_agb_Original_Fixed, type = 'l', main = paste0("Original LAI method vs Updated ",site), col="red", xlab = 'year', ylab = 'agb', ylim = c(0,5))
lines(mean_agb_LAI$year, mean_agb_LAI$mean_agb_LAI, type = 'l', col = 'blue')
lines(mean_agb_LAI$year, mean_agb_Original, type = 'l', col = 'orange')
legend(x = 'topright', legend = c("Original Fixed", 'Updated LAI', 'Original'), fill = c('red', 'blue','orange'))

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
dir = paste0('LAI_Plots/')
file.copy(from=plots.png.paths, to=dir)



