### SDA runs visualization script

rm(list=ls())
library(linkages)
library(ggplot2)
library(data.table)
library(dplyr)
library(config)
library(XML)
library(methods)
library(janitor)

###################

### user inputs ###

###################

methods  = c('default','chojnacky_normal','lambert_normal')
site = 'Harvard Forest' #specify site

for (j in methods){


#setwd('/Volumes/My Passport/HF_default')
dir = paste0('/Users/haydengallo/Documents/linkages_package/HF_',j,'_1900') #change working directory
setwd(dir)

sppname = c('red maple','american beech','red oak','black oak','eastern hemlock') # HF
#sppname = c('red maple','american beech','red spruce','white pine','red oak') # RH
#sppname = c('red maple','sugar maple','white pine','white oak','red oak') # GE
#sppname = c('sugar maple', 'yellow birch', 'american beech', 'white ash', 'eastern hemlock') # NRP12
#sppname = c('red maple','white pine','red oak', 'eastern hemlock')
#sppname = c('sugar maple', 'yellow birch', 'northern white cedar', 'eastern hemlock') #Sylvania

load('/Users/haydengallo/Documents/linkages_package/sda.obs.HARVARD.Rdata') # load obs data for correct site

obs.mean <- obs.list$obs.mean
obs.cov <- obs.list$obs.cov

# read in settings

settings <- xmlParse('pecan.SDA.xml')
#settings <- read.settings("pecan.SDA.xml")
settings <- xmlToList(settings)

n = as.numeric(settings$ensemble$size)
spp = length(settings$pfts)
years = (lubridate::year(settings$state.data.assimilation$start.date)+1):lubridate::year(settings$state.data.assimilation$end.date)

adj.agb.total = matrix(0,n,length(years))
adj.agb.pft.array = array(0,dim = c(n, length(years), spp))
pred.agb.total = matrix(0,n,length(years))
pred.agb.pft.array = array(0,dim = c(n, length(years), spp))
bias.total = matrix(0,n,length(years))
bias.pft.array = array(0,dim = c(n, length(years), spp))

load('./SDA/sda.output.Rdata') # load correct output
for (i in 1:length(years)){
  pred.agb.total[,i] = apply(FORECAST[[i+1]], 1, sum)
  pred.agb.pft.array[,i,] = FORECAST[[i+1]]
  adj.agb.total[,i] = apply(ANALYSIS[[i+1]],1,sum)
  adj.agb.pft.array[,i,] = as.matrix(ANALYSIS[[i+1]])
  bias.total[,i] = adj.agb.total[,i] - pred.agb.total[,i]
  bias.pft.array[,i,] = adj.agb.pft.array[,i,] - pred.agb.pft.array[,i,]
}

### Push to dataframes

# convert to data frames so we can use ggplot
# melt, name columns, adjust, and add type column
# make adjusted values at a slightly later time point so not overlapping entirely if same
adj.agb.total.melt = reshape::melt(adj.agb.total)
colnames(adj.agb.total.melt) = c('ensemble','year','agb')
adj.agb.total.melt$year = adj.agb.total.melt$year + years[1] - 1
adj.agb.total.melt$type = rep('adjusted',length(adj.agb.total.melt$year))

adj.agb.pft.melt = reshape::melt(adj.agb.pft.array)
colnames(adj.agb.pft.melt) = c('ensemble','year','species','agb')
adj.agb.pft.melt$year = adj.agb.pft.melt$year + years[1] - 1
adj.agb.pft.melt$type = rep('adjusted',length(adj.agb.pft.melt$year))

pred.agb.total.melt = reshape::melt(pred.agb.total)
colnames(pred.agb.total.melt) = c('ensemble','year','agb')
pred.agb.total.melt$year = pred.agb.total.melt$year + years[1] - 1
pred.agb.total.melt$type = rep('predicted',length(pred.agb.total.melt$year))

method_pred_agb_melt = reshape::melt(pred.agb.pft.array)
colnames(method_pred_agb_melt) = c('ensemble','year','species','agb')
method_pred_agb_melt$year = method_pred_agb_melt$year + years[1] - 1
method_pred_agb_melt$type = rep('predicted',length(method_pred_agb_melt$year))

bias.total.melt = reshape::melt(bias.total)
colnames(bias.total.melt) = c('ensemble','year','bias')
bias.total.melt$year = bias.total.melt$year + years[1] - 1

bias.pft.array.melt = reshape::melt(bias.pft.array)
colnames(bias.pft.array.melt) = c('ensemble', 'year', 'species', 'bias')
bias.pft.array.melt$year = bias.pft.array.melt$year + years[1] - 1

# bias.pft.melt = reshape::melt(bias.pft.array)
# colnames(bias.pft.melt) = c('ensemble','year','species','bias')
# bias.pft.melt$year = bias.pft.melt$year + years[1] - 1

# format observed data
inds = which(lubridate::year(names(obs.mean)) %in% years)
agb.pft.obs = matrix(0,length(inds),spp)
agb.pft.var = matrix(0,length(inds),spp)
agb.obs.tot.var = vector()
track = 1
for (yr in inds){
  agb.pft.obs[track,] = obs.mean[[yr]]
  agb.pft.var[track,] = diag(obs.cov[[yr]])
  agb.obs.tot.var[track] = sum(colSums(obs.cov[[yr]]))
  track = track + 1
}
agb.pft.obs.melt = reshape2::melt(agb.pft.obs)
names(agb.pft.obs.melt) = c('year','species','agb.obs')
agb.pft.obs.melt$year = agb.pft.obs.melt$year + years[1] - 1
agb.pft.obs.melt$species = plyr::mapvalues(agb.pft.obs.melt$species, from = c(1:spp), to = sppname)
agb.pft.obs.melt$species = as.factor(agb.pft.obs.melt$species)

agb.pft.var.melt = reshape2::melt(agb.pft.var)
names(agb.pft.var.melt) = c('year','species','agb.var')
agb.pft.var.melt$species = plyr::mapvalues(agb.pft.var.melt$species, from = c(1:spp), to = sppname)
agb.pft.var.melt$year = agb.pft.var.melt$year + years[1] - 1
agb.pft.obs.melt = left_join(agb.pft.obs.melt, agb.pft.var.melt, by = c('year','species')) %>%
  mutate(b05.data = qnorm(0.05, mean = agb.obs, sd = sqrt(agb.var)),
         b95.data = qnorm(0.95, mean = agb.obs,sd = sqrt(agb.var)),
         b25.data = qnorm(0.25, mean = agb.obs,sd = sqrt(agb.var)),
         b75.data = qnorm(0.75, mean = agb.obs,sd = sqrt(agb.var)))

agb.obs.var = as.data.frame(agb.obs.tot.var)
colnames(agb.obs.var) = c('total.var')
agb.obs.var$year = c(1:length(inds)) + years[1] - 1

agb.obs.total = agb.pft.obs.melt %>% group_by(year) %>% summarize(total.obs = sum(agb.obs)) %>%
  left_join(agb.obs.var, by = 'year') %>% mutate(b05.data = qnorm(0.05, mean = total.obs, sd = sqrt(total.var)),
                                                 b95.data = qnorm(0.95, mean = total.obs,sd = sqrt(total.var)),
                                                 b25.data = qnorm(0.25, mean = total.obs,sd = sqrt(total.var)),
                                                 b75.data = qnorm(0.75, mean = total.obs,sd = sqrt(total.var))
  )

assign(paste0('agb_obs_total_',j),agb.obs.total)


method_pred_agb = method_pred_agb_melt %>%
  group_by(year, species) %>%
  summarize(b05 = quantile(agb,0.05),
            b95 = quantile(agb,0.95),
            b25 = quantile(agb,0.25),
            b75 = quantile(agb,0.75),
            mean.pred = mean(agb))
method_pred_agb$species = as.factor(method_pred_agb$species)
method_pred_agb$species = plyr::mapvalues(method_pred_agb$species, from = c(1:spp), to = sppname)

pred.intervals = pred.agb.total.melt %>%
  group_by(year) %>%
  summarize(b05 = quantile(agb,0.05),
            b95 = quantile(agb,0.95),
            b25 = quantile(agb,0.25),
            b75 = quantile(agb,0.75),
            mean.pred = mean(agb))
adj.intervals = adj.agb.total.melt %>%
  group_by(year) %>%
  summarize(b05 = quantile(agb,0.05),
            b95 = quantile(agb,0.95),
            b25 = quantile(agb,0.25),
            b75 = quantile(agb,0.75),
            mean.adj = mean(agb))
just.mean.tot = adj.intervals %>% dplyr::select(year, mean.adj)


assign(paste0('pred_agb_',j),method_pred_agb)
assign(paste0('pred_intervals_',j),pred.intervals)
assign(paste0('adj_intervals_',j),adj.intervals)
assign(paste0('just_mean_tot_',j),just.mean.tot)




################################################

### Plotting Bias by Species for each method ###

################################################

methods_title = c('Default', 'Chojnacky', 'Lambert')

bias_pft_plot = bias.pft.array.melt %>% group_by(year, species) %>%
  summarize(mean.bias = mean(bias),
            b05 = quantile(bias,0.05),
            b95 = quantile(bias,0.95),
            b25 = quantile(bias,0.25),
            b75 = quantile(bias,0.75))
bias_pft_plot$species = as.factor(bias_pft_plot$species)
bias_pft_plot$species = plyr::mapvalues(bias_pft_plot$species, from = c(1:spp), to = sppname)

bias_by_species = bias_pft_plot %>% ggplot(aes(x = year, y = mean.bias)) +
  geom_ribbon(aes(ymin = b05, ymax = b95, fill = '90%')) +
  geom_ribbon(aes(ymin = b25, ymax = b75, fill = '50%')) +
  geom_line(aes(col = 'mean')) +
  scale_color_manual(values = c('mean'='black'),name =NULL) +
  scale_fill_manual(values = c('90%'='coral2','50%'='rosybrown1')) +
  facet_grid(species~., scales='free') +
  geom_hline(yintercept = 0) +
  labs(col = 'species', title = 'Bias by Species Original Allometric Equation',
       y = 'aboveground biomass (Kg C/m2)')

assign(paste0('bias_by_species_',j),bias_by_species)
assign(paste0('bias_pft_plot_',j),bias_pft_plot)

}

bias_by_species_chojnacky_normal
bias_by_species_default
bias_by_species_lambert_normal

bias_pft_plot_chojnacky_normal$method = c('Chojnacky')
bias_pft_plot_default$method = c('Default')
bias_pft_plot_lambert_normal$method = c('Lambert')

all_methods_bias_pft = rbind(bias_pft_plot_chojnacky_normal, bias_pft_plot_default,bias_pft_plot_lambert_normal)

all_methods_bias_pft <- split(all_methods_bias_pft, f = all_methods_bias_pft$species)

sppnames_cleaned <- make_clean_names(sppname, case = 'snake')

for (i in 1:length(sppname)){

  species_bias_plot = all_methods_bias_pft[[i]] %>% ggplot(aes(x = year, y = mean.bias)) +
    geom_ribbon(aes(ymin = b05, ymax = b95, fill = '90%')) +
    geom_ribbon(aes(ymin = b25, ymax = b75, fill = '50%')) +
    geom_line(aes(col = 'mean')) +
    scale_color_manual(values = c('mean'='black'),name =NULL) +
    scale_fill_manual(values = c('90%'='coral2','50%'='rosybrown1')) +
    facet_grid(method~., scales='free') +
    geom_hline(yintercept = 0) +
    labs(col = 'species', title = paste0(sppname[i], ' Bias by Allometric Method ', site),
         y = 'aboveground biomass (Kg C/m2)')

  print(species_bias_plot)
  #ggsave(plot = species_bias_plot, filename = paste0('/Users/haydengallo/Documents/linkages_package/HF_long_run_plots/bias_pft_plot_',sppnames_cleaned[i],'.jpg'))
  #assign(paste0('bias_pft_plot_',sppnames_cleaned[i]),species_bias_plot)

}

################################################

### Plotting Bias by Species for each method ###

################################################

default_plot_agb = left_join(pred_intervals_default, agb_obs_total_default, by = c('year'))  %>% left_join(just_mean_tot_default, by = c('year'))
lambert_plot_agb = left_join(pred_intervals_lambert_normal, agb_obs_total_lambert_normal, by = c('year'))  %>% left_join(just_mean_tot_chojnacky_normal, by = c('year'))
chojnacky_plot_agb = left_join(pred_intervals_chojnacky_normal, agb_obs_total_lambert_normal, by = c('year'))  %>% left_join(just_mean_tot_chojnacky_normal, by = c('year'))


method_tot_abg_comp_plot = ggplot() +
  geom_line(data = default_plot_agb, aes(x = year, y = mean.pred)) +
  geom_ribbon(data = default_plot_agb, aes(x = year, ymin = b05, ymax = b95, fill = '90% Default')) +
  geom_ribbon(data = default_plot_agb, aes(x = year, ymin = b25, ymax = b75, fill = '50% Default')) +
  geom_line(data = default_plot_agb, aes(x = year, y = mean.pred)) +
  scale_color_manual(values = c('mean'='black'),name =NULL) +
  geom_line(data = lambert_plot_agb, aes(x = year, y = mean.pred)) +
  geom_ribbon(data = lambert_plot_agb, aes(x = year, ymin = b05, ymax = b95, fill = '90% Lambert')) +
  geom_ribbon(data = lambert_plot_agb, aes(x = year, ymin = b25, ymax = b75, fill = '50% Lambert')) +
  geom_line(data = lambert_plot_agb, aes(x = year, y = mean.pred)) +
  geom_line(data = chojnacky_plot_agb, aes(x = year, y = mean.pred)) +
  geom_ribbon(data = chojnacky_plot_agb, aes(x = year, ymin = b05, ymax = b95, fill = '90% Chojnacky')) +
  geom_ribbon(data = chojnacky_plot_agb, aes(x = year, ymin = b25, ymax = b75, fill = '50% Chojnacky')) +
  geom_line(data = chojnacky_plot_agb, aes(x = year, y = mean.pred)) +
  scale_fill_manual(values = c('90% Default'='coral2','50% Default'='rosybrown1','90% Lambert'='blue','50% Lambert'='lightblue','90% Chojnacky'='darkgreen','50% Chojnacky'='lightgreen' )) +
  labs(title = paste0('Allometric Method Aboveground Biomass Comparison at ', site), y = 'aboveground biomass (Kg C/m2)')

method_tot_abg_comp_plot

###########################################################

### Comparison of CIs for LAI specific and non-specific ###

###########################################################










