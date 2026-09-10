# This code can be used to reproduce results from the paper Unequal barriers to nature’s contributions to people impact quality of life

# Margot Neyret, 2026

#### Libraries ####
library(data.table)
library(cowplot)
#library(corrplot)
library(glmmTMB)
library(piecewiseSEM)
library(tidySEM)
library(lavaanPlot)
library(semPlot)
library(semptools)
library(tidySEM)
library(ggplot2)
library(scales)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(ordinal)
library(lme4)
#devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)

#### Load data ####
ggthemr('dust')

res_dir = 'Results/'
Barriers_satisfaction = fread(file = "Barriers_data.csv")


# Order barriers for figures
Barriers_satisfaction[, Barrier := factor(Barrier, 
                             levels = c('Availability', 'Quality', 'Knowledge', 
                                        'Time','Cost','Legal',
                                        'Crowding',    'Motivation', 'Transport', 'Safety_social','Companionship', 'Health',
                                        'Infrastructure','Safety_env', 'Belonging',  'Management*', 'Culture', 'Sustainability*', 'n_barriers'))]


###############################################
#### Barriers differ across NCP categories ####
###############################################

## General statistics
Barriers_stats = unique(Barriers_satisfaction[, list(Respondent, NCP  ,  Barrier ,Barrier_value)])
# How many people reported at least 1 barrier?
Barriers_stats[Barrier != 'n_barriers' & Barrier_value > 0, length(unique(Respondent))]/ Barriers_stats[, length(unique(Respondent))]

# What are the most commonly reported barriers?
n = Barriers_stats[Barrier != 'n_barriers' & Barrier_value > 0, .N]
Barriers_stats[Barrier != 'n_barriers' & Barrier_value > 0, .N/n, by = Barrier]


## Comparisons across NCP and NCP categories

# Cast dataset, keeping only numeric values for socio-demog vraiables
Barriers_satisfaction[, Socio_value := as.numeric(Socio_value)]
Barriers_satisfaction = Barriers_satisfaction[!is.na(Socio_value),]
Barriers_satisfaction = dcast(Barriers_satisfaction, Respondent + NCP + Barrier + Barrier_value+ NCP_type + weights_hunters + Satisfaction_value +QoL_value ~ Socio, value.var = 'Socio_value')


freq_barriers = Barriers_satisfaction[Barrier != 'n_barriers', list(Barrier_value = sum(Barrier_value)), by = c('NCP_type', 'NCP', 'Barrier')]
freq_barriers[, value_stand_type := Barrier_value / sum(Barrier_value), by = NCP_type]
freq_barriers[, value_stand_ncp := Barrier_value / sum(Barrier_value), by = NCP]

# Barriers per NCP category
fig_barriers_NCPcat = ggplot(freq_barriers, aes(y = value_stand_type, x = Barrier, fill = NCP_type, group = Barrier)) +  
  facet_wrap(~NCP_type, nrow = 3) +
  geom_col(stat = 'identity', position = position_dodge()) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab('Frequency') + xlab('Barrier') +
  geom_rect(data = data.frame(NCP_type = c('Material', 'Non-material', 'Regulating'),
                              barrmax = c(6, 21, 3)), aes(xmin = barrmax+0.5, xmax = +Inf, ymin = -Inf, ymax = +Inf), inherit.aes = F, alpha = 0.3)  #+ xlim(c(-0.5, 7.5))

ggsave(plot = fig_barriers_NCPcat, paste0(res_dir, 'Fig_S3.pdf'), width = 8, height = 6)
 
# Barriers per NCP
a = ggplot(freq_barriers[NCP_type == 'Non-material',], aes(y = value_stand_ncp, Barrier, fill = NCP)) +  
                facet_wrap(~ NCP, scales = 'free_y', nrow = 3, ncol = 3) +
                geom_col(stat = 'identity', position = position_dodge()) + 
                scale_fill_viridis_d(option = "B", begin = 0.1, end = 0.9, name = 'NCP category') + 
                #  guides(fill = guide_legend(nrow = 3))+ 
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 45, hjust=1)) +
                ylab('Frequency') + xlab('Barrier') + xlab('') 
              
  b =            ggplot(freq_barriers[NCP_type == 'Regulating',], aes(y = value_stand_ncp, Barrier, fill = NCP)) +  
                facet_wrap(~ NCP, scales = 'free_y', nrow = 2, ncol = 3) +
                geom_col(stat = 'identity', position = position_dodge()) + 
                scale_fill_viridis_d(option = "B", begin = 0.1, end = 0.9, name = 'NCP category') + 
                #  guides(fill = guide_legend(nrow = 3))+ 
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 45, hjust=1)) +
                ylab('Frequency') + xlab('Barrier') + xlab('')
              
              
 c  =          ggplot(freq_barriers[NCP_type == 'Material',], aes(y = value_stand_ncp, Barrier, fill = NCP)) +  
                facet_wrap(~ NCP, scales = 'free_y', nrow = 1, ncol = 3) +
                geom_col(stat = 'identity', position = position_dodge()) + 
                scale_fill_viridis_d(option = "B", begin = 0.1, end = 0.9, name = 'NCP category') + 
                #  guides(fill = guide_legend(nrow = 3))+ 
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 45, hjust=1)) +
                ylab('Frequency') + xlab('Barrier') 

a <- a + theme(plot.margin = margin(t = 20, r = 5, b = 0, l = 5))
b <- b + theme(plot.margin = margin(t = 20, r = 5, b = 0, l = 5))
c <- c + theme(plot.margin = margin(t = 20, r = 5, b = 0, l = 5))

fig_barriers_NCP = 
  plot_grid( a, b, c,
             labels = c('Non-material NCP', 'Regulating NCP', 'Material NCP' ), 
             #label_y = 1.05,
             ncol = 1, rel_heights = c(3, 2, 1.2), align = 'hh')

ggsave(plot = fig_barriers_NCP, paste0(res_dir, 'FigS4.pdf'), width = 10, height = 12)


########################################################################
#### Marginalized groups tend to report more barriers to access NCP ####
########################################################################

# Select barriers with at least 15 respondents
barriers_nresp = Barriers_satisfaction[Barrier_value>0, length(unique(Respondent)), by = list(Barrier =Barrier)]

# Create table for results
all_barriers_results = data.table(expand.grid(
  Socio = c('Mobility_num','Man'  ,'Income_num' ,'Rural'),
  NCP_type = c('Non-material', 'Regulating',   'Material','All' ),
  Barrier = barriers_nresp[V1>=15,unique(Barrier)]))
all_barriers_results[Barrier !='n_barriers', NCP_type :='All']
all_barriers_results = unique(all_barriers_results)

# Loop across barriers and socio variables
for (j in  unique(all_barriers_results$Barrier)){
  print(j)
  if (j == 'n_barriers'){
    
    for (t in  c('Non-material', 'Regulating',   'Material')){
      if (t == 'Material'){
        mod = glmmTMB(data = Barriers_satisfaction[Barrier == 'n_barriers' & NCP_type == t , ] ,
                      Barrier_value ~   Mobility_num + Man + Income_num + Rural
                      + NCP + (1|Respondent)
                      , family = "poisson", weights = weights_hunters)
        
      }
      else {
        mod = glmmTMB(data = Barriers_satisfaction[Barrier == 'n_barriers' & NCP_type == t , ] ,
                      Barrier_value ~ Mobility_num + Man + Income_num + Rural
                      + (1|NCP) + (1|Respondent)
                      , family = "poisson", weights = weights_hunters)
        
      }
      
      for (k in c('Mobility_num', 'Man', 'Income_num', 'Rural')){
        all_barriers_results[Barrier == 'n_barriers' & NCP_type == t & Socio == k,  c('Estimate', 'Std.Error', 'P') := 
                                     as.list(summary(mod)$coefficients$cond[k, c(1, 2, 4)])]
      }}}
  
  if (j != 'n_barriers'){
    mod = glmmTMB(data = Barriers_satisfaction[Barrier == j, ] ,
                  Barrier_value ~ Mobility_num + Man + Income_num+Rural+
                    + (1|NCP) + (1|Respondent)
                  , family = "poisson", weights = weights_hunters)
    
    for (k in c('Mobility_num', 'Man', 'Income_num', 'Rural')){
      all_barriers_results[Barrier == j & Socio == k, c('Estimate', 'Std.Error', 'P') := 
                                   as.list(summary(mod)$coefficients$cond[k, c(1, 2, 4)])]
    }}
}


# Make nicer labels for plots
all_barriers_results[, pretty_socio := dplyr::recode(Socio, "Rural" = "Residence (rural)",
                                                           "Income_num" = "Income (high)",
                                                           "Mobility_num" = "Physical mobility (high)",
                                                           "Man" = "Gender (man)")]
all_barriers_results[, c('Estimate',   'Std.Error') := list(round(Estimate, 2), round(Std.Error, 2))]

all_barriers_results[, Label := factor(ifelse(P > 0.05, 'Not significant', 'Significant'),
                                             levels = c( 'Significant','Not significant'))]

# all_barriers_results contains all results in tables S1, S2

## Fig. 2
fig_n_barriers = ggplot(all_barriers_results[Barrier == 'n_barriers'& NCP_type != 'All' , ], 
                                    aes(x = NCP_type, y = Estimate, ymin = Estimate-Std.Error,
                                        ymax = Estimate+Std.Error, color = Label, alpha = Label )) + 
  geom_point() + geom_errorbar(width = 0.4) + coord_flip() + 
  geom_hline(yintercept = 0, color = 'grey') + 
  theme(legend.position = 'bottom', panel.grid.major = element_line(linetype="solid",size=0.1),
legend.title=element_blank()) +
  facet_wrap(~pretty_socio) +
  xlab('NCP category') + ylab("Effect on number of barriers reported") +
  scale_alpha_manual(values = c(1, 0.6))
 
ggsave(plot = fig_n_barriers, paste0(res_dir, 'Fig2.pdf'),
       width = 8, height = 4)

## Fig. 3
fig_ind_barriers = ggplot(all_barriers_results[Barrier != 'n_barriers' & Socio %in% c('Rural', "Income_num",'Mobility_num', 'Man'), ], 
                                      aes(x = Barrier, y = Estimate, ymin = Estimate-Std.Error,
                                          ymax = Estimate+Std.Error, color = Label, alpha = Label)) + 
  geom_point() + geom_errorbar(width = 0.5) +
  facet_wrap(~pretty_socio, nrow = 2, scales = 'free_x') + coord_flip() + 
  geom_hline(yintercept = 0, color = 'grey') + 
  theme(legend.position = 'bottom', panel.grid.major = element_line(linetype="solid",size=0.1),
legend.title=element_blank()) +
  geom_text(data = barriers_nresp[V1>15 & Barrier != "n_barriers",], 
          aes(label =  V1, x = Barrier, y = +Inf), 
          inherit.aes = F, size = 3, hjust = 1.1, vjust = 0.5, color = 'grey50' , fontface = "italic")+
  scale_alpha_manual(values = c(1, 0.6))
fig_ind_barriers
ggsave(plot = fig_ind_barriers, paste0(res_dir, 'Fig3.pdf'),
       width = 8, height = 7)



##########################################################################
#### Access barriers impact satisfaction with NCP and quality of life ####
##########################################################################

## General statistics
  Barriers_satisfaction_N = Barriers_satisfaction[Barrier == "n_barriers",]
Barriers_satisfaction_N[, nbarriers := Barrier_value]
Barriers_satisfaction_N[, Satisfaction_value := factor(Satisfaction_value, levels = c(1, 2, 3, 4))]

# Check overall satisfaction levels and n barriers
Barriers_satisfaction_N[!is.na(Satisfaction_value), table(Satisfaction_value)/.N]

clm_satis_barriers = clmm(Satisfaction_value ~ nbarriers + (1|NCP), data=Barriers_satisfaction_N, Hess=T, nAGQ=1, weights = weights_hunters)
summary(clm_satis_barriers)


## Fill table S3
# Who reports higher satisfaction?
clmm_satis_socio = clmm(Satisfaction_value~ Rural + Mobility_num + Man + Income_num + (1|NCP), 
                        data=Barriers_satisfaction_N, Hess=T, nAGQ=1, weights = weights_hunters)
summary(clmm_satis_socio)

Barriers_satisfaction_N[, table(Satisfaction_value,NCP)]

# Who reports higher connectedness?
clmm_connect_socio = clmm(as.factor(Connectedness_num)~ Rural + Mobility_num + Man + Income_num + (1|NCP), data=Barriers_satisfaction_N, Hess=T, nAGQ=1, weights = weights_hunters)
summary(clmm_connect_socio)

# Who reports higher quality of life?
# Need to merge the two first QoL values to have enough power
Barriers_satisfaction_N[, QoL_merge := QoL_value]
Barriers_satisfaction_N[QoL_merge == 1, QoL_merge := 2]
clmm_qol_socio = clmm(as.factor(QoL_merge)~ Rural + Mobility_num + Man + Income_num + (1|NCP), data=Barriers_satisfaction_N, Hess=T, nAGQ=1, weights = weights_hunters)
summary(clmm_qol_socio)


## SEM

# We need to make everything numeric again
Barriers_satisfaction_N[, Satisfaction_value := as.numeric(Satisfaction_value)]

# Model definition
modsem = "Satisfaction_value ~ nbarriers +  Mobility_num + Man + Income_num + Rural 
           QoL_value ~ Satisfaction_value + Mobility_num + Man + Income_num + Rural + Connectedness_num
          nbarriers ~ Mobility_num + Man + Income_num + Rural 
         Connectedness_num ~  Man + Income_num + Rural + Mobility_num + nbarriers

# Residuals correlations
 Satisfaction_value ~~ QoL_value
 Mobility_num ~~ Man
 Mobility_num ~~ Income_num
 Income_num ~~ Man
 Rural ~~ Man
 Income_num ~~ Rural
"


lavaan_sem = lavaan::sem(modsem, data = Barriers_satisfaction_N, sampling.weights = 'weights_hunters')

# Model results
summary(lavaan_sem)
lavaan::fitMeasures(lavaan_sem, c("chisq", "df", "pvalue", "cfi", "rmsea")) # Should be RMSEA <= 0.05; CFI >= 0.95 or more

# Plot model
mylayout = matrix(
  c(2, 2, # Value satisfaction
    3, 1, # QoL_value
    1, 3, # NBARRIERS
    2.5, 3, # Connectedness
    
    2, 3.9, # MOBILIT
    1, 4,# MAN
    3, 4.2, # INCOME NUM
    4, 4 # RURAL
  ),ncol = 2, byrow = TRUE)
p = semPaths(lavaan_sem, 'standardized', residuals = FALSE,
             style = "lisrel",
             layout = mylayout, fade = FALSE
)
p_pa2 <- mark_sig(p, lavaan_sem)
plot(p_pa2)
table_results(lavaan_sem)

# Sensitivity analysis: removing weights
lavaan_sem_noweight = lavaan::sem(modsem, data = Barriers_satisfaction_N)
p_noweight = semPaths(lavaan_sem_noweight, 'standardized', residuals = FALSE,
                      style = "lisrel",
                      layout = mylayout, fade = FALSE
)
p_pa_nw <- mark_sig(p_noweight, lavaan_sem_noweight)
plot(p_pa_nw)
table_results(lavaan_sem_noweight)

# Sensitivity analysis: check all paths independently
mod1_ordinal = clmm(factor(Satisfaction_value) ~ nbarriers +  Mobility_num + Man + Income_num + Rural+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )
summary(mod1_ordinal) 
mod2_ordinal = clmm(factor(QoL_value) ~ nbarriers + as.numeric(Satisfaction_value)+ Mobility_num + Man + Income_num + Rural+ Connectedness_num + (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )
summary(mod2_ordinal)
mod3 = glmer(nbarriers ~ Mobility_num + Man + Income_num + Rural+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters, family = 'poisson' )
mod4_ordinal = clmm(factor(Connectedness_num) ~  Man + Income_num + Rural + Mobility_num + nbarriers+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )
summary(mod4_ordinal)

# Sensitivity analysis: use piecewise SEM
mod1 = glmer(Satisfaction_value ~ nbarriers +  Mobility_num + Man + Income_num + Rural+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )
mod2 = glmer(QoL_value ~ Satisfaction_value  + Mobility_num + Man + Income_num + Rural + Connectedness_num + (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )
mod3 = glmer(nbarriers ~ Mobility_num + Man + Income_num + Rural+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters, family = 'poisson' )
mod4 = glmer(Connectedness_num ~  Man + Income_num + Rural + Mobility_num + nbarriers+ (1|NCP), Barriers_satisfaction_N, weights =weights_hunters )

sem = psem(mod1, mod2, mod3, mod4, data = Barriers_satisfaction_N)
plot(sem)
summary(sem)
coefs(sem)
