# This code can be used to merge the data later used for the paper Unequal barriers to nature’s contributions to people impact quality of life
# This script is provided for information only. The raw data is not distributed due to data privacy; instead, the pre-formatted dataset is provided.
# Margot Neyret, 2026


#### Libraries ####
library(data.table)
library(readxl)
library(terra)
library(ggplot2)
library(FactoMineR)
library(gridExtra)
library(scales)
library(cowplot)
library(factoextra)
library(corrplot)
library(ggthemr)

ggthemr('dust')

source("config.R")
res_dir = 'Results/'

#### Survey data ####
Survey0  = data.table(read_excel(paste0(survey_data_path,"Survey_data_final/2024-11-25_Responses.xlsx"),sheet = 1, skip = 1,
                                 col_types = c('guess', 'guess', 'guess', 'date', 'date', 'date', rep('guess', 1621-6))
))
Survey = Survey0

# Replace colnames
Match_names  = read_excel(paste0(survey_data_path,"Data_analysis/Recodes/Rename_results.xlsx"),sheet = 1)
match_name = Match_names$colName
names(match_name) = Match_names$colID

# Check if we're missing some columns
colnames(Survey)[! colnames(Survey) %in% Match_names$colID]
colnames(Survey) = match_name[colnames(Survey)]

# Remove tests
Survey = Survey[!is.na(Started),]

Survey[, Started := as.POSIXct(Started), by = Started]
Survey[, Finished:= as.POSIXct(Finished), by = Finished]

# Uncomment to keep only submitted answers
Survey = Survey[SubmittedYN == TRUE,]
Survey = Survey[Finished >  as.POSIXct('2024-04-03'),]

# Remove useless columns
Survey = Survey[, .SD, .SDcols = colnames(Survey)[colnames(Survey) != '_REMOVE']]


# Relative priority scores
ES = gsub('_PriorityScores', '', colnames(Survey)[grepl('_PriorityScores', colnames(Survey))]) # List of NCP

Survey[, `_totalscore` := sum(.SD), .SDcols = paste0( ES, '_PriorityScores'), by = Respondent] 
Survey[, `_totalscore`]
Survey[,  (paste0(ES, '_relPriorityScores')) := lapply(.SD,function(x){x/`_totalscore`}), .SDcols =  paste0(ES, '_PriorityScores' ), by = Respondent] 


Survey_melt = melt.data.table(Survey, id.vars = c( "Respondent", "Publication", "SubmittedYN",
                                                   "SubmittedTime" , "Started"     , "Finished"   ,
                                                   "ConsentPub"    , "ConsentPart" , "Approved"   ,
                                                   "Hidden"        , "Identified"  , "Processed"   ,
                                                   "Language"      , "AcceptPolicy"))
Survey_melt[, c('NCP', 'question', 'Spe1', 'Spe2', 'Spe3') := tstrsplit(variable, '_')]
Survey_melt[, NCP:=gsub(' ','', NCP)]

chosen_NCP = Survey_melt[!is.na(value) & question == "satisfaction" , list(chosenNCP = unique(NCP)), by = Respondent]
chosen_NCP_n = chosen_NCP[, list(n_chosen = .N), by = chosenNCP  ]
chosen_NCP_n[, chosenNCP := factor(chosenNCP, levels = chosen_NCP_n$chosenNCP[order(chosen_NCP_n$n_chosen)])]

# Add NCP category
chosen_NCP[, type := ifelse(
  chosenNCP %in% c('Water', 'Temp', 'Floods', 'Erosion', 'Climate', 'Air'), 'Regulating',
  ifelse(chosenNCP %in% c('Timber', 'Agri'), 'Material',
         'Non-material'))]


#### Socio-demographic data #### 
Socio = Survey_melt[question == 'Socio' & !is.na(value), list(Spe1, Spe2, Respondent, value)]

# Activities in or outside job
Activities_additional = fread(paste0(survey_data_path,'Data_analysis/Recodes/recode_activities.txt'), sep = '\t', header = TRUE)
Activities_additional[, c("Spe2a", "Spe2b", "Spe2c") := tstrsplit(Description, ';')]
Activities_additional = melt(Activities_additional, id.vars = c('Spe1',    'Respondent'), measure.vars = c('Spe2a',  'Spe2b',  'Spe2c'), value.name = 'Spe2')
Activities_additional = Activities_additional[!is.na(Spe2),]
Activities_additional$value = "TRUE"

Activities = rbind(Socio[Spe1 %in% c('OutsideJobNature', 'JobNature') & !is.na(Spe2) & !(Spe2 %in% c('no', 'other', 'others')),],             
                   Activities_additional[, list(Spe1, Respondent, Spe2, value)])
Activities[, value := as.numeric(as.logical(value))]
Activities[, Spe1 := paste(gsub('Nature', '', Spe1), Spe2, sep = '')]
Activities = Activities[, list(value = max(value)), by = c('Respondent', 'Spe1')]

Socio = rbind(Socio[!(Spe1 %in% c('OutsideJobNature', 'JobNature')), list(Spe1, Respondent, value)], Activities)

Socio = dcast(unique(Socio), Respondent ~ Spe1, value.var = 'value')

### Recode as factors ###
Socio[, ChildhoodRural := factor(Childhood, levels = c(
  "In vast majority in a city.", "Mostly in a city.", "Mostly in the countryside or in the mountains.", "In vast majority in the countryside or in the mountains."))]
Socio[, ChildhoodRegion := factor(ChildhoodRegion, levels = c("No.", "Yes, parly.", "Yes, entirely."))]
Socio[, TimeNature := factor(TimeNature,levels = c(
  "Less than 1 hour.",
  "More than 1 hour and less than 3 hours."  ,
  "More than 3 hours and less than 5 hours." ,
  "More than 5 hours and less than 7 hours." ,
  "More than 7 hours and less than 9 hours." ,
  "More than 9 hours and less than 11 hours.",
  "11 hours and more."
))]
Socio[, Mobility := factor(Mobility, levels = c("Very limited",  "Limited", "Very good", "Good"))]
Socio[, Connectedness := as.factor(as.numeric(gsub('INS_', '', Connectedness)))]
Socio[, Woman := factor(Gender, levels = c('Man', 'Prefer not to say', 'Woman'))]

Socio[, Diploma := factor(Diploma, levels = c(
  "No diploma" ,
  "General Certificate of Secondary Education",
  "National Vocational Qualification / Business and Technology Education Council",
  "A-levels",
  "BTEC Higher National Diploma" ,
  "Bachelor, Master or more"
))]

Socio[, Income := as.factor(Income)]
Socio[Income == "Prefer not to answer", Income := NA]

Socio[, Age := factor(Age, levels = c(
  "18-29 years" , "30-44 years" ,    "45-59 years"      ,       "60-74 years" ,      "75 years and more"
))]

Socio[, Household:= as.numeric(Household)]
Socio[Household>7, Household:= NA]

Socio[, TimeRegion := factor(TimeRegion, levels = c("I do not currently live in the study region", "5 years or less.", "5 to 10 years.", "10 to 20 years.", "20 years and more."))]

### Alternatively recode numerically to analyse as ordinal variables ###
Socio[, ChildhoodRural_num := dplyr::recode(Childhood,
                                            "In vast majority in the countryside or in the mountains." =3,
                                            "Mostly in a city." = 1,                                      
                                            "Mostly in the countryside or in the mountains." = 2,         
                                            "In vast majority in a city." = 0
)]
Socio[, ChildhoodRegion_num := dplyr::recode(ChildhoodRegion,
                                             "Yes, entirely." = 2,
                                             "Yes, parly." = 1,                                      
                                             "No." = 0
)]
Socio[, TimeNature_num := dplyr::recode(TimeNature,
                                        "Less than 1 hour." = 0,
                                        "More than 1 hour and less than 3 hours."  = 1,
                                        "More than 3 hours and less than 5 hours."  = 2,
                                        "More than 5 hours and less than 7 hours." = 3,
                                        "More than 7 hours and less than 9 hours."  = 4,
                                        "More than 9 hours and less than 11 hours." = 5,
                                        "11 hours and more." = 6
)]
Socio[, Mobility_num := dplyr::recode(Mobility,
                                      "Good" = 3,
                                      "Very good" = 4,
                                      "Limited" = 2,
                                      "Very limited" = 1)]
Socio[, Connectedness_num := as.numeric(gsub('INS_', '', Connectedness))]
Socio[, Diploma_num := as.numeric(dplyr::recode(Diploma,
                                                "BTEC Higher National Diploma" = '4',
                                                "Bachelor, Master or more" = '5',
                                                "No diploma" = '0',
                                                "A-levels" = '3',
                                                "Prefer not to say" = 'NA',
                                                "General Certificate of Secondary Education" = '1',
                                                "National Vocational Qualification / Business and Technology Education Council" = '2'))]
Socio[, Income_num := as.numeric(dplyr::recode(Income,
                                               "Prefer not to answer" = 'NA',
                                               "11 000<U+20AC> a year or less" = '1',
                                               "11 001<U+20AC> to 26 000<U+20AC>"  = '2'  ,
                                               "26 001<U+20AC> to 41 000<U+20AC>" = '3',
                                               "41 001<U+20AC> to 56 000<U+20AC>"  = '4'   ,
                                               "56 001<U+20AC> to 71 000<U+20AC>" = '5',
                                               "71 001<U+20AC> or more" = '6'
))]

Socio[, Age_num := as.numeric(dplyr::recode(Age,
                                            "18-29 years" = '1', 
                                            "30-44 years" = '2',   
                                            "45-59 years" = '3',
                                            "60-74 years" = '4',
                                            "75 years and more"='5'
))]

Socio[, TimeRegion_num := as.numeric(dplyr::recode(TimeRegion, 
                                                   "I do not currently live in the study region" = '0', 
                                                   "5 years or less." = '1',
                                                   "5 to 10 years." = '2', 
                                                   "10 to 20 years." = "3", 
                                                   "20 years and more."= '4'))]

Socio[, Man := factor(Gender=='Man')]

### Look at reported activities to id hunters later on
# MCA on activities :> not really working
Act = melt(Socio[, .SD, .SDcols = c('Respondent', "Jobactivism"   ,              "Jobagri"      ,               "Jobanimals"     ,             "Jobbiodiv"     ,             
                                    "Jobcultural"                , "Jobforestry"               ,  "Jobgardening_api"            ,"Jobhabits"                  ,
                                    "Jobhabits_collective"       , "Jobhunting"                ,  "Joblivestock"                ,"Jobplanning"                ,
                                    "Jobresearch"                , "Jobsport"                  ,  "Jobstudies"                  ,"Jobteaching"                ,
                                    "Jobtourism"                 , "Jobwater"                  ,  "OutsideJobactivism"          ,"OutsideJobagri"             ,
                                    "OutsideJobanimals"          , "OutsideJobbiodiv"          ,  "OutsideJobcultural"          ,"OutsideJobforestry"         ,
                                    "OutsideJobgardening_api"    , "OutsideJobhabits"          ,  "OutsideJobhabits_collective" ,"OutsideJobharvesting"       ,
                                    "OutsideJobhunting"          , "OutsideJoblivestock"       ,  "OutsideJobplanning"          ,"OutsideJobresearch"         ,
                                    "OutsideJobsport"            , "OutsideJobteaching"        ,  "OutsideJobtourism"           ,"OutsideJobwater")
], id.vars = 'Respondent')
Act[, variable := gsub('Job','', variable) ]
Act[, variable := gsub('Outside', '',variable) ]
Act = Act[, list(value = sum(as.numeric(value),0, na.rm = T)), by = c('Respondent', 'variable')]
Act[value >1, value := 1]

Act = dcast(Act, Respondent~variable)


#### Spatial data ####
perimeter = vect(paste0(raw_data_path,"ESNET/ESNET_FRONTIERES/ESNET_FRONTIERS.shp"))
perimeter = buffer(perimeter, 5000)

spatial_points = vect(paste0(survey_data_path,'Survey_data_final/2024-11-25_Spatial/66zg4cvh4uh7\ All_Layers\ -\ Points.shp'))
tissu_urb = rast(paste0(raw_data_path,"GlobalHumanSettlement/GHS_2154.tif"))
#		Typologies are described in 8 classes, with a 1km resolution.
#			30 : Urban Center grid cell
#			23 : Dense Urban Cluster grid cell
#			22 : Semi-dense Urban Cluster grid cell
#			21 : Suburban or Peri-urban grid cell
#			13 : Rural cluster grid cel
#     12 : Low Density Rural grid cell
#			11 : Very Low Density Rural grid cell
#			10 : Water grid cell
#		(https://human-settlement.emergency.copernicus.eu/download.php?ds=DUC)
municipalities = vect(paste0(raw_data_path,"Communes/Communes_ESNET_large.shp"))
corresp_insee = fread(paste0(raw_data_path,"Communes/Correspondance_insee_postal.csv"))
insee_pop = vect(paste0(raw_data_path,"INSEE/rp2021_carreaux_1km_shp/carreaux_1km_met.shp"))

spatial_points = project(spatial_points, tissu_urb)
municipalities = project(municipalities, tissu_urb)
perimeter = project(perimeter, tissu_urb)

tissu_urb = crop(tissu_urb, perimeter)
spatial_points = spatial_points[spatial_points$label == '2.3 Chez moi',]
spatial_points$home_type = terra::extract(tissu_urb, spatial_points)$GHS_2154
spatial_points$Code_commune_INSEE = terra::extract(municipalities, spatial_points[spatial_points$label == '2.3 Chez moi',])$insee

sp_dt = as.data.table(spatial_points[spatial_points$label == '2.3 Chez moi',])
sp_dt[, Respondent := responId]
sp_dt = sp_dt[, list(Respondent,  respNumber, home_type, Code_commune_INSEE)]
sp_dt[, N := length(home_type), by = Respondent]

sp_dt = merge(sp_dt, corresp_insee[, list(Code_commune_INSEE, Code_postal)], by = 'Code_commune_INSEE')
sp_dt = merge(sp_dt, Socio[, list(Respondent, Postcode_reported = Postcode)], by = 'Respondent')

# Keep only the ones that match reported postcode
sp_dt = sp_dt[Code_postal== Postcode_reported, ]

# Keep the second report, likely to be more accurate
sp_dt = sp_dt[!((N  == 2) & (respNumber == 0)),]

# Transform the rural into factorial
sp_dt[ , Urban := as.numeric(as.factor(home_type))]
sp_dt[ , Rural := 7-Urban]

sp_dt[is.na(home_type) , Urban := NA]

# Keep respondent that completed the survey
sp_dt = sp_dt[Respondent %in% Survey_melt$Respondent,]
sp_dt = unique(sp_dt)

Socio = merge(Socio, sp_dt[, list(Respondent, Rural)], by = 'Respondent')

Socio = unique(Socio)


#### Priorities ####
Priorities = Survey_melt[question == 'relPriorityScores' & !is.na(value), list(NCP, Respondent, value = as.numeric(value), Started)]
# Find ppl who didn't answer this question
no_answer_priority = Priorities[, list(score_NCP = sum(value, na.rm = T)), by = Respondent]
no_answer_priority = no_answer_priority[score_NCP == 0,]

# Identify probable hunters to apply weight
main_priority = Priorities[, list(main_NCP = NCP[value == max(value)]), by = Respondent]
hunting_is_main_priority = main_priority[main_NCP == 'Hunting',]$Respondent
hunting_priority_above50 = Priorities[NCP == 'Hunting' & value > 0.5, Respondent]

# FDCI: 16000 adhérents, should be downweighted
hunting_is_among_activities = Act[hunting>0, Respondent]
hunters = unique(c(hunting_is_main_priority, hunting_priority_above50, hunting_is_among_activities))

# We have a total of ~711 respondents and ~199 hunters.
# The FDCI reported 16000 members for 1 206 374 inhabitants, among which ~25% < 18 YO so 2%
# Matches with https://www.ledauphine.com/culture-loisirs/2022/09/11/nombre-age-profession-qui-sont-les-chasseurs-de-nos-departements
length(unique(Survey$Respondent)); length(hunters)
# The total weight should not exceed 5%. H = hunters, NH = non-hunters
# H*w / (NH + H*w) = 0.05
#w = 0.1 for now
weight_hunters = 0.1


#### Barriers ####
Barriers = Survey_melt[question == 'barriers' & !is.na(Spe1) , list(question, Respondent, Spe1, value, NCP)]

a = Barriers[value == TRUE, unique(Spe1), by = NCP]

barriers_initial = unique(Barriers$Spe1)

### deal with additional barriers
Barriers_additional = fread(paste0(survey_data_path, 'Data_analysis/Recodes/recode_barriers.txt'), sep = '\t', header = TRUE)

Barriers_additional[, c("Spe1a", "Spe1b", "Spe1c", "Spe1d") := tstrsplit(Spe1, ';')]
Barriers_additional = melt(Barriers_additional, id.vars = c('question',   'Respondent',   "type",'NCP'), measure.vars = c('Spe1a',  'Spe1b',  'Spe1c',  'Spe1d'), value.name = 'Spe1')
Barriers_additional = Barriers_additional[!is.na(Spe1),]
Barriers_additional$value = "TRUE"

unique(Barriers_additional$Spe1)[!unique(Barriers_additional$Spe1) %in% unique(Barriers$Spe1)]
Barriers_additional[grepl('conflict_hunting', Spe1), Spe1 := 'riskpeople']
Barriers_additional[grepl('conflict_dogs', Spe1), Spe1 := 'riskenv']
Barriers_additional[grepl('pollution', Spe1), Spe1 := 'quality']
Barriers_additional[grepl('distance', Spe1), Spe1 := 'availability']
Barriers_additional[grepl('space', Spe1), Spe1 := 'space']
Barriers_additional[grepl('legal', Spe1), Spe1 := 'legal']
Barriers_additional[grepl('risk_people',Spe1), Spe1 := 'conflict'] # Check
Barriers_additional[Spe1 == 'risk_env', Spe1 := 'riskenv']
Barriers_additional[Spe1 == 'other',Spe1 := 'people'] # Check
Barriers_additional[Spe1 == 'what',Spe1 := 'Knowledge']
Barriers_additional[Spe1 == 'availability',Spe1 := 'Availability']
Barriers_additional[Spe1 == 'quality',Spe1 := 'Quality']
Barriers_additional[Spe1 == 'knowledge',Spe1 := 'Knowledge']

Barriers = Barriers[!Spe1 %in% c("other", "others", "specify", 'comments') ,] # Remove specific barriers
# Add the recoded ones
Barriers = rbind(Barriers, Barriers_additional[, list(question, Respondent, Spe1, value, NCP)]) # Remove duplicates
Barriers[, value := as.numeric(as.logical(value))]

# Rename barriers with final names
Barriers[, barrier := dplyr::recode(Spe1,           
                                    "space" = "Availability" ,
                                    "degraded"= 'Quality',
                                    "expensive"= 'Cost',
                                    "where"= 'Knowledge', 
                                    "legal"= 'Legal',
                                    "busy"= 'Time',
                                    "people"= 'Crowding',
                                    "transport"= 'Transport',
                                    "alone"= 'Companionship',
                                    "marginalised"= 'Belonging',
                                    "cultural"= 'Culture', 
                                    "infra"= 'Infrastructure', 
                                    "motiv"= 'Motivation',
                                    "riskpeople"= 'Safety_social',
                                    "riskenv"= 'Safety_env',
                                    "health"= 'Health',
                                    "management"= 'Management*',
                                    "pollution"= 'Quality', 
                                    "sustainability"= "Sustainability*")]

Barriers = Barriers[, list(value = max(value, na.rm = T)), by = c('Respondent','NCP', 'barrier')]

# Order barriers for figures
Barriers[, barrier := factor(barrier, 
                             levels = c('Availability', 'Quality', 'Knowledge', 
                                        'Time','Cost','Legal',
                                        'Crowding',    'Motivation', 'Transport', 'Safety_social','Companionship', 'Health','Infrastructure','Safety_env', 'Belonging',  'Management*', 'Culture', 'Sustainability*'))]

barriersNames = unique(Barriers$barrier)

Barriers = merge(Barriers, chosen_NCP[, list(Respondent, NCP = chosenNCP, type, Chosen = 1)], all = T)
Barriers = Barriers[!is.na(Chosen)]
Barriers[, nchosen_NCP := sum(Chosen), by = NCP]
Barriers[, nchosen_type := sum(Chosen), by = type]
Barriers[, nbarrier_type := sum(value), by = type]
Barriers[, nbarrier_ncp := sum(value), by = NCP]


#### Merge all data ####

# Add social data
Barriers_cast = dcast(Barriers, Respondent + NCP + type ~ barrier, value.var = 'value')
Barriers_cast[, n_barriers := rowSums(.SD, na.rm = T), .SDcols = as.character(barriersNames)]
Barriers_melt = melt(Barriers_cast, id.vars = c('Respondent', 'NCP', 'type'), variable.name = 'barrier')

Barriers_socio = merge(Barriers_melt,  unique(Socio[, c(
  "Mobility_num"    ,
  "Connectedness_num"   ,
  "ChildhoodRural_num" , "ChildhoodRegion_num",
  "ChildhoodRural" , "ChildhoodRegion",'TimeNature_num', 'TimeNature','TimeRegion_num','TimeRegion',
  "Man",
  "Diploma_num"          ,
  "Income_num" , 
  'Respondent',
  'Age_num',
  'Rural',
  "Mobility"    ,
  "Gender",
  "Diploma"          ,
  "Income" , 
  'Age',
  'CSP'
)]), by= 'Respondent')
Barriers_socio$Man = as.numeric(as.factor(Barriers_socio$Man))
# Add weights for hunters
Barriers_socio[, weights_hunters := ifelse(Respondent %in% hunters, weight_hunters, 1)]

Barriers_melt_socio = melt(Barriers_socio, id.vars = c('Respondent', 'weights_hunters', 'barrier', 'value', 'NCP', 'type'), variable.name = 'Socio', value.name = "value_socio")
Barriers_melt_socio = Barriers_melt_socio[!is.na(value_socio) & !is.na(value),]


# Add satisfaction data

# For variance partitioning and check if related with satisfaction with NCP
Satisfaction = Survey_melt[question == 'satisfaction' & !is.na(value), list(Respondent, Spe1, value, NCP)]

Satisfaction[,  value_num := as.numeric(ifelse(value %in% c("Complètement d'accord","Complètement d'accord." ,'Fully agree'), 4,
                                               ifelse(value %in% c("Plutôt d'accord","Plutôt d'accord.", "Mostly agree"), 3,
                                                      ifelse(value %in% c("Plutôt pas d'accord","Plutôt pas d'accord.","Mostly disagree"), 2,
                                                             ifelse(value %in% c("Pas du tout d'accord","Pas du tout d'accord.","Fully disagree"), 1, NA)))))]

Barriers_satisfaction = merge(
  Barriers_melt_socio[,
                      list(Respondent, barrier, value, NCP, type, weights_hunters, Socio, value_socio)], 
  Satisfaction[, list(Respondent, NCP, value_satisfaction = value_num)], by = c('Respondent', 'NCP'))


# Add QoL data
QV = Survey_melt[question %in% c("QV", "QVN" ) & !is.na(value), list(question, Respondent, Spe1, value)]

#### Data preparation ####
QV[value %in% c('Fully disagree',  "Mostly agree" ,  "Mostly disagree", 'Fully agree',
                "Complètement d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"),
   qvvalue_num := as.factor(ifelse(value %in% c("Complètement d'accord",'Fully agree'), 4,
                                   ifelse(value %in% c("Plutôt d'accord", "Mostly agree"), 3,
                                          ifelse(value %in% c("Plutôt pas d'accord","Mostly disagree"), 2,
                                                 ifelse(value %in% c("Pas du tout d'accord","Fully disagree"), 1, NA)))))]

QV = dcast.data.table(QV[Spe1 == 'All',], Respondent ~question, value.var = 'qvvalue_num')

Barriers_satisfaction_QV = merge(Barriers_satisfaction, QV[, list(Respondent,     QV)], by = 'Respondent')



colnames(Barriers_satisfaction_QV) = 
c("Respondent" ,        "NCP"    ,            "Barrier"       ,     "Barrier_value"     ,         "NCP_type" ,          "weights_hunters"   ,
"Socio" ,             "Socio_value"   ,     "Satisfaction_value" ,"QoL_value")

Barriers_satisfaction_QV[, Respondent := as.numeric(as.factor(Respondent))]



####################################
#### Description of respondents ####
####################################

Socio = dcast(unique(Barriers_satisfaction_QV[,list(Respondent, weights_hunters,Socio, Socio_value)]), 
             Respondent + weights_hunters ~Socio, value.var = 'Socio_value')

n = nrow(Socio)
Socio[, .N/n, by = Gender]
Socio[, .N/n, by = Rural<=3]
Socio[, .N/n, by = ChildhoodRural]
Socio[, .N/n, by = Income]


### Figure S1
a = ggplot(Socio, aes(y= Age_num, fill = Gender)) + 
  geom_histogram(stat="count")+ 
  theme(legend.position = 'bottom')+ 
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  ylab('') + 
  xlab('') + ggtitle('Age and gender') 
b = ggplot(Socio, aes(y= CSP)) + geom_histogram(stat="count") +
  scale_y_discrete(labels = label_wrap(25)) +
  ylab('') + xlab('') + ggtitle('Socio-professional')
c = ggplot(Socio, aes(y= ChildhoodRural, fill = ChildhoodRegion)) + geom_histogram(stat="count") +
  scale_y_discrete(labels = label_wrap(20)) + labs(fill = "In study\nregion?") + ylab('Encvironment') +
  theme(legend.position = 'bottom')+guides(fill=guide_legend(nrow=2,byrow=TRUE))+ ggtitle('Childhood')

ggsave(plot = grid.arrange(a, b, c, ncol = 3),
       file =  paste0(res_dir, "FigS1.jpeg"), width = 12.5, height = 4)

### Multivariate analysis
# Use the ordinal variables to numeric

Socio_num = Socio[, list( # Numeric variables
  Age_num,
  ChildhoodRural_num,     
  ChildhoodRegion_num, 
  Diploma_num, 
  TimeRegion_num, Income_num, Mobility_num, Rural,
  Man_num = as.numeric(Man),
  Connectedness_num, 
  TimeNature_num,
  weights_hunters
)]

Socio_num = Socio_num[, lapply(.SD, as.numeric)]

Socio_num_complete = Socio_num[complete.cases(Socio_num),]

colnames(Socio_num_complete) = gsub('_num', '', colnames(Socio_num_complete))
pca_socio = PCA(Socio_num_complete[,1:11], row.w = Socio_num_complete$weights_hunters, quanti.sup = c(10:11))
fig_pca12 = fviz_pca_var(pca_socio, repel = T, title = '')
fig_pca34 = fviz_pca_var(pca_socio, repel = T, title = '', axes = c(3, 4)) ### Paper 1 figure S2

ggsave(plot = grid.arrange(fig_pca12, fig_pca34,  ncol = 2),
       file =  paste0(res_dir, "FigS2.jpeg"), width = 8)

# Correlation with connectedness
M = cor(Socio_num[, c('Income_num', 'Mobility_num', 'Rural', 'Connectedness_num', 'Man_num')], use = 'pairwise.complete.obs')
corrplot(M,  order = 'hclust',  type = 'lower', diag = FALSE)


################################
##### Save restricted data #####
################################

Barriers_satisfaction_QV = Barriers_satisfaction_QV[!(Socio %in% c('CSP','TimeRegion_num', 'TimeRegion', 'Diploma_num', 'Diploma', 
'ChildhoodRural_num',  'ChildhoodRegion_num', 'ChildhoodRural', 'ChildhoodRegion'  , 'Age_num', 'Age'  ))]
fwrite(Barriers_satisfaction_QV, file = "Barriers_data.csv")



