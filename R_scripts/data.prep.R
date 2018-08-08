#Pitchf/x Data Preparation for Multinomial Model
#Inputs:  (none)
#Data Files:  pitches

#Data Prep Function
data.prep <- function(x){
  #Load file & remove undetermined pitches
  pitches <- read.table('~/Downloads/pitches',sep=',',header=T)
  pit.all <- rbind(pitches[pitches$pitch_type!='',],rep(0,129))
  pit.all[nrow(pit.all),c('pitch_type','inning','type')] <- c('UN',1,'X')
  
  #Pitch Type Table & Merge with Pitch Data
  pit.type <- data.frame(Type0 = c('AB','CH','CU','EP','FA','FC','FF','FO','FS','FT','IN','KC','KN','PO','SC','SI','SL','UN'),
                         Desc = c('Automatic Ball','Changeup','Curve Ball','Euphus','Fastball','Fastball Cutter',
                                  'Fastball Four-Seam','Fastball Forkball','Fastball Splitter','Fastball Two-Seam',
                                  'Intentional Ball','Knucklecurve','Knuckleball','Pitch Out','Screwball','Sinker',
                                  'Slider','Unidentified'),
                         Type1 = c('Other','Changeup','Breaking','Junk','Fastball','Fastball','Fastball','Changeup',
                                   'Changeup','Fastball','Other','Breaking','Junk','Other','Breaking','Fastball','Breaking',
                                   'Other'),
                         Type2 = c('Other','Off-Speed','Off-Speed','Off-Speed','Fastball','Fastball','Fastball','Off-Speed',
                                   'Off-Speed','Fastball','Other','Off-Speed','Off-Speed','Other','Off-Speed','Fastball',
                                   'Off-Speed','Other'))
  pit.all <- merge(x=pit.all,y=pit.type,by.x='pitch_type',by.y='Type0')
  
  #Convert Predictors to Factors
  pit.all$pitch_type <- as.factor(pit.all$pitch_type)
  pit.all$Type1 <- as.factor(pit.all$Type1)
  pit.all$Type2 <- as.factor(pit.all$Type2)
  pit.all$outs <- as.factor(pit.all$outs)
  pit.all$inning <- as.factor(pit.all$inning)
  pit.all$stand <- as.factor(pit.all$stand)
  pit.all$type <- as.factor(pit.all$type)
  pit.all$zone <- as.factor(pit.all$zone)
  
  #Create Additional Predictors
  pit.all$count <- as.factor(paste0(pit.all$balls,'-',pit.all$strikes))
  pit.all$base <- as.factor(paste0(as.numeric(!is.na(pit.all$on_1b)),as.numeric(!is.na(pit.all$on_2b)),as.numeric(!is.na(pit.all$on_3b))))
  pit.all$rundiff <- sign(.5-pit.all$top)*(pit.all$away_team_runs - pit.all$home_team_runs)
  
  #All Pitcher IDs & Pitcher Total
  pit.id <- unique(pit.all$pitcher_id)[-length(unique(pit.all$pitcher_id))]
  pit.nid <- length(pit.id)
  
  return(list(pit.all=pit.all,pit.id=pit.id,pit.nid=pit.nid))
}
