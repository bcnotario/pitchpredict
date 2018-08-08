#Multinomial Logistic Regression Pitch Model & Accuracy
#Inputs: pit.dat (Pitchf/x prepped data), i (Pitcher Number)
#Packages: nnet

#Pitcher Model & Accuracy
pit.mod <- function(pit.dat,i){
  #Subset of Pitch Data for Specific Pitcher
  pit.subs <- pit.dat[pit.dat$pitcher_id==pit.id[i],]
  pit.subs <- pit.subs[order(pit.subs$uid),]
  pit.n <- nrow(pit.subs) 
  
  #Prior Pitch Tables
  pit.pre0 <- pit.subs[,c('pitch_type','Type1','Type2','type','start_speed','end_speed','zone','nasty','spin_dir','spin_rate')]
  pre1.i <- which(pit.subs$pcount_pitcher==1); pre1.n <- length(pre1.i)
  #One Pitch Prior
  pit.pre1 <- pit.pre0[c(1,1:(nrow(pit.subs)-1)),]; for(k in 1:pre1.n){pit.pre1[pre1.i[k],] <- c('UN','Other','Other','X',0,0,0,0,0,0)}
  colnames(pit.pre1) <- c('pitch_type1','Type1.1','Type1.2','type1','start_speed1','end_speed1','zone1','nasty1','spin_dir1','spin_rate1')
  #Two Pitches Prior
  pit.pre2 <- pit.pre1[c(1,1:(nrow(pit.pre1)-1)),]; for(k in 1:pre1.n){pit.pre2[pre1.i[k],] <- c('UN','Other','Other','X',0,0,0,0,0,0)}
  colnames(pit.pre2) <- c('pitch_type2','Type2.1','Type2.2','type2','start_speed2','end_speed2','zone2','nasty2','spin_dir2','spin_rate2')
  #New Pitch Data
  pit.temp <- cbind(pit.subs[,c(1:68,126:131)],pit.pre1,pit.pre2)
  #Training (80%) & Test Data (20%)
  pit.train <- pit.temp[1:floor(pit.n*.8),]
  pit.tests <- pit.temp[(floor(pit.n*.8)+1):pit.n,] 
  
  #Pitch Type Models Using Training Data
  glmm0.temp <- multinom(pitch_type ~ 
                           #Situational Predictors
                           count + outs + base + inning + rundiff + pcount_at_bat + pcount_pitcher +
                           #Batter Predictors
                           stand +
                           #Prior Pitch
                           pitch_type1 + type1 + zone1 +
                           #Prior Pitch (2 pitches ago)
                           pitch_type2 + type2 + zone2
                         ,data=pit.train,trace=FALSE)
  glmm1.temp <- multinom(Type1 ~ count + outs + base + inning + rundiff + pcount_at_bat + pcount_pitcher + stand +
                           Type1.1 + type1 + zone1 + Type2.1 + type2 + zone2,data=pit.train,trace=FALSE)
  glmm2.temp <- multinom(Type2 ~ count + outs + base + inning + rundiff + pcount_at_bat + pcount_pitcher + stand +
                           Type1.2 + type1 + zone1 + Type2.2 + type2 + zone2,data=pit.train,trace=FALSE)
  
  #Test Data
  test.mat <- cbind(as.character(pit.tests$pitch_type),0,as.character(pit.tests$Type1),0,as.character(pit.tests$Type2),0)
  for(j in 1:nrow(pit.tests)){
    #Pitch Type Test
    sim0.temp <- pit.tests[j,c('count','outs','base','inning','rundiff','pcount_at_bat','pcount_pitcher','stand',
                               'pitch_type1','type1','zone1','pitch_type2','type2','zone2')]
    test.mat[j,2] <- as.character(predict(glmm0.temp,newdata=sim0.temp))
    #Type 1 Test
    sim1.temp <- pit.tests[j,c('count','outs','base','inning','rundiff','pcount_at_bat','pcount_pitcher','stand',
                               'Type1.1','type1','zone1','Type2.1','type2','zone2')]
    test.mat[j,4] <- as.character(predict(glmm1.temp,newdata=sim1.temp))
    #Type 2 Test
    sim2.temp <- pit.tests[j,c('count','outs','base','inning','rundiff','pcount_at_bat','pcount_pitcher','stand',
                               'Type1.2','type1','zone1','Type2.2','type2','zone2')]
    test.mat[j,6] <- as.character(predict(glmm2.temp,newdata=sim2.temp))
  }
  
  #Pitch Type Accuracy
  correct0 <- sum(test.mat[,1]==test.mat[,2])/nrow(pit.tests)
  correct1 <- sum(test.mat[,3]==test.mat[,4])/nrow(pit.tests)
  correct2 <- sum(test.mat[,5]==test.mat[,6])/nrow(pit.tests)
  return(data.frame(i=i,pit.id[i],n=pit.n,t0=correct0,t1=correct1,t2=correct2,
                    dev0=glmm0.temp$deviance,dev1=glmm1.temp$deviance,dev2=glmm2.temp$deviance))
}
