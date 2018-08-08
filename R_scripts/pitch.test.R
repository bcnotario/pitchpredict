#Test Model Accuracy for Sample of Pitchers
#Inputs: pit.dat (Pitchf/x prepped data), sample.n (Sample Size)

#Test Pitcher Models (Sample)
pit.test <- function(pit.dat,sample.n){
  #Sample
  test.s <- sort(sample(pit.dat$pit.nid,sample.n,replace=FALSE))
  
  #Model Accuracy per Pitcher
  test.all <- matrix(0,sample.n,9)
  colnames(test.all) <- c('i','ID','Pitches','Type0 Accuracy','Type1 Accuracy','Type2 Accuracy','Dev0','Dev1','Dev2')
  for(j in test.s){
  test.all[which(test.s==j),] <- data.matrix(pit.mod(pit.dat$pit.all,j))
  }
  return(test.all)
}
