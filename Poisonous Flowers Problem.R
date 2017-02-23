#Code to solve the poisonous flowers puzzle

#Initialize
IterationLimit <- 100
InitFlwrCnt <- 1000
NextSprayNum <- c()
NextSprayName <- c()
NextSprayKills <- 0
NextSprayGrowBack <- 0
CurrentFlwrCnt <- InitFlwrCnt
DATA <- structure(list(Iteration=integer(),
                       NextSprayNum=integer(),
                       NextSprayName=character(),
                       NextSprayKills=integer(),
                       NextSprayGrowBack=integer(),
                       NextSprayNetAffect=integer(),
                       CurrentFlwrCnt=integer()), 
              class = "data.frame")
Iteration <- 0

#Spray Types (killed, grow back)

Sprays <- data.frame(name = c('WeakSpray','MediumSpray','StrongSpray','IllegalSpray'), stringsAsFactors = FALSE)
Sprays$Num <- c(1,2,3,4)
Sprays$Kill <- c(3,5,14,17)
Sprays$GrowBack <- c(12,17,8,2)
Sprays$NetAffect <- Sprays$GrowBack - Sprays$Kill


#BEGIN WHILE LOOP#

while (Iteration < IterationLimit && CurrentFlwrCnt > 0) {

  Iteration <- Iteration + 1
  
  
    #Pick a next spray
    
    #If the current flower count = 3, 5, 14 or 17, use the corresponding spray to end the simulation, thus solving the problem
    if(is.na(match(CurrentFlwrCnt,Sprays$Kill)) == FALSE) {
      
      NextSprayNum <- match(CurrentFlwrCnt,Sprays$Kill)
      NextSprayName <- Sprays$name[NextSprayNum]
      NextSprayKills <- Sprays$Kill[NextSprayNum]
      NextSprayGrowBack <- 0
      
    } else {
      
                      ################## TODO pick one of the options that work randomly and continue
      #Sprays that will do something
      UsableSprays <- Sprays$Num[Sprays$Kill < CurrentFlwrCnt]
      
      #sprays that pretty much useless
      PointlessSprays <- Sprays$Num[(Sprays$Kill*6) < (CurrentFlwrCnt)]
      
      if (length(PointlessSprays) > 0 ) {
        possibleSprays <- UsableSprays[-c(PointlessSprays)]
      } else {
        possibleSprays <- UsableSprays
      }
                      
      NextSprayNum <- possibleSprays[round(runif(1,1,length(possibleSprays)))]


                      
                      #Just use the min
                        #NextSprayNum <- match(min(UsableSprays),Sprays$NetAffect)
                        #Sprays$NetAffect[Sprays$NetAffect>0]
                        #NextSprayNum <- round(runif(1,1,length(Sprays$NetAffect[Sprays$NetAffect>0])))
                    
                      
                      
                      NextSprayName <- Sprays$name[NextSprayNum]
                      NextSprayKills <- Sprays$Kill[NextSprayNum]
                      NextSprayGrowBack <- Sprays$GrowBack[NextSprayNum]
      
    }


################## TODO Record what happens
CurrentFlwrCnt <- CurrentFlwrCnt - NextSprayKills + NextSprayGrowBack
DATA[Iteration,] <- c(Iteration,NextSprayNum,NextSprayName,NextSprayKills,NextSprayGrowBack,NextSprayGrowBack-NextSprayKills,CurrentFlwrCnt)


#End of while loop
}

print(DATA)

barplot(table(DATA[,7]))
# rank(table(DATA[,7]))
# blah <- rank(table(DATA[,7]))
# sort(DATA[,7], decreasing=F)
# DATA[,7]
# typeof(CurrentFlwrCnt)
# DATA$CurrentFlwrCnt
