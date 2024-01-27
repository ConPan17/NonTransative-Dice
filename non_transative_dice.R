sumPath <- function(numVec){
  
 levelVec <- levels(factor(numVec)) #all the numbers transformed to levels
 countVec <- c()
 
 for(i in 1:length(levelVec)){
   countVec <- c(countVec,as.integer(levelVec[i]),length(which(numVec == levelVec[i])))
   #counts occurences of each number and puts it next to that number
 }
 countVec
 
}# 4 3, three fours

twoDiceProb <- function(a,b,sides){ #probability that one dice will beat the other dice
  
   totalProb <- c(0,0)
  
   diceOne <- sumPath(a) #now in the for 3 5 2 4, five threes and two fours, a and b list of numbers
   diceTwo <- sumPath(b)
   
   
   for(i in 1:(length(diceOne)/2)){
     firstProb <- diceOne[2*i]/sides #isolate the first number of sides
     firstDie <- diceOne[2*i-1] #isolate the first die
     for(i in 1:(length(diceTwo)/2)){
       secondProb <- diceTwo[2*i]/sides
       secondDie <- diceTwo[2*i-1]
       
       netProb <- firstProb*secondProb #probability for one step
       
       if(firstDie < secondDie){
         totalProb[1] <- totalProb[1] + netProb #add the probabilities in
       }else if(secondDie < firstDie){
         totalProb[2] <- totalProb[2] + netProb
       }
       #if same don't add in the prababilities
     }
   }
   
   totalProb
}

twoDiceProb(c(1,2,2,21),c(1,3,3,20),4)

randomDiceRolls <- function(rolls,sides){
    floor(runif(rolls)*sides) + 1 #random dice roll function
}


checkIfTransative <- function(diceCount, sides,totalDiceVec){
   
   for(i in 1:diceCount){
      firstDice <- totalDiceVec[(sides*(i-1)+1):(sides*i)] #isolate the first die
      
      if(i == diceCount){ #reset to one so can check first die against last die
         secondDice <- totalDiceVec[1:sides]
      }else{
         secondDice <- totalDiceVec[(sides*(i)+1):(sides*(i+1))] #isolate the second die
      }
      
      probVector <- twoDiceProb(firstDice,secondDice,sides) #returns vector of two probabilities
      if(probVector[1] > probVector[2]){ #if first die beats second die, not transative
         return(FALSE) #second must beat first
         break
      }else if(probVector[1] == probVector[2]){#equal probabilities at any step does not count
         return(FALSE)
         break
      } 
      
      if(i == diceCount){
         return(TRUE)
      }
   }
   
}

checkIfTransative(3,4,c(1,4,7,7,2,6,6,6,3,8,5,5))




findNonTransativeDice <- function(diceCount, sides, maxValue){
   solutionVec <- c()
   #netDiceList <- list(c(0,0,0,0))
   totalDiceVec <- c(0,0,0,0)
   
   for(a in 1:100000){
      #repeatVec = TRUE
      
      # while(repeatVec){ #make sure does not make the same vector
      #    totalDiceVec <- c()
      #    for(i in 1:diceCount){
      #       totalDiceVec <- c(totalDiceVec,randomDiceRolls(4,maxValue))
      #    }
      #    if(list(totalDiceVec) %in% netDiceList){ #in the list of already created vectors
      #       
      #    }else{
      #       repeatVec = FALSE
      #    }
      # }
      
      #print(totalDiceVec)
      
         totalDiceVec <- c()
         for(i in 1:diceCount){
            totalDiceVec <- c(totalDiceVec,randomDiceRolls(4,maxValue))
         }
      
      
      if(checkIfTransative(diceCount,sides, totalDiceVec)){# checks if transative
         solutionVec <- c(solutionVec,totalDiceVec)
         print(totalDiceVec)
      }
      #netDiceList <- list(netDiceList,totalDiceVec) #adds to a total list of all tried ones
      
      if(a %% 100 == 0){
         print(a)
      }
   }
   solutionVec
}

findNonTransativeDice(3,4,6)
solutions <- findNonTransativeDice(3,4,5)
solutionList <- split(testSolutions, ceiling(seq_along(testSolutions)/12)) #cut off values split vector into list


diceProbTable <- function(diceVec,sides){
   probFrame <- data.frame(
      pairs = "A-B",
      FDie = ".32",
      SDie = ".44",
      Tie = ".32",
      stringsAsFactors = FALSE
   )
   numberOfDice <- length(diceVec)/sides
   
   for(i in 1:numberOfDice){
      firstDice <- diceVec[(sides*(i-1)+1):(sides*i)]
      
      if(i == numberOfDice){
         secondDice <- diceVec[1:sides]
      }else{
         secondDice <- diceVec[(sides*(i)+1):(sides*(i+1))]
      }
      
      placement <- i + 1
      if(i == numberOfDice){
         placement <- 1
      }
      
      probVec <- twoDiceProb(firstDice,secondDice,sides)
      newRow <- c(paste(LETTERS[i],"-",LETTERS[placement],sep = ""),probVec[1],probVec[2],1 - (probVec[2]+probVec[1]))
      probFrame <- rbind(probFrame,newRow)
   }
   
   probFrame[-1,]
   
}

diceProbTable(c(1,2,3,4,2,2,2,3,1,1,3,3),4)

checkIfTransative(3,4,c(2,3,6,1,5,1,1,6,1,5,3,4))
twoDiceProb(c(3,8,4,4),c(3,2,7,9),4)

checkIfTransative(3,4,c(1,2,3,4,2,2,2,3,1,1,3,3))


solutionFrame <- as.data.frame(I(solutionList))
colnames(solutionFrame) <- c("DiceSet")

randomVec <- c()
for(i in 1:100){
   randomVec <-c(randomVec,randomDiceRolls(12,4))
}

randomVec <- split(randomVec, ceiling(seq_along(randomVec)/12))

randomVecpar(mfrow = c(1,2))
testVec <- lapply(solutionList, sum)
boxplot(x = unlist(testVec), horizontal = TRUE)

testVec <- lapply(randomVec, sum)
boxplot(x = unlist(testVec), horizontal = TRUE)

par(mfrow = c(1,1))

#check if rows sum to same number
sumDieSame <- function(diceVec){
   numDice <- 4
   sides <- length(diceVec)/numDice
   sumVec <- c()
   for(i in 1:numDice){
      sumVec <- c(sumVec,sum(diceVec[(sides*(i-1)+1):(sides*i)]))
   }
   sum(duplicated(sumVec)) > 0
}
sum(unlist(lapply(randomVec, sumDieSame)))
