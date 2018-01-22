rm(list=ls())

craps <- function(){  
  first<-sum(sample(1:6,2,replace = T))
  if (first==7|first==11){
    return (1)
  }else if (first==2|first==3|first==12){
    return (0)
  }else {
    for(i in 1:100) {
      sec<-sum(sample(1:6,2,replace = T))
      if (sec==7) return (0)
      if (sec==first) return (1)
    }
  }
}

sum=0
for(i in 1:1000){
  sum=craps()+sum
}
#(a) CRAPS게임의 승률
sum/1000

winner<-function(ky,sy){
  while(sy!=0&ky!=0){
    if (craps()==1){
      sy=sy-1
      ky=ky+1
    }else{
      ky=ky-1
      sy=sy+1
    }
    
    if(ky==0){ 
      return (0)
    }else if (sy==0){ 
      return (1)}
  }  
}

#(b) 조건이 주어졌을 때 KY의 승률
sum2 <- sum(unlist(replicate(1000, winner(12,9))))
sum2/1000
sum3 <- sum(unlist(replicate(1000, winner(20,9))))
sum3/1000


