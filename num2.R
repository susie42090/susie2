Mat1 <- matrix(c(1, 0, 1, 1, 1,
                 0, 0, 0, 1, 1,                 
                 0, 1, 1, 1, 1,                 
                 0, 1, 1, 1, 1,                 
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE) 
Mat1
Mat2 <- matrix(c(1, 0, 1, 1, 1,                  
                 1, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE) 
k=0
Largest<-function(matrix){
  
  n<-nrow(matrix)
  for (z in n:1){
  for(i in 1:(n-z+1)){
    for(j in 1:(n-z+1)){
      k=sum(matrix[i:(i+z-1),j:(j+z-1)])
      if(k==z^2) return (z^2)
    }}}}



  for(i in 1:2){
  for(j in 1:2){
    k=sum(matrix[i:(i+3),j:(j+3)])
    if(k==16) return (k)
  }
}

for(i in 1:3){
  for(j in 1:3){
    k=sum(matrix[i:(i+2),j:(j+2)])
    if(k==9) return (k)
  }
}
  
for(i in 1:4){
    for(j in 1:4){
      k=sum(matrix[i:(i+1),j:(j+1)])
      if(k==4) return (k)
  }
}  

for(i in 1:5){
    for(j in 1:5){
      k=sum(matrix[i:i,j:j])
      if(k==1) return (k)
  }
}    


Largest(Mat1)
Largest(Mat2)

k
Mat1[1,]
how<-function(){
  for(i in 1:5){
   if(sum(Mat1[i,]+Mat1[i+1,])==n*n)
     {return (n*n)}
   }
}

how()


for(i in 1:5){
  a=sum(Mat1[i,]+Mat1[(i+1),])
}

a


n<-nrow(Mat1)
n

for(i in n:1){
  A<-matrix(1,n,n)
  if(identical(A,Mat1)){ 
    return (n*n)}else {
      n=n-1
  }}
A

if(identical(A,Mat1)){ 
   return (n*n)}else {
     n=n-1
       A<-matrix(1,n,n)
    }


sum(Mat1)





