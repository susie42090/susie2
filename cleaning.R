rm(list=ls())
cow<-read.csv("C:\\Users\\susie\\Desktop\\cow_data.csv")

# 1."폐기용", "식용"을 기입하는 함수
make_edible <- function(data){     
  data$is_edible <- c()
  data$is_edible<-ifelse((data$age >= 50)&(data$grade=="3"|data$grade=="등외"), "폐기용", "식용")
  return (data)
}
cow<-make_edible(cow)

for(i in 1:nrow(cow)){
  if(substr(cow$address[i],4,4) == " "){
    cow$address2[i]<-substr(cow$address[i],1,7)
  }else if(substr(cow$address[i],5,5) == " "){
    cow$address2[i]<-substr(cow$address[i],1,8)
  }else{
    cow$address2[i]<-substr(cow$address[i],1,9)
  }
}

# 2."1++" 등급이 가장 많은 세 지역을 구하고 각 지역별로 "1++"등급이 총 몇 마리인지 구하세요
library(dplyr)

cow1<-cow %>%select(address,address2,grade,price)%>% 
  filter(grade=="1++")%>%
  filter(substr(address,3,4)!= "특별")

tab<-table(cow1$address2)
tab1<-sort(tab, decreasing = T)
count<-head(tab1, 3)
count

# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요
cow2<-cow %>%select(address,address2,grade,price,slaughter_date)%>% 
  filter(address2=="전라북도 정읍시"|address2=="전라남도 고흥군"
         |address2=="경기도 안성시")

cow2$price<-as.numeric(sub(",","",cow2$price))
head(cow2)

cow3<-cow2 %>%
  group_by(address2,grade) %>%
  summarise(mean_price=mean(price))

cow3

# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요
cow2$month<-substr(cow2$slaughter_date,5,6)
cow2$month<-as.numeric(cow2$month)
month2<-c(0,0,0,0,0,0,0,0,0,0,0,0)
month3<-c(0,0,0,0,0,0,0,0,0,0,0,0)
month4<-c(0,0,0,0,0,0,0,0,0,0,0,0)

for(i in 1:nrow(cow2)){
  for(j in 1:12){
    if(cow2$address2[i]=="전라북도 정읍시"){
      if(cow2$month[i]==j) {
        month2[j]=month2[j]+1
      }
    } 
    else if(cow2$address2[i]=="경기도 안성시"){
      if(cow2$month[i]==j) {
        month3[j]=month3[j]+1
      }
    }
    else {
       if(cow2$month[i]==j) {
          month4[j]=month4[j]+1
         }
      } 
   }
}

x<-c(1,2,3,4,5,6,7,8,9,10,11,12)
data<-data.frame(x, month2, month3, month4)
data

library(ggplot2)
ggplot(data=data, aes(x=x, y=month2,colour="정읍시"))+geom_line()+
  geom_line(aes(y=month3,colour="안성시"))+geom_line()+
  geom_line(aes(y=month4,colour="고흥군"))+geom_line()+
  xlab("월")+ylab("도축된 소(마리)")+scale_color_hue("도축된 지역")+
  xlim("1","2","3","4","5","6","7","8","9","10","11","12")
