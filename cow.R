cow<-read.csv("C:\\Users\\susie\\Desktop\\cow_data.csv")
head(cow)
# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 기입하는 함수를 작성해주세요.
# 함수의 매개변수는 대상 데이터 1개 ex) my_function(cow_data)
cow$is_edible
head(cow)
data<-cow
#edible<-my_function(cow){
make_edible <- function(data){     
  data$is_edible <- c()
  data$is_edible<-ifelse((data$age >= 50)&(data$grade=="3"|data$grade=="등외"), "폐기용", "식용")
  return (data)
}
new_cow <-make_edible(cow)
str(new_cow)
head(new_cow$is_edible)

#cow$is_edible<-my_function(cow){

# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군 단위로 구해주세요)
#
# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요

install.packages("dplyr")
library(dplyr)

cow1<-cow %>% filter(grade=="1++")%>% 
      select(address,grade)
cow2<-cow1 %>% select(address,grade) %>%
    group_by(address)%>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    head(10)

cow2<-cow1 %>% count(address) %>%
  group_by(address)%>%
  arrange(desc(n))


head(cow1)
nrow(cow1)

head(cow2)
sum(cow2$n)

cow1
cow2
table(cow1$address)
strsplit(cow1$address," ")

str(cow1)

cow3 <- cow1 %>% mutate("도/시/군" = ifelse(substr(address,4,4) == " ", substr(address, 1, 7),
                                       ifelse(substr(address, 6, 6) == " ", substr(address, 1, 10), substr(address, 1, 8))))
head(cow3)
cow.0 <- subset(cow, substr(cow$`도/시/군`, 3, 4) != "특별")
cow.1 <- subset(cow.0, grade == "1++")
tab <- table(cow.1$"도/시/군")
tab.1 <- sort(tab, decreasing = T)
count <- head(tab.1, 3)
tab
