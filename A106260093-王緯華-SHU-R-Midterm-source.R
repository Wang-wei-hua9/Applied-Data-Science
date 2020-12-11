# 2020/12/11(五), 109學年第一學期 資料科學應用 R期中考
#
# 學號: A106260093       姓名: 王緯華
#
# 本檔案為各題之程式碼檔，無執行結果

#ex.1
study <- function(x, y){
  #x <- 13
  #y <- 8
  #預算限制式
  Eng.hr <- x
  #Eng.hr
  Comp.hr <- y
  #Comp.hr
  Tuition <-  ((400*x) + (600*y) )
  #Tuition
  ifelse((limt <= 12000), limt, 0)
  #limt
  #效用函數
  U <- sqrt(x)*sqrt(y)
  #U
  Fit <- ifelse(Tuition <=12000, "*"," ") 
  
  study.table <- data.frame(Eng.hr, Comp.hr, Tuition, U, Fit)
  study.table
}

x <- rep(13:17, 5)
y <- rep(8:12,  each=5)
study(x, y)


#ex.2(a)
library(readxl)
student_test <- read_excel("data/Score-109.xlsx",  na = "NA", skip=1)
student_test
#印出前後五筆資料
head(student_test, 5)
tail(student_test, 5)


#ex.2(b) 印出"請問有哪些同學兩科成績同時不及格
#將遺失值填入0
student_test[is.na(student_test)] <- 0
student_test
id <- which((student_test$Calculus+ student_test$English) < 60)
student_test[id, ]

#ex.2(c) 寫相關係數函數
x1 <- student_test$Calculus
x2 <- student_test$English

my.cor <- function(x1, x2){
  
  # x1 <- 5
  # x2 <- 10
  x1.bar <- mean(x1)
  x2.bar <- mean(x2)
  
  a <- sum((x1- x1.bar)*(x2 - x2.bar))
  b <- sqrt(sum((x1- x1.bar)^2)) * sqrt(sum((x2 - x2.bar)^2))
  ans <- a/b
  ans
}

x1 <- student_test$Calculus
x2 <- student_test$English
my.cor(x1, x2)

#ex.2(d) 計算微積分及英文兩成績之相關係數
x1 <- student_test$Calculus
x2 <- student_test$English
my.cor(x1, x2)

cor(x1, x2)


#ex.3(a)
my_dnorm <- function(x, u = 0, z = 1){
  #x <- 1
  #u <- 0
  #z <- 1
  e <- 2.718282
  density <- (1/(sqrt(2*pi)*z))*e^(-(x-u)^2/2*z)
  density
  
}
my_dnorm(2.5, 3, 2)

#ex.3(b)
x <- rep(-3:3)
dnorm <- my_dnorm(x, 3, 2)
my.dnorm <- my_dnorm(x, 3, 2)
my.dnorm.table <-data.frame(x, dnorm, my.dnorm)
my.dnorm.table






