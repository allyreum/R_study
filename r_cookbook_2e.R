print('welcome to R Cookbook world')

# chapter 5: Data Structures
# In a vector, all elements must have the same mode.
# : a scalar is simply avector that contains exactly one element. 
# In a list, the elements can have different modes.


v <- c(10,20,30)
names(v) <- c('go','sweet','home')
v

mode(c(3.1415,2.1)) # [1] "numeric"

d <- as.Date('2010-03-15')
mode(d)
class(d)
length(d) # 숫자 객체  1개

pi
pi[1]
pi[2] # NA, pi is a scalar

d <- 1:12
dim(d) <- c(2,3,2) #  3-dimensional array with dimensions 2 × 3 × 2:
d

c <- list(1,2,3,'x','y','z')
dim(c) <- c(2,3)
c

# 5.1 Appending Data to a Vector
v <- c(1,2,3)
newitems <- c(6,7,8)
c(v, newitems)
v[length(v) + 1] <-42

# The memory management in R works poorly when you repeatedly extend a vector by one element.
# Try to replace that loop with vector-level operations. 
# GPT의 예시
my_vector <- numeric(0)
for (i in 1:10) {
  my_vector <- c(my_vector, i)
}
my_vector <- 1:10

v1 <- c(1,2,3)
v1 <- c(v1,4)
v1

w <- c(5,6,7,8)
v1 <- c(v1,w)
v1

v_na <- c(1,2,3)
v_na[10] <- 10
v_na # R extends the vector automaticall


# 5.2 Inserting Data into a Vector
append(1:10, 90, after = 5) # inserted at the position given by after 
append(1:10, 90, after = 0) # after = 0, insert the new items at the head of the vector


# 5.3 Understanding the Recycling Rule
(1:6) + (1:3)
cbind(1:6, 1:3) # recycled repeatedly until the vector addition is complete
(1:6)+ 10


# 5.4 Creating a Factor (Categorical Variable)
f <- factor(v)
# f <- factor(v, levels) # not definite levels
f <- factor(c('A','A','B','C','A','B'))


# 5.5 Combining Multiple Vectors into One Vector and a Factor
freshmen <-c(1,2,1,1,5)
sophomores<-c(3,2,3,3,5)
juniors<-c(5,3,4,3,3)
comb <- stack(list(fresh = freshmen, 
                   soph = sophomores, 
                   jrs = juniors)) # n combine the groups using the stack function, anova 돌릴 때 필요할지 도?
aov(values ~ ind, comb)


# 5.6 Creating a List
lst <- list(0.5, 0.841, 0.977)
lst2 <- list(3.14, 'Moe', c(1,2,3,4), mean)
lst3 <- list()  # omg method
lst3[[1]] <- 3.14
lst3[[2]] <- 'Moe'
lst3[[3]] <- c(1,2,3,4)
lst3[[4]] <-mean
lst4 <- list(mid = 0.5, right = 0.841, far.right = 0.9777)

  
# 5.7 Selecting List Elements by Position
lst4
lst4[[1]]
lst4['mid']
lst4[c(1,2)]


# 5.8 Selecting List Elements by Name
years <- list(Kennedy =1960, Johnson=1964, Carter=1976, Clinton=1994)
years[["Kennedy"]]
years$Kennedy


# 5.9 Building a Name/Value Association List
# for a dictionary, hash or lookup table
lst <- list(mid = 0.5, right = 0.841, far.right = 0.987)

values <- c(1,2,3)
names <- c('a', 'b', 'c')
lst <- list()
lst[names] <- names

lst <-list(
  far.left = 0.023,
  far.right = 0.977
)

lst<-list()           # 빈 리스트 만들고, 값 넣는 것도 가능함
lst$far.left<-0.024
lst$far.right<-0.06

values <- pnorm(-2:2)  # 빈 리스트 만들고, 벡터 형식으로 값을 채워 넣을 수 있음
names <- c("far.left",'left', 'mid', 'right', "far.right")
lst <- list()  
lst[names] <- values


# 5.10 Removing an Element from a List
years[c("Carter","Clinton")] <- NULL


# 5.11 Flatten a List into a Vector(using unlist)
iq.score <- list(100,120,103,80,99)
mean(iq.score)      # 리스트 평균 계산할 수 없음
mean(unlist(iq.score))

cat(iq.score,'\n') # cat scalars and vectors, but we cannot cat a list
cat("IQ scores:", unlist(iq.score),'\n')


# 5.12 Removing NULL Elements from a List
library(tidyverse)
lst <- list("Moe",NULL,"Curly")
compact(lst)      # compact함수는 리스트로부터 NULL을 제거함(NA 제거못함)


# 5.13 Removing List Elements Using a Condition
lst <- list(NA,0,NA,1,2)
lst %>% discard(is.na)         # NA값들을 제거

lst<-list(3,"dog",2,"cat",1)
lst %>% discard(is.character)

is.na.or.null <- function(x){
  is.na(x)||is.null(x) # NA와 NULL인 애들 제외를 위함
}

lst<-list(NA,0,NULL,1,2)
lst %>% discard(is.na.or.null)

mods <- list(lm(x ~ y1), lm(x ~ y2), lm(x ~ y3))
filter_r2 <- function(model){
  summary(model)$r.squared < 0.7
}

mods %>% discard(filter_r2)


# 5.14 Initializing a Matrix
vec <- 1:6
matrix(vec,2,3) 
matrix(vec,2,3,byrow=TRUE)
v <- c(1:6)
dim(v) <- c(2,3) #  vector and then shapes it into a 2 × 3 matrix
v

matrix(0,2,3)
matrix(NA,2,3)


# 5.15 Performing Matrix Operations
a<-matrix(1:9,3,3)
t(a)
solve(a)
a %*% t(matrix(vec,2,3))
diag(a)


# 5.16 Giving Descriptive Names to the Rows and Columns of a Matrix
corr_mat <- matrix(c(1.000,0.556,0.390,
                     0.556,1.000,0.444,
                     0.390,0.444,1.000), 3,3)
colnames(corr_mat) <- c('A', 'M', 'G')
rownames(corr_mat) <- c('A', 'M', 'G')
corr_mat['M','G'] # the correlation between variables


# 5.17 Selecting One Row or Column from a Matrix
corr_mat[1, , drop=FALSE] # selecting a row returns a row vector
corr_mat[ , 3, drop=FALSE] # selecting a row returns a column vector


# 5.18 Initializing a Data Frame from Column Data
# data.frame은 문자열을 자동으로 요인으로 변환
# tibble과 as_tibble함수는 문자열 데이터를 변환하지 않음


# 5.19 Initializing a Data Frame from Row Data
l1 <- list(a=1, b=2, c='x')
l2 <- list(a=3, b=4, c='Y')
l3 <- list(a=5, b=6, c='z')

rbind(l1,l2,l3)
list.of.lists <- list(l1,l2,l3)
bind_rows(list.of.lists)

# 데이터 프레임의 요인
data.frame(a=1, b=2, c='a', stringsAsFactors = FALSE) # stringsAsFactors = FALSE : 문자열로 취급, default
obs <- list(l1,l2,l3)
df <- do.call(rbind, Map(as.data.frame, obs)) # transform the rows into data frames using the Map function 
# do.call(함수, 인수목록 = list형태)
# do.call() 함수는 특히 동적으로 함수를 호출해야 할 때 유용. 함수 이름이나 인수 목록이 변수로 주어지는 경우에 사용!


# 5.20 Appending Rows to a Data Frame
suburbs <- read.csv('./data/suburbs.txt')
suburbs2 <- read_csv('./data/suburbs.txt') # tibble
newRow <- data.frame(ctiy = 'West Dundee', county = 'Kane', state = 'IL', pop = 7352)
# rbind > 새로운 행은 데이터프레임과 동일한 열 이름을 사용! 아닌 경우에는 fail
# rbind의 첫번째 인자와 동일한 형식이 됨
rbind(some_tibble, some_data.frame) # tibble
rbind(som_data.frame, some_tibble) # data.frame
