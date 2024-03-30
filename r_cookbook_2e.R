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


# 5.21 Preallocating a Data Frame
data5.21 <- suburbs %>% head(3)
data5.21 %>% dplyr::select(1) # 열추출
data5.21 %>% dplyr::select(1,3,4) # 열추출
data5.21[[1]] #  data5.21[c(1)]와 동일
data5.21[c(1,3)]


# 5.22 Selecting Data Frame Columns by Position
# list operator: df[["name"]], df$name
# matrix operator: df[, 'name']


# 5.23 Rename columns from a data frame
# df %>% rename(newname = oldname,....)
df <- data.frame(v1 = 1:3, v2 =4:6, v3 = 7:9)
df %>% rename(tom = v1, dick =v2)
colnames(df) <- c('home', 'house','friday')

df2 <- data.frame(v1 = 1:3, v2 =4:6, v3 = 7:9)
df2 %>% select(tom = v1, v2)
# rename은 명시하는 열의  이름을 바꾸면서 다른 모든 열은 건드리지 않음
# select는 선택한 열에 대해서만 반환됨


# 5.24 Removing NAs from a Data Frame
dfm <- data.frame(x = c(1,NA,3,4,5),
                  y= c(1,2,NA,4,5))
clean_dfm <- na.omit(dfm)
colSums(dfm) # cumsum fail because the input contains NA values
cumsum(na.omit(dfm))


# 5.25 Excluding Columns by Nam
dfm %>% select(-y)


# 5.26 Combining Two Data Frames
df1 <- data.frame(a = c(1,2))
df2 <- data.frame(b = c(7,8))

cbind(df1,df2)
rbind(df1,df2) # names do not match previous names

df2 <- df2 %>% rename(a = b)
rbind(df1, df2)
# The rbind function requires that the data frames have the same width: same number of columns and same column names


# 5.27 Merging Data Frames by Common Column
born <- tibble(
  name = c('stella', 'judy', 'anna', 'alley', 'harry'),
  year.born = c(1887, 1902, 1903, 1964, 1922),
  place.born = c('Brooklyn', 'Seoul', 'Osaka', 'Paris', 'Sydney')
)

died <- tibble(
  name = c('stella', 'harry'),  year.died = c(1952,1975)
)

inner_join(born, died, by ='name')
full_join(born, died, by ='name')
full_join(born, died) # by를 지정하지 않는 경우에는 자동으로 조인시도하고, 어느  기준으로 했는 지 표시해서 반환

aset<- data.frame(key1 =1:3, value =2) 
bset<- data.frame(key2 =1:3, value =3) 
inner_join(aset, bset, by = c('key1' = 'key2')) # 이름이 다른 경우 매개변수에 대한 내용 추가


# 5.28 Converting One Atomic Value into Another
as.integer(3.14)
as.numeric("foo")
as.character(101)

as.numeric(c("1","2.718","7.389","20.086"))
as.numeric(c("1","2.718","7.389","20.086", "etc."))
as.character(101:105)


# 5.29 Converting One Structured Data Type into Another

# 1. When you convert a list into a vector, the conversion works cleanly if your list contains atomic values that are all of the same mode. 
list_tmp <- list(1,2,3,'x','y','z')
as.vector(list_tmp)
unlist(list_tmp) # Use unlist rather than as.vector

# 2. Converting a data frame into a vector makes sense only if the data frame contains one row or one column. 
# To extract all its elements into one, long vector, use as.vector(as.matrix(dfrm)) 
df2_tmp <-data.frame(year = c(1994, 1990),  sex = c(2,1)) # makes sense only if the data frame is all-numeric or all-character
df_tmp <-data.frame( name = c('stella', 'harry'),  year.died = c(1952,1975)) #  first converted to character strings.
as.vector(as.matrix(df_tmp))
as.vector(as.matrix(df2_tmp))


# 3. Data frame to list
# Using as.list essentially removes the class (data.frame) and thereby exposes the underlying list. 
# That is useful when you want R to treat your data structure as a list—say, for printing
as.list(df_tmp)

# 4. Be careful when converting a data frame into a matrix. 
# If the data frame contains only numeric values then you get a numeric matrix. 
# If it contains only character values, you get a character matrix. 
# But if the data frame is a mix of numbers, characters, and/or factors,  then all values are first converted to characters. 
# The result is a matrix of character strings.

# chapter 6:   Data Transformations
# 6.1 Applying a Function to Each List Element
library(tidyverse)

lst <- list( a= c(1,2,3), b= c(4,5,6))
lst %>% map(mean) # map이랑 lapply랑 비슷함

lapply(lst, mean)
sapply(lst, mean)

fun6.1 <- function(x){
  if(x>1) {
    1
  } else {
    "Less than 1"
  }
}

fun6.1(5)
fun6.1(0)

lst2 <- list(0.5, 1.5, .9, 2)
map(lst2,fun6.1)
lapply(lst2,fun6.1)


#6.2 Applying a function to all rows of a DataFrame
fun6.3 <- function(a,b,c){
  sum(seq(a,b,c))
}

df6.3 <- data.frame(n = c(1,2,3),
                 m = c(8,10,22),
                 c= c(1,2,3) )

df6.3 %>% rowwise() %>% # rowwise()는 데이터프레임을 행 단위로 처리하도록 설정
  mutate(output = fun6.3(n,m,c))

df6.3 %>% mutate(output = fun6.3(n,m,c))
 # df6.3의 각 행에 대해 하나의 값으로 전달되어야 하므로 error


#6.3 Applying a function to each row of a DataFrame
# result <- apply(matrix, 1, function) 2번째 인자가 1= row별
long <- matrix(1:15, 3, 5)
apply(long,1,sum)
apply(long,1,range)


# 6.4 Applying a Function to Every Column
# result <- apply(matrix, 2, function) 2번째 인자가 2 = column별
head(batches)
map_df(batches, class)


# 6.5  Applying a Function to Parallel Vectors or Lists


# 6.6 Applying a Function to Groups of Data
# tapply(vector, factor, function)
attach(suburbs)
tapply(pop,county,sum)
tapply(pop,county,mean)
tapply(pop,county,length)
detach(suburbs)

df6.6 <- tibble(
  my_g = c('a','b', 'a','a','b', 'a'),
  value = 1:6
)

df6.6 %>% group_by(my_g) %>% 
  summarise(
    avg_values = mean(value),
    sum_values = sum(value),
    n_value = n()
  )

tapply(df6.6$value, df6.6$my_g, mean)


# 6.7 조건에 따라 새로운 열 만들기
# df %>% mutate(new_field = 
#                 case_when(my_field == "something" ~ "reusult",
#                           my_field != "somthing else" ~ "other result",
#                           TRUE ~"all other results")            )

df6.7 <- data.frame(vals = 1:5)
df6.7 %>% mutate(new_vals = case_when(vals <= 2 ~ "2 or less",
                                      vals > 2 & vals <= 4 ~ "2 to 4",
                                      TRUE ~ "over 4"))


# chapter 7: Strings and Dates
# 7.1 Getting the Length of a String
s <- c('Kim', 'Lee', 'Jand', 'Baek')
nchar(s)
length(s)


# 7.2 Concatenating Strings
paste('I','want','to','go','home')
paste0('I','want','to','go','home') # without space

paste("The square root of twice pi is approximately", sqrt(pi*2))
paste(s, "loves", "sweet home", collapse = " and ")


# 7.3 Extracting Substrings
substr("Statistics", 1, 4)
substr("Statistics", 7, 10)
cties <- suburbs$city
substr(cties, nchar(cties)-1, nchar(cties))


# 7.4 Splitting a String According to a Delimiter
path <- "/home/mike/data/trials.csv"
strsplit(path, "/")

paths <- c("/home/mike/data/trials.csv",
           "/home/mike/data/errors.csv",
           "/home/mike/corr/reject.doc")
strsplit(paths, "/")


# 7.5 Replacing Substrings
string <- "aaa aaa aaa"

sub("a", "X", string)
# sub(old, new, string)  Use sub to replace the first instance of a substring

gsub("a", "X", string)
# gsub(old, new, string) Use gsub to replace all instances of a substring


# 7.6 Generating All Pairwise Combinations of Strings
locations <- c("NY", "LA", "CHI", "HOU")
treatments <- c("T1", "T2", "T3")
outer(locations, treatments, paste, sep="-")
# outer(strings1, strings2, paste, sep="")

expand.grid(treatments, treatments) # pair of vectors

m <- outer(treatments, treatments, paste, sep="-")
m[!lower.tri(m)] # distinct


# 7.7 Getting the Current Date
Sys.Date()
class(Sys.Date())


# 7.8  Converting a String into a Date
as.Date("2024-03-30")
as.Date('12/31/2024')
as.Date('12/31/2024', format = '%m/%d/%Y')


# 7.9 Converting a Date into a String
format(Sys.Date())
as.character(Sys.Date())
format(Sys.Date(), format = '%m/%d/%Y')


# 7.10  Converting Year, Month, and Day into a Date
year <- 2024
month <- 3
day <- 30
as.Date(ISOdate(year, month, day)) # ISOdate(year, month, day)
as.Date(ISOdate(2012,2,29))
ISOdate(2024,2,29) 
ISOdate(2023,2,29) # 2023 is not a leap year


# 7. 11 Getting the Julian Date -> julian(d7.11)


# 7.12 Extracting the Parts of a Date
d7.12 <- as.Date("2024-03-15")
p <- as.POSIXlt(d7.12)
p$mday # Day of the month
p$mon # Month (0 = January)
p$year + 1900 # Year

d2 <- as.Date("2030-02-01")
as.POSIXlt(d2)$wday # Day of the week (0–6, 0 = Sunday)
as.POSIXlt(d2)$yday # Day of the year (0–365)
as.POSIXlt(d2)$year + 1900


# 7.13 Creating a Sequence of Dates
st_day <- as.Date("2020-05-01")
ed_day <- as.Date("2024-04-01")
seq(from = st_day, to = ed_day, by=1) 
seq(from = ed_day, by = 1, length.out = 7)
seq(from = ed_day, by = "3 months", length.out = 4)
seq(as.Date("2020-05-01"), by = "months", len = 3)



# chapter 8: Probability
# dnorm: Normal density
# pnorm: Normal distribution function
# qnorm: Normal quantile function
# rnorm: Normal random variates
# binom: n = number of trials; p = probability of success for one trial
# geom: p = probability of success for one trial
# hyper :m = number of white balls in urn; n = number of black balls in urn; k = number of balls drawn from urn


# 8.1 Counting the Number of Combinations
choose(5,3)


# 8.2 Generating Combinations
items <- 2:5
k <- 2
combn(items, k) # k게씩 선택하는 조합 모두 생성
combn(c('t1','t2','t3','t4','t5'),3)


# 8.3 Generating Random Numbers
runif(1) # runif: a uniform random number between 0 and 1 
rnorm(1) # rnorm : the Normal distribution’s random number generator

runif(1, min=-3, max=3) # One uniform variate between -3 and +3
rnorm(1) # One standard Normal variate
rnorm(1, mean=100, sd=15) # One Normal variate, mean 100 and SD 15
rbinom(1, size=10, prob=0.5) # One binomial variate
rpois(1, lambda=10) # One Poisson variate
rexp(1, rate=0.1) # One exponential variate
rgamma(1, shape=2, rate=0.1) # One gamma variate

rnorm(3, mean = c(-10,0,+10), sd = 1)
rnorm(6, mean = c(-10,0,+10), sd = 1)
means <- rnorm(30, mean = 0, sd = 0.2)
rnorm(30, mean = means, sd = 1)


# 8.4  Generating Reproducible Random Numbers
set.seed(42) # Or use any other positive integer
set.seed(165)
runif(10)
set.seed(165)
runif(10)


# 8.5 Generating a Random Sample
world_series <- read_csv('./data/world_series.csv')
sample(world_series$year, 10)

set.seed(42)
x <- rnorm(1000,4,10) # rnorm(1, mean=4, sd = 10)
medians <- numeric(1000)
for(i in 1:1000){
  medians[i] <- median(sample(x, replace = TRUE))
}

sample(c("H","T"), 10, replace =TRUE)
sample(c(FALSE, TRUE), 20, replace = TRUE)
sample(c(FALSE, TRUE), 20, replace = TRUE, prob = c(0.2, 0.8)) # 성공확률 0.8, 실패확률 0.2


# 8.7 Randomly Permuting a Vector
sample(1:10) # sample(v, size = length(v), replace = FALSE)


# 8.8 Calculating Probabilities for Discrete Distributions

# Discrete Distributions - density - distribution
# Binomial - dbinom(x, size, prob) pbinom(x, size, prob)
# Geometric - dgeom(x, prob) pgeom(x, prob)
# Poisson - dpois(x, lambda) ppois(x, lambda)

pbinom(7, size = 10, prob = 0.5) # p(x<=7)
pbinom(7, 10, 0.5, low.tail = FALSE) # P(X > x)

pbinom(7, 10, 0.5) - pbinom(3, 10, 0.5) # P(3 <= X <= 7)
pbinom(c(3,7), 10, 0.5) # 두 개의 누적확률 한번에 구함
diff(pbinom(c(3,7), 10, 0.5))


# 8.9 Calculating Probabilities for Continuous Distributions
pnorm(q =.8, mean = 0, sd = 1)

# Normal: pnorm(x, mean, sd)
# Student's t: pt(x, df)
# Exponential: pexp(x, rate)
# Gamma: pgamma(x, shape, rate)
# Chi-squared (χ2): pchisq(x, df)

pnorm(66, mean = 70, sd =3) # P(X ≤ 66) given that X ~ N(70, 3)
pexp(20, rate = 1/40)
pexp(50, rate = 1/40, lower.tail = FALSE)
pexp(50, rate = 1/40) - pexp(20, rate = 1/40) 










