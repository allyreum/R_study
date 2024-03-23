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
