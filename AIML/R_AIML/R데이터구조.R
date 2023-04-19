x <- c(FALSE, FALSE, TRUE)
x + x # 0 0 2    logical vector 가 숫자로 변환 가능

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)
df

x <- c(1:10, 5:15, -3:-1)
which.min(x) #위치를 반환해줌

x <- c(10:1)
which(x<5) #위치를 반환해줌

sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","%")
sstr[sstr %in% c(letters, LETTERS)]

name_vec <- c("Seoul-si", "Seongdong-gu", "Haengdang-dong")
strsplit(name_vec, "-")

head(subset(airquality, Temp > 80, select = c(Ozone, Temp)))

y <- 0
for(i in 1:10){
  y <- y+i
  cat("summation from 0 to ", i, " is ", y, "\n", sep="")
}

