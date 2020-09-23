# 필수 패키지
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("ggmap")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("rgdal")


# 라이브러리
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

getwd()
setwd("C:/Users/User/Downloads/analysis/")

accident_snow <- read.csv("accident.csv", header = T)

#필요한 데이터만 추출
# 사망+중상+경상+부상신고자수
snow <- dplyr::select(accident_snow, c(사고일시:사고유형, 노면상태:도로형태))

snow_1 <- snow %>% 
  mutate(일시 = ymd_h(accident_snow$사고일시)) %>%
  mutate(사고피해자수 = 사망자수+중상자수+경상자수+부상신고자수) %>%
  dplyr::select(-c(사망자수:부상신고자수, 노면상태, 사고유형, 사고일시,도로형태)) %>%
  dplyr::select(c(5,1,2,3,4,6))



# 구 개수 추출
yuseong <- nrow(snow_1[grep("유성구", snow_1$시군구),]) #63개
daeduk <- nrow(snow_1[grep("대덕구", snow_1$시군구),]) #16개
west <-nrow(snow_1[grep("서구", snow_1$시군구),]) #41개
middle <-nrow(snow_1[grep("중구", snow_1$시군구),]) #19개
east <- nrow(snow_1[grep("동구", snow_1$시군구),]) #20개

# 구별 데이터만 추출
snow_yuseong <- snow_1[grep("유성구", snow_1$시군구),]
snow_daeduk <- snow_1[grep("대덕구", snow_1$시군구),]
snow_west <- snow_1[grep("서구", snow_1$시군구),]
snow_middle <- snow_1[grep("중구", snow_1$시군구),]
snow_east <- snow_1[grep("동구", snow_1$시군구),]

# 구별 사고율(단순 barplot으로 먼저 표현)
gu <- cbind(yuseong, daeduk, west, middle, east)
gu_table <- prop.table(gu)
barplot(gu_table)

# 날씨 데이터 (2017~2019)

weather_2017 <- read.csv("weather_2017.csv", header=T)
weather_2018 <- read.csv("weather_2018.csv", header=T)
weather_2019 <- read.csv("weather_2019.csv", header=T)
weather_avg <- read.csv("weather_total_data.csv", header=T)

weather <- rbind(weather_2017, weather_2018, weather_2019)
weather <- weather[,-c(1,2)]
colnames(weather) <-c("일시", "temp", "rain", "snow", "temp_land")
weather <- weather %>%
  mutate(rain = ifelse(rain==0, 0.01, rain), snow = ifelse(snow == 0, 0.01, snow)) %>%
  mutate(일시 = as.POSIXct(일시))

# 후에 NA값을 0으로 대체하기 위해 기존 값에 있던 0값이 유효값임을 알기 위해 0.01로 치환
# 위 작업 후 NA값을 0으로 치환
weather$rain[is.na(weather$rain)] <- 0
weather$snow[is.na(weather$snow)] <- 0



# 총 교통량(구단위) (2017~2019.3)
traffic <- read.csv("road.csv", header=T)


## 사고, 날씨 통합

class(snow_1$일시)
class(weather$일시)



union <- merge(snow_1, weather, key=일시)

for (i in 1:nrow(union)) {
  a = unlist(strsplit(union$시군구[i]," "))
  union$구[i] <- a[grep("구$",a)] # 구만 추출
}






###### 분석 시작 ######
# 구별 사고율
head(union)

cor(union$사고피해자수, union$snow)


p <- ggplot(union) + geom_bar(aes(구, fill=구), stat='count')
p + scale_fill_brewer(palette = 4) +
  ggtitle("구별 사고량") + theme(plot.title = element_text(size=18)) +
  theme_minimal()

# 구별 기온

# 구별 지표면온도
union %>%
  ggplot(mapping = aes(y=geom_boxplot(aes(구, temp_land, fill=구, colour = "snow4", outlier.shape=NA)))) 
                                      + theme_classic()
                                      
                                      
# 교통량 대비 사고율




# 교통량 대비 월별 사고율

# 요인별 상관계수

# 사고건 당 평균기온


# 대표적인 온도


union <- read.csv('union.csv', header = TRUE)

union <- transform(union,
                   범위=cut(temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                   include.lowest=TRUE,
                   right=FALSE
                   )
)
union

ggplot(union, aes(범위, 사고피해자수))

cut_value <- cut(union$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
p <- table(cut_value)
barplot(p, main="대전", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 20))

ggplot(p, aes(범위)) + geom_bar()
?barplot

# 서구
union_seogu <- union %>%
  filter(구=="서구")
seogu_p <- table(cut(union_seogu$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
barplot(seogu_p,main="서구", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 10))

# 중구
union_junggu <- union %>%
  filter(구=="중구")
junggu_p <- table(cut(union_junggu$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
barplot(junggu_p,main="중구", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 10))

# 유성구
union_yuseonggu <- union %>%
  filter(구=="유성구")
yuseonggu_p <- table(cut(union_yuseonggu$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
barplot(yuseonggu_p,main="유성구", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 10))

# 대덕구
union_daedeokgu <- union %>%
  filter(구=="대덕구")
daedeokgu_p <- table(cut(union_daedeokgu$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
barplot(daedeokgu_p,main="유성구", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 10))

# 동구
union_donggu <- union %>%
  filter(구=="동구")
donggu_p <- table(cut(union_donggu$temp, breaks=c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
barplot(donggu_p,main="동구", xlab="온도 구간", ylab="사고피해자수수", ylim=c(0, 10))

