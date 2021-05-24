
# 2.데이터 탐색
getwd()
setwd('C:/Rtest/')

## 1] 데이터셋 로딩
### 가장 빠르게 메모리 로딩하는 방식 선택
library(data.table)
data <- fread(input='data.csv',header=TRUE,sep=',',stringsAsFactors=FALSE,strip.white=TRUE,na.strings=c(''))

## 2] 전반적인 데이터셋 내용 및 구조 탐색
### 규모 확인
data_dim <- dim(data)
sprintf('로딩 데이터셋 규모 : 레코드 %d개, 컬럼변수 %d개',data_dim[1],data_dim[2])

### 컬럼 변수명 확인
names(data)

### 데이터 일부 샘플링을 통한 구조 확인
data

### 기술 통계 분석
library(skimr)
skimr::skim(data)
#### senior[고령자할인 여부]는 고객의 분포가 미해당(0)으로 치우쳐 있다.
#### charge_total[총 청구금액]에 결측치가 포함되어 있는 레코드가 11개 있다.
#### insurance[보험 등급]은 미가입이 가장 많고, 등급이 올라갈수록 가입자수가 감소하는 추세를 보인다.

## 3] 전반적인 데이터셋 시각화
library(dplyr)
my_data <- data %>% select(-customerID) 
round(cor(my_data,use='complete.obs'))
## 현재 데이터셋에서 customerID는 numeric하지 않으므로, 상관관계 매트릭스를 적용할 수 없다.
## 이를 제외한 나머지 columns에 대해, NA값을 제거하고 상호관련성을 파악

### 다음은 전체 컬럼간 유의미한 지표를 나타내는 결과
### tenure - contract,charge_total
### service - charge_month
### insurance - charge_month,charge_total
### streaming - charge_month,charge_total
### charge_month - charge_total

### (1)범주형 변수 시각화
ctg_data <- my_data[,c('gender','senior','tenure','service','insurance','billing','payment','churn'),with=FALSE]
plot(ctg_data)
cor(ctg_data,method='spearman',use='complete.obs')
plot(ctg_data$gender,ctg_data$insurance)

#### 범주형 변수에서 성별에 상관없이 보험등급은 고루 분포되어 있음을 알 수 있다.

cor(ctg_data$gender,ctg_data$insurance,method='spearman',use='complete.obs')
library(psych)
pairs.panels(ctg_data,method='spearman',hist.col = 'green',density = TRUE, ellipses = TRUE)

library(corrplot)
ctg_pure <- na.omit(ctg_data)
corrplot(cor(ctg_pure),method='pie')

#### 고령자 할인을 받을수록 고속 인터넷을 사용한다

### (2) 연속형 변수 시각화
cnt_data <- my_data[,c('dependents','phone_lines','streaming','contract','charge_month','charge_total')]
plot(cnt_data)
cor(cnt_data,method='pearson',use='complete.obs')

plot(cnt_data$streaming,cnt_data$charge_month)
#### 연속형 변수에서 부가서비스 수가 많을수록 월 청구금액이 증가하고 있다.
cor(cnt_data$streaming,cnt_data$charge_month,method='pearson',use='complete.obs')

pairs.panels(cnt_data,method='pearson',hist.col = 'green',density = TRUE, ellipses = TRUE)
#### 부양 가족수가 많을 수록 약정 주기가 길어짐을 알수 있다.(통신사 이동에 둔감)

cnt_pure <- na.omit(cnt_data)
corrplot(cor(cnt_pure),method='pie')

### (3) 범주 -연속 변수간 시각화
plot(ctg_data$insurance,cnt_data$charge_total)
#### 보험 등급이 높은 고객은 총 청구금액이 높음을 알 수 있다.
cor(ctg_data$insurance,cnt_data$charge_total,method='spearman',use='complete.obs')

# 3.개별변수 요약과 집계
## 1] 범주형 변수를 2개 선정해 특성요약과 시각화
head(ctg_data)
str(ctg_data)
summary(ctg_data)

### (1) 고객 이탈여부 [이항형]
ctg_data$churn

### 일부 내용, 구조 파악
head(ctg_data$churn,10)
tail(ctg_data$churn,15)
length(ctg_data$churn)
NROW(ctg_data$churn)
psych::describe(ctg_data$churn)

#### 이항형 척도인 이탈 여부에서 평균등의 통계는 무의미함.
library(ggplot2)
library(Hmisc)
Hmisc::describe(ctg_data$churn)

### 이탈여부 팩터형 변환
ctg_data$churn_f <- factor(ctg_data$churn,levels = c(0,1), labels = c('N','Y'))
head(ctg_data)
str(ctg_data)

### 이탈여부 빈도분석
churn_f_freq <- table(ctg_data$churn_f)
churn_f_freq

### 이탈여부 비율분석
churn_f_prop <- prop.table(churn_f_freq)
churn_f_prop 

### 이탈여부 비율 소숫점 처리
round(churn_f_prop,3)

### 이탈여부 백분율 분석
churn_f_pect <- round(churn_f_prop,3)*100
churn_f_pect

### 이탈여부 시각화
par(mfrow=c(2,2))

barplot(churn_f_freq,main='통신사 이탈 인원수 분포: Simple Bar Plot',xlab='이탈여부',ylab='인원수')
barplot(churn_f_freq,main='통신사 이탈 인원수 분포: Horizontal Bar Plot',xlab='인원수',ylab='이탈여부',horiz=TRUE)
barplot(churn_f_prop,main='통신사 이탈 인원수 분포: Simple Bar Plot',xlab='이탈여부',ylab='분포비율',density=c(20,60),legend=rownames(churn_f_freq))
barplot(churn_f_prop,main='통신사 이탈 인원수 분포: Horizontal Bar Plot',xlab='분포비율',ylab='이탈여부',horiz=TRUE,col=c("lightblue","pink"),beside=TRUE,legend=rownames(churn_f_freq))
par(mfrow=c(1,1))

#### 현재 데이터셋의 75%는 현재 통신사를 이탈하지 않고, 계속 이용중이다.
#### 현재 결과는 종속변수에 대한 수치이므로, 이에 영향을 끼친 독립,조절변수를 알아보아야 한다.
#### 75%의 잔류 고객들이 어떤 요인들로 인해 유지하려고 하는지를 분석하여, 해당 요인을 강화할 수 있다.
#### 25%의 이탈 고객들이 어떤 요인들로 인해 이탈하게 되었는지를 분석하여, 이탈 방지 대책을 수립할 수 있다.

### (2) 고객등급 [서열형]
ctg_data$tenure

### 일부 내용, 구조 파악
head(ctg_data$tenure,10)
tail(ctg_data$tenure,15)
length(ctg_data$tenure)
NROW(ctg_data$tenure)
str(ctg_data$tenure)

Hmisc::describe(ctg_data$tenure)

### 고객등급 팩터형 변환
ctg_data$tenure_f <- factor(ctg_data$tenure,levels=c(1,2,3),labels=c('일반고객','우수고객','장기고객'))
head(ctg_data)
str(ctg_data)

### 고객등급 빈도분석
tenure_f_freq <- table(ctg_data$tenure_f)
tenure_f_freq

### 빈도분석 결과를 데이터프레임화
tenure_df <- as.data.frame(tenure_f_freq)
View(tenure_df)

### 고객등급 비율분석
tenure_f_prop <- prop.table(tenure_f_freq)
tenure_f_prop

### 고객등급 비율분석 데이터프레임화 및 내림차순 정렬
tenure_prop_df <- as.data.frame(tenure_f_prop)
arrange(tenure_prop_df,desc(Freq))

### 고객등급 비율 소숫점 처리
round(tenure_f_prop,3)

### 고객등급 백분율 분석
tenure_f_pect <- round(tenure_f_prop,3)*100
tenure_f_pect

### 고객등급 백분율분석 데이터프레임화 및 내림차순 정렬
tenure_pect_df <- as.data.frame(tenure_f_pect)
arrange(tenure_pect_df,desc(Freq))

### 고객등급 시각화
par(mfrow=c(2,2))

barplot(tenure_f_freq,main="고객 등급별 분포",xlab="고객등급",ylab="인원수")
barplot(tenure_f_freq,main="고객 등급별 분포",xlab="인원수",ylab="고객등급",las=1,horiz=TRUE)
barplot(tenure_f_prop,main="고객 등급별 분포",xlab="고객등급",ylab="분포비율",density=c(10,50,90),legend=rownames(tenure_f_prop))
barplot(tenure_f_prop,main="고객 등급별 분포",xlab="분포비율",ylab="고객등급",las=1,horiz=TRUE,col=c("green","yellow","red"),beside=TRUE,legend=rownames(tenure_f_prop))
par(mfrow=c(1,1))

#### 현재 데이터셋의 고객들은 이용기간이 24개월 미만인 일반고객이 가장많고, 48개월이상 이용중인 장기고객, 24개월이상~48개월미만인 우수고객 순으로 분포되어 있다.
#### 일반적으로 통신사 약정이 2년 주기인점을 감안하면, 일반고객층을 타켓팅하여 장기고객으로 유도할만한 기획상품을 만들어 볼 수 있다.

#### 추가적으로 앞서 시각화한 이탈여부에 대해 고객등급이 미치는 영향을 분석해본다면
#### 이탈률이 높은 등급의 고객들을 대상으로, 현재 통신사를 유지할 수 있을만한 기획상품을 고려할 수 있다.
#### 이탈률이 낮은 등급의 고객들을 대상으로, 추가적인 프로모션을 통해 지속성을 유지할 수 있다.

## 2] 연속형 변수를 2개 선정해 특성요약과 시각화
head(cnt_data)
str(cnt_data)
summary(cnt_data)

### (1) 월 청구 금액구간 [등간형]
cnt_data$charge_month

### 일부 내용, 구조 파악
head(cnt_data$charge_month,5)
tail(cnt_data$charge_month,10)
cnt_data$charge_month[10:25]
length(cnt_data$charge_month)
NROW(cnt_data$charge_month)
str(cnt_data$charge_month)

### 월 청구 금액 구간 기술 통계
summary(cnt_data$charge_month)
psych::describe(cnt_data$charge_month)
Hmisc::describe(cnt_data$charge_month)
skimr::skim(cnt_data$charge_month)

### 월 청구 금액 구간 중심성 파악
mean(cnt_data$charge_month,na.rm=TRUE)
mean(cnt_data$charge_month,na.rm=TRUE,trim=0.3)
### 극단치가 어느정도 존재함을 알 수 있음.
median(cnt_data$charge_month)

charge_m_freq <- table(cnt_data$charge_month)
charge_m_freq
names(charge_m_freq)
names(which.max(charge_m_freq))

### 월 청구 금액 구간 팩터형 변환
cnt_data$charge_m_f <- factor(cnt_data$charge_month,levels=c(1,2,3,4),labels = c('0~30$','30~60$','60~90$','90~120$'))

### 월 청구 금액 구간 빈도수 계산
charge_m_f_freq <- table(cnt_data$charge_m_f)
charge_m_f_freq
names(charge_m_f_freq)
names(which.max(charge_m_f_freq))
head(cnt_data)

### 월 청구 금액 구간 변동,중심성 파악
var(cnt_data$charge_month)
var(cnt_data$charge_month,na.rm=TRUE)

sd(cnt_data$charge_month)
sd(cnt_data$charge_month,na.rm=TRUE)

range(cnt_data$charge_month)
range(cnt_data$charge_month,na.rm=TRUE)

max(cnt_data$charge_month)
min(cnt_data$charge_month)

library(fBasics)
skewness(cnt_data$charge_month)
skewness(cnt_data$charge_month,na.rm=TRUE)
kurtosis(cnt_data$charge_month)
kurtosis(cnt_data$charge_month,na.rm=TRUE)

### 월 청구 금액 구간 시각화
plot(cnt_data$charge_month,type='p',pch=21,bg='yellow',xlab='레코드 순번',ylab='월 청구금액구간')
abline(h=seq(from=1,to=4,by=1),col='gray',lty=2)
abline(v=seq(from=500,to=7500,by=500),col='gray',lty=2)

par(mfrow=c(2,2))
hist(cnt_data$charge_month,main="hist(), Frequency 옵션",xlab="월 청구금액 구간",ylab="인원수")
hist(cnt_data$charge_month,probability=TRUE,main="hist(), Probability 옵션",xlab="월 청구금액 구간",ylab='밀도')
plot(density(cnt_data$charge_month),main="density() 확률밀도 옵션")
hist(cnt_data$charge_month,probability=TRUE,main="hist() 히스토그램과 density() 확률밀도함수 통합",xlab="월 청구금액 구간",ylab="밀도")
lines(density(cnt_data$charge_month))
par(mfrow=c(1,1))

boxplot(cnt_data$charge_month,main='박스플롯',ylab='월 청구금액구간')

#### 월 청구금액은 0~30$미만 부터, 90~120$미만까지 분포하고 있으며 outlier는 존재하지 않는다.
#### 상위 25%와 중앙값이 60~90$미만으로 일치하고 있다.
#### 60$이상 월 청구되는 고객이 절반이상을 차지한다.
#### 월 청구금액과 이탈여부간 분석을 통해, 이탈률이 높은 청구금액대 고객에게 추가적인 할인을 제공하여, 이탈 방지 대책을 수립할 수 있다.

### (2) 총 지출금액 [비율형]
cnt_data$charge_total

### 일부 내용, 구조 파악
head(cnt_data$charge_total,5)
tail(cnt_data$charge_total,7)
cnt_data$charge_total[20:25]

length(cnt_data$charge_total)
NROW(cnt_data$charge_total)
str(cnt_data$charge_total)

### 총 지출금액 기술 통계
summary(cnt_data$charge_total)
psych::describe(cnt_data$charge_total)
Hmisc::describe(cnt_data$charge_total)
skimr::skim(cnt_data$charge_total)

#### 극단치를 제외한 평균과 전체 평균간의 차이를 통해 평균적으로 2000$미만을 현재까지 통신사에 지출한 고객들이 분포해있다.

### 총 지출금액 중심성 파악
mean(cnt_data$charge_total)
mean(cnt_data$charge_total,na.rm=TRUE)

### 결측치가 존재하기 떄문에 제거 후 기술통계함수를 적용함

median(cnt_data$charge_total)
median(cnt_data$charge_total,na.rm=TRUE)

charge_t_freq <- table(cnt_data$charge_total)
charge_t_freq
sort(charge_t_freq,decreasing = TRUE)

names(charge_t_freq)
names(which.max(charge_t_freq))

### 비율형은 빈도 분석을 통한 결과 해석은 무의미 하다. (연속적이기 때문)

### 총 지출금액 변동성,정규성 파악
var(cnt_data$charge_total,na.rm=TRUE)

sd(cnt_data$charge_total,na.rm=TRUE)

range(cnt_data$charge_total,na.rm=TRUE)
      
max(cnt_data$charge_total,na.rm=TRUE)
min(cnt_data$charge_total,na.rm=TRUE)

skewness(cnt_data$charge_total,na.rm=TRUE)
kurtosis(cnt_data$charge_total,na.rm=TRUE)

### 총 지출금액 시각화
plot(cnt_data$charge_total,type="p",pch=21,bg='red',xlab='레코드 순번',ylab='총 지출금액($)')
abline(h = seq(from=100,to=8700,by=100),col='black',lty=3)
abline(v = seq(from=500,to=7500,by=500),col='black',lty=3)

par(mfrow=c(2,2))
hist(cnt_pure$charge_total,main="hist(), Frequency 옵션",xlab='총 지출금액($)',ylab='빈도수')
hist(cnt_pure$charge_total,probability=TRUE,main="hist(), Probability 옵션",xlab='총 지출금액($)',ylab='밀도')
plot(density(cnt_pure$charge_total),main='density() 확률밀도 옵션',xlab='총 지출금액($)',ylab='밀도')
hist(cnt_pure$charge_total,probability=TRUE,main="hist() 히스토그램과 density() 확률밀도함수 통합",xlab='총 지출금액($)',ylab='밀도')
lines(density(cnt_pure$charge_total))
par(mfrow=c(1,1))

### 결측치가 있는 경우 lines 함수가 작동하지 않기 떄문에, 결측치를 제거한 record를 대상으로 시각화하였다.
par(mfrow=c(1,2))
boxplot(cnt_data$charge_total,main="박스플롯",ylab="총 지출금액($)")
boxplot(cnt_pure$charge_total,main="박스플롯(Pure)",ylab="총 지출금액($)")
par(mfrow=c(1,1))

### 그러나 해당 결측치는 비어있는 값이기 때문에 전체적인 경향성에는 영향을 미치지 않았다.

#### 0~500$미만에 해당하는 빈도수가 가장 많고, 총 지출금액이 증가할수록 빈도수가 낮아짐
#### 중앙값은 약 1400$(1397.475)이며, 최댓값 약 8700$(8684.8)와는 6배의 차이를 보인다.
#### 총 지출금액에는 여러가지 요인들로 인해 변동될 수 있다. 대표적으로는 가입기간이 길수록, 총 지출금액이 증가할 수 있다. 그러나, 이외에 부양가족수,가입회선 수, 부가서비스 및 보험 등의 가입여부에 따라서도 변동 가능하다. 
#### 월 평균 지출금액의 구간이 60~90$ 였던 것을 감안하면, 24개월미만인 고객들의 총 지출 평균금액과 총 지출금액의 중앙값이 비슷한 구간에 형성된다. 따라서, 고객 분포를 확인하지 않고서도 월 평균 지출금액과 총 지출금액간의 상관성을 통해 24개월 미만의 고객의 분포가 가장 많음을 예측해볼 수 있다.


# 4.다차원변수 요약과 집계
## 1] 3장에서 다룬 범주형 변수 2개간 특성요약과 시각화

### (1)고객등급과 이탈여부간 특성분석
summary(ctg_data$churn_f)
summary(ctg_data$tenure_f)
## 고객등급에 따른 이탈여부 교차빈도분석
table(ctg_data$tenure_f,ctg_data$churn_f,useNA='ifany')
tn_ch_freq <- table(ctg_data$tenure_f,ctg_data$churn_f,useNA='ifany')
#### 24개월미만의 가입기간을 보유한 일반고객층에서 가장많은 이탈이 발생했다.
#### 이들이 이탈하게 되는 부가적인 이유를 분석하여, 이탈방지 대책을 마련하면 좋을 것이다.

### 이탈여부에 따른 고객등급 교차빈도분석
table(ctg_data$churn_f,ctg_data$tenure_f,useNA='ifany')
ch_tn_freq <-table(ctg_data$churn_f,ctg_data$tenure_f,useNA='ifany')

### (2)고객등급과 이탈여부간 요약집계
addmargins(tn_ch_freq) # default 방향 : 2

addmargins(tn_ch_freq,1)
addmargins(tn_ch_freq,2)

tn_ch_freq_sum <- addmargins(tn_ch_freq,2)
tn_ch_freq_sum

prop.table(tn_ch_freq,1) # 각 고객등급에서 이탈여부 분포비율 비교
prop.table(tn_ch_freq,2) # 이탈여부별 고객등급 분포비율비교

### 비율분석
tn_ch_prop <- prop.table(tn_ch_freq,1)
tn_ch_prop

addmargins(round(tn_ch_prop,3),2)

### 백분율분석
tn_ch_result <- round(tn_ch_prop,3)*100
tn_ch_result


### (3) 범주형 변수간 시각화
par(mfrow=c(2,2))

barplot(ch_tn_freq,main="통신사 고객등급에 따른\n이탈여부 분포: Stacked",xlab='고객등급',ylab='빈도수',col=c('lightblue','pink'),legend=rownames(ch_tn_freq))

barplot(ch_tn_freq,main="통신사 고객등급에 따른\n이탈여부 분포: Grouped",xlab='고객등급',ylab='빈도수',beside=TRUE,col=c('lightblue','pink'),legend=rownames(ch_tn_freq))


barplot(tn_ch_freq,main="통신사 이탈여부에 따른\n고객등급 분포: Stacked",xlab="이탈여부",ylab='빈도수',
        col=rainbow(3),legend=rownames(tn_ch_freq))

barplot(tn_ch_freq,main="통신사 이탈여부에 따른\n고객등급 분포: Grouped",xlab="이탈여부",ylab="빈도수",
        col=rainbow(3),legend=rownames(tn_ch_freq),beside=TRUE)

par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(churn_f~tenure_f,data=ctg_data,main="통신사 고객등급에 따른\n이탈여부 분포",xlab='고객등급',ylab='이탈여부',col=rainbow(length(unique(ctg_data$churn_f))))
plot(tenure_f~churn_f,data=ctg_data,main="통신사 이탈여부에 따른\n고객등급 분포",xlab='이탈여부',ylab='고객등급',col=rainbow(length(levels(ctg_data$tenure_f))))
mosaicplot(tenure_f~churn_f,data=ctg_data,main="통신사 고객등급에 따른\n이탈여부 분포",xlab="고객등급",ylab="이탈여부",col=rainbow(2))
mosaicplot(churn_f~tenure_f,data=ctg_data,main="통신사 이탈여부에 따른\n고객등급 분포",xlab="이탈여부",ylab="고객등급",col=rainbow(3))
par(mfrow=c(1,1))
#### 통신사를 이탈하지 않는 고객 중 가장 적은 분포층은 우수고객이다.
#### 통신사를 이탈한 고객 중 가장 많은 분포층은 일반고객이다.

#### 일반고객이 이탈하게 되는 이유를 찾아 보완하고,
#### 이탈과 유지 모두 소극적인 우수고객층에 대해서 추가분석이 필요하다.

#### 각 고객등급별로 이탈여부를 살펴보면, 일반고객은 이탈가능성이 가장 높다.
#### 장기고객의 경우에는 90%가 그대로 통신사를 유지하며 이용하고 있다.

## 2] 3장에서 다룬 연속형 변수 2개간 특성요약과 시각화

### (1) 월 청구 금액구간과 총 지출금액간 특성분석
summary(cnt_pure$charge_month)

str(cnt_pure$charge_total)
psych::describe(cnt_pure$charge_total)

### (2) 월 청구금액 구간에 따른 총 지출금액변수간 관련성 파악
var(cnt_pure$charge_month,cnt_pure$charge_total)

cor(cnt_pure$charge_month,cnt_pure$charge_total,method='spearman')
cor(cnt_pure$charge_month,cnt_pure$charge_total,method='pearson')
#### 강한 양의 상관관계를 가지고 있다고 볼수 있다

### (3)연속형 변수간 시간화
plot(cnt_pure$charge_total ~ cnt_pure$charge_month,data=cnt_pure,pch=19,main='월 청구금액 구간과 총 지출금액간 관련성',xlab='월 청구금액구간',ylab='총 지출금액')
abline(lm(cnt_pure$charge_total ~ cnt_pure$charge_month,data=cnt_pure),col="red",lwd=2,lty=1)
lines(lowess(cnt_pure$charge_total ~ cnt_pure$charge_month),col="blue",lwd=2,lty=2)
#### 월 청구금액이 높으면 총 지출금액이 높아지는 추세를 보인다.

p <- ggplot(data = cnt_pure, aes(x=cnt_pure$charge_month,y=cnt_pure$charge_total))+ geom_point() + labs(title = "월 청구금액구간과 총 지출금액간 관련성",x='월 청구금액구간',y='총 지출금액')
p

library(plotly)
ggplotly(p)

#### 월 청구금액 구간과 총 지출금액 사이에는 월 청구금액이 높으면 총 지출금액이 높아지는 경향성을 띠나,
#### 총 지출금액은 가입기간과 연관성이 더 크기 때문에 
#### 조절변수로 고객등급을 추가했을 때,보다 정확한 데이터르 분포를 시각화 할 수 있을 것으로 보인다.

## 3] 3장에서 다룬 범주형 변수와 연속형 변수 중 1개 관계를 선정해 특성요약과 시각화

### 범주형 변수 컬럼명 파악
ctg_names <- names(ctg_data)
### 연속형 변수 컬럼명 파악
cnt_names <- setdiff(names(data),ctg_names)
cnt_names <- setdiff(cnt_names,'customerID')
cnt_names

### 범주형 변수 (고객등급 기술통계)
Hmisc::describe(ctg_data$tenure_f)

### 연속형 변수 (총 지출금액 기술통계)
summary(cnt_data$charge_total)
psych::describe(cnt_data$charge_total)

### (1)2차원 변수간 요약집계 분석
aggregate(formula = cnt_data$charge_total ~ ctg_data$tenure_f,data = data, FUN = mean, na.rm=TRUE)
aggregate(cnt_data$charge_total ~ ctg_data$tenure_f,data,mean, na.rm=TRUE, trim=0.05)
aggregate(cnt_data$charge_total ~ ctg_data$tenure_f,data,sd, na.rm=TRUE)

my_data = read.csv(file='data.csv',header=TRUE,sep=',',stringsAsFactors=FALSE,strip.white=TRUE,na.strings=c('',' '))

my_data$tenure_f <- factor(my_data$tenure,levels=c(1,2,3),labels=c('일반고객','우수고객','장기고객'))
library(magrittr)
library(dplyr)
my_data %>% filter(!is.na(charge_total)) %>%
  group_by(tenure_f) %>% 
  dplyr::summarize(Avg = mean(charge_total),SD = sd(charge_total)) %>%
  arrange(desc(Avg))

#### 통신사 이용기간이 길어질수록 (고객등급이 높을 수록), 총 지출금액이 증가한다.
#### 이전의 분석에서 일반고객층의 이탈빈도가 많음을 고려하면, 새로운 고객을 끌어들이는 것보다
#### 일반고객들을 장기고객으로 지속적인 유치하는 것이 통신사 입장에서 효과적일 수 있다.

### (2)2차원 변수간 시각화
par(mfrow=c(1,1))
boxplot(my_data$charge_total ~ my_data$tenure_f, data = my_data, main="고객등급에 따른 총 지출금액 분포 비교",xlab="고객등급",ylab="총 지출금액",col=c(2,3,4),varwidth=T,notch=T)

p1 <- ggplot(my_data,aes(tenure_f,charge_total)) + geom_point(color = 'red',shape=20,size=2)
p2 <- ggplot(my_data,aes(tenure_f,charge_total)) + geom_jitter(color = 'blue', shape=8,size=0.8)
p3 <- ggplot(my_data,aes(tenure_f,charge_total)) + geom_boxplot(fill = "lightblue", outlier.color = "orange", outplier.shape = 17, outlier.size = 2, notch =TRUE)
p4 <- ggplot(my_data,aes(tenure_f,charge_total)) + geom_violin(fill = "lightpink")

library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

#### 일반 고객의 경우 boxplot을 통해 outlier가 존재하는 결과를 확인할 수 있고
#### violinplot을 통해 적은 금액대에 총 지출금액이 모여있음을 알수 있다.
#### 우수고객과 장기고객의 경우에는 violinplot을 통해 전체적인 금액대에 총 지출금액이 분포함을 알 수 있다.
#### jitter를 통해 장기고객의 경우 2300$ 구간에 일부 영역이 존재하지 않음과 6000$ 구간에 데이터가 몰려있다.

#### 이를 통해 일반고객은 사용기간은 짧지만, 부가서비스나 회선 수 등에 따라 금액이 outlier에 해당하는 경우가 상당 수 존재함을 알 수 있고, 그 외 24개월 이상 사용자인 우수,장기 고객의 경우 금액이 고르게 분포되어 있음을 알수 있다.

#### 일반 고객이 이탈하는 경우가 상당히 많은데, 이를 총 지출금액과 연관지어서 생각해 본다면
#### 가입 초기 할인을 목적으로 과도한 금액대의 상품 사용을 제안받는다. 그러나, 약정 기간에 도달할 시점에서
#### 또 다른 통신사로 이탈함을 보아, 총 지출금액을 낮추면서, 장기고객으로 유치할 수 있는 기획이 필요하다.


# 5. 데이터 가공
## 1] 데이터셋 중 변수 리코딩 작업 2개 실시
str(my_data)

### (1) 성별 [범주]
str(my_data$gender)
table(my_data$gender)

### 수치형을 문자형으로 변환
my_data$gender_n <- dplyr::recode(my_data$gender,'1'='남성','2'='여성')
### 리코딩 결과 변수 컬럼명 확인
head(my_data)

### 리코딩 결과 변수특성 확인
sapply(my_data[c('gender','gender_n')],unique)


### (2) 월 청구금액 구간 [등간]
str(my_data$charge_month)
table(my_data$charge_month)

### 수치형을 문자형으로 변환
my_data$charge_month_n <- ifelse(my_data$charge_month == 1 ,yes = '30$미만',
                                 ifelse(my_data$charge_month == 2, yes = '60$미만',
                                        ifelse(my_data$charge_month == 3, yes = '90$미만',no = '120$미만')))

str(my_data)
sapply(my_data[c('charge_month','charge_month_n')],unique)


## 2] 데이터셋 중 요약변수 2개 만들기

### (1) 요인화를 통한 요약변수 [인터넷옵션,보험등급,부가서비스 가입수] : 전체 서비스 이용개수

### 1,2,3,4,5,6,7,8,9
service_rate <- c('service','insurance','streaming')

my_data$sr_sat <- apply(my_data[service_rate],1,sum)
table(my_data$sr_sat)
head(my_data)

#### 전체 서비스에 대한 이용 분포를 살펴보면, 4~6개의 옵션을 추가하여 사용하는 사람들이 43%를 차지함
#### 2~3개를 사용하는 사용자들에게 할인 혜택을 제공하여 옵션을 늘리거나,
#### 7~9개를 사용하는 사용자들에게 불필요한 옵션을 제거하여, 총 금액적인 부분에서도 감면을 받아 지속가능한 고객으로 유치할 수 있다.

### (2) 요인화를 통한 요약변수 [고령자여부,부양가족수,가입회선수] : 전체 이용 회선수

# 0,1,2,3,4,5
service_line <- c('senior','dependents','phone_lines')
my_data$sl_sat <- apply(my_data[service_line],1,sum)
table(my_data$sl_sat)
head(my_data)

#### 인터넷에만 가입되어 있는 고객도 일부 존재한다.
#### 일반적으로 휴대폰 2~3개 개통한 고객들이 주 고객층을 이룬다.
#### 가족단위의 가입 유치 및 회선 유지를 통해 통신사의 수익을 지속적으로 보장할 수 있다.
## 3] 데이터셋 중 파생변수 2개 만들기

### (1) 유형화에 따른 서열 유형화 [인터넷 옵션] : 가입고객과 미가입고객으로 재유형화
#### 가입, 미가입
str(my_data$service)
table(my_data$service)
my_data$service_z[my_data$service %in% c('1')] <- "미가입"
my_data$service_z[my_data$service %in% c('2','3')] <- "가입"

table(my_data$service_z)
head(my_data[c('service','service_z')],15)

#### 통신사에 등록된 고객 중 인터넷에 가입되어 있는 회선이 78%
#### 인터넷에도 가입되어 있는 고객을 대상으로 회선 유지를 할 수 있는 전략을 세울 수 있다.

### (2) 구간화에 따른 서열 구간화 [총 지출금액] : 구간에 따른 재등급화

#### VIP,GOLD,SILVER,BRONZE,PINE
str(my_data$charge_total)
psych::describe(my_data$charge_total)

#### 1500$미만 :Pine , 1500~3000$미만:Bronze, 3000~4500$미만:SILVER, 4500~6000$미만:GOLD, 6000$이상:VIP
my_data$charge_total_r <- cut(my_data$charge_total,breaks=c(-Inf,1500,3000,4500,600,Inf),include.lowest=TRUE,right=FALSE,labels=c('Pine','Bronze','SILVER','GOLD','VIP'))

head(my_data[c('charge_total','charge_total_r')],15)

my_data$charge_total_r %>% table %>% print %>% sort(decreasing=TRUE) %>% as.data.frame

#### 1500$ 미만 지출한 고객이 가장 많다.
#### 6000$ 이상 지출한 고객도 예상외로 많다.
#### 3000$~6000$를 지출하는 Silver,Gold 고객을 대상으로 추가적인 분석이 필요하다.

## 4] 5장에서 만든 리코딩/요약/파생변수 중 범주형 변수 2개간 특성요약과 시각화
### 리코딩된 성별(이항)과 총 지출금액에 따라 부여된 파생 등급(서열) 분포간의 관계
str(my_data$gender_n)
summary(my_data$gender_n)

str(my_data$charge_total_r)
summary(my_data$charge_total_r)

table(my_data$gender_n,my_data$charge_total_r)
table(my_data$charge_total_r,my_data$gender_n)

#### 모든 등급에서 남성이 약간의 우위를 점하고 있다.

gd_ch_freq <- table(my_data$gender_n,my_data$charge_total_r)
ch_gd_freq <- table(my_data$charge_total_r,my_data$gender_n)

addmargins(gd_ch_freq)
addmargins(ch_gd_freq)

addmargins(round(prop.table(gd_ch_freq),3),2)

par(mfrow=c(2,2))

barplot(ch_gd_freq,main='성별에 따른 지출등급분포',xlab='성별',ylab='분포자수',col=rainbow(5),legend=rownames(ch_gd_freq))
barplot(ch_gd_freq,main='성별에 따른 지출등급분포',xlab='성별',ylab='분포자수',beside=TRUE,col=rainbow(5),legend=rownames(ch_gd_freq))
barplot(gd_ch_freq,main='지출등급에 따른 성별분포',xlab='지출등급',ylab='분포자수',col=rainbow(2),legend=rownames(gd_ch_freq))
barplot(gd_ch_freq,main='지출등급에 따른 성별분포',xlab='지출등급',ylab='분포자수',beside=TRUE,col=rainbow(2),legend=rownames(gd_ch_freq))
par(mfrow=c(1,1))

#### 각 등급에 속해있는 빈도는 남성과 여성이 거의 유사하다.
#### 성별에 무관하게 Pine 등급 고객이 가장 많고, GOLD 등급 고객이 가장 적다.
#### 성별이나, 등급을 따지지 않고 전체 고객에 대한 이탈방지 대책을 마련해야한다.

## 5] 5장에서 만든 리코딩/요약/파생변수 중 연속형 변수 2개간 특성요약과 시각화
### 요약된 서비스이용개수(비율)와 요악된 전체 회선수(비율)간의 관계

str(my_data$sr_sat)
summary(my_data$sr_sat)
psych::describe(my_data$sr_sat)

str(my_data$sl_sat)
summary(my_data$sl_sat)
psych::describe(my_data$sl_sat)

var(my_data$sr_sat,my_data$sl_sat)
cor(my_data$sr_sat,my_data$sl_sat,method = 'spearman') # 범주형 데이터간 상관계수

#### 상관 계수는 작은 값으로 나왔다.

plot(sr_sat~sl_sat,my_data,pch=19,main='전체 회선수가 서비스 이용개수에 미치는 영향',xlab='전체 회선수',ylab='서비스 가입수')
abline(lm(sr_sat~sl_sat,my_data),col='red',lwd=2,lty=1)
lines(lowess(my_data$sr_sat~my_data$sl_sat),col='blue',lwd=2,lty=2)

#### 추세선에 따르면 전체 회선수가 많을수록 서비스 가입개수도 증가하는 형태를 보인다.
#### 가족단위로 가입하는 고객을 대상으로 가입을 유치하고, 이탈하지 않도록 묶음상품을 기획할 수 있다.

## 6] 5장에서 만든 리코딩/요약/파생변수 중 1개 관계를 선정해 특성요약과 시각화
### 유형화에 의해 파생된 인터넷가입여부(이항)와 요인화로 요약화된 서비스이용개수(비율)의 관계

str(my_data$service_z)
Hmisc::describe(my_data$service_z)

str(my_data$sr_sat)
Hmisc::describe(my_data$sr_sat)
aggregate(sr_sat~service_z,my_data,mean,na.rm=TRUE)
aggregate(sr_sat~service_z,my_data,mean,na.rm=TRUE,trim=0.05)
aggregate(sr_sat~service_z,my_data,sd,na.rm=TRUE)

my_data %>% filter(!is.na(sr_sat)) %>% group_by(service_z) %>% dplyr::summarize(Avg = mean(sr_sat),SD = sd(sr_sat)) %>% arrange(desc(Avg))

#### 인터넷에 가입되어 있는 고객의 경우 평균적으로 사용하는 서비스가 5개 이상이고
#### 인터넷을 사용하지 않는 고객의 경우 평균적으로 사용하는 서비스가 1개임을 알 수 있다.

p1 <- ggplot(my_data,aes(service_z,sr_sat)) + geom_point(color = 'red', shape = 20, size = 2) + xlab('인터넷 회선') + ylab("서비스 이용개수") + ggtitle('인터넷 회선 가입여부별 서비스 이용개수')
p2 <- ggplot(my_data,aes(service_z,sr_sat)) + geom_jitter(color = 'blue', shape = 8, size = 0.8) + xlab('인터넷 회선') + ylab("서비스 이용개수") + ggtitle('인터넷 회선 가입여부별 서비스 이용개수')
p3 <- ggplot(my_data,aes(service_z,sr_sat)) + geom_boxplot(fill = 'lightblue', outlier.color = 'orange', outlier.shape = 17, outlier.size = 2, notch=TRUE) + xlab('인터넷 회선') + ylab("서비스 이용개수") + ggtitle('인터넷 회선 가입여부별 서비스 이용개수')
p4 <- ggplot(my_data,aes(service_z,sr_sat)) + geom_violin(fill='lightpink') + xlab('인터넷 회선') + ylab("서비스 이용개수") + ggtitle('인터넷 회선 가입여부별 서비스 이용개수')
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
                   

#### point plot과 jitter plot을 통해 인터넷 미가입 고객은 서비스 이용개수가 1개임을 알수 있고
#### 인터넷 가입 고객의 경우에는 다양한 분포를 갖되, 4~6사이에 대부분의 고객이 몰려있다.
#### 평균적으로 인터넷 가입 고객은 5개의 서비스를 가입하여 사용중이나, 최소 1개부터 최대 9개까지 사용하는 고객들이 존재한다.
#### 지나치게 많은 서비스를 이용하는 고객에 대해서는 불필요한 부분이 있는지 점검하여, 금액적인 부분에서 감면혜택을 받아 본 통신사를 지속적으로 이용가능하도록 유도하고
#### 정말 필요한 서비스만 이용하는 고객에 대해서는 무료 체험과 같은 이벤트를 통해, 추가적인 서비스 이용을 유도할 수 있는 방법을 사용할 수 있을 것이다.

