#다듬어 지지 않음
#다음부터 주석처리도 더 깔끔하게...
library(dlookr)
library(readxl)
library(tidyverse)
library(dplyr)
library(webr)
#install.packages("FSA")
library(FSA)
library(inspectdf)
library(ggplot2)
library(dplyr)
#install.packages(c("ggmap","sf"))
library(ggmap)
library(sf)


rdata <- read_excel("rdata1.xlsx")
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))
str(rdata)


summary(rdata)
is.na(rdata) %>% colSums()
rdata %>% inspect_na() %>% show_plot()
colSums(is.na(rdata))

diagnose_category(rdata) %>% write.csv("범주형기술통계.csv")

rdata %>% PieDonut(aes(성별))
rdata %>% PieDonut(aes(반,성별))

table(rdata$출신초) %>% pie()
table(rdata$출신초) %>% barplot()
table(rdata$출신초,rdata$반) %>% barplot()


school_table <- table(rdata$출신초)  # 출신 초등학교별 빈도 계산

barplot(school_table, 
        main = "출신 초등학교별 학생 수", 
        col = rainbow(length(school_table)), 
        las = 2,        # x축 라벨을 세로로 표시
        cex.names = 0.8) # 글자 크기 조정





ggplot(rdata, aes(x = as.factor(반), fill = 출신초)) +
  geom_bar(position = "dodge") +  # 출신 초등학교별 나란히 배치
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # x축 라벨 회전
  labs(title = "학급별 출신 초등학교 빈도", x = "반", y = "학생 수", fill = "출신 초등학교")



rdata %>%
  filter(반 == 1) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "1반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

rdata %>%
  filter(반 == 2) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "2반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

rdata %>%
  filter(반 == 3) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "3반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

rdata %>%
  filter(반 == 4) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "4반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 5) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "5반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 6) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "6반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 7) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "7반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 8) %>%  # 6반만 선택
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  labs(title = "8반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 1) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "1반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거



rdata %>%
  filter(반 == 2) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "2반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 3) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "3반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

rdata %>%
  filter(반 == 4) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "4반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 5) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "5반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 6) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "6반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

rdata %>%
  filter(반 == 7) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "7반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거


rdata %>%
  filter(반 == 8) %>% 
  ggplot(aes(x = "", fill = 출신초)) +  # x축을 공백("")으로 설정해 원형 차트 형식
  geom_bar(position = "fill") +  # 비율 그래프
  coord_polar(theta = "y") +  # 파이 차트 변환
  facet_wrap(~ 성별) +  # 성별별 그래프 나누기
  labs(title = "8반 출신 초등학교 분포", fill = "출신 초등학교") +
  theme_void()  # 배경 제거

register_google(key = "YOUR_GOOGLE_API_KEY")

gimhae_map <- get_map(location = c(lon = 128.8889, lat = 35.2287), zoom = 12, maptype = "roadmap")


rdata <- read_excel("rdata1.xlsx")
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))
str(rdata)
rdata$month <- substr(rdata$생년월일, 6, 7)

month_freq <- as.data.frame(table(rdata$month))
colnames(month_freq) <- c("Month", "Frequency")

month_freq$Month <- factor(month_freq$Month, levels = sprintf("%02d", 1:12))

ggplot(month_freq, aes(x = Month, y = Frequency, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "월별 학생 생일 빈도", x = "월", y = "학생 수") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")


#### # ----------------- 1. 학급별 월별 생일 분포 -----------------####
class_month_freq <- rdata %>%
  group_by(학년, 반, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
class_month_freq$month <- factor(class_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화
ggplot(class_month_freq, aes(x = month, y = Frequency, fill = interaction(학년, 반))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "학급별 월별 학생 생일 빈도", x = "월", y = "학생 수", fill = "학급") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")





library(readxl)
library(dplyr)
library(ggplot2)

# 엑셀 데이터 불러오기
rdata <- read_excel("rdata1.xlsx")

# 학년, 반, 성별, 출신초를 범주형(factor)으로 변환
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))

# 생년월일에서 월 추출
rdata$month <- substr(rdata$생년월일, 6, 7)

# ----------------- 1. 학급별 월별 생일 분포 -----------------
class_month_freq <- rdata %>%
  group_by(학년, 반, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
class_month_freq$month <- factor(class_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 학급별로 각각 월별 생일 분포 출력
ggplot(class_month_freq, aes(x = month, y = Frequency, fill = 반)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 학년) +  # 학년별로 그래프 분리
  labs(title = "학급별 월별 학생 생일 빈도", x = "월", y = "학생 수", fill = "반") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#

library(readxl)
library(dplyr)
library(ggplot2)

# 엑셀 데이터 불러오기
rdata <- read_excel("rdata1.xlsx")

# 학년, 반, 성별, 출신초를 범주형(factor)으로 변환
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))

# 생년월일에서 월 추출
rdata$month <- substr(rdata$생년월일, 6, 7)

# 1반 데이터 필터링
rdata_1반 <- rdata %>%
  filter(반 == "1")  # 반이 "1"인 데이터만 선택

# ----------------- 1. 1반 학생들의 월별 생일 분포 -----------------

# 1반 데이터 필터링
rdata_1반 <- rdata %>%
  filter(반 == "1")  # 반이 "1"인 데이터만 선택


class_1반_month_freq <- rdata_1반 %>%
  group_by(학년, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
class_1반_month_freq$month <- factor(class_1반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반 학생들의 월별 생일 분포
ggplot(class_1반_month_freq, aes(x = month, y = Frequency, fill = 학년)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "1반 학생들의 월별 생일 빈도", x = "월", y = "학생 수", fill = "학년") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# ----------------- 2. 1반에서 남/여별 월별 생일 분포 -----------------
gender_1반_month_freq <- rdata_1반 %>%
  group_by(학년, 성별, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
gender_1반_month_freq$month <- factor(gender_1반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반에서 남/여별 월별 생일 분포
ggplot(gender_1반_month_freq, aes(x = month, y = Frequency, fill = 성별)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "1반 학생들의 성별별 월별 생일 빈도", x = "월", y = "학생 수", fill = "성별") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


# ----------------- 1. 2반 학생들의 월별 생일 분포 -----------------

# 1반 데이터 필터링
rdata_3반 <- rdata %>%
  filter(반 == "3")  # 반이 "1"인 데이터만 선택


class_3반_month_freq <- rdata_3반 %>%
  group_by(학년, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
class_3반_month_freq$month <- factor(class_3반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반 학생들의 월별 생일 분포
ggplot(class_3반_month_freq, aes(x = month, y = Frequency, fill = 학년)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "3반 학생들의 월별 생일 빈도", x = "월", y = "학생 수", fill = "학년") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# ----------------- 2. 2반에서 남/여별 월별 생일 분포 -----------------
gender_3반_month_freq <- rdata_3반 %>%
  group_by(학년, 성별, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
gender_3반_month_freq$month <- factor(gender_3반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반에서 남/여별 월별 생일 분포
ggplot(gender_3반_month_freq, aes(x = month, y = Frequency, fill = 성별)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "3반 학생들의 성별별 월별 생일 빈도", x = "월", y = "학생 수", fill = "성별") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")





# ----------------- 1. 3반 학생들의 월별 생일 분포 -----------------

# 엑셀 데이터 불러오기
rdata <- read_excel("rdata1.xlsx")

# 학년, 반, 성별, 출신초를 범주형(factor)으로 변환
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))

# 생년월일에서 월 추출
rdata$month <- substr(rdata$생년월일, 6, 7)



# 1반 데이터 필터링
rdata_8반 <- rdata %>%
  filter(반 == "8")  # 반이 "1"인 데이터만 선택


class_8반_month_freq <- rdata_8반 %>%
  group_by(학년, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
class_8반_month_freq$month <- factor(class_8반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반 학생들의 월별 생일 분포
ggplot(class_8반_month_freq, aes(x = month, y = Frequency, fill = 학년)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "8반 학생들의 월별 생일 빈도", x = "월", y = "학생 수", fill = "학년") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# ----------------- 2. 8반에서 남/여별 월별 생일 분포 -----------------
gender_8반_month_freq <- rdata_8반 %>%
  group_by(학년, 성별, month) %>%
  summarise(Frequency = n(), .groups = "drop")

# 월 정렬 (1~12월 순서 유지)
gender_8반_month_freq$month <- factor(gender_8반_month_freq$month, levels = sprintf("%02d", 1:12))

# 시각화: 1반에서 남/여별 월별 생일 분포
ggplot(gender_8반_month_freq, aes(x = month, y = Frequency, fill = 성별)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "8반 학생들의 성별별 월별 생일 빈도", x = "월", y = "학생 수", fill = "성별") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
















####--------------------------------- 반복문 이용-------------------------------------------- ####
# 엑셀 데이터 불러오기
rdata <- read_excel("rdata1.xlsx")

# 학년, 반, 성별, 출신초를 범주형(factor)으로 변환
rdata <- rdata %>%
  mutate(across(c(학년, 반, 성별, 출신초), as.factor))

# 생년월일에서 월 추출
rdata$month <- substr(rdata$생년월일, 6, 7)

# 분석할 반 목록
반_목록 <- c("3", "4", "5", "6", "7", "8")

for (반_번호 in 반_목록) {
  # 해당 반 데이터 필터링
  rdata_반 <- rdata %>%
    filter(반 == 반_번호)
  
  # ----------------- 1. 해당 반 학생들의 월별 생일 분포 -----------------
  class_반_month_freq <- rdata_반 %>%
    group_by(학년, month) %>%
    summarise(Frequency = n(), .groups = "drop")
  
  # 월 정렬 (1~12월 순서 유지)
  class_반_month_freq$month <- factor(class_반_month_freq$month, levels = sprintf("%02d", 1:12))
  
  # 시각화: 해당 반 학생들의 월별 생일 분포
  print(
    ggplot(class_반_month_freq, aes(x = month, y = Frequency, fill = 학년)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste0(반_번호, "반 학생들의 월별 생일 빈도"), 
           x = "월", y = "학생 수", fill = "학년") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  )
  
  # ----------------- 2. 해당 반에서 남/여별 월별 생일 분포 -----------------
  gender_반_month_freq <- rdata_반 %>%
    group_by(학년, 성별, month) %>%
    summarise(Frequency = n(), .groups = "drop")
  
  # 월 정렬 (1~12월 순서 유지)
  gender_반_month_freq$month <- factor(gender_반_month_freq$month, levels = sprintf("%02d", 1:12))
  
  # 시각화: 해당 반에서 남/여별 월별 생일 분포
  print(
    ggplot(gender_반_month_freq, aes(x = month, y = Frequency, fill = 성별)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste0(반_번호, "반 학생들의 성별별 월별 생일 빈도"), 
           x = "월", y = "학생 수", fill = "성별") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")

    ####--------------------------------- 반복문 이용-프린트------------------------------------------- ####
    
    
    # 엑셀 데이터 불러오기
    rdata <- read_excel("rdata1.xlsx")
    
    # 학년, 반, 성별, 출신초를 범주형(factor)으로 변환
    rdata <- rdata %>%
      mutate(across(c(학년, 반, 성별, 출신초), as.factor))
    
    # 생년월일에서 월 추출
    rdata$month <- substr(rdata$생년월일, 6, 7)
    
    # 분석할 반 목록
    반_목록 <- c("3", "4", "5", "6", "7", "8")
    
    for (반_번호 in 반_목록) {
      # 해당 반 데이터 필터링
      rdata_반 <- rdata %>%
        filter(반 == 반_번호)
      
      # ----------------- 1. 해당 반 학생들의 월별 생일 분포 -----------------
      class_반_month_freq <- rdata_반 %>%
        group_by(학년, month) %>%
        summarise(Frequency = n(), .groups = "drop")
      
      # 월 정렬 (1~12월 순서 유지)
      class_반_month_freq$month <- factor(class_반_month_freq$month, levels = sprintf("%02d", 1:12))
      
      # ggplot 객체 저장 후 print()
      p1 <- ggplot(class_반_month_freq, aes(x = month, y = Frequency, fill = 학년)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste0(반_번호, "반 학생들의 월별 생일 빈도"), 
             x = "월", y = "학생 수", fill = "학년") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2")
      
      print(p1)  # 명시적으로 print() 실행
      
      # ----------------- 2. 해당 반에서 남/여별 월별 생일 분포 -----------------
      gender_반_month_freq <- rdata_반 %>%
        group_by(학년, 성별, month) %>%
        summarise(Frequency = n(), .groups = "drop")
      
      # 월 정렬 (1~12월 순서 유지)
      gender_반_month_freq$month <- factor(gender_반_month_freq$month, levels = sprintf("%02d", 1:12))
      
      # ggplot 객체 저장 후 print()
      p2 <- ggplot(gender_반_month_freq, aes(x = month, y = Frequency, fill = 성별)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste0(반_번호, "반 학생들의 성별별 월별 생일 빈도"), 
             x = "월", y = "학생 수", fill = "성별") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set1")
      
      print(p2)  # 명시적으로 print() 실행
    }
    
    
    
    