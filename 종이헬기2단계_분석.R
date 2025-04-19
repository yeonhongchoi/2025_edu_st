library(dlookr)
#install.packages("readxl")
library(readxl)
library(tidyverse)
library(RHINO)
library(tm)
library(tidytext)
library(wordcloud2)

#### 1. 데이터 로딩 ####
rdata <- read_excel("종이헬리콥터_프로젝트_2단계.xlsx")%>% as_tibble()


#### 2. 텍스트 마이닝 ####
#library(RHINO)
initRhino()
#rdata$창의적요소[1:5] %>% sapply(getMorph, "NV") %>% sapply(paste, collapse=" ") %>% data.frame()
rdata$창의적요소[1:5] %>%
  sapply(getMorph, "NV") %>%
  sapply(paste, collapse = " ") %>%
  as.data.frame()


(data <- rdata %>% mutate(형태소=창의적요소 %>% sapply(getMorph, "NV") %>% sapply(paste, collapse=" ")) %>% 
    mutate_at(vars("형태소"), ~na_if(., "")))

#data <- data %>% mutate(형태소=gsub("때", "", 형태소)) %>% 
  #mutate(형태소=gsub("청년 다방", "청년다방", 형태소))


#### 가. dtm 만들기 ####
library(tm)
inspect(dtm <- data %>% filter(!is.na(형태소)) %>% pull(형태소) %>%       # 형태소 분석한결과 선택 
          VectorSource() %>% VCorpus() %>%                                # 말뭉치(쿠퍼스) 만들기
          DocumentTermMatrix(control = list(wordLengths=c(2, Inf))) %>%   # dtm 만들기
          removeSparseTerms(0.97))                                        # 희소 단어 삭제

#### 나. 워드 클라우드 만들기 ####
library(tidytext)
library(wordcloud2)
#t(dtm) %>% tidy() %>% group_by(term) %>% summarise(Freq=sum(count)) %>% arrange(desc(Freq)) %>% 
  #filter(term!="떡볶이") %>% head(200) %>% wordcloud2(size=0.4, color="random-dark")

t(dtm) %>% tidy() %>% group_by(term) %>% summarise(Freq=sum(count)) %>% arrange(desc(Freq)) %>% 
head(200) %>% wordcloud2(size=0.4, color="random-dark")



#data %>% filter(grepl("볶음밥", 좋았던곳이유개선점)) %>% select(좋았던곳이유개선점)



#### 다. 단어간 상관관계 ####
#findAssocs(dtm, "떡볶이", 0.3)
library(reshape2)
#findAssocs(dtm, "맛있", 0.01) %>% data.frame() %>% rownames_to_column("term") %>% 
  wordcloud2(size=0.3, color="random-dark")
  
  
findAssocs(dtm, "날개", 0.3)
#library(reshape2)
findAssocs(dtm, "자르", 0.01) %>% data.frame() %>% rownames_to_column("term") %>% 
  wordcloud2(size=0.3, color="random-dark")