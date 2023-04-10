#23-0331 fri 16:49

#
#install.packages('devtools')
#devtools::install_github('bbc/bbplot')

library(tidyverse)
library(lubridate)
library(bbplot)
library(showtext)
showtext_auto()

#불러오기
  #netflix data
(read_tsv("~/Documents/netflix/files/all-weeks-countries_23-0407.tsv") -> netflix1)

  #영문 -> 한글 변환
(read_csv("./files/netflix_rank1_nametable.csv",
         skip = 1) |> select(2,3,4) -> netflix_nametable)
#netflix_nametable |> view()

  #한국만 데이터셋 분리
(netflix1 |> filter(
  country_iso2 %in% c("KR")
) -> netflix2_kor)

  #한국 & TV만 분리
(netflix2_kor |> 
  filter(category %in% c("TV")) -> netflix2_kor_tv)
netflix2_kor_tv #920개 TV프로그램

###날짜 #2021-07-04 ~ #2023-04-02
  #table(netflix2_kor_tv$week) 
  #table(netflix_0331$country_name)  #South Korea

###check
  #Films 920 #TV 920
  # netflix2_kor |> group_by(week) |> summarise(n = n())
  # netflix2_kor |> filter(week == "2023-04-02")

###dataset 
  #rank 1등만  #tv
(netflix2_kor_tv |> 
  mutate(
  year = year(week),
  month = month(week),
  day = day(week)
) |> 
  filter(weekly_rank == "1") -> netflix3_tv_rank1)

##### mapping table
  ### netflix_nametable3                                      ### 한국 이름                          
colnames(netflix_nametable) <- c("kor_name", "eng_name", "original")

    #tidyr separate
(netflix_nametable |> 
  separate(eng_name, 
           into = c("eng_name"), sep = "\n\n") -> netflix_nametable2)

    #tidyr separate
      # 환혼 :part1 #공동경제구역 :: 두번 입력
(netflix_nametable2 |> separate(eng_name, 
                               into = c("eng_name"), 
                               sep = ": Se") -> netflix_nametable3)

######name table join  
(left_join(netflix3_tv_rank1,
          netflix_nametable3,
          by = c("show_title" = "eng_name")) -> netflix4_TV_name)

    ##### ggplot ##### --------------------------------------------------------- 한국 1위
netflix4_TV_name |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week))) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity") +
  labs(title = "NETFLIX 한국 1위 TV 부문",
       subtitle = "2021.07.04 ~ 2023.04.02",
       y = "show_title") + bbc_style() +
  geom_line()                                   #group = 1


#오리지널 표시
###original color
scale_colour_manual(color)
color = c(일반 = "black",
          오리지널 = "red")

netflix4_TV_name |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week),
             color = original)) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity", show.legend = F, size = 2) +
  labs(title = "NETFLIX 한국 1위 TV 부문",
       subtitle = "2021.07.04 ~ 2023.04.02",
       y = "show_title") + 
  bbc_style() +
  scale_color_manual(values = color) +
  geom_line(show.legend = F)                                   #group = 1

#면분할
netflix4_TV_name |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week),
             color = original)) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity", show.legend = F) +
  labs(title = "NETFLIX TV 부문 한국 1위",
       subtitle = "2021.07.04 ~ 2023.04.02",
       y = "show_title") + 
#  bbc_style() +
  facet_wrap(.~year, nrow = 2) +
  scale_color_manual(values = color) +
  geom_line(show.legend = F)                                   #group = 1

#2022
netflix4_TV_name |> filter(year == "2022") |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week),
             color = original)) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity", show.legend = F, size = 3) +
  labs(title = "NETFLIX TV 부문 한국 1위",
       subtitle = "2022년 1월 ~ 12월",
       y = "show_title") + 
  bbc_style() +
  facet_wrap(.~year, nrow = 2) +
  scale_color_manual(values = color) +
  geom_line(show.legend = F)  

#2023
netflix4_TV_name |> filter(year == "2023") |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week),
             color = original)) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity", show.legend = F, size = 3) +
  labs(title = "NETFLIX TV 부문 한국 1위",
       subtitle = "2022년 1월 ~ 12월",
       y = "show_title") + 
  bbc_style() +
  facet_wrap(.~year, nrow = 2) +
  scale_color_manual(values = color) +
  geom_line(show.legend = F)  

######### show_title()
### netflix_5_n # 1위 점유주
(netflix4_TV_name |> group_by(show_title) |> 
    mutate(
      count = n()
    ) -> netflix5_n)


    #ggplot 최다 1위 정렬
ggplot(data = netflix5_n, 
       aes(x = kor_name |> fct_infreq() |> fct_rev(),
           y = stat(count))) +
    geom_bar(stat = "count", aes(fill = original)) +
  scale_fill_manual(values = color) +
  coord_flip() +
  labs(x = "title") +
  bbc_style() +
  geom_label(aes(label = stat(count)), stat = "count", size = 5) +
  scale_y_continuous(breaks = seq(1,10,1))   # y축 값이 1씩 나오도록 

#original vs normal #전체
netflix4_TV_name |> 
  ggplot(aes(x = original)) +
  geom_bar(stat = "count", aes(fill = original), show.legend = F) +
  scale_fill_manual(values = color) +
  geom_label(aes(label = stat(count)), stat = "count", size = 10) +
  labs(title = "NETFLIX 한국 1위 TV 오리지널 콘텐츠 건수",
       subtitle = "2021.07.04 ~ 2023.04.02") +
  bbc_style()

#면분할
netflix4_TV_name |> 
  ggplot(aes(x = original)) +
  geom_bar(stat = "count", aes(fill = original), show.legend = F) +
  scale_fill_manual(values = color) +
  geom_label(aes(label = stat(count)), stat = "count", size = 10) +
  facet_wrap(.~year) +
  bbc_style()

  # 피지컬: 100
netflix2_kor_tv |> filter(show_title %in% c("Physical: 100")) |> 
  ggplot(aes(x = weekly_rank |> as.factor())) + geom_bar(stat = "count")


#
(netflix2_kor_tv |> 
    mutate(
      year = year(week),
      month = month(week),
      day = day(week)
    ) |> 
    filter(weekly_rank %in% c("1", "2", "3")) -> netflix3_tv_rank123)

netflix3_tv_rank123
(left_join(netflix3_tv_rank123,
           netflix_nametable3,
           by = c("show_title" = "eng_name")) -> netflix4_TV_name123)


