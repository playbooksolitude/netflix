#23-0410 mon

#
getwd()
library(tidyverse)
library(showtext)
showtext_auto()
library(bbplot)


read_tsv("./files/most-popular_23-0516.tsv") -> nf_popular1

(read_csv("./files/most-popular_28week_23-0516.csv",
          skip = 0) -> netflix_rank28_nametable)

#ggplot
color2 = c("일반" = "black", 
           "대한민국 작품" = "red")

netflix_rank28_nametable |> group_by(kor_name) |> 
  summarise(sum = sum(hours_viewed_first_28_days),
            category) |> 
  arrange(desc(sum)) |> 
  mutate(original = ifelse(kor_name %in% c("오징어 게임",
                                               "더 글로리",
                                      "이상한 변호사 우영우",
                                      "지금 우리 학교는"),
                  "대한민국 작품", 
                  "일반"),
         rank = row_number(desc(sum))) -> netflix_rank28_nametable2


#geom_bar()
netflix_rank28_nametable2 |> 
  ggplot(aes(kor_name |> fct_reorder(sum), 
             sum/1000000,
             fill = original)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #facet_wrap(.~category, scales = "free") +
  scale_fill_manual(values = color2) +
  labs(title = "NETFLIX 역대 인기 순위",
       subtitle = "단위: 백만시간") +
  geom_text(aes(label = rank), hjust = -.5, size = 5) +
  bbc_style() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 


netflix_rank28_nametable2 |> 
  ggplot(aes(kor_name |> fct_reorder(sum), 
             sum/1000000,
             fill = original)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #facet_wrap(.~category, scales = "free") +
  scale_fill_manual(values = color2) +
  labs(title = "NETFLIX 역대 인기 순위",
       subtitle = "단위: 백만시간") +
  geom_text(aes(label = rank), hjust = -.5, size = 5,
            color = "black") +
  bbc_style() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  coord_polar() +
  ylim(-200, 2000) +
  theme_void()


#ggplot
netflix_rank28_nametable |> 
  ggplot(aes(kor_name |> fct_reorder(rank), 
             as.factor(rank))) + 
  geom_bar(stat = "identity") +
  facet_wrap(.~category, scales = "free") +
  coord_flip() +
  bbc_style() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 


#이름 변경
netflix_rank28_nametable |> group_by(kor_name) |> 
  summarise(sum = sum(hours_viewed_first_28_days),
            category) |> 
  arrange(desc(sum)) |> 
  mutate(original = ifelse(kor_name %in% 
                             c("오징어 게임",
                               "더 글로리",
                               "이상한 변호사 우영우",
                               "지금 우리 학교는"),
                           "대한민국 작품", 
                           "일반"),
         rank = row_number(desc(sum))) |> 
  ggplot(aes(kor_name |> fct_reorder(sum), 
             sum/1000000,
             fill = original)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = color2) +
  labs(title = "NETFLIX 역대 인기 순위",
       subtitle = "단위: 백만시간") +
  geom_text(aes(label = rank), hjust = -.5, size = 5) +
  bbc_style() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

#
# ----------------------------------------------------------
# arr_1csv |> 
#   count(항공사, 출발공항명, 구분, sort = T) |> 
#   filter(n > 90) |> 
#   mutate(num = row_number(), .before = 1) -> temp_d
# temp_d -> temp_l
# 
# angle2 <- 90-(360*(temp_l$num-.5)/nrow(temp_d))
# temp_l$hjust <- ifelse(angle2 < -90, 1, 0)
# temp_l$angle2 <- ifelse(angle2 < -90, angle2 + 180, angle2)


#
netflix_rank28_nametable2 |> 
  mutate(num = row_number(), .before = 1,
         sum2 = sum / 1000000) |> 
  filter(num < 21) -> temp_d

temp_d -> temp_l
temp_d

angle2 <- 90-(360*(temp_l$num-.5)/nrow(temp_d))
temp_l$hjust <- ifelse(angle2 < -90, 1, 0)
temp_l$angle2 <- ifelse(angle2 < -90, angle2 + 180, angle2)

#
ggplot(temp_d,
       aes(x = as.factor(num),
           y = sum2)) +
  geom_bar(stat = "identity",
           aes(fill = original)) +
  coord_polar(start = 0) +
  theme_void() +
  theme(legend.position = "top") +
  ylim(-200, 2000) +
  geom_text(data = temp_l,
            aes(x = num, y = sum2+1,
                label = kor_name,
                hjust = hjust),
            color = "black", size = 4,
            angle = temp_l$angle2, inherit.aes = F) +
  labs(title = "넷플릭스 역대 기록") +
  scale_fill_manual(values = color2)
  
  #scale_fill_nord("afternoon_prarie") +
  #scale_fill_discrete_qualitative("Harmonic")
  #scale_fill_discrete_qualitative("Dark2")
  
color2 = c("일반" = "black", 
           "대한민국 작품" = "red")
temp_d |> names()
