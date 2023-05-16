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
                                      "이상한 변호사 우영우"),
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
  mutate(original = ifelse(kor_name %in% c("오징어 게임",
                                           "더 글로리",
                                           "이상한 변호사 우영우"),
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