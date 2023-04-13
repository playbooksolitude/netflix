#23-0410 mon

#
getwd()

read_tsv("./files/most-popular_23-0410.tsv") -> nf_popular1

nf_popular1
nf_popular1 |> group_by(show_title, season_title) |> 
  summarise(sum = sum(hours_viewed_first_28_days)) |> 
  arrange(desc(show_title),desc(sum)) 


#ggplot
color2 = c("일반" = "black", 
           "대한민국 작품" = "red")
nf_popular1 |> group_by(season_title) |> 
  summarise(sum = sum(hours_viewed_first_28_days)) |> 
  arrange(desc(sum)) |> filter(season_title != "N/A") |> 
  mutate(original = ifelse(season_title %in% c("The Glory: Season 1",
                                               "Squid Game: Season 1",
                                      "Extraordinary Attorney Woo: Season 1"),
                  "대한민국 작품", 
                  "일반"),
         rank = row_number(desc(sum))) |> 
  ggplot(aes(season_title |> fct_reorder(sum), 
             sum/1000000,
             fill = original)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = color2) +
  labs(title = "NETFLIX 역대 인기 순위",
       subtitle = "단위: 백만시간") +
  geom_text(aes(label = rank), hjust = -.5, size = 5) +
  bbc_style() #+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) 

#ggplot
nf_popular1 |> group_by(season_title) |> 
  summarise(sum = sum(hours_viewed_first_28_days)) |> 
  arrange(desc(sum)) |> filter(season_title != "N/A") |> 
  ggplot(aes(season_title |> fct_reorder(sum), 
             sum/1000000)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  bbc_style()
