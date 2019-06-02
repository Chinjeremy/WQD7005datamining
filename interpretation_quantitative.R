twentyfour=read.csv('22_4.csv')
twentyfour=as.data.frame(twentyfour)
twentyfour=twentyfour%>%select(board,perc_change)%>%group_by(board)%>%summarise(mean(perc_change))

graph0= ggplot(twentyfour, aes(x =twentyfour$board , y =twentyfour$`mean(perc_change)` , fill = twentyfour$board)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

graph0 +labs(x = "Stock Board")+labs(y='% Change')+labs(caption = "(based on crawled data from KLSE)")+
  labs(title = "22 April 2019")+
  geom_text(aes(label = twentyfour$`mean(perc_change)`),
            hjust = 1.5,
            color = "white",
            size = 3) +
  theme_classic()

twentythree=read.csv('23_4.csv')
twentythree=as.data.frame(twentythree)
twentythree=twentythree%>%select(board,perc_change)%>%group_by(board)%>%summarise(mean(perc_change))

graph1= ggplot(twentythree, aes(x =twentythree$board , y =twentythree$`mean(perc_change)` , fill = twentythree$board)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

graph1 +labs(x = "Stock Board")+labs(y='% Change')+labs(caption = "(based on crawled data from KLSE)")+
  labs(title = "23 April 2019")+
  geom_text(aes(label = twentythree$`mean(perc_change)`),
            hjust = 1.5,
            color = "white",
            size = 3) +
  theme_classic()


twentyfourindex=read.csv('22_4_index.csv')
indexplot=twentyfourindex%>%select(Symbol,X.Chg)

twentythreeindex=read.csv('23_4_index.csv')
indexplot2=twentythreeindex%>%select(Symbol,X.Chg)

graph= ggplot(indexplot, aes(x =indexplot$Symbol , y =indexplot$X.Chg , fill = indexplot$Symbol)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

graph +labs(x = "Index Symbol")+labs(y='% Change')+labs(caption = "(based on crawled data from consumer products index)")+
  labs(title = "22 April 2019")+
  geom_text(aes(label = indexplot$X.Chg),
            hjust = 1.5,
            color = "white",
            size = 3) +
  theme_classic()

graph2= ggplot(indexplot2, aes(x =indexplot2$Symbol , y =indexplot2$X.Chg , fill = indexplot2$Symbol)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

graph2 +labs(x = "Index Symbol")+labs(y='% Change')+labs(caption = "(based on crawled data from consumer products index)")+
  labs(title = "23 April 2019")+
  geom_text(aes(label = indexplot$X.Chg),
            hjust = 1.5,
            color = "white",
            size = 3) +
  theme_classic()

by_cyl <- mtcars %>% group_by(cyl)
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))