library(ggplot2)

df <- as.data.frame(HairEyeColor[,,'Female'])
names(df)
ggplot(df, aes(x = Hair, y=Freq, fill = Eye))+
  geom_bar(stat = 'identity', position='dodge')+
  scale_fill_manual(values=c('Brown', 'Blue', 'Darkgrey', 'Darkgreen'))
