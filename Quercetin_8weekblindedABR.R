library(tidyverse)

library(readxl)
Quercetin_8weekABR <- read_excel("Blinded Hearing Outcomes Quercetin.xlsx", 
                                 sheet = "8 week ABR")

ggplot()+
  geom_line(data = Quercetin_8weekABR, linetype="dashed", size = 0.8, mapping =  aes(x=freq, y=grp6, color="red"))+
  geom_point(data = Quercetin_8weekABR, shape = 16, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp6, color="red"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp6lower, ymax=grp6upper, color="red"), width=0.25)+
  geom_line(data = Quercetin_8weekABR, size = 0.8, mapping = aes(x=freq, y=grp1, color="black"))+
  geom_point(data = Quercetin_8weekABR, shape = 0, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp1, color="black"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp1lower, ymax=grp1upper, color = "black"), width=0.25)+
  geom_line(data = Quercetin_8weekABR, size = 0.8, mapping = aes(x=freq, y=grp4, color="green4"))+
  geom_point(data = Quercetin_8weekABR, shape = 1, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp4, color="green4"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp4lower, ymax=grp4upper, color="green4"), width=0.25)+
  geom_line(data = Quercetin_8weekABR, size = 0.8, mapping = aes(x=freq, y=grp3, color="blue"))+
  geom_point(data = Quercetin_8weekABR, shape = 2, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp3, color="blue"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp3lower, ymax=grp3upper, color="blue"), width=0.25)+
  geom_line(data = Quercetin_8weekABR, size = 0.8, linetype="dashed", mapping = aes(x=freq, y=grp5, color="orange"))+
  geom_point(data = Quercetin_8weekABR, shape = 15, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp5, color="orange"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp5lower, ymax=grp5upper, color="orange"), width=0.25)+
  geom_line(data = Quercetin_8weekABR, size = 0.8, mapping = aes(x=freq, y=grp2, color="purple"))+
  geom_point(data = Quercetin_8weekABR, shape = 5, size = 2.5, stroke = 1.2, mapping =  aes(x=freq, y=grp2, color="purple"))+
  geom_errorbar(data = Quercetin_8weekABR, size=0.8, mapping = aes(x=freq, ymin=grp2lower, ymax=grp2upper, color="purple"), width=0.25)+
  labs(x="Tone Frequency (kHz)", y="ABR Threshold (dB SPL)", title = "8 week ABR")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_identity(name = element_blank(),
                       breaks = c("black", "purple", "blue", "green4", "orange", "red"),
                       labels = c("CMV+GCV+QP188, n=16", "CMV+GCV, n=10", "CMV+QP188, n=8", "CMV+P188, n=8", "Saline+GCV+QP188, n=7", "Saline+P188, n=6"),
                       guide = "legend")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80))+
  scale_x_continuous(breaks = c(8, 16, 32))+
  theme_classic()+
  guides(color = guide_legend(
    override.aes=list(shape = c(0,5,2,1,15,16))))+
  theme(axis.text.x=element_text(size=15), axis.title=element_text(size=18))+
  theme(axis.text.y=element_text(size=15), axis.title=element_text(size=18))

Quercetin_8weekABR$kwallisgroup <- as.factor(Quercetin_8weekABR$kwallisgroup)
Quercetin_8weekABR$kwallisgroup = factor(Quercetin_8weekABR$kwallisgroup, labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6"))

class(Quercetin_8weekABR$kwallisgroup)

kruskal.test(kwallisthreshold ~ kwallisgroup, data = Quercetin_8weekABR)

library(FSA)

dunnTest(kwallisthreshold ~ kwallisgroup, data=Quercetin_8weekABR, method="bonferroni")
