read.table("PLS_Corr.csv")
data = read.table("PLS_Corr.csv", sep = ",", header = TRUE)
data = read.table("PLS_Corr.csv", sep = ",", h = T)

library(Hmisc)
library(zoo)
library(ggplot2)
library(dplyr)
library(ggpubr)

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"))

ggplot(data=data, aes(x=Seed, y=CorrelationCONT_LV1, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationCONT_LV1-LowerBoundCONT_LV1, ymax=CorrelationCONT_LV1+UpperBoundCONT_LV1),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "CONT", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationSCD_LV1, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationSCD_LV1-LowerBoundSCD_LV1, ymax=CorrelationCONT_LV1+UpperBoundSCD_LV1),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "SCD+", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationMCI_LV1, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationMCI_LV1-LowerBoundMCI_LV1, ymax=CorrelationMCI_LV1+UpperBoundMCI_LV1),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "MCI", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationCONT_LV2, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationCONT_LV2-LowerBoundCONT_LV2, ymax=CorrelationCONT_LV2+UpperBoundCONT_LV2),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "CONT", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationSCD_LV2, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationSCD_LV2-LowerBoundSCD_LV2, ymax=CorrelationSCD_LV2+UpperBoundSCD_LV2),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "SCD+", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationMCI_LV2, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationMCI_LV2-LowerBoundMCI_LV2, ymax=CorrelationMCI_LV2+UpperBoundSCD_LV2),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "MCI", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationCONT_LV3, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationCONT_LV3-LowerBoundCONT_LV3, ymax=CorrelationCONT_LV3+UpperBoundCONT_LV3),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "CONT", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationSCD_LV3, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationSCD_LV3-LowerBoundSCD_LV3, ymax=CorrelationSCD_LV3+UpperBoundSCD_LV3),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "SCD+", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup

ggplot(data=data, aes(x=Seed, y=CorrelationMCI_LV3, color=Seed, fill=Seed))+
  geom_bar(stat="identity", position=position_dodge(), width=0.5, color = "black")+
  geom_errorbar(aes(ymin=CorrelationMCI_LV3-LowerBoundMCI_LV3, ymax=CorrelationMCI_LV3+UpperBoundMCI_LV3),
                width=.2, position=position_dodge(.9), color = "black")+
  labs(title = "MCI", y="Correlation")+
  scale_x_discrete(limits=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  scale_color_discrete(breaks=c("LeftHippocampus", "LeftSuperiorParietal","RightInferiorTemporal", "Performance"))+
  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ylim(-1.0, 1.0) +
  cleanup
  