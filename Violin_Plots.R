read.table("cimaq_dataset.csv")
data = read.table("cimaq_dataset.csv", sep = ",", header = TRUE)
data = read.table("cimaq_dataset.csv", sep = ",", h = T)

library(Hmisc)
library(zoo)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(beeswarm)

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"))

ggplot(data, aes(x = Diagnostic_3, y = CS_Hippocampus_L, 
                 color = Diagnostic_3, fill = Diagnostic, alpha=1)) +
  geom_violin(trim=FALSE, position = position_dodge(0.8), color = "black") +
  geom_jitter(width=0.1, color = "Black", size = 2, alpha=1) +
  geom_boxplot(width=0.4, color = "Black", alpha=0.1) +
  scale_x_discrete(limits=c("CONT", "SCD+", "MCI"))+
  labs(title="Left hippocampus", y="BOLD fMRI Signal")+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none")+
  cleanup

ggplot(data, aes(x = Diagnostic_3, y = CS_Parietal_Sup_L_winsorized, 
                 color = Diagnostic_3, fill = Diagnostic, alpha=1)) +
  geom_violin(trim=FALSE, position = position_dodge(0.8), color = "black") +
  geom_jitter(width=0.1, color = "Black", size = 2, alpha=1) +
  geom_boxplot(width=0.4, color = "Black", alpha=0.1) +
  scale_x_discrete(limits=c("CONT", "SCD+", "MCI"))+
  labs(title="Left hippocampus", y="BOLD fMRI Signal")+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none")+
  cleanup

ggplot(data, aes(x = Diagnostic_3, y = CS_Temporal_Inf_R, 
                 color = Diagnostic_3, fill = Diagnostic, alpha=1)) +
  geom_violin(trim=FALSE, position = position_dodge(0.8), color = "black") +
  geom_jitter(width=0.1, color = "Black", size = 2, alpha=1) +
  geom_boxplot(width=0.4, color = "Black", alpha=0.1) +
  scale_x_discrete(limits=c("CONT", "SCD+", "MCI"))+
  labs(title="Left hippocampus", y="BOLD fMRI Signal")+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none")+
  cleanup
