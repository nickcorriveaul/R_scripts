#Données cognition
read.table("Donnees_article_encodage.csv")
data = read.table("Donnees_article_encodage.csv", sep = ",", header = TRUE)
data = read.table("Donnees_article_encodage.csv", sep = ",", h = T)

library(zoo)
library(ggplot2)
library(dplyr)
library(ggpubr)

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"))

#Left superior frontal gyrus
qplot(Time, Enc_LH_Interaction_Cluster1_Superiorfrontal, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Pars opercularis gauche
qplot(Time, Enc_LH_Interaction_Cluster2_Parsopercularis, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Pars orbitalis droit
qplot(Time, Enc_RH_Interaction_Cluster1_Parsorbitalis, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Gyrus supramarginal droit
qplot(Time, Enc_RH_Interaction_Cluster2_Supramarginal, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Hippocampe gauche
qplot(Time, Enc_LH_Hippocampus, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Hippocampe droit
qplot(Time, Enc_RH_Hippocampus, data=data, xlab = "Time", ylab = "BOLD Signal", group=Group, color=Group, geom = "smooth", method = "lm") +
  scale_x_continuous(breaks=c(1,2)) +
  theme(axis.text=element_text(size=20, face = "bold"), axis.title=element_text(size=20, face = "bold"), plot.title=element_text(size=20, face="bold"), 
        legend.title=element_text(size = 15, face = "bold"), legend.text=element_text(size = 15, face="bold")) +
  cleanup

#Scatterplot

scatter = ggplot(data, aes(T1_TimeToDiagnosis, Enc_LH_Interaction_Cluster2_Parsopercularis))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=20, face ="bold"), axis.title=element_text(size=20, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("o")+
  ylab("o") +
  cleanup

scatter = ggplot(data, aes(T1_TimeToDiagnosis, Enc_RH_Interaction_Cluster2_Supramarginal))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=20, face ="bold"), axis.title=element_text(size=20, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("o")+
  ylab("o") +
  cleanup

scatter = ggplot(data, aes(T1_TimeToDiagnosis, Enc_LH_Hippocampus))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=20, face ="bold"), axis.title=element_text(size=20, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("o")+
  ylab("o") +
  cleanup


data$Time <- factor(data$Time, 
                       levels = c(1, 2),
                       labels = c("Y0", "Y2"))

ggline(data=data, x = "Time", y = "Enc_LH_Interaction_Cluster1_Superiorfrontal", color = "Group",
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_LH_Interaction_Cluster2_Parsopercularis", color = "Group",
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_RH_Interaction_Cluster2_Supramarginal", color = "Group",
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_RH_Interaction_Cluster1_Parsorbitalis", color = "Group",
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_LH_Interaction_Cluster5_Occipital", color = "Group", palette = c("gray", "black"),
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_LH_Hippocampus", color = "Group",
       add = c("mean_se","dotplot"))
ggline(data=data, x = "Time", y = "Enc_RH_Hippocampus", color = "Group",
       add = c("mean_se","dotplot"))

ggline(data, x = "Time", y = "Enc_RH_Interaction_Cluster2_Supramarginal", color = "Group",
       add = c("mean_se", "jitter"))+
  labs(title="Right Supramarginal Gyrus", y="BOLD fMRI Signal")+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"))

ggline(data, x = "Time", y = "Enc_LH_Hippocampus", color = "Group",
       add = c("mean_se", "jitter"))+
  labs(title="Left hippocampus", y="BOLD fMRI Signal")+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"))

ggline(data, x = "Time", y = "Enc_LH_Interaction_Cluster2_Parsopercularis", color = "Group",
       add = c("mean_se", "jitter"))+
  labs(title="Left Pars Opercularis", y="BOLD fMRI Signal")+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"))

