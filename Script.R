library(readxl)
Df <- read_excel("Dataset.xlsx")
View(Df)
names(Df)

library(tidyverse)
library(blandr)
library(ggpubr)
library(rstatix)
library(rcompanion)

## convert Stride length from cm to m
Df <- Df %>% mutate(OG_SL = OG_SL / 100, TM_SL = TM_SL / 100)

##DATA SUBSETS
Df_Track <- Df %>% filter(Group == "TRACK" )
Df_Track_Males <- Df %>% filter(Group == "TRACK", Gender =="M" )
Df_Track_Females <- Df %>% filter(Group == "TRACK", Gender =="F" )

Df_Recreational <- Df %>% filter(Group == "RECREATIONAL" )
Df_Recreational_Males <- Df %>% filter(Group == "RECREATIONAL", Gender =="M" )
Df_Recreational_Females <- Df %>% filter(Group == "TRACK", Gender =="F" )

## Set Factors
Df$Gender <- as.factor(Df$Gender)
Df$Group <- as.factor(Df$Group)

#####################AGREEGMENT ANALYSIS ##################################

##TRACK GROUP BA

BA_speed_track <- blandr.draw(Df_Track$OG_Speed, Df_Track$TM_Speed, ciDisplay = FALSE , ciShading = FALSE,
                        plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                        plotTitle = "A) Speed") +
  theme_classic()


BA_CT_track <- blandr.draw(Df_Track$OG_CT, Df_Track$TM_CT, ciDisplay = FALSE , ciShading = FALSE,
                     plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                     plotTitle = "B) Contact Time") +
  theme_classic()

BA_FT_track <- blandr.draw(Df_Track$OG_TF, Df_Track$TM_TF, ciDisplay = FALSE , ciShading = FALSE,
                     plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                     plotTitle = "C) Flight Time") +
  theme_classic()

BA_SL_track <- blandr.draw(Df_Track$OG_SL, Df_Track$TM_SL, ciDisplay = FALSE , ciShading = FALSE,
                    plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                    plotTitle = "D) Stride Length") +
  theme_classic()

BA_SF_track <- blandr.draw(Df_Track$OG_SF, Df_Track$TM_SF, ciDisplay = FALSE , ciShading = FALSE,
                    plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                    plotTitle = "E) Stride Frequency") +
  theme_classic()


BA_track <- ggarrange(BA_speed_track, BA_CT_track,BA_FT_track,BA_SL_track,
                      BA_SF_track)
BA_track
ggsave("BA_track.png")


## Recreational Group BA

BA_speed_Recreational <- blandr.draw(Df_Recreational$OG_Speed, Df_Recreational$TM_Speed, ciDisplay = FALSE , ciShading = FALSE,
                        plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                        plotTitle = "A) Speed") +
  theme_classic()

BA_CT_Recreational <- blandr.draw(Df_Recreational$OG_CT, Df_Recreational$TM_CT, ciDisplay = FALSE , ciShading = FALSE,
                     plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                     plotTitle = "B) Contact Time") +
  theme_classic()

BA_FT_Recreational <- blandr.draw(Df_Recreational$OG_TF, Df_Recreational$TM_TF, ciDisplay = FALSE , ciShading = FALSE,
                     plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                     plotTitle = "C) Flight Time") +
  theme_classic()


BA_SL_Recreational<- blandr.draw(Df_Recreational$OG_SL, Df_Recreational$TM_SL, ciDisplay = FALSE , ciShading = FALSE,
                    plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                    plotTitle = "D) Stride Length") +
  theme_classic()


BA_SF_Recreational <- blandr.draw(Df_Recreational$OG_SF, Df_Recreational$TM_SF, ciDisplay = FALSE , ciShading = FALSE,
                    plotProportionalBias = TRUE, plotProportionalBias.se = TRUE,
                    plotTitle = "E) Stride Frequency") +
  theme_classic()

BA_Recreational <- ggarrange(BA_speed_Recreational,
  BA_CT_Recreational,BA_FT_Recreational,BA_SL_Recreational,BA_SF_Recreational)
BA_Recreational
ggsave("BA_Recreational.png")


###################CORRELATION and REGRESSION ANALYSIS ######################

## Speed by training status: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_Speed", data = Df_Recreational)
sd(Df_Recreational$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Recreational)
sd(Df_Recreational$OG_Speed)

Df_Recreational %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Recreational <- lm(TM_Speed ~ OG_Speed, data=Df_Recreational)
summary(lm_Speed_Recreational)

SC_Speed_Recreational <- ggplot(data=Df_Recreational, aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)") +
  geom_point() +
  stat_regline_equation(label.x=6, label.y=9.5) + theme_classic()

groupwiseMean(var = "TM_Speed", data = Df_Track)
sd(Df_Track$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Track)
sd(Df_Track$OG_Speed)

Df_Track %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Trackl <- lm(TM_Speed ~ OG_Speed, data=Df_Track)
summary(lm_Speed_Trackl)

SC_Speed_Track <- ggplot(data=Df_Track, aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)")  +
  geom_point() +
  stat_regline_equation(label.x=7.6, label.y=10.7) + theme_classic()

## Contact Time by training status: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_CT", data = Df_Recreational)
sd(Df_Recreational$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Recreational)
sd(Df_Recreational$OG_CT)

Df_Recreational %>% cor_test(OG_CT,TM_CT)

lm_CT_Recreational <- lm(TM_CT ~ OG_CT, data=Df_Recreational)
summary(lm_CT_Recreational)

SC_CT_Recreational <- ggplot(data=Df_Recreational, aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.12, label.y=0.2) + theme_classic()

groupwiseMean(var = "TM_CT", data = Df_Track)
sd(Df_Track$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Track)
sd(Df_Track$OG_CT)

Df_Track %>% cor_test(OG_CT,TM_CT)

lm_CT_Track <- lm(TM_CT ~ OG_CT, data=Df_Track)
summary(lm_CT_Track)

SC_CT_Track <- ggplot(data=Df_Track, aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.075, label.y=0.165) + theme_classic()

## Flight Time by training status: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_TF", data = Df_Recreational)
sd(Df_Recreational$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Recreational)
sd(Df_Recreational$OG_TF)

Df_Recreational %>% cor_test(OG_TF,TM_TF)

SC_FT_Recreational <- ggplot(data=Df_Recreational, aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.09, label.y=1.0) + theme_classic()

lm_FT_Recreational <- lm(TM_TF ~ OG_TF, data=Df_Recreational)
summary(lm_FT_Recreational)

groupwiseMean(var = "TM_TF", data = Df_Track)
sd(Df_Track$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Track)
sd(Df_Track$OG_TF)

Df_Track %>% cor_test(OG_TF,TM_TF)

lm_FT_Track <- lm(TM_TF ~ OG_TF, data=Df_Track)
summary(lm_FT_Track)

SC_FT_Track <- ggplot(data=Df_Track, aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.10, label.y=0.24) + theme_classic()

## Stride Length by training status: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SL", data = Df_Recreational)
sd(Df_Recreational$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Recreational)
sd(Df_Recreational$OG_SL)

Df_Recreational %>% cor_test(OG_SL,TM_SL)

lm_SL_Recreational <- lm(TM_SL ~ OG_SL, data=Df_Recreational)
summary(lm_SL_Recreational)

SC_SL_Recreational <- ggplot(data=Df_Recreational, aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=1.65, label.y=5) + theme_classic()

groupwiseMean(var = "TM_SL", data = Df_Track)
sd(Df_Track$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Track)
sd(Df_Track$OG_SL)

Df_Track %>% cor_test(OG_SL,TM_SL)

lm_SL_Track <- lm(TM_SL ~ OG_SL, data=Df_Track)
summary(lm_SL_Track)

SC_SL_Track <- ggplot(data=Df_Track, aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=1.9, label.y=3) + theme_classic()

## Stride Frequency by training status: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SF", data = Df_Recreational)
sd(Df_Recreational$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Recreational)
sd(Df_Recreational$OG_SF)

Df_Recreational %>% cor_test(OG_SF,TM_SF)

lm_SF_Recreational <- lm(TM_SF ~ OG_SF, data=Df_Recreational)
summary(lm_SF_Recreational)

SC_SF_Recreational <- ggplot(data=Df_Recreational, aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=3.4, label.y=4.85) + theme_classic()

groupwiseMean(var = "TM_SF", data = Df_Track)
sd(Df_Track$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Track)
sd(Df_Track$OG_SF)

Df_Track %>% cor_test(OG_SF,TM_SF)

lm_SF_Track <- lm(TM_SF ~ OG_SF, data=Df_Track)
summary(lm_SF_Track)

SC_SF_Track <- ggplot(data=Df_Track, aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=3.4, label.y=5.5) + theme_classic()


SC_group <- ggarrange(SC_Speed_Recreational,SC_Speed_Track,SC_CT_Recreational,SC_CT_Track,
          SC_FT_Recreational,SC_FT_Track,SC_SL_Recreational,
          SC_SL_Track,SC_SF_Recreational,SC_SF_Track, ncol = 2, nrow = 5)
ggsave("SC_group.png")
################################By Sex Recreational group
## Speed by Sex: Mean SD 95 CI and correlation

groupwiseMean(var = "TM_Speed", data = Df_Recreational_Females)
sd(Df_Recreational_Females$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Recreational_Females)
sd(Df_Recreational_Females$OG_Speed)

Df_Recreational_Females %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Recreational_Females <- lm(TM_Speed ~ OG_Speed, data=Df_Recreational_Females)
summary(lm_Speed_Recreational_Females)

SC_Speed_Recreational_Females <- ggplot(data=Df_Recreational_Females, 
                                        aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)") +
  geom_point() +
  stat_regline_equation(label.x=7.7, label.y=10) + theme_classic()

groupwiseMean(var = "TM_Speed", data = Df_Recreational_Males)
sd(Df_Recreational_Males$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Recreational_Males)
sd(Df_Recreational_Males$OG_Speed)

Df_Recreational_Males %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Recreational_Males <- lm(TM_Speed ~ OG_Speed, data=Df_Recreational_Males)
summary(lm_Speed_Recreational_Males)

SC_Speed_Recreational_Males <- ggplot(data=Df_Recreational_Males, 
                                        aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)") +
  geom_point() +
  stat_regline_equation(label.x=7.6, label.y=9.5) + theme_classic()

## Contact Time by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_CT", data = Df_Recreational_Females)
sd(Df_Recreational_Females$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Recreational_Females)
sd(Df_Recreational_Females$OG_CT)

Df_Recreational_Females %>% cor_test(OG_CT,TM_CT)

lm_CT_Recreational_Females <- lm(TM_CT ~ OG_CT, data=Df_Recreational_Females)
summary(lm_CT_Recreational_Females)

SC_CT_Recreational_Females <- ggplot(data=Df_Recreational_Females, 
                                     aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.075, label.y=0.165) + theme_classic()

groupwiseMean(var = "TM_CT", data = Df_Recreational_Males)
sd(Df_Recreational_Males$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Recreational_Males)
sd(Df_Recreational_Males$OG_CT)

Df_Recreational_Males %>% cor_test(OG_CT,TM_CT)

lm_CT_Recreational_Males <- lm(TM_CT ~ OG_CT, data=Df_Recreational_Males)
summary(lm_CT_Recreational_Males)

SC_CT_Recreational_Males <- ggplot(data=Df_Recreational_Males, 
                                     aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.12, label.y=0.195) + theme_classic()

## Flight Time by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_TF", data = Df_Recreational_Females)
sd(Df_Recreational_Females$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Recreational_Females)
sd(Df_Recreational_Females$OG_TF)

Df_Recreational_Females %>% cor_test(OG_TF,TM_TF)

lm_FT_Recreational_Females <- lm(TM_TF ~ OG_TF, data=Df_Recreational_Females)
summary(lm_FT_Recreational_Females)

SC_FT_Recreational_Females <- ggplot(data=Df_Recreational_Females, 
                             aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.1, label.y=0.25) + theme_classic()

groupwiseMean(var = "TM_TF", data = Df_Recreational_Males)
sd(Df_Recreational_Males$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Recreational_Males)
sd(Df_Recreational_Males$OG_TF)

Df_Recreational_Males %>% cor_test(OG_TF,TM_TF)

lm_FT_Recreational_Males <- lm(TM_TF ~ OG_TF, data=Df_Recreational_Males)
summary(lm_FT_Recreational_Males)

SC_FT_Recreational_Males <- ggplot(data=Df_Recreational_Males, 
                                     aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.09, label.y=0.95) + theme_classic()

## Stride Length by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SL", data = Df_Recreational_Females)
sd(Df_Recreational_Females$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Recreational_Females)
sd(Df_Recreational_Females$OG_SL)

Df_Recreational_Females %>% cor_test(OG_SL,TM_SL)

lm_SL_Recreational_Females <- lm(TM_SL ~ OG_SL, data=Df_Recreational_Females)
summary(lm_SL_Recreational_Females)

SC_SL_Recreational_Females <- ggplot(data=Df_Recreational_Females, 
                                    aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=1.9, label.y=3.1) + theme_classic()

groupwiseMean(var = "TM_SL", data = Df_Recreational_Males)
sd(Df_Recreational_Males$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Recreational_Males)
sd(Df_Recreational_Males$OG_SL)

Df_Recreational_Males %>% cor_test(OG_SL,TM_SL)

lm_SL_Recreational_Males <- lm(TM_SL ~ OG_SL, data=Df_Recreational_Males)
summary(lm_SL_Recreational_Males)

SC_SL_Recreational_Males <- ggplot(data=Df_Recreational_Males, 
                                    aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=1.8, label.y=3.3) + theme_classic()

## Stride Frequency by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SF", data = Df_Recreational_Females)
sd(Df_Recreational_Females$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Recreational_Females)
sd(Df_Recreational_Females$OG_SF)

Df_Recreational_Females %>% cor_test(OG_SF,TM_SF)

lm_SF_Recreational_Females <- lm(TM_SF ~ OG_SF, data=Df_Recreational_Females)
summary(lm_SF_Recreational_Females)

SC_SF_Recreational_Females <- ggplot(data=Df_Recreational_Females, 
                                     aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=3.4, label.y=5.5) + theme_classic()

groupwiseMean(var = "TM_SF", data = Df_Recreational_Males)
sd(Df_Recreational_Males$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Recreational_Males)
sd(Df_Recreational_Males$OG_SF)

Df_Recreational_Males %>% cor_test(OG_SF,TM_SF)

lm_SF_Recreational_Males <- lm(TM_SF ~ OG_SF, data=Df_Recreational_Males)
summary(lm_SF_Recreational_Males)

SC_SF_Recreational_Males <- ggplot(data=Df_Recreational_Males, 
                                     aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=3.85, label.y=5.1) + theme_classic()

SC_bySex_Recreational <- ggarrange(SC_Speed_Recreational_Females,SC_Speed_Recreational_Males,
                      SC_CT_Recreational_Females,SC_CT_Recreational_Males,
                      SC_FT_Recreational_Females,SC_FT_Recreational_Males,
                      SC_SL_Recreational_Females,SC_SL_Recreational_Males,
                      SC_SF_Recreational_Females,SC_SF_Recreational_Males,
                      ncol = 2, nrow = 5)
ggsave("SC_bySex_Recreational.png")

################################By Sex TRACK group
## Speed by Sex: Mean SD 95 CI and correlation

groupwiseMean(var = "TM_Speed", data = Df_Track_Females)
sd(Df_Track_Females$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Track_Females)
sd(Df_Track_Females$OG_Speed)

Df_Track_Females %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Track_Females <- lm(TM_Speed ~ OG_Speed, data=Df_Track_Females)
summary(lm_Speed_Track_Females)

SC_Speed_Track_Females <- ggplot(data=Df_Track_Females, 
                                        aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)") +
  geom_point() +
  stat_regline_equation(label.x=7.7, label.y=10) + theme_classic()

groupwiseMean(var = "TM_Speed", data = Df_Track_Males)
sd(Df_Track_Males$TM_Speed)

groupwiseMean(var = "OG_Speed", data = Df_Track_Males)
sd(Df_Track_Males$OG_Speed)

Df_Track_Males %>% cor_test(OG_Speed,TM_Speed)

lm_Speed_Track_Males <- lm(TM_Speed ~ OG_Speed, data=Df_Track_Males)
summary(lm_Speed_Track_Males)

SC_Speed_Track_Males <- ggplot(data=Df_Track_Males, 
                                 aes(x=OG_Speed, y=TM_Speed)) +
  geom_smooth(method="lm") + labs(x = "OG Speed (m/s)",
                                  y = "TR Speed (m/s)") +
  geom_point() +
  stat_regline_equation(label.x=8.7, label.y=11) + theme_classic()

## Contact Time by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_CT", data = Df_Track_Females)
sd(Df_Track_Females$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Track_Females)
sd(Df_Track_Females$OG_CT)

Df_Track_Females %>% cor_test(OG_CT,TM_CT)

lm_CT_Track_Females <- lm(TM_CT ~ OG_CT, data=Df_Track_Females)
summary(lm_CT_Track_Females)

SC_CT_Track_Females <- ggplot(data=Df_Track_Females, 
                                     aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.075, label.y=0.165) + theme_classic()

groupwiseMean(var = "TM_CT", data = Df_Track_Males)
sd(Df_Track_Males$TM_CT)

groupwiseMean(var = "OG_CT", data = Df_Track_Males)
sd(Df_Track_Males$OG_CT)

Df_Track_Males %>% cor_test(OG_CT,TM_CT)

lm_CT_Track_Males <- lm(TM_CT ~ OG_CT, data=Df_Track_Males)
summary(lm_CT_Track_Males)

SC_CT_Track_Males <- ggplot(data=Df_Track_Males, 
                              aes(x=OG_CT, y=TM_CT)) +
  geom_smooth(method="lm") + labs(x = "OG CT (s)",
                                  y = "TR CT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.115, label.y=0.165) + theme_classic()

## Flight Time by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_TF", data = Df_Track_Females)
sd(Df_Track_Females$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Track_Females)
sd(Df_Track_Females$OG_TF)

Df_Track_Females %>% cor_test(OG_TF,TM_TF)

lm_FT_Track_Females <- lm(TM_TF ~ OG_TF, data=Df_Track_Females)
summary(lm_FT_Track_Females)

SC_FT_Track_Females <- ggplot(data=Df_Track_Females, 
                                     aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.1, label.y=0.25) + theme_classic()

groupwiseMean(var = "TM_TF", data = Df_Track_Males)
sd(Df_Track_Males$TM_TF)

groupwiseMean(var = "OG_TF", data = Df_Track_Males)
sd(Df_Track_Males$OG_TF)

Df_Track_Males %>% cor_test(OG_TF,TM_TF)

lm_CT_Track_Males <- lm(TM_TF ~ OG_TF, data=Df_Track_Males)
summary(lm_CT_Track_Males)

SC_FT_Track_Males <- ggplot(data=Df_Track_Males, 
                              aes(x=OG_TF, y=TM_TF)) +
  geom_smooth(method="lm") + labs(x = "OG FT (s)",
                                  y = "TR FT (s)") +
  geom_point() +
  stat_regline_equation(label.x=0.1, label.y=0.19) + theme_classic()

## Stride Length by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SL", data = Df_Track_Females)
sd(Df_Track_Females$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Track_Females)
sd(Df_Track_Females$OG_SL)

Df_Track_Females %>% cor_test(OG_SL,TM_SL)

lm_SL_Track_Females <- lm(TM_SL ~ OG_SL, data=Df_Track_Females)
summary(lm_FT_Track_Females)

SC_SL_Track_Females <- ggplot(data=Df_Track_Females, 
                                     aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=1.9, label.y=3.1) + theme_classic()

groupwiseMean(var = "TM_SL", data = Df_Track_Males)
sd(Df_Track_Males$TM_SL)

groupwiseMean(var = "OG_SL", data = Df_Track_Males)
sd(Df_Track_Males$OG_SL)

Df_Track_Males %>% cor_test(OG_SL,TM_SL)

lm_SL_Track_Males <- lm(TM_SL ~ OG_SL, data=Df_Track_Males)
summary(lm_SL_Track_Males)

SC_SL_Track_Males <- ggplot(data=Df_Track_Males, 
                              aes(x=OG_SL, y=TM_SL)) +
  geom_smooth(method="lm") + labs(x = "OG SL (m)",
                                  y = "TR SL (m)") +
  geom_point() +
  stat_regline_equation(label.x=2.05, label.y=2.75) + theme_classic()

## Stride Frequency by Sex: Mean SD 95 CI and correlation
groupwiseMean(var = "TM_SF", data = Df_Track_Females)
sd(Df_Track_Females$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Track_Females)
sd(Df_Track_Females$OG_SF)

Df_Track_Females %>% cor_test(OG_SF,TM_SF)

lm_SF_Track_Females <- lm(TM_SF ~ OG_SF, data=Df_Track_Females)
summary(lm_SF_Track_Females)

SC_SF_Track_Females <- ggplot(data=Df_Track_Females, 
                                     aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=3.4, label.y=5.5) + theme_classic()

groupwiseMean(var = "TM_SF", data = Df_Track_Males)
sd(Df_Track_Males$TM_SF)

groupwiseMean(var = "OG_SF", data = Df_Track_Males)
sd(Df_Track_Males$OG_SF)

Df_Track_Males %>% cor_test(OG_SF,TM_SF)

lm_SL_Track_Males <- lm(TM_SF ~ OG_SF, data=Df_Track_Males)
summary(lm_SL_Track_Males)

SC_SF_Track_Males <- ggplot(data=Df_Track_Males, 
                              aes(x=OG_SF, y=TM_SF)) +
  geom_smooth(method="lm") + labs(x = "OG SF (seps/s)",
                                  y = "TR SF (seps/s)") +
  geom_point() +
  stat_regline_equation(label.x=4.1, label.y=5.4) + theme_classic()

SC_bySex_Track <- ggarrange(SC_Speed_Track_Females,SC_Speed_Track_Males,
                            SC_CT_Track_Females,SC_CT_Track_Males,
                            SC_FT_Track_Females,SC_FT_Track_Males,
                            SC_SL_Track_Females,SC_SL_Track_Males,
                            SC_SF_Track_Females,SC_SF_Track_Males,
                            ncol = 2, nrow = 5)
ggsave("SC_bySex_Track.png")

########################### ANOVAS #######################

library(effectsize) ##for eta squared
library(kableExtra)

#ANOVAS Speed Treadmill
ANOVA_Speed_TM = aov(TM_Speed ~ Group + Gender, data=Df)
summary(ANOVA_Speed_TM)
eta_squared(ANOVA_Speed_TM, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(TM_Speed ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(TM_Speed ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(TM_Speed ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(TM_Speed ~ Gender, paired = FALSE)

#ANOVAS Speed Overground
ANOVA_Speed_Overground = aov(OG_Speed ~ Group + Gender, data=Df)
summary(ANOVA_Speed_Overground)
eta_squared(ANOVA_Speed_Overground, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(OG_Speed ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(OG_Speed ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(OG_Speed ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(OG_Speed ~ Gender, paired = FALSE)

#ANOVAS Contact Time Treadmill
ANOVA_CT_TM = aov(TM_CT ~ Group + Gender, data=Df)
summary(ANOVA_CT_TM)
eta_squared(ANOVA_CT_TM, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(TM_CT ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(TM_CT ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(TM_CT ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(TM_CT ~ Gender, paired = FALSE)

#ANOVAS Contact Time Overground
ANOVA_CT_Overground = aov(OG_CT ~ Group + Gender, data=Df)
summary(ANOVA_CT_Overground)
eta_squared(ANOVA_CT_Overground, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(OG_CT ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(OG_CT ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(OG_CT ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(OG_CT ~ Gender, paired = FALSE)

#ANOVAS Flight Time Treadmill
ANOVA_FT_TM = aov(TM_TF ~ Group + Gender, data=Df)
summary(ANOVA_FT_TM)
eta_squared(ANOVA_FT_TM, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(TM_TF ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(TM_TF ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(TM_TF ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(TM_TF ~ Gender, paired = FALSE)

#ANOVAS Flight Time Overground
ANOVA_FT_Overground = aov(OG_TF ~ Group + Gender, data=Df)
summary(ANOVA_FT_Overground)
eta_squared(ANOVA_FT_Overground, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(OG_TF ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(OG_TF ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(OG_TF ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(OG_TF ~ Gender, paired = FALSE)

#ANOVAS Stride Length Treadmill
ANOVA_SL_TM = aov(TM_SL ~ Group + Gender, data=Df)
summary(ANOVA_SL_TM)
eta_squared(ANOVA_SL_TM, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(TM_SL ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(TM_SL ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(TM_SL ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(TM_SL ~ Gender, paired = FALSE)

#ANOVAS Stride Length Overground
ANOVA_SL_Overground = aov(OG_SL ~ Group + Gender, data=Df)
summary(ANOVA_SL_Overground)
eta_squared(ANOVA_SL_Overground, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(OG_SL ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(OG_SL ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(OG_SL ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(OG_SL ~ Gender, paired = FALSE)

#ANOVAS Stride Frequency Treadmill
ANOVA_SF_TM = aov(TM_SF ~ Group + Gender, data=Df)
summary(ANOVA_SF_TM)
eta_squared(ANOVA_SF_TM, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(TM_SF ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(TM_SF ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(TM_SF ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(TM_SF ~ Gender, paired = FALSE)

#ANOVAS Stride Frequency Overground
ANOVA_SF_Overground = aov(OG_SF ~ Group + Gender, data=Df)
summary(ANOVA_SF_Overground)
eta_squared(ANOVA_SF_Overground, partial = TRUE)

##POST HOC with effect size by sex
Df %>% group_by(Gender) %>%
  pairwise_t_test(OG_SF ~ Group, paired = FALSE)

Df %>% group_by(Gender) %>%
  rstatix:::cohens_d(OG_SF ~ Group, paired = FALSE)

##POST HOC by group
Df %>% group_by(Group) %>%
  pairwise_t_test(OG_SF ~ Gender, paired = FALSE)

Df %>% group_by(Group) %>%
  rstatix:::cohens_d(OG_SF ~ Gender, paired = FALSE)
