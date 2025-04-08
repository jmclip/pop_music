# final analysis
library(knitr)
library(janitor)
library(tidytext)
#library(reshape2)
library(stringr)
library(syuzhet)
#library(tif)
library(RColorBrewer)
#library(forcats)
#library(topicmodels)
#library(ggwordcloud)
library(psych)
library(scales)
#library(tayloRswift)
library(knitr)
library(kableExtra)
#library(readr)
#library(xtable)
#library(corrr)
library(qualvar)
library(stargazer)
library(lubridate)
library(tidyverse)
library(textdata)
library(factoextra)

all_sent <- read_csv("sequel/tswift_analysis_with_billboard.csv")  %>% 
  mutate(song = word(Title, 0, end = pmin(str_count(Title, "\\w+"),3)))


# album info
albums_all <- c("taylor swift", "fearless", "Fearless (Taylor's Version)", "speak now",
                "Speak Now (Taylor's Version)",  "red", "Red (Taylor's Version)", 
                "1989", "1989 (deluxe edition)", "1989 (Taylor's Version)", "reputation", 
                "lover", "folklore","folklore (deluxe version)", "evermore", "evermore (deluxe version)",
                "midnights", "midnights (3am edition)", "midnights (the til dawn edition)", 
                "tortured poets department", "tortured poets department (anthology)")

Albums_all <- c("Taylor Swift", "Fearless", "Fearless (TV)", "Speak Now", "Speak Now (TV)",
                "Red", "Red (TV)", "1989", "1989 (DE)", "1989 (TV)",
                "Reputation", "Lover", "folklore", "folklore (DE)", "evermore", 
                "evermore (DE)", "Midnights", "Midnights (3E)", "Midnights (TD)", 
                "TTPD", "TTPD (Anthology)")


# factor albums
all_sent <- all_sent %>% mutate(
  Album = factor(Album, levels = Albums_all, labels = Albums_all))

#non-duplicate list of albums (including most expansive version)
keepers <-c("Taylor Swift", "Fearless (TV)", "Speak Now (TV)", "Red (TV)", "1989 (TV)", 
            "Reputation", "Lover", "folklore (DE)", "evermore (DE)", "Midnights (TD)", "TTPD (Anthology)")

album_colors <- c("Taylor Swift" = "paleturquoise3","Fearless" = "gold2","Speak Now" = "darkgoldenrod4","Red" = "red3",
                  "1989" = "lightsteelblue2", "Reputation"= "gray30", "Lover" = "plum2","folklore" = "gray",
                  "evermore" = "orangered4","Midnights (3E)" = "slateblue4", "TTPD" = "#a79e8f",
                  "Fearless (TV)" = "gold2","Speak Now (TV)" = "darkgoldenrod4","Red (TV)" = "red3", "1989 (TV)" = "lightsteelblue2",
                  "1989 (DE)" = "lightsteelblue2", "Reputation"= "gray30", "Lover" = "plum2","folklore (DE)" = "gray",
                  "evermore (DE)" = "orangered4", "Midnights" = "slateblue4", "Midnights (TD)" = "slateblue4", "TTPD (Anthology)" = "#a79e8f")



######## PCA #########
## PCA 
# early_late <- c("Taylor Swift", "Fearless (TV)", "Speak Now (TV)", "evermore (DE)", "Midnights (TD)", "TTPD (Anthology)")
# mid <-c("Red", "1989", "Reputation", "Lover", "folklore")

data <- all_sent[,c(1:16,18:134,136:159, 187:201)] 

data_normalized <- all_sent %>% filter(Album %in% keepers) %>%
  select(Album, song, Tone, NRC_anger, NRC_fear, tone_neg, Affect, loudness, energy, acousticness, track_num.x,
         hot100_peak, hot100_weeks, stream_peak, stream_weeks, key, mode)  
  # select(Title, Album, NRC_joy, NRC_sadness, NRC_anger, Authentic, Analytic, NRC_trust, track_num.x,
  #        #song, NRC_anticipation,
  #        NRC_fear,Tone, focuspresent, #cogproc,
  #        #focusfuture, focuspast, Linguistic, Affect, tone_neg, emo_anger, tone_pos,
  #       # Linguistic, want, curiosity, allure,  quill, fount, glitter,
  #        emotion, ppron, conflict, cogproc, moral, song, speechiness, time_signature, 
  #        danceability, liveness, valence, energy, loudness, acousticness, tempo, mode, 
  #        duration_ms, instrumentalness, key)
data_a.pca <- data_normalized %>% select(where(is.numeric)) %>% 
  select(!c(track_num.x)) %>% #want, moral, Linguistic, key, ppron,tone_neg, mode, curiosity)) %>% 
  scale() %>% as.data.frame() %>%  princomp(cor = T)
data_m.pca <- data[,c(2:3,6,157:172)] %>% #filter(Album %in% keepers) %>% 
  select(where(is.numeric)) %>% select(!track_num.x) %>%
  scale() %>% as.data.frame() %>%  princomp(cor = T)


#data.pca$loadings[, 1:3]
fviz_eig(data_a.pca, addlabels = TRUE)
#fviz_eig(data_m.pca, addlabels = TRUE)
#ggsave(path = "img/", "ts_pca_eig.png", width = 8)
# Graph of the variables
fviz_pca_var(data_a.pca, col.var = "contrib", axes = c(1,2), labelsize = 3, repel = TRUE,
             gradient.cols = c("gray50", "gray70", "gray10"))
fviz_pca_var(data_a.pca, col.var = "contrib", axes = c(1,3), labelsize = 3, repel = TRUE,
             gradient.cols = c("gray50", "gray70", "gray10"))

fviz_pca_var(data_m.pca, col.var = "contrib", axes = c(1,2), labelsize = 3, repel = TRUE,
             gradient.cols = c("gray50", "gray70", "gray10"))
#ggsave(path = "img/", "ts_pca_var.png", width = 8)

   

# working with the raw data
pca_a_out <- (data.frame((data_normalized$Album), data_normalized$track_num.x, data_normalized$song, data_a.pca$scores))
names(pca_a_out)[1:3] = c("Album", "track_num", "song")


# pca_m_out <- (data.frame((data_normalized$Album), data_normalized$track_num.x, data_normalized$song, data_m.pca$scores))
# names(pca_m_out)[1:3] = c("Album", "track_num", "song")


pca_a_out2 <- pca_a_out %>%  group_by(Album) %>% filter((! Album %in% keepers) )%>%
  mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.2))

# now filter for regular analysis
# data_normalized <-data_normalized %>% filter(Album %in% keepers)
# pca_out<-(data.frame((data_normalized$Album), data_normalized$Title, data_normalized$song, 
#                      data_normalized$track_num.x, data_m.pca$scores))
# names(pca_out)[1:4] = c("Album", "Title", "song", "track_num")



## PCA ANALYSIS
pca_a_out %>% group_by(track_num) %>% mutate(track_avg_1 = mean(Comp.1), track_avg_2 = mean(Comp.2)) %>%  
  ungroup() %>%  group_by(Album) %>% mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.2)) %>% 
  filter(Comp.2 < 3 & Comp.2 >-3 & Comp.1 >- 3  & Comp.1 < 3) %>%
  ggplot()  +
  #geom_text(aes(Comp.1, Comp.2, color = Album, label = song)) + #CHANGE
  geom_text(aes(track_avg_1, track_avg_2, label = track_num), color = "black",  size = 3) + 
  geom_text(aes(album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 3.25) +
  geom_text(data = pca_a_out2, aes( album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 1.8) +
  scale_color_manual(values = c(album_colors)) + scale_fill_manual(values = c(album_colors)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "gray", linewidth=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray", linewidth=0.5) +
  labs(x = "Neg emotion to positive", y = "Music: (neg) acoustic to (pos) energy", color="Album",
       title = "PCA of Albums in Swift's Catalogue: Music AND lyrics") + guides( color = "none") ##############
  # annotate("text", x = 2.25, y=0.25, label = "Saudade", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = 0.15, y=2.75, label = "Glitter pen", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = 1.75, y=-2.75, label = "Melancholy and anger", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = 1.75, y=-2.5, label = "female rage: the musical", color = "black", size = 2.5, fontface = 4) +
  # annotate("text", x = -2.5, y=-3, label = "Fountain pen", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = -2.0, y=1.75, label = "For the hope of it all", color = "black", size = 4.5, fontface = 4) +
  # guides( color = "none")
ggsave(path = "sequel/figures/","ts_pca_tracks_songs.png", width = 8)




## PCA ANALYSIS: full
pca_out %>% group_by(track_num) %>% mutate(track_avg_1 = mean(Comp.1), track_avg_2 = mean(Comp.2)) %>%  
  ungroup() %>%  group_by(Album) %>% mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.2)) %>% 
  ggplot()  +
  geom_point(aes(Comp.1, Comp.2),color = "gray75") +
  geom_text(aes(track_avg_1, track_avg_2, label = track_num), color = "black",  size = 3) +
  geom_text(aes(album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 3.25) +
  geom_text(data = pca_a_out2, aes( album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 1.8) +
  scale_color_manual(values = c(album_colors, "black")) +
  geom_hline(yintercept=0, linetype="dashed",  color = "gray", linewidth=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray", linewidth=0.5) +
  labs(x = "Lyric-focused to emotion-focused", y = "Emotion: negative to positive emotion", color="Album",
       title = "PCA of Albums in Swift's Catalogue") +
  annotate("text", x = 2.25, y=0.25, label = "Saudade", color = "black", size = 4.5, fontface = 4) +
  annotate("text", x = 0.15, y=2.75, label = "Glitter pen", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = 1, y=2.5, label = "quill pen", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = 1.75, y=-2.75, label = "Melancholy and anger", color = "black", size = 4.5, fontface = 4) +
  annotate("text", x = 1.75, y=-2.5, label = "female rage: the musical", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = -2.5, y=-3, label = "Fountain pen", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = -2.5, y=-3.2, label = "fountain pen", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = -2.0, y=1.75, label = "Ambition and hope", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = -2.0, y=2.45, label = "glitter pen", color = "black", size = 2.5, fontface = 4) +
  #annotate("text", x = -2.75, y=0.75, label = "Yearning", color = "black", size = 4.5, fontface = 4) +
  guides( color = "none")
##ggsave(path = "img/", "ts_pca_tracks_full.png", width = 8)

### MUSIC 
pca_m_out %>% group_by(track_num) %>% mutate(track_avg_1 = mean(Comp.1), track_avg_2 = mean(Comp.2)) %>%  
  ungroup() %>%  group_by(Album) %>% mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.2)) %>% 
  filter(Comp.2 < 2 & Comp.2 >-2 & Comp.1 >- 2  & Comp.1 < 2) %>%
  ggplot()  +
  geom_text(aes(Comp.1, Comp.2, label = song, color = Album), size =2, show.legend = FALSE)  +
  geom_text(aes(track_avg_1, track_avg_2, label = track_num), color = "black",  size = 3) +
  geom_text(aes(album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 3.25) +
  geom_text(data = pca_a_out2, aes( album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 1.8) +
  scale_color_manual(values = c(album_colors, "black")) +
  geom_hline(yintercept=0, linetype="dashed",  color = "gray", linewidth=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray", linewidth=0.5) +
  labs(x = "", y = "", color="Album",
       title = "PCA of Albums in Swift's Catalogue: Music ONLY") + guides( color = "none") ##############

## PCA ANALYSIS: tracks COMP 3!!!
pca_a_out %>% group_by(track_num) %>% mutate(track_avg_1 = mean(Comp.1), track_avg_2 = mean(Comp.3)) %>%  
  ungroup() %>%  group_by(Album) %>% mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.3)) %>% 
  filter(Comp.1 >-2 ) %>%
  #filter( Album == "Speak Now (TV)") %>%
  ggplot()  +
  geom_text(aes(Comp.1, Comp.3, label = song, fill = Album), size =2, show.legend = FALSE) +
  geom_text(aes(track_avg_1, track_avg_2, label = track_num), color = "black",  size = 3, show.legend = FALSE) +
  geom_text(aes(album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 3) +
  scale_color_manual(values = c(album_colors, "black")) +
  geom_hline(yintercept=0, linetype="dashed",  color = "gray", size=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray", size=0.5) +  theme(legend.position = "None")
  labs(x = "Lyric-focused to emotion-focused", y = "Emotion: negative to positive emotion", color="Album",
       title = "PCA of Albums in Swift's Catalogue") +
  # annotate("text", x = 2.25, y=0.25, label = "Saudade", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = 0.15, y=2.75, label = "Glitter pen", color = "black", size = 4.5, fontface = 4) +
  # #annotate("text", x = 1, y=2.5, label = "quill pen", color = "black", size = 2.5, fontface = 4) +
  # annotate("text", x = 1.75, y=-2.75, label = "Melancholy and anger", color = "black", size = 4.5, fontface = 4) +
  # annotate("text", x = 2.25, y=-2.5, label = "female rage: the musical", color = "black", size = 2.5, fontface = 4) +
  # annotate("text", x = -2.5, y=-3, label = "Fountain pen", color = "black", size = 4.5, fontface = 4) +
  # #annotate("text", x = -2.5, y=-3.2, label = "fountain pen", color = "black", size = 2.5, fontface = 4) +
  # annotate("text", x = -2.0, y=1.75, label = "Ambition and hope", color = "black", size = 4.5, fontface = 4) +
  # #annotate("text", x = -2.0, y=2.45, label = "glitter pen", color = "black", size = 2.5, fontface = 4) +
  # #annotate("text", x = -2.75, y=0.75, label = "Yearning", color = "black", size = 4.5, fontface = 4) +
  theme(legend.position = "None")
  ggsave("ts_pca_tracks_titles_full.png", width = 12, dpi = 500)

## PCA ANALYSIS: tracks
pca_out %>% group_by(track_num) %>% mutate(track_avg_1 = mean(Comp.1), track_avg_2 = mean(Comp.2)) %>%  
  ungroup() %>%  group_by(Album) %>% mutate(album_avg_1 = mean(Comp.1), album_avg_2 = mean(Comp.2)) %>% 
  #filter( Album == "Speak Now (TV)") %>%
  filter(Comp.2 < 4 & Comp.1 < 2.5 ) %>%
  ggplot()  +
  geom_text(aes(Comp.1, Comp.2, label = song, color = Album), size =2, show.legend = FALSE) +
  geom_text(aes(track_avg_1, track_avg_2, label = track_num), color = "black",  size = 3, show.legend = FALSE) +
  geom_text(aes(album_avg_1, album_avg_2, label = Album, color= as.factor(Album)), size = 3) +
  geom_text(data = pca_a_out2, aes( album_avg_1, album_avg_2, 
                                    label = Album, color= as.factor(Album)), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c(album_colors, "black")) +
  geom_hline(yintercept=0, linetype="dashed",  color = "gray", size=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray", size=0.5) +
  labs(x = "Lyric-focused to emotion-focused", y = "Emotion: negative to positive emotion", color="Album",
       title = "PCA of Albums in Swift's Catalogue") +
  annotate("text", x = 2.25, y=0.25, label = "Saudade", color = "black", size = 4.5, fontface = 4) +
  annotate("text", x = 0.15, y=2.75, label = "Glitter pen", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = 1, y=2.5, label = "quill pen", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = 1.75, y=-2.75, label = "Melancholy and anger", color = "black", size = 4.5, fontface = 4) +
  annotate("text", x = 2.25, y=-2.5, label = "female rage: the musical", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = -2.5, y=-3, label = "Fountain pen", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = -2.5, y=-3.2, label = "fountain pen", color = "black", size = 2.5, fontface = 4) +
  annotate("text", x = -2.0, y=1.75, label = "Ambition and hope", color = "black", size = 4.5, fontface = 4) +
  #annotate("text", x = -2.0, y=2.45, label = "glitter pen", color = "black", size = 2.5, fontface = 4) +
  #annotate("text", x = -2.75, y=0.75, label = "Yearning", color = "black", size = 4.5, fontface = 4) +
  theme(legend.position = "None")

#ggsave(path = "img/", "ts_pca_tracks_titles_zoom.png", width = 12, height = 7)



# #palette = album_colors)
# fviz_pca_var(data.pca, col.var = "contrib", 
#              gradient.cols = c("gray80", "gray50", "gray10"),
#              ggtheme = theme_minimal())
# 
# #take2
# data.pca <-prcomp(data_normalized, center = TRUE) 
# data.pca.plot <- autoplot(data.pca, data = data_normalized, colour = "Album",
#                           loadings = TRUE, loadings.colour = 'blue',
#                           loadings.label = TRUE) #
# data.pca.plot
