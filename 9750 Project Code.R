#### Installation/ Library #####
library(dplyr)
library(ggplot2)
install.packages('animation')
library(animation)
install.packages("gganimate")
library(gganimate)
install.packages("patchwork")
library(patchwork)
install.packages("devtools")
library(devtools)
devtools::install_github("thomasp85/gganimate")
devtools::install_github("thomasp85/transformr")
install.packages(c("waffle", "extrafont"))
library(waffle)
library(extrafont)
install.packages("echarts4r")
library(echarts4r)
devtools::install_github("JohnCoene/echarts4r.assets")
install.packages("showtext")
install.packages("sysfonts")
library(sysfonts)
library(showtext)
install.packages("ggvis")
library(ggvis)
install.packages("data.table")
install.packages("Publish")
library(data.table)
library(Publish)
library(RColorBrewer)
library(readr)
library(scales)
devtools::install_github('yihui/knitr')
install.ffmpeg()

######### Demographics from Data ##############                 
demo = read.csv("/Users/arielgilgeours/Documents/demographics.csv")
demo

#Tables
table(demo$Gender, demo$Age.Reported)
prop.table(table(demo$Gender, demo$Age.Reported))

table(demo$Gender) 
table(demo$Age.Reported)
table(demo$Race)

#Basic QPlots
qplot(Gender, data = demo)
qplot(Age.Reported, data = demo)
qplot(Race, data = demo)
ggplot(data = demo)+
  aes(x=Age.Reported, y=Race, color=Gender) + geom_point() + facet_grid(. ~ Race)


# Infographics of Demographics
# Compiled with Pathwork or Iron

#race  
demo.race = c(White=41, Asian = 15, `African American\ or Black` = 4, Other = 3)
r = waffle(demo.race, rows = 4, colors = c("lightsalmon1", "lightgoldenrod1", "palegreen1", "slategray1"), use_glyph = "child", glyph_size = 6, 
       title = "Race",
       xlab = "1 person = 1 person") 

#gender
demo.gender = c(Female=36, Male=27)
g = waffle(demo.gender, row = 7, colors = c("lightpink", "lightskyblue"), use_glyph = c("female", "male"), glyph_size = 6,
       title = "Gender",
       xlab = "1 person = 1 person")

# age group
demo.age = c(`20-29`=36, `30-39`=14, `40-49`=7, `50-59`=5, `60-69`=1)

a = waffle(demo.age, row = 7, use_glyph = c("user"), glyph_size = 6,
       title = "Age Group",
       xlab = "1 person = 1 person")

(a + g)
r

########## Animations #############=
#Four Different Viruses 
titer = read.csv("/Users/arielgilgeours/Documents/titer.csv")
titer

#Cali 09
cali09 = filter(titer, Virus == "A/California/7/2009")

cali09graph = ggplot(cali09, aes(Participant.ID, Antibody.Titer, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Virus) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Influenza A/California/7/2009")+
  labs(x = "Participants", y = "Antibody Titer")+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
cali09graph
cali09ani = cali09graph + transition_time(Study.Time.Collected) +
  labs(title = "Influenza A/California/7/2009  Day: {frame_time}")

animate(cali09ani, renderer = ffmpeg_renderer(), width = 557, height = 484)


#Brisbane 07
bris07 = filter(titer, Virus == "A/Brisbane/59/2007")

bris07graph = ggplot(bris07, aes(Participant.ID, Antibody.Titer, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Virus) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Influenza A/Brisbane/59/2007")+
  labs(x = "Participants", y = "Antibody Titer")+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
bris07graph
bris07anim = bris07graph + transition_time(Study.Time.Collected) +
  labs(title = "Influenza A/Brisbane/59/2007  Day: {frame_time}")

animate(bris07anim, renderer = ffmpeg_renderer(), width = 528, height = 484)

#Uruguay 07
uru07 = filter(titer, Virus == "A/Uruguay/716/2007")

uru07graph = ggplot(uru07, aes(Participant.ID, Antibody.Titer, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Virus) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Influenza A/Uruguay/716/2007")+
  labs(x = "Participants", y = "Antibody Titer")+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
uru07graph
uru07anim = uru07graph + transition_time(Study.Time.Collected) +
  labs(title = "Influenza A/Uruguay/716/2007  Day: {frame_time}")

animate(uru07anim, renderer = ffmpeg_renderer(), width = 577, height = 484)

#Brisbane 08
bris08 = filter(titer, Virus == "B/Brisbane/60/2008")

bris08graph = ggplot(bris08, aes(Participant.ID, Antibody.Titer, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Virus) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("InfluenzaB/Brisbane/60/2008")+
  labs(x = "Participants", y = "Antibody Titer")+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
bris08graph
bris08anim = bris08graph + transition_time(Study.Time.Collected) +
  labs(title = "Influenza B/Brisbane/60/2008  Day: {frame_time}")

animate(bris08anim, renderer = ffmpeg_renderer(), width = 557, height = 484)

# First Animation 
gotthis = ggplot(titer, aes(Participant.ID, Antibody.Titer, color = Race)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Virus)
gotthis

gotthis + transition_time(Study.Time.Collected) +
  labs(title = "Day: {frame_time}")




#ELISA
elispot = read.csv("/Users/arielgilgeours/Documents/elispot.csv")
elispot

#Effector Cells
effectorcells = elispot %>% filter(Analyte %in% c("Effector Cells H1N1 IgG", "Effector Cells Seasonal IgG","Effector Cells Total IgG" ))
effectorcells
                         
effectorcellsgraph = ggplot(effectorcells, aes(Participant.ID, Spot.Number.Reported, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Analyte) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Effector Cells")+
  labs(x = "Participants", y = "Spot Number Reported")+
  theme(plot.title = element_text(hjust = 0.5))

effectorcellsgraph
effectoranim = effectorcellsgraph + transition_time(Study.Time.Collected) +
  labs(title = "Effector Cells Day: {frame_time}")

animate(effectoranim, renderer = ffmpeg_renderer(), width = 713, height = 484)
anim_save("effectoranim")



#Memory Cells H1N1 & Seasonal 
memorycells = elispot %>% filter(Analyte %in% c("Memory Cells H1N1 IgG", "Memory Cells Seasonal IgG","Memory Cells Total IgG" ))
memorycells

memorycellsgraph = ggplot(memorycells, aes(Participant.ID, Spot.Number.Reported, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Analyte) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Memory Cells")+
  labs(x = "Participants", y = "Spot Number Reported")+
  theme(plot.title = element_text(hjust = 0.5))

memorycellsgraph
memoryanim1 = memorycellsgraph + transition_time(Study.Time.Collected) +
  labs(title = "Memory Cells Day: {frame_time}")
animate(memoryanim1, renderer = ffmpeg_renderer(), width = 658, height = 484)


"Memory Cells Total IgG"
memorycellstotal = filter(elispot, Analyte == "Memory Cells Total IgG" )
memorycellstotal

memorycellstotalgraph = ggplot(memorycellstotal, aes(Participant.ID, Spot.Number.Reported, color = Gender)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Analyte) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 0))+
  ggtitle("Memory Cells ")+
  labs(x = "Participants", y = "Spot Number Reported")+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

memorycellstotalgraph
memoryanim2 = memorycellstotalgraph + transition_time(Study.Time.Collected) +
  labs(title = "Memory Cell Total Day: {frame_time}")
animate(memoryanim2, renderer = ffmpeg_renderer(), width = 528, height = 484)


# First Animation
progress = ggplot(elispot, aes(Participant.ID, Spot.Number.Reported, color = Race)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Analyte)
progress + transition_time(Study.Time.Collected) +
  labs(title = "Day: {frame_time}")








