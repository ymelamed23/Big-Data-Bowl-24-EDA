#set path
path=
input=paste0(path, "nfl-big-data-bowl-2024/")
output=paste0(path, "Output/")

#Load packages
library(ngscleanR)
library(tidyverse)
library(patchwork)
library(gganimate)
library(cowplot)
library(repr)
library(magick)
library(nflfastR)
library(ggvoronoi)

#### Load data
# load data except detailed tracking
df_games <- read.csv(paste0(input, 'games.csv'))
df_players <- read.csv(paste0(input, 'players.csv'))
df_plays <- read.csv(paste0(input, 'plays.csv'))
df_tack <- read.csv(paste0(input, 'tackles.csv'))

#plot with the alpha being the players force 
#load tracking data

#weeks of NFL season
weeks <- seq(1, 9)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0(input, "tracking_week_",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}


#adjust tracking data
df_tracking = df_tracking %>%
  # Flip positional values
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160 / 3 - y, y))
#flip player direction
#week1_adj_data <- week1_adj_data %>%
 # mutate(dir = ifelse(playDirection == "left", dir + 180, dir),
  #       dir = ifelse(dir > 360, dir - 360, dir),
   #      o = ifelse(playDirection == "left", o + 180, o),
    #     o = ifelse(o > 360, o - 360, o))

#for animating plays, ## declaring values for field coordinates, # General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

### ANIMATE RANDOM PLAY ### -----------------------------------------------------------------------------------------------
#picking a random play
# 85
# 250 has missed tackle
# 260 clear tackle
# 290 sort of missatribution
# 300 assist
# 390 with L.Jackson, cool

set.seed(460)
example_play <- df_plays %>%
  filter(passResult=="") %>% 
  select(gameId, playId, playDescription) %>% 
  sample_n(1)
#commanders hit on Ettiene
#example_play = df_plays %>% 
 # select(gameId, playId, playDescription) %>% 
  #filter(gameId=="2022110300", playId=="3506")

graph_animation = function(example_play) {
  
  
  #merging games data to play
  example_play <- inner_join(example_play,
                             df_games,
                             by = c("gameId" = "gameId"))
  
  #merging tracking data to play
  example_play <- inner_join(example_play,
                             df_tracking,
                             by = c("gameId" = "gameId",
                                    "playId" = "playId"))
  example_play = left_join(example_play,
                               df_players,
                               by="nflId")
  #add force
  example_play = example_play %>% 
    mutate(force=weight/2.2046*a/1.09361)
  #add team variable
  example_play$team=example_play$club
  
  #make force for footabll equal to 9.8
  example_play = example_play %>% 
    mutate(force_graph=if_else(!is.na(force), force, 9.8))
  example_play = example_play %>% 
    filter(club=="football") %>%
    select(frameId, x, y) %>% 
    rename("football_x"=x, "football_y"=y) %>% 
    left_join(example_play, by = "frameId") %>% 
    mutate("dist_to_fb" =  sqrt((x - football_x)^2 + (y - football_y)^2)) %>% 
    left_join(df_tack, by = c("gameId" = "gameId", "nflId" = "nflId",
                              "playId" = "playId")) 
  #find min dist to football and add variables for tackle, etc
  #extract only the max frame, since some players are same dist to football at certain points (unlikely to matter)
  example_play = example_play %>% 
    group_by(nflId) %>% 
    filter(club!="football") %>% 
    slice_min(dist_to_fb) %>% 
    group_by(nflId) %>% 
    slice_max(frameId) %>% 
    mutate(tackle_frame=tackle, assist_frame=assist, pff_missedTackle_frame=pff_missedTackle,
           forcedFumble_frame=forcedFumble) %>% 
    select(frameId, nflId, tackle_frame, assist_frame, pff_missedTackle_frame, forcedFumble_frame) %>% 
    right_join(example_play, by = c("frameId", "nflId")) %>%
    #change frames so that when a player starts the tackle (nearest dist to ball), the following frames are 'tackle' as well
    group_by(nflId) %>% 
    arrange(frameId) %>% 
    fill(tackle_frame, .direction = "down") %>% 
    fill(assist_frame, .direction = "down") %>% 
    ungroup()
  
  #pull team colors
  colors=teams_colors_logos %>% 
    select(team_abbr, team_name, team_color) %>% 
    add_row(team_abbr="football", team_name="football", team_color='brown') 
  #fix team abbreviations
  colors=subset(colors, team_abbr!="LA")
  colors$team_abbr[colors$team_abbr=="LAC"]="LA"
  #join to data
  example_play = example_play %>% 
    left_join(colors, by = c("team"= "team_abbr"))
  #make fill variable for graph 
  example_play=example_play %>% 
    mutate(fill_graph=if_else(tackle_frame%in%1|assist_frame%in%1, "#07F71F",
                        if_else(pff_missedTackle_frame%in%1, "#FC958D", team_color)))
  
  #colors used for plot - using colors of team
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash_right <- 38.35
  hash_left <- 12
  hash_width <- 3.3
  
  plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))
  # Specific boundaries for a given play
  ymin <- max(round(min(example_play$x, 
                        na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(example_play$x, 
                        na.rm = TRUE) + 10, -1), 120)
  # Hash marks
  df_hash <- 
    expand.grid(x = c(0, 23.36667, 29.96667, xmax), 
                y = (10:110)) %>% 
    filter(!(floor(y %% 5) == 0), y < ymax, y > ymin)
  field_base <- ggplot() +
    annotate("text", x = df_hash$x[df_hash$x < 55/2],
             y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) +
    annotate("text", x = df_hash$x[df_hash$x > 55/2],
             y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
    annotate("segment", x = xmin, y = seq(max(10, ymin), min(ymax, 110), by = 5),
             xend =  xmax, yend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
    annotate("text", x = rep(hash_left, 11), y = seq(10, 110, by = 10),
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
             angle = 270, size = 4) +
    annotate("text", x = rep((xmax - hash_left), 11), y = seq(10, 110, by = 10),
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
             angle = 90, size = 4) +
    annotate("segment", x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin),
             xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "black")
  play_animation <- field_base +
    geom_point(data = example_play, 
               aes(x = (xmax - y), y = x,
                   shape = team, fill = fill_graph,
                   group = nflId, size = as.factor(force_graph), color = team_color),
               alpha = .7) +
    geom_text(data = example_play, 
              aes(x = (xmax-y), y = x, label = jerseyNumber), 
              color = "white", vjust = 0.36, size = 3.5) +
    #titling plot with play description
    labs(title = plot_title) +
  #  scale_size_manual(values = c(4, 6, 6), guide = FALSE) +
    scale_shape_manual(values = c(16, 21, 21), guide = FALSE) +
    #scale_fill_manual(values=example_play$fill_graph) +
    scale_fill_identity()+
    scale_color_identity() +
   # scale_color_manual(values=example_play$team_color) +
   # scale_fill_manual(values = c("brown", "#FFB612", "#101820"), guide = FALSE) +
   # scale_color_manual(values = c("brown", "#FFB612", "#101820"), guide = FALSE) +
    ylim(ymin, ymax) + coord_fixed() +
    cowplot::theme_nothing() + theme(plot.title = element_text()) +
    transition_time(frameId) + ease_aes('linear') + NULL
  ex_play_length <- length(unique(example_play$frameId))
  animate(play_animation, fps = 10, nframe = ex_play_length)}
options(bitmapType='cairo')
graph_animation(example_play)

anim_save(paste0(output,"ceh_gif.gif"))

