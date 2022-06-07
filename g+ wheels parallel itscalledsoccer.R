#This script generates g+ wheels from data obtained from the American Soccer Analysis API via {itscalledsoccer}
#Explanation here: https://www.americansocceranalysis.com/home/2020/5/5/the-art-of-the-wheel
#Created by: Eliot McKinley
#email: etmckinley@gmail.com
#Date: Feb 21, 2022

#choose league, seasons, and minute cutoff

league_select = "mlsnp"  #select only one
seasons_select = c(2022) #can select many
minute_limit = 500

#define what the base save folder is
save_folder = "./Figures/g+ wheels/"

#how many parallel workers
workers = 6

#choose a font. These wheels were built for Bebas Neue and I can't guarantee that the spacing will work well with another font
font = "Bebas Neue"

##Processing

library(RCurl)
library(tidyverse)
library(ggimage)
library(ggpattern)
library(png)
library(cowplot)
library(ggbeeswarm)
library(furrr)
library(itscalledsoccer)

min.limit = -0.07
max.limit = 0.07
lower.limit = -0.12
upper.limit = .183


asa_client <- AmericanSoccerAnalysis$new()

players = asa_client$get_players()

teams = asa_client$get_teams()

value_aa = asa_client$get_player_goals_added(
  leagues = league_select,
  minimum_minutes = minute_limit,
  split_by_team = TRUE,
  split_by_season = TRUE,
  season_name = paste(seasons_select, collapse = ",")
) %>%
  unnest(data) %>%
  mutate(goals_added_above_avg = goals_added_above_avg / minutes_played *
           96) %>%
  group_by(player_id, team_id, season_name) %>%
  mutate(Value_AA = sum(goals_added_above_avg)) %>%
  left_join(players %>% select(Player = player_name, player_id)) %>%
  rename(
    Min = minutes_played,
    action = action_type,
    value = goals_added_above_avg,
    Season = season_name
  ) %>%
  mutate(
    Position = case_when(
      str_detect(general_position, "FB") ~ "Fullback",
      str_detect(general_position, "AM") ~ "Attacking Midfielder",
      str_detect(general_position, "CM") ~ "Central Midfielder",
      str_detect(general_position, "DM") ~ "Defensive Midfielder",
      str_detect(general_position, "ST") ~ "Striker",
      str_detect(general_position, "W") ~ "Winger",
      str_detect(general_position, "CB") ~ "Centerback"
    ),
    value.bar = ifelse(
      value > max.limit,
      max.limit,
      ifelse(value < min.limit, min.limit, value)
    )
  ) %>%
  left_join(teams %>% select(team_id, Team = team_name)) %>%
  mutate(Team = ifelse(team_id == "raMyrr25d2", "NJ-NY Gotham FC", Team)) %>%
  mutate(player.team.season = paste(Player, Team, Season)) %>%
  ungroup()

icons = data.frame(
  action = c(
    "Receiving",
    "Passing",
    "Dribbling",
    "Interrupting",
    "Fouling",
    "Shooting"
  ),
  image = c(
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/receiving.png",
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/passing.png",
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/dribbling.png",
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/interrupting.png",
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/fouls.png",
    "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/shooting.png"
  )
)


league_logo = data.frame(
  league = c("mls" , "nwsl", "uslc", "usl1", "mlsnp"),
  league.logo = c(
    "https://app.americansocceranalysis.com/league_logos/mls_logo_white.png",
    "https://app.americansocceranalysis.com/league_logos/nwsl_logo_white.png",
    "https://app.americansocceranalysis.com/league_logos/uslc_logo_white.png",
    "https://app.americansocceranalysis.com/league_logos/usl1_logo_white.png",
    "https://app.americansocceranalysis.com/league_logos/mlsnp_logo_white.png"
  ),
  x = c(-.455,-.448, -.435, -.435,-.445),
  y = c(-.45,-.45,-.47,-.47,-.455),
  scale = c(.075, .075, .12, .12, .09)
) %>%
  filter(league == league_select)

value_aa$action = factor(
  value_aa$action,
  levels = c(
    "Receiving",
    "Passing",
    "Dribbling",
    "Interrupting",
    "Fouling",
    "Shooting"
  )
)

colors = c("#F82D97",
           "#FF6A62" ,
           "#F8FF01",
           "#2EF8A0",
           "#01C4E7",
           "#A510D3")

#get player to test plotting
#plot.data=value_aa %>% filter(player_id == "aDQ0Pv1WQE")

invisible(ifelse (dir.exists(paste0(
  save_folder, league_select
)), FALSE, dir.create(paste0(
  save_folder, league_select
))))

save_value_aa <- function(plot.data) {
   if 
  # checks if file already exists, comment out and replace with '(TRUE){' to run for all players
  (!file.exists(paste0(
    save_folder ,
    league_select,
    "/",
    plot.data$player.team.season[1],
    ".png"
  )))
    {
  #  (TRUE){
    
    font = "Bebas Neue"
    
    p = ggplot(plot.data) +
      #for whatever reason you need to plot a blank column for the ordering of categories to work
      geom_col(
        aes(x = action,
            y = 0,
            fill = action),
        width = 1,
        color = "black",
        size = 0.1,
        alpha = 0
      ) +
      #plot positive bars
      geom_col(
        data = plot.data %>% filter(value.bar > 0),
        aes(x = action,
            y = value.bar,
            fill = action),
        width = 1,
        color = "#15202B",
        size = .1,
        alpha = 1
      ) +
      #plot negative striped bars
      geom_col_pattern(
        data = plot.data %>% filter(value.bar <= 0),
        aes(
          x = action,
          y = value.bar,
          pattern_color = action
        ),
        pattern_spacing = 0.01,
        pattern_density = 1,
        pattern = "stripe",
        width = 1,
        color = "#15202B",
        pattern_fill = "#15202B",
        size = 0.1,
        alpha = 1
      ) +
      #add bounding boxes to each column
      annotate(
        "rect",
        xmin = seq(from = 0.5, to = 5.5, by = 1),
        xmax = seq(from = 1.5, to = 6.5, by = 1),
        ymin = min.limit,
        ymax = max.limit,
        color = "gray",
        fill = "transparent",
        size = 0.2
      ) +
      #add colored segments at zero
      annotate(
        "segment",
        x = seq(from = 0.5, to = 5.5, by = 1),
        xend = seq(from = 1.5, to = 6.5, by = 1),
        y = 0,
        yend = 0,
        color = colors,
        size = 0.5
      ) +
      #add white circle to the middle of the wheel
      annotate(
        "rect",
        xmin = 0.5,
        xmax = 6.5,
        ymax = min.limit,
        ymin = lower.limit,
        color = "transparent",
        fill = "white",
        size = 0.2
      ) +
      #add center value text
      annotate(
        "text",
        x = 1,
        y = lower.limit,
        label =
          formatC(
            round(max(plot.data$Value_AA),
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          ),
        color = "#15202B",
        family = font,
        fontface = "bold",
        size = 8.5,
        vjust = 0.5,
        alpha = 1
      ) +
      #add category labels and values
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Receiving"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 1.16,
        y = max.limit + 0.053,
        size = 5.5,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("                  Receiving"),
        x = 1.16,
        y = max.limit + 0.053,
        size = 4,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font
      ) +
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Passing"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 2.05,
        y = max.limit + 0.075,
        size = 5.5,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("                  Passing"),
        x = 2.05,
        y = max.limit + 0.074,
        size = 4,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font
      ) +
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Dribbling"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 2.9,
        y = max.limit + 0.072,
        size = 5.5,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("                  Dribbling"),
        x = 2.9,
        y = max.limit + 0.072,
        size = 4,
        color = "white",
        hjust = 0,
        vjust = 0,
        family = font
      ) +
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Interrupting"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 4.08,
        y = max.limit + 0.068,
        size = 5.5,
        color = "white",
        hjust = 1,
        vjust = 0,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("Interrupting                   "),
        x = 4.08,
        y = max.limit + 0.068,
        size = 4,
        color = "white",
        hjust = 1,
        vjust = 0,
        family = font
      ) +
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Fouling"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 5.03,
        y = max.limit + 0.075,
        size = 5.5,
        color = "white",
        hjust = 1,
        vjust = 1,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("Fouling                  "),
        x = 5.01,
        y = max.limit + 0.076,
        size = 4,
        color = "white",
        hjust = 1,
        vjust = 1,
        family = font
      ) +
      annotate(
        "text",
        label = paste(
          formatC(
            round(plot.data$value[plot.data$action == "Shooting"],
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          )
        ),
        x = 5.9,
        y = max.limit + 0.067,
        size = 5.5,
        color = "white",
        hjust = 1,
        vjust = 1,
        family = font,
        fontface = "bold"
      ) +
      annotate(
        "text",
        label = paste("Shooting                    "),
        x = 5.9,
        y = max.limit + 0.06,
        size = 4,
        color = "white",
        hjust = 1,
        vjust = 1,
        family = font
      ) +
      #add upper value label
      annotate(
        "text",
        label = paste(
          formatC(
            round(max(plot.data$Value_AA),
                  digits = 2),
            digits = 2,
            format = "f",
            small.interval = 2,
            width = 3,
            flag = "+"
          ),
          "          "
        ),
        x = 0.5,
        y = max.limit + 0.1,
        size = 7.5,
        color = "white",
        hjust = 1,
        vjust = 0,
        family = font,
        fontface = "bold"
      ) +
      #add units to label
      annotate(
        "text",
        label = "                 goals added per 96\'",
        x = 0.5,
        y = max.limit + 0.1,
        size = 5.5,
        color = "white",
        hjust = 0.5,
        vjust = 0,
        family = font
      ) +
      #add icos
      geom_image(
        data = icons,
        aes(
          x = action,
          y = max.limit + 0.04,
          image = image
        ),
        size = 0.08,
      ) +
      #to polar coordinates
      coord_polar(start = 0,
                  direction = 1) +
      #add colors
      scale_fill_manual(
        values =
          c(
            "Receiving" = "#F82D97",
            "Passing" = "#FF6A62",
            "Dribbling" = "#F8FF01",
            "Interrupting" = "#2EF8A0",
            "Fouling" = "#01C4E7",
            "Shooting" = "#A510D3"
          )
      ) +
      scale_color_manual(
        values =
          c(
            "Receiving" = "#F82D97",
            "Passing" = "#FF6A62",
            "Dribbling" = "#F8FF01",
            "Interrupting" = "#2EF8A0",
            "Fouling" = "#01C4E7",
            "Shooting" = "#A510D3"
          )
      ) +
      scale_pattern_color_manual(
        values =
          c(
            "Receiving" = "#F82D97",
            "Passing" = "#FF6A62",
            "Dribbling" = "#F8FF01",
            "Interrupting" = "#2EF8A0",
            "Fouling" = "#01C4E7",
            "Shooting" = "#A510D3"
          )
      ) +
      #scale y axis
      scale_y_continuous(limits =
                           c(lower.limit,
                             upper.limit)) +
      #them adjustments
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(
          face = "bold",
          size = 36,
          family = font
        ),
        plot.subtitle = element_text(family = font,
                                     size = 16),
        text = element_text(color = "white"),
        plot.margin = margin(0.1,
                             0.05,-0.9,
                             0.1,
                             "in")
      ) +
      #fix labels
      labs(
        title =
          paste(plot.data$Player[1]),
        subtitle =
          paste0(
            "vs. the average ",
            plot.data$Season[1],
            " ",
            plot.data$Position[1],
            "\n",
            round(plot.data$Min[1]),
            " minutes"
          )
      )
    #set seed for beeswarm
    set.seed(42)
    #plot beeswarm
    bees = ggplot(
      value_aa %>%
        filter(
          Position == plot.data$Position[1],
          Season == plot.data$Season[1],
          Min > minute_limit
        ) %>%
        select(Player, Position, Season, Value_AA) %>%
        unique() %>%
        mutate(order = Player == plot.data$Player[1]) %>%
        arrange(order)
    ) +
      geom_quasirandom(
        aes(
          x = Position,
          y = Value_AA,
          alpha = Player == plot.data$Player[1],
          shape = Player == plot.data$Player[1],
          size = Player == plot.data$Player[1]
        ),
        stroke = 0.2,
        fill = "white",
        color = "white",
        width = 0.4,
        groupOnX = TRUE
      ) +
      #fix sizes, alphas, and shapes of the beeswarm
      scale_size_manual(values =
                          c(0.8,
                            1.25)) +
      scale_alpha_manual(values =
                           c(0.4,
                             1)) +
      scale_shape_manual(values =
                           c(1,
                             21)) +
      #scale to max and min limits for value_aa per 96
      scale_y_continuous(limits =
                           c(-0.2,
                             .31)) +
      #fix themes
      theme_minimal_vgrid() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.margin =
          margin(-0.2,-0.1,-0.2 ,-0.1,
                 "in")
      ) +
      labs(x = "",
           y = "") +
      coord_flip()
    
    
    #use cowplot to put it all together
    p1 = ggdraw() +
      draw_plot(p) +
      draw_image(
        paste0(
          "https://app.americansocceranalysis.com/club_logos/",
          plot.data$team_id[1],
          ".png"
        ),
        x = 0.82,
        y = 0.82,
        width = 0.15,
        height = 0.15
      ) +
      draw_line(
        x =
          c(0.48,
            0.45,
            0.45),
        y =
          c(0.38,
            0.42,
            0.695),
        color = "white",
        size = 0.75
      ) +
      draw_line(
        x =
          c(0.335,
            0.665),
        y =
          c(0.695,
            0.695),
        color = "white",
        size = 0.75
      ) +
      draw_plot(
        bees,
        x = 0.419,
        y = 0.64,
        height = 0.015,
        width = 0.24
      ) +
      draw_image(
        league_logo$league.logo,
        x = league_logo$x,
        y = league_logo$y,
        scale = league_logo$scale
      ) + #add league logo
      draw_image(
        "https://raw.githubusercontent.com/etmckinley/goals_added_wheels/main/asa_icon_white.png",
        x = .46,
        y = -.448,
        scale = .1
      ) #add watermark
    
    ggsave(
      plot = p1,
      filename = paste0(
        save_folder,
        league_select,
        "/",
        plot.data$player.team.season[1],
        ".png"
      ),
      width = 6.5,
      height = 6,
      bg = "#15202B"
      
    )
  }
}

#initialize multisession
plan(multisession, workers = workers)

#generate plots
plots = value_aa %>%
  filter(Season %in% seasons_select,
         Min >= minute_limit) %>%
  ungroup() %>%
  split(.$player.team.season) %>%
  future_walk(., save_value_aa, .progress = TRUE)

#close multisession
plan(sequential)
