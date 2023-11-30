library(tidyverse)
library(nflverse)

#from intro article
library(nflplotR)
library(ggplot2)
#library(gt)
library(nflreadr)
library(dplyr, warn.conflicts = FALSE)
options(nflreadr.verbose = FALSE)

#demo code
pbp <- nflreadr::load_pbp(2022) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) %>%
  dplyr::filter(plays > 200) %>%
  dplyr::slice_max(qb_epa, n = 10)

combined <- offense %>%
  dplyr::inner_join(defense, by = "team")

qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) %>%
  dplyr::filter(plays > 200) %>%
  dplyr::slice_max(qb_epa, n = 10)

#stealing sample graphs

#EPA scatter plot
ggplot2::ggplot(combined, aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = off_epa , y0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2022 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  ggplot2::scale_y_reverse()

#EPA bar Graph
ggplot2::ggplot(defense, aes(x = team, y = off_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2022 NFL Offensive EPA per Play",
    y = "Offense EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = nflplotR::element_nfl_logo(size = 1)
  )

#horizontal bar graph

ggplot2::ggplot(defense, aes(y = team, x = def_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2022 NFL Defensive EPA per Play",
    x = "Defense EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the y-axis is so we remove the title
    axis.title.y = ggplot2::element_blank())
    # this line triggers the replacement of team abbreviations with logos
    #axis.text.y = nflplotR::element_nfl_logo(color = "b/w", size = 1))

#Player EPA graphs
ggplot2::ggplot(qbs, aes(x = reorder(team, -qb_epa), y = qb_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::geom_nfl_headshots(aes(player_gsis = id), width = 0.075, vjust = 0.45) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2020 NFL Quarterback EPA per Play",
    y = "EPA/play"
  ) +
  ggplot2::ylim(0, 0.4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = nflplotR::element_nfl_logo(size = 1)
  )

