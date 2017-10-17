remove(list = ls())
library(animation)

game <- sportvu_df("~/Desktop/SportVU Data/0021500411.json")
pbp <- NBAapi::get_pbp(GameID = "0021500411", StartPeriod = "1" ,EndPeriod = "10")
pbp2 <- convert_time(pbp)

shots_only <- extract_shots(game, pbp2)

plt_df <- shots_only
plt_df$game <- filter(plt_df$game, event_id == 2)

pdf("event_tmp.pdf", width = 10, height = 7)
plot_fullcourt()
NBAsportvu:::plot_shot(plt_df, static = T)
dev.off()

ani.options(ani.width=1000, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveGIF({
  for (i in 1:nrow(gif_df)) {
    plot_fullcourt()
    NBAsportvu:::plot_shot(plt_df, loop = i, static = F)
  }
}, movie.name = paste0("event_gif_tmp",".gif"))
