year <- c(2000:2024)
time <- data.frame(year)
time$rec <- 1
year <- c(2024)
time1 <- data.frame(year)
time1$rec <- 2
time <- rbind(time, time1)
time$record <- ifelse(time$year <= 2019 & time$year >= 2002, 235,
                      ifelse(time$year > 2019 & time$year <= 2021, 280,
                             ifelse(time$year > 2021 & time$year <= 2023, 470,
                                    ifelse(time$year == 2024 & time$rec <= 1, 500, 
                                           ifelse(time$year == 2024 & time$rec == 2, 805, 0 )))))

time$label <- ifelse(time$year == 2002, "Milene Domingues",
                      ifelse(time$year == 2020, "Pernille Harder",
                             ifelse(time$year == 2022, "Keira Walsh",
                                    ifelse(time$year == 2024 & time$rec == 1, "Mayra Ramírez",
                                           ifelse(time$year == 2024 & time$rec == 2, "Racheal Kundananji" , "" )))))

logo_91 <- readPNG ("C:/Users/johan/Documents/91st/png/logo-black.png")

plot <- 
ggplot(time, aes(x = year + rec/10, y = record)) + 
  geom_line (size = 1) + 
  annotation_raster(logo_91, xmin = 2022, xmax = 2027, ymin = 50, ymax = 350) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(2000, 2027)) +
  ggtitle("Transfer record sum in women's football\n(Thousand €)") + 
  geom_text(aes(x = year, y = record, label = label), hjust=00, vjust=-1.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(family = "serif"),
        axis.title = element_blank(),
        legend.text = element_text(family = "serif"),
        plot.title = element_text(family = "serif", size = 18, hjust = .5))

ggsave(filename="C:/Users/johan/Documents/91st/transfers.png",
       plot=plot, width = 40, height = 28, unit ="cm", dpi = 300)