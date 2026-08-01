
library(ggplot2)
library(dplyr)
library(ggforce)

# Create data
skills <- data.frame(
    skill = c("R","Python","Bash","Git","SQL","HTML","CSS","JS"),
    level = c(1.00,0.9,0.8,0.8,0.45,0.25,0.25,0.15)
)

# Create remainder for stacked bar
skills <- skills %>%
    mutate(remainder = 1 - level) #  - level

# Convert to long format
skills_long <- skills %>%
    tidyr::pivot_longer(cols = c(level, remainder),
                        names_to = "type",
                        values_to = "value")

skills_long$skill <- factor(skills_long$skill, levels = rev(skills$skill))
skills_long$type <- factor(skills_long$type, levels = c("remainder","level"))

label_df <- skills_long %>%
    distinct(skill) %>%
    mutate(value = 0.01)

# Plot
plot <- ggplot(skills_long, aes(x = skill, y = value, fill = type)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_flip() +
    scale_fill_manual(values = c("level" = "gray55", "remainder" = "gray85")) +
    theme_minimal() +
    geom_text(
        data = label_df,
        aes(x = skill, y = value, label = skill),
        inherit.aes = FALSE,
        color = "white",
        size = 14,
        hjust = 0
    ) +
    theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
    )

ggsave("/Users/8yt/Documents/Personal/cv-master/Skillsv2.png",plot = plot)


# Ring layout
# skills <- data.frame(
#     skill = c("R","Python","Bash","Git","SQL","HTML","CSS","JS"),
#     level = c(1.00,0.9,0.8,0.8,0.45,0.25,0.25,0.15)
# )
# skills <- skills %>%
#     mutate(
#         ring_id = rev(seq_len(n())),
#         r0 = ring_id - 0.35,   # inner radius
#         r  = ring_id + 0.35,   # outer radius
#         start = 0,
#         end_full = 2 * pi,
#         end_level = 2 * pi * level
#     )
# 
# # Background rings
# bg_rings <- skills %>%
#     transmute(
#         skill, r0, r,
#         start = 0,
#         end = 2 * pi
#     )
# 
# # Filled rings
# fg_rings <- skills %>%
#     transmute(
#         skill, r0, r,
#         start = 0,
#         end = end_level
#     )
# 
# ggplot() +
#     geom_arc_bar(
#         data = bg_rings,
#         aes(x0 = 0, y0 = 0, r0 = r0, r = r, start = start, end = end),
#         fill = "grey85",
#         color = NA
#     ) +
#     geom_arc_bar(
#         data = fg_rings,
#         aes(x0 = 0, y0 = 0, r0 = r0, r = r, start = start, end = end),
#         fill = "grey55",
#         color = NA
#     ) +
#     coord_fixed() +
#     xlim(-10, 10) +
#     ylim(-10, 10) +
#     theme_void() +
#     theme(
#         plot.background = element_rect(fill = "transparent", color = NA),
#         panel.background = element_rect(fill = "transparent", color = NA)
#     )
# 
# 
# 
