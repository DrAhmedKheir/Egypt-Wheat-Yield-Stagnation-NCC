library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

base_dir <- "D:/MMENCC"
infile <- file.path(base_dir, "Fig3ANCC.xlsx")

out_dir <- file.path(base_dir, "Figure_MME_Yield_Phenology")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read_excel(infile) %>%
  mutate(
    Years = as.integer(Years),
    MMESim = as.numeric(MMESim),
    Sdev = as.numeric(Sdev),
    DaysToAnthesis = as.numeric(DaysToAnthesis),
    DaysToMaturity = as.numeric(DaysToMaturity)
  ) %>%
  filter(Years >= 1980, Years <= 2019)

x_breaks <- seq(1979, 2021, by = 2)

dark_green <- "#006400"
dark_brown <- "#6B3F00"

p1 <- ggplot(df, aes(x = Years)) +
  geom_ribbon(
    aes(ymin = MMESim - Sdev, ymax = MMESim + Sdev),
    fill = "grey70",
    alpha = 0.45
  ) +
  geom_line(
    aes(y = MMESim),
    color = dark_green,
    linewidth = 1.8
  ) +
  labs(
    title = "a)",
    x = NULL,
    y = expression("Simulated grain yield (t ha"^{-1}*")")
  ) +
  scale_x_continuous(
    limits = c(1979, 2021),
    breaks = x_breaks
  ) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, by = 4),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 16, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(linewidth = 0.9, color = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

p2 <- ggplot(df, aes(x = Years)) +
  geom_line(
    aes(y = DaysToAnthesis, linetype = "Days to anthesis"),
    color = dark_brown,
    linewidth = 1.7
  ) +
  geom_line(
    aes(y = DaysToMaturity, linetype = "Days to maturity"),
    color = dark_brown,
    linewidth = 1.7
  ) +
  labs(
    title = "b)",
    x = "Years",
    y = "Days",
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(1979, 2021),
    breaks = x_breaks
  ) +
  scale_linetype_manual(
    values = c(
      "Days to anthesis" = "solid",
      "Days to maturity" = "dotted"
    )
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 16, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_line(linewidth = 0.9, color = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    legend.position = "top",
    legend.text = element_text(size = 16, face = "bold", color = "black")
  )

fig <- p1 / p2 + plot_layout(heights = c(1, 1))

ggsave(
  file.path(out_dir, "Figure_MME_Yield_Phenology_FINAL32.png"),
  fig,
  width = 11,
  height = 9,
  dpi = 600
)

ggsave(
  file.path(out_dir, "Figure_MME_Yield_Phenology_FINAL.pdf"),
  fig,
  width = 11,
  height = 9
)