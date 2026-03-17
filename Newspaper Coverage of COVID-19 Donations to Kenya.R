#Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

#Read Excel file
dataframe2 <- read_excel("C:/Users/lenovo/Downloads/Newspaper Counts.xlsx")

# PART 1: Monthly counts by outlet

# Prepare data
dataframe_prepared <- dataframe2 %>%
  mutate(
    month_year = floor_date(DATE, unit = "month"),
    total_articles = 1
  )

# Aggregate monthly totals per outlet
monthly_based_data <- dataframe_prepared %>%
  group_by(outlet, month_year) %>%
  summarise(total_articles = sum(total_articles), .groups = "drop")

# Compute totals per outlet
outlet_totals <- monthly_based_data %>%
  group_by(outlet) %>%
  summarise(total_articles_outlet = sum(total_articles), .groups = "drop")

# Compute grand total
grand_total <- sum(outlet_totals$total_articles_outlet)

# Create legend labels
monthly_based_data <- monthly_based_data %>%
  left_join(outlet_totals, by = "outlet") %>%
  mutate(outlet_label = paste0(outlet, " (", total_articles_outlet, ")"))

# Plot 1
Revised_plot_3 <- ggplot(
  monthly_based_data,
  aes(x = month_year, y = total_articles,
      color = outlet_label,
      group = outlet_label)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0,10), breaks = 0:10) +
  scale_x_datetime(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    limits = c(min(monthly_based_data$month_year),
               as.POSIXct("2023-03-31"))
  ) +
  labs(
    title = "MONTHLY COUNTS OF INCLUDED ARTICLES PER OUTLET",
    x = "Month-Year",
    y = "Total Articles",
    color = paste0("Outlet (Total Articles)\nGrand Total: ", grand_total)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

print(Revised_plot_3)

ggsave(
  filename = "C:/Users/lenovo/Downloads/Revised_monthly_articles.png",
  plot = Revised_plot_3,
  width = 12,
  height = 6,
  dpi = 300
)

# PART 2: Monthly totals (all outlets)

monthly_totals <- dataframe2 %>%
  filter(DATE >= as.Date("2020-08-01"),
         DATE <= as.Date("2023-03-31")) %>%
  mutate(month_year = floor_date(DATE, "month")) %>%
  count(month_year) %>%
  arrange(month_year)

# Create monthly labels
labels_vector <- monthly_totals %>%
  mutate(label = paste0(format(month_year, "%b %Y"), ": ", n)) %>%
  pull(label)

# Split into two lines
mid_point <- ceiling(length(labels_vector) / 2)

line1 <- paste(labels_vector[1:mid_point], collapse = "   |   ")
line2 <- paste(labels_vector[(mid_point + 1):length(labels_vector)],
               collapse = "   |   ")

aggregate_strip <- paste0(line1, "\n", line2)

# Plot 2
monthly_plot2 <- ggplot(monthly_totals,
                        aes(x = month_year, y = n)) +
  geom_line(linewidth = 1.3, color = "steelblue") +
  geom_point(size = 3, color = "maroon") +
  scale_x_date(
    limits = c(as.Date("2020-08-01"), as.Date("2023-03-31")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30,5),
    minor_breaks = seq(0,30,1)
  ) +
  labs(
    title = "Total Newspaper Articles Per Month",
    subtitle = paste0(
      "All outlets combined (Aug 2020 – Mar 2023)\n",
      aggregate_strip
    ),
    x = "Month-Year",
    y = "Number of Articles"
  ) +
  coord_cartesian(ylim = c(0,30)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 9,
      lineheight = 1.1,
      color = "maroon"
    ),
    axis.title.x = element_text(color = "navy", face = "bold", size = 12),
    axis.title.y = element_text(color = "navy", face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey75", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.3)
  )

print(monthly_plot2)

ggsave(
  filename = "C:/Users/lenovo/Downloads/monthly_plot2.png",
  plot = monthly_plot2,
  width = 12,
  height = 6,
  dpi = 300
)