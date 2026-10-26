# =========================================
# 1.9.1 Wrangling and plotting in parallel ####
# =========================================

# Challenge: Visualizing body size and shape 

# Summary table 
# Calculate mean body mass for each species and island 
# Use; pipe data to group_by() and summarise()
summary_table <- penguins %>%
  group_by(species, island) %>%
  summarise(
    sample_size = n(), # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE), # Calculate mean ignoring missing cells
    sd_mass_g = sd(body_mass_g, na.rm = TRUE) # Standard deviation calculation
  )
summary_table
# Visualization 
# use; pipe same dataset into ggplot();
# create boxplot; body_mass_g by species
# island map to fill or facet_wrap()
penguins %>%
  ggplot(aes(
    x = Species, 
    y = body_mass_g, 
    fill = island)) +
  geom_boxplot() +
  labs(
    x = "Penguin Species", 
    y = "Body Mass (g)", 
    fill = "island"
  ) +
  theme_minimal

# =========================================
# 1.9.2 Piping directly to visualization ####
# =========================================

# Piping summarise to ggplot()
# Pipe directly from aggregation to plotting with error bars
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - sd_mass,
                    ymax = mean_mass + sd_mass), 
                width = 0.2) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard deviation", 
       y = "Mean Body Mass",
       x = "Species") +
  theme_minimal()

mass_compare_plot

# Create a new plot 
# swapping sd_mass (standard deviation) for standard error; 
# divide sd by the square root of n (sd/sqrt(n))

mass_compare_plot_se <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = sum(!is.na(body_mass_g)),
    se_mass = sd_mass/sqrt(n),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - se_mass,
                    ymax = mean_mass + se_mass), 
                width = 0.2) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard error", 
       y = "Mean Body Mass",
       x = "Species") +
  theme_minimal()

mass_compare_plot_se

# =========================================
# 1.10 Saving, exporting, and version milestone ####
# =========================================

# Create output directories if they don't exist 
if (!dir.exists("outputs/figures")) dir.create("outputs/figures") # folder for figs
if (!dir.exists("outputs/tables")) dir.create("outputs/tables") # folder for tables
if (!dir.exists("Rdata")) dir.create("Rdata") # folder for Rdata objects

# From Challenge 1 and 2 

# Tables 
# CSV; colleborators that dont use R
# Exporting summary table as  universal text file (CSV)
write_csv(summary_table,"outputs/tables/penguin_species_summary_table.csv")

# RDS object; preserves my exact vector configs (i.e. numeric type definitions, col type)
saveRDS(summary_table, "outputs/tables/penguin_species_summary_table.rds")

# Figures
# ggplot built-function; ggsave()
ggsave("outputs/figures/mass_boxplot.png",
       plot = penguins_boxplot,
       width = 120, 
       height = 120,
       units = "mm",
       dpi = 300)

# From Challenge; last two

# Tables 
# CSV; colleborators that dont use R
# Exporting summary table as  universal text file (CSV)
write_csv(mass_summary_table_se,"outputs/tables/penguin_species_mass_summary.csv")

# RDS object; preserves my exact vector configs (i.e. numeric type definitions, col type)
saveRDS(mass_summary_table_se, "outputs/tables/penguin_species_mass_summary.rds")

# Figures
# ggplot built-function; ggsave()
ggsave("outputs/figures/mass_compare_plot.png",
       plot = mass_compare_plot_se,
       width = 120, 
       height = 120,
       units = "mm",
       dpi = 300)

