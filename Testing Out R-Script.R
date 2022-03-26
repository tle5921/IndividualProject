library(fpp3)

# Path where data is
file_path <- "~/BAS 475 Spring T TH/multiTimeline3.0.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- ymd(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$Interest <- as.numeric(
  ifelse(g_trends$Interest == "<1", 0, g_trends$Interest)
)

autoplot(g_trends)
