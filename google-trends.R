# Use development version, instead of CRAN version
# devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)

state_abb_us <- paste0('US-', state.abb)
df <- gtrends(keyword = "rsv", geo = state_abb_us[1],
              time = "2020-01-01 2025-01-21",
              category = 0, gprop='web')
