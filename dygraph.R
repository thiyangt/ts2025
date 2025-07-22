install.packages("dygraphs")
library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector()

# install.packages(calendR)
library(calendR)

# Data
set.seed(2)
data <- rnorm(365)

# Calendar
calendR(year = 2021,
        special.days = data,
        gradient = TRUE,
        low.col = "#FCFFDD",
        special.col = "#00AAAE",
        legend.pos = "right",
        legend.title = "Title")