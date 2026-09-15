# ============================================================
# ONS EXPLORE LOCAL STATISTICS
# IMPORT + FILTER + METADATA + CHART FUNCTION
# ============================================================


# 1. LOAD PACKAGES
library(tidyverse)
library(readxl)


# 2. SETTINGS

west_of_england_colours <- c(
  "B&NES" = "#590075",
  "Bristol" = "#CE132D",
  "North Somerset" = "#ED8073",
  "South Gloucestershire" = "#1D4F2B"
)


# 3. IMPORT MAIN ONS DATA

ons <- read_csv(
  "data/all-datasets.csv",
  show_col_types = FALSE
) %>%
  mutate(
    period = as.Date(period)
  )


# 4. KEEP ENGLISH LOCAL AUTHORITIES

english_las <- ons %>%
  filter(
    substr(areacd, 1, 3) %in% c(
      "E06",
      "E07",
      "E08",
      "E09"
    ),
    areacd != "E09000001"
  ) %>%
  mutate(
    highlighted = case_when(
      areanm == "Bath and North East Somerset" ~ "B&NES",
      areanm == "Bristol, City of" ~ "Bristol",
      areanm == "North Somerset" ~ "North Somerset",
      areanm == "South Gloucestershire" ~ "South Gloucestershire",
      TRUE ~ "Other local authorities"
    )
  )


# 5. PLOT FUNCTION

plot_indicator <- function(indicator_name) {
  
  plot_data <- english_las %>%
    filter(
      indicator == indicator_name,
      indicator != "Population by age and sex",
      !is.na(value)
    )
  
  number_of_periods <- n_distinct(plot_data$period)
  
  if (number_of_periods > 1) {
    
    ggplot(
      plot_data,
      aes(
        x = period,
        y = value,
        group = areacd,
        colour = highlighted
      )
    ) +
      
      geom_line(
        data = filter(
          plot_data,
          highlighted == "Other local authorities"
        ),
        colour = "grey75",
        linewidth = 0.4,
        alpha = 0.6
      ) +
      
      geom_line(
        data = filter(
          plot_data,
          highlighted != "Other local authorities"
        ),
        linewidth = 1.2
      ) +
      
      scale_colour_manual(
        values = west_of_england_colours
      ) +
      
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      
      labs(
        title = indicator_name,
        x = NULL,
        y = NULL,
        colour = NULL
      ) +
      
      theme_minimal()
    
  } else {
    
    single_period_data <- plot_data %>%
      arrange(value) %>%
      mutate(
        position = row_number()
      )
    
    ggplot(
      single_period_data,
      aes(
        x = position,
        y = value
      )
    ) +
      
      geom_col(
        fill = "grey80",
        width = 0.85
      ) +
      
      geom_col(
        data = filter(
          single_period_data,
          highlighted != "Other local authorities"
        ),
        aes(
          fill = highlighted
        ),
        width = 0.85
      ) +
      
      geom_point(
        data = filter(
          single_period_data,
          highlighted != "Other local authorities"
        ),
        aes(
          colour = highlighted
        ),
        size = 3
      ) +
      
      scale_fill_manual(
        values = west_of_england_colours
      ) +
      
      scale_colour_manual(
        values = west_of_england_colours
      ) +
      
      scale_x_continuous(
        breaks = NULL
      ) +
      
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      
      labs(
        title = indicator_name,
        subtitle = paste(
          "All English local authorities,",
          format(unique(single_period_data$period), "%Y")
        ),
        x = "English local authorities, ordered from lowest to highest",
        y = NULL,
        fill = NULL,
        colour = NULL
      ) +
      
      guides(
        fill = guide_legend(
          override.aes = list(
            colour = NA
          )
        ),
        colour = "none"
      ) +
      
      theme_minimal() +
      
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  }
}


# 6. METADATA SHEETS

metadata_sheets <- excel_sheets(
  "data/all-datasets_meta.xlsx"
)


# 7. READ ONE METADATA SHEET

read_metadata_sheet <- function(sheet_name) {
  
  if (!grepl("^[0-9]+$", sheet_name)) {
    return(NULL)
  }
  
  x <- read_excel(
    "data/all-datasets_meta.xlsx",
    sheet = sheet_name,
    col_names = FALSE,
    n_max = 10,
    .name_repair = "minimal"
  )
  
  indicator_raw <- x[[1]][1]
  description <- x[[1]][2]
  
  source_rows <- x[[1]][
    str_detect(
      x[[1]],
      "^Source"
    )
  ]
  
  source <- paste(
    source_rows,
    collapse = " | "
  )
  
  header_row_number <- which(
    x[[1]] == "Area code"
  )[1]
  
  unit <- NA_character_
  
  if (!is.na(header_row_number)) {
    
    header_values <- as.character(
      unlist(
        x[header_row_number, ]
      )
    )
    
    unit_headers <- header_values[
      str_detect(
        header_values,
        "\\([^\\)]+\\)$"
      )
    ]
    
    if (length(unit_headers) > 0) {
      
      unit <- unit_headers[1] %>%
        str_extract("\\([^\\)]+\\)$") %>%
        str_remove_all("[()]")
    }
  }
  
  tibble(
    sheet = sheet_name,
    indicator_raw = indicator_raw,
    description = description,
    source = source,
    unit = unit
  )
}


# 8. BUILD METADATA TABLE

metadata <- map_dfr(
  metadata_sheets,
  read_metadata_sheet
) %>%
  mutate(
    indicator = str_remove(
      indicator_raw,
      "\\s*\\[note\\s*\\d+\\]$"
    )
  ) %>%
  select(
    sheet,
    indicator,
    description,
    unit,
    source
  )


# 9. KEEP METADATA FOR ENGLISH LA INDICATORS

metadata_english <- metadata %>%
  semi_join(
    english_las %>%
      distinct(indicator),
    by = "indicator"
  )

