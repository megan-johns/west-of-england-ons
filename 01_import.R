# ============================================================
# ONS EXPLORE LOCAL STATISTICS
# IMPORT + FILTER + METADATA + CHART FUNCTION
# ============================================================


# 1. LOAD PACKAGES
library(tidyverse)
library(readxl)
library(ggiraph)


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


# 5. FORMATTING HELPERS

# Monthly series show month and year; annual series show the year only

format_period <- function(period) {

  gaps <- as.numeric(diff(sort(unique(period))))

  if (length(gaps) > 0 && median(gaps) < 200) {
    format(period, "%b %Y")
  } else {
    format(period, "%Y")
  }
}


# Precision is set per indicator so small rates keep their decimals and
# large counts are not cluttered with them

format_value <- function(value) {

  if (all(value == round(value))) {
    accuracy <- 1
  } else {
    typical <- median(abs(value[value != 0]))
    accuracy <- 10^(floor(log10(typical)) - 2)
    accuracy <- min(max(accuracy, 0.001), 1)
  }

  scales::number(
    value,
    accuracy = accuracy,
    big.mark = ","
  )
}


get_indicator_data <- function(indicator_name) {

  english_las %>%
    filter(
      indicator == indicator_name,
      indicator != "Population by age and sex",
      !is.na(value)
    ) %>%
    mutate(
      period_label = format_period(period),
      value_label = format_value(value)
    )
}


# 6. PLOT FUNCTION

plot_indicator <- function(indicator_name, unit = NA_character_) {

  plot_data <- get_indicator_data(indicator_name)

  unit_label <- if (is.na(unit) || unit == "") "" else paste0(" ", unit)

  number_of_periods <- n_distinct(plot_data$period)

  if (number_of_periods > 1) {

    highlighted_data <- plot_data %>%
      filter(
        highlighted != "Other local authorities"
      ) %>%
      mutate(
        tooltip = paste0(
          "<b>", htmltools::htmlEscape(areanm), "</b><br>",
          period_label, "<br>",
          value_label, htmltools::htmlEscape(unit_label)
        ),
        point_id = paste(areacd, period)
      )

    ggplot(
      plot_data,
      aes(
        x = period,
        y = value,
        group = areacd,
        colour = highlighted
      )
    ) +

      # Background lines stay static to keep the SVG small
      geom_line(
        data = filter(
          plot_data,
          highlighted == "Other local authorities"
        ),
        colour = "grey75",
        linewidth = 0.4,
        alpha = 0.6
      ) +

      geom_line_interactive(
        data = highlighted_data,
        aes(
          data_id = highlighted
        ),
        linewidth = 1.2
      ) +

      # Invisible points carry the hover values; one appears on hover
      geom_point_interactive(
        data = highlighted_data,
        aes(
          tooltip = tooltip,
          data_id = point_id
        ),
        size = 2.5,
        alpha = 0,
        show.legend = FALSE
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
      
      theme_minimal(base_family = "Arial")

  } else {

    single_period_data <- plot_data %>%
      arrange(value) %>%
      mutate(
        position = row_number(),
        tooltip = paste0(
          "<b>", htmltools::htmlEscape(areanm), "</b><br>",
          period_label, "<br>",
          value_label, htmltools::htmlEscape(unit_label), "<br>",
          "Rank ", n() - position + 1, " of ", n(), " (highest = 1)"
        )
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
      
      geom_col_interactive(
        data = filter(
          single_period_data,
          highlighted != "Other local authorities"
        ),
        aes(
          fill = highlighted,
          tooltip = tooltip,
          data_id = areacd
        ),
        width = 0.85
      ) +

      geom_point_interactive(
        data = filter(
          single_period_data,
          highlighted != "Other local authorities"
        ),
        aes(
          colour = highlighted,
          tooltip = tooltip,
          data_id = areacd
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
      
      theme_minimal(base_family = "Arial") +

      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  }
}


# 7. INTERACTIVE WRAPPER

# ggiraph bundles web fonts by default (~19 MB). Arial is used for text
# measurement but not shipped; browsers use the system copy or fall back.

system_fonts <- gdtools::font_set(sans = "Arial")
system_fonts$dependencies <- list()

interactive_indicator <- function(indicator_name, unit = NA_character_) {

  girafe(
    ggobj = plot_indicator(indicator_name, unit),
    width_svg = 8,
    height_svg = 4.5,
    font_set = system_fonts,
    options = list(
      opts_hover(
        css = girafe_css(
          css = "",
          line = "stroke-width:2.5px;",
          point = "fill-opacity:1;stroke-opacity:1;",
          area = "stroke:#222;stroke-width:1px;"
        )
      ),
      opts_tooltip(
        css = paste0(
          "background:#fff;color:#222;padding:6px 8px;",
          "border-radius:4px;box-shadow:0 1px 4px rgba(0,0,0,.25);",
          "font-family:Arial,sans-serif;font-size:13px;"
        ),
        opacity = 0.95
      ),
      opts_toolbar(
        saveaspng = TRUE,
        pngname = "chart"
      ),
      opts_sizing(
        rescale = TRUE
      )
    )
  )
}


# 8. DATA TABLE FOR THE FOUR WEST OF ENGLAND AUTHORITIES

indicator_table <- function(indicator_name) {

  table_data <- get_indicator_data(indicator_name)

  number_of_las <- n_distinct(table_data$areacd)

  table_data <- table_data %>%
    mutate(
      rank = min_rank(desc(value)),
      .by = period
    ) %>%
    filter(
      highlighted != "Other local authorities"
    )

  if (n_distinct(table_data$period) > 1) {

    # Latest period first; one column per authority
    wide_table <- table_data %>%
      arrange(
        desc(period),
        highlighted
      ) %>%
      select(
        Period = period_label,
        highlighted,
        value_label
      ) %>%
      pivot_wider(
        names_from = highlighted,
        values_from = value_label,
        values_fill = "-"
      )

    knitr::kable(
      wide_table,
      align = c("l", rep("r", ncol(wide_table) - 1))
    )

  } else {

    table_data %>%
      arrange(rank) %>%
      transmute(
        `Local authority` = areanm,
        Period = period_label,
        Value = value_label,
        Rank = paste(rank, "of", number_of_las)
      ) %>%
      knitr::kable(
        align = c("l", "l", "r", "r")
      )
  }
}


# 9. METADATA SHEETS

metadata_sheets <- excel_sheets(
  "data/all-datasets_meta.xlsx"
)


# 10. READ ONE METADATA SHEET

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


# 11. BUILD METADATA TABLE

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


# 12. KEEP METADATA FOR ENGLISH LA INDICATORS

metadata_english <- metadata %>%
  semi_join(
    english_las %>%
      distinct(indicator),
    by = "indicator"
  )

