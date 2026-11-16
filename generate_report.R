# Pipeline Activity Calendar — Self-Contained HTML Report Generator
# =================================================================
# Usage:  Rscript generate_report.R
# Output: Pipeline_Calendar.html (single file, no server needed)
#
# INPUT FILE (the only file you edit to update the dashboard):
#
#   Pipeline_Calendar_Data.csv
#
#   This CSV has two sections, identified by the "Section" column:
#
#   HANDOFF rows — columns used:
#     Group, Region, Activity, Start_Month (1-12), End_Month (1-12), Note, Remarks
#
#   PIPELINE rows — columns used:
#     Workflow, Category, Event, Col_Seq, Month, Year
#
#   To add a new handoff entry: add a row with Section=HANDOFF and fill
#   Group, Region, Activity, Start_Month, End_Month. Other columns optional.
#
#   To add a pipeline event: add a row with Section=PIPELINE and fill
#   Workflow, Category, Event, Col_Seq, Month, Year.
# =================================================================

required <- c("tidyverse", "lubridate", "scales", "base64enc",
              "DT", "htmlwidgets", "plotly", "htmltools")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(lubridate)
library(scales)
library(base64enc)
library(DT)
library(htmlwidgets)
library(plotly)
library(htmltools)

# ============================================================
# Constants
# ============================================================
INPUT_FILE <- "Pipeline_Calendar_Data.csv"

ACTIVITY_COLORS <- c(
  "Trait Handoff"   = "#2a78d6",
  "Planting"        = "#1baf7a",
  "Harvesting"      = "#eb6834",
  "Advancement"     = "#eda100",
  "HMU/SPR Handoff" = "#e87ba4"
)

ACTIVITY_ORDER <- c("Trait Handoff", "Planting", "Harvesting",
                     "Advancement", "HMU/SPR Handoff")

PIPELINE_COLORS <- c("HMU" = "#2a78d6", "Testing" = "#1baf7a", "Workflow" = "#6250d6")

cur_date       <- Sys.Date()
cur_month      <- month(cur_date)
cur_month_name <- month.abb[cur_month]
next_month     <- (cur_month %% 12) + 1
next2_month    <- (next_month %% 12) + 1

# ============================================================
# Load and split the single input file
# ============================================================
all_data <- read_csv(INPUT_FILE, show_col_types = FALSE)

ho_data <- all_data %>%
  filter(Section == "HANDOFF") %>%
  transmute(
    region       = Region,
    activity     = Activity,
    start_month  = as.integer(Start_Month),
    end_month    = as.integer(End_Month),
    note         = replace_na(Note, ""),
    remarks      = replace_na(Remarks, ""),
    group        = Group
  )

pipeline_data <- all_data %>%
  filter(Section == "PIPELINE") %>%
  transmute(
    workflow = Workflow,
    category = Category,
    activity = Event,
    col_seq  = as.integer(Col_Seq),
    month    = Month,
    year     = Year
  )

# Build pipeline axis ticks from the data
fiscal_months <- c("Apr","May","Jun","Jul","Aug","Sep",
                    "Oct","Nov","Dec","Jan","Feb","Mar")
year_labels <- sort(unique(pipeline_data$year))

pipeline_ticks <- pipeline_data %>%
  select(col_seq, month, year) %>%
  distinct() %>%
  arrange(col_seq)

# Display names
display_name <- function(region) {
  region <- gsub("Philippines \\(ME1 & 3\\)", "Philippines ME1 & ME3", region)
  region <- gsub("Indonesia \\(Dry_Flat_ Undulated_DM\\)", "Indonesia Dry/Flat", region)
  region <- gsub("Indonesia \\(Wet_Slopy_ Undulated_ID\\)", "Indonesia Wet/Slopy", region)
  region <- gsub("Pak_ME4_Autumn_Yellow", "Pakistan ME4 Autumn", region)
  region <- gsub("Pak_ME6_Spring", "Pakistan ME6 Spring", region)
  region <- gsub("Thailand \\(Flat & Hilly\\)", "Thailand Flat & Hilly", region)
  region <- gsub("India ME4, 5 & 6", "India ME4/5/6", region)
  region
}
ho_data$display_name <- display_name(ho_data$region)

# ============================================================
# Summary stats
# ============================================================
now_acts <- ho_data %>%
  filter(start_month <= cur_month, end_month >= cur_month)

soon_acts <- ho_data %>%
  filter(start_month %in% c(next_month, next2_month))

n_regions   <- length(unique(ho_data$region))
n_now       <- nrow(now_acts)
n_upcoming  <- nrow(soon_acts)
n_handoffs  <- now_acts %>% filter(grepl("Handoff", activity)) %>% nrow()

# ============================================================
# Build Handoff Calendar Gantt (plotly)
# ============================================================
build_ho_gantt <- function(df) {
  df$y_label <- paste0(df$display_name, "  |  ", df$activity)

  region_ord <- unique(df$display_name)
  y_levels <- character(0)
  for (r in region_ord) {
    acts <- df$activity[df$display_name == r] %>% unique()
    acts <- acts[order(match(acts, ACTIVITY_ORDER))]
    y_levels <- c(y_levels, paste0(r, "  |  ", acts))
  }
  df$y_label <- factor(df$y_label, levels = y_levels)
  df <- df[!is.na(df$y_label), ]

  df$duration <- df$end_month - df$start_month + 1
  df$hover <- paste0(
    "<b>", df$display_name, "</b><br>",
    "<b>", df$activity, "</b>: ",
    month.abb[df$start_month], " – ", month.abb[df$end_month],
    ifelse(df$note != "", paste0("<br><i>", df$note, "</i>"), "")
  )

  p <- plot_ly()
  for (act in ACTIVITY_ORDER) {
    d <- df[df$activity == act, ]
    if (nrow(d) == 0) next
    p <- p %>% add_bars(
      data = d,
      y = ~y_label, x = ~duration, base = ~start_month - 0.5,
      orientation = "h",
      marker = list(color = ACTIVITY_COLORS[act],
                    line = list(color = "#fcfcfb", width = 0.5)),
      name = act, text = ~hover, hoverinfo = "text"
    )
  }

  n_rows <- length(unique(df$y_label))

  p %>% layout(
    barmode = "overlay",
    xaxis = list(title = "", tickvals = 1:12, ticktext = month.abb,
                 range = c(0.3, 12.7), gridcolor = "#e5e5e5",
                 zeroline = FALSE, tickfont = list(size = 12)),
    yaxis = list(title = "", categoryorder = "array",
                 categoryarray = rev(levels(df$y_label)),
                 tickfont = list(size = 11)),
    shapes = list(list(type = "line", layer = "above",
                       x0 = cur_month, x1 = cur_month,
                       y0 = -0.5, y1 = n_rows - 0.5,
                       line = list(color = "#C62828", width = 2, dash = "dash"))),
    annotations = list(list(x = cur_month, y = 1.04, yref = "paper",
                            xanchor = "center",
                            text = paste0("<b>", cur_month_name, " (today)</b>"),
                            showarrow = FALSE,
                            font = list(color = "#C62828", size = 11))),
    legend = list(orientation = "h", y = -0.06, x = 0.5,
                  xanchor = "center", font = list(size = 12)),
    margin = list(l = 250, b = 60, t = 40, r = 20),
    plot_bgcolor = "#fcfcfb", paper_bgcolor = "#fcfcfb",
    hoverlabel = list(align = "left", font = list(size = 12))
  )
}

ho_gantt <- build_ho_gantt(ho_data)

# ============================================================
# Build Pipeline Gantt (plotly)
# ============================================================
build_pipeline_gantt <- function(df, ticks) {
  df$hover <- paste0(
    "<b>", df$workflow, "</b><br>",
    df$activity, "<br>",
    df$month, " (", df$year, ")"
  )

  p <- plot_ly()
  for (cat in names(PIPELINE_COLORS)) {
    d <- df[df$category == cat, ]
    if (nrow(d) == 0) next
    p <- p %>% add_bars(
      data = d,
      y = ~workflow, x = 1, base = ~col_seq - 0.5,
      orientation = "h",
      marker = list(color = PIPELINE_COLORS[cat],
                    line = list(color = "#fcfcfb", width = 0.5)),
      name = cat, text = ~hover, hoverinfo = "text"
    )
  }

  yr_starts <- ticks %>%
    group_by(year) %>%
    summarise(s = min(col_seq), .groups = "drop")

  shapes <- lapply(yr_starts$s, function(s) {
    list(type = "line", x0 = s - 0.5, x1 = s - 0.5,
         y0 = -0.5, y1 = length(unique(df$workflow)),
         line = list(color = "#bbb", width = 1, dash = "dot"))
  })

  yr_annots <- lapply(seq_len(nrow(yr_starts)), function(i) {
    list(x = yr_starts$s[i] + 5, y = 1.06, yref = "paper",
         text = paste0("<b>", yr_starts$year[i], "</b>"),
         showarrow = FALSE, font = list(size = 11, color = "#555"),
         xanchor = "center")
  })

  tick_sub <- ticks[seq(1, nrow(ticks), by = 3), ]

  p %>% layout(
    barmode = "overlay",
    xaxis = list(title = "", tickvals = tick_sub$col_seq,
                 ticktext = tick_sub$month, tickangle = -45,
                 gridcolor = "#e5e5e5", tickfont = list(size = 10)),
    yaxis = list(title = "", autorange = "reversed",
                 tickfont = list(size = 10)),
    shapes = shapes, annotations = yr_annots,
    legend = list(orientation = "h", y = -0.12, x = 0.5,
                  xanchor = "center", font = list(size = 12)),
    margin = list(l = 320, b = 80, t = 40, r = 20),
    plot_bgcolor = "#fcfcfb", paper_bgcolor = "#fcfcfb",
    hoverlabel = list(align = "left", font = list(size = 12))
  )
}

pipeline_gantt <- build_pipeline_gantt(pipeline_data, pipeline_ticks)

# ============================================================
# Build DT: Handoff detail table
# ============================================================
ho_table_data <- ho_data %>%
  mutate(
    Period = paste0(month.abb[start_month], " – ", month.abb[end_month]),
    Note   = ifelse(note == "", "—", note)
  ) %>%
  select(Group = group, Region = display_name, Activity = activity,
         Period, Note, Remarks = remarks) %>%
  arrange(Group, Region, match(Activity, ACTIVITY_ORDER))

ho_dt <- datatable(
  ho_table_data, rownames = FALSE, filter = "top",
  extensions = "Buttons",
  options = list(dom = "Bfrtip", buttons = list("csv", "excel"),
                 pageLength = 25, autoWidth = TRUE,
                 columnDefs = list(
                   list(width = "100px", targets = c(0)),
                   list(width = "160px", targets = c(1)),
                   list(width = "120px", targets = c(2, 3)),
                   list(width = "300px", targets = c(5))
                 )),
  class = "display compact stripe"
) %>%
  formatStyle("Activity",
    backgroundColor = styleEqual(names(ACTIVITY_COLORS),
                                  paste0(ACTIVITY_COLORS, "22")),
    fontWeight = "bold")

# ============================================================
# Build DT: Pipeline detail table
# ============================================================
pl_table_data <- pipeline_data %>%
  select(Workflow = workflow, Category = category,
         Activity = activity, Month = month, Year = year) %>%
  arrange(Workflow, Year, match(Month, month.abb))

pl_dt <- datatable(
  pl_table_data, rownames = FALSE, filter = "top",
  extensions = "Buttons",
  options = list(dom = "Bfrtip", buttons = list("csv", "excel"),
                 pageLength = 25, autoWidth = TRUE),
  class = "display compact stripe"
) %>%
  formatStyle("Category",
    backgroundColor = styleEqual(names(PIPELINE_COLORS),
                                  paste0(PIPELINE_COLORS, "22")),
    fontWeight = "bold")

# ============================================================
# Reminder cards HTML
# ============================================================
make_reminder_html <- function(df, tag) {
  if (nrow(df) == 0) return("")
  cards <- df %>%
    rowwise() %>%
    mutate(html = paste0(
      '<div style="padding:8px 14px;margin:5px 0;border-radius:5px;',
      'border-left:5px solid ', ACTIVITY_COLORS[activity], ';background:#fff;">',
      '<strong>[', tag, '] ', display_name, ' &mdash; ', activity, '</strong>',
      '<span style="color:#666;margin-left:8px;">',
      month.abb[start_month], '&ndash;', month.abb[end_month],
      ifelse(note != "", paste0('  (', note, ')'), ''),
      '</span></div>'
    )) %>% pull(html)
  paste(cards, collapse = "\n")
}

now_html  <- make_reminder_html(now_acts, "NOW")
soon_html <- make_reminder_html(soon_acts, "SOON")

reminders_html <- paste0(
  if (n_now > 0) paste0(
    '<h4 style="color:#C62828;margin-top:0;">Active Now (', cur_month_name, ')</h4>\n',
    now_html, '\n') else "",
  if (n_upcoming > 0) paste0(
    '<h4 style="color:#e65100;margin-top:12px;">Coming Up (',
    month.abb[next_month], '&ndash;', month.abb[next2_month], ')</h4>\n',
    soon_html, '\n') else "",
  if (n_now == 0 && n_upcoming == 0)
    '<p style="color:#888;">No current or upcoming activities.</p>' else ""
)

# ============================================================
# Remarks HTML
# ============================================================
remarks_df <- ho_data %>%
  filter(remarks != "") %>%
  select(display_name, remarks) %>% distinct()

remarks_html <- if (nrow(remarks_df) > 0) {
  paste(apply(remarks_df, 1, function(r) {
    paste0(
      '<div style="margin-bottom:12px;padding:10px 14px;background:#fff;',
      'border-radius:5px;border-left:4px solid #2a78d6;">',
      '<strong>', r["display_name"], '</strong>',
      '<p style="color:#555;margin:4px 0 0;">', r["remarks"], '</p></div>')
  }), collapse = "\n")
} else '<p style="color:#888;">No remarks.</p>'

pipeline_notes_html <- '
<ul style="margin:0;padding-left:20px;color:#555;">
  <li>Pre-commercial team will plant both versions</li>
  <li>IBD to be considered for primary version nomination but region can decide</li>
  <li>Connect with SAF to confirm EQ testing</li>
  <li>Decision on EQ for primary nomination based on other region experiences</li>
  <li>TI will ship the available version instead of 2 versions</li>
</ul>'

# ============================================================
# CSS + JS
# ============================================================
css <- '
* { box-sizing: border-box; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  background: #f0f2f5; color: #333; margin: 0; padding: 0;
}
.header {
  background: linear-gradient(135deg, #1a3a5c 0%, #2a78d6 100%);
  color: #fff; padding: 28px 32px 20px; margin-bottom: 24px;
}
.header h1 { margin: 0 0 4px; font-size: 26px; font-weight: 700; }
.header p  { margin: 0; opacity: 0.85; font-size: 14px; }
.container { max-width: 1400px; margin: 0 auto; padding: 0 24px 40px; }
.stat-row  { display: flex; gap: 16px; margin-bottom: 24px; flex-wrap: wrap; }
.stat-card {
  flex: 1; min-width: 180px; background: #fff; border-radius: 8px;
  padding: 18px 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  border-top: 4px solid;
}
.stat-card .value { font-size: 32px; font-weight: 700; }
.stat-card .label { font-size: 13px; color: #666; margin-top: 2px; }
.section {
  background: #fff; border-radius: 8px; padding: 20px 24px; margin-bottom: 24px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
}
.section h2 {
  font-size: 17px; font-weight: 600; margin: 0 0 16px;
  padding-bottom: 10px; border-bottom: 2px solid #e5e7eb;
}
.section h2 .dot {
  display: inline-block; width: 10px; height: 10px; border-radius: 50%;
  margin-right: 8px; vertical-align: middle;
}
.tab-bar {
  display: flex; gap: 0; margin-bottom: 24px; background: #fff;
  border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
}
.tab-btn {
  flex: 1; padding: 14px; text-align: center; cursor: pointer;
  font-weight: 600; font-size: 14px; color: #666;
  border-bottom: 3px solid transparent; transition: all 0.2s;
}
.tab-btn:hover { background: #f8f9fa; }
.tab-btn.active { color: #2a78d6; border-bottom-color: #2a78d6; background: #f0f7ff; }
.tab-content { display: none; }
.tab-content.active { display: block; }
@media (max-width: 768px) {
  .stat-row { flex-direction: column; }
  .container { padding: 0 12px 24px; }
}
'

js <- '
function switchTab(tabId) {
  document.querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
  document.querySelectorAll(".tab-content").forEach(c => c.classList.remove("active"));
  document.querySelector("[data-tab=\'" + tabId + "\']").classList.add("active");
  document.getElementById(tabId).classList.add("active");
  window.dispatchEvent(new Event("resize"));
}
'

# ============================================================
# Assemble full HTML page
# ============================================================
page <- tagList(
  tags$html(lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("Pipeline Activity Calendar"),
      tags$style(HTML(css)),
      tags$script(HTML(js))
    ),
    tags$body(
      tags$div(class = "header",
        tags$h1("Pipeline Activity Calendar"),
        tags$p(paste0("Generated: ", format(cur_date, "%B %d, %Y"),
                       "  |  Data source: ", INPUT_FILE))
      ),

      tags$div(class = "container",
        tags$div(class = "stat-row",
          tags$div(class = "stat-card", style = "border-top-color:#2a78d6;",
            tags$div(class = "value", n_regions),
            tags$div(class = "label", "Regions Tracked")),
          tags$div(class = "stat-card", style = "border-top-color:#1baf7a;",
            tags$div(class = "value", n_now),
            tags$div(class = "label", paste(cur_month_name, "Activities"))),
          tags$div(class = "stat-card", style = "border-top-color:#eda100;",
            tags$div(class = "value", n_upcoming),
            tags$div(class = "label", "Upcoming (2 Months)")),
          tags$div(class = "stat-card", style = "border-top-color:#C62828;",
            tags$div(class = "value", n_handoffs),
            tags$div(class = "label", paste(cur_month_name, "Handoffs")))
        ),

        tags$div(class = "tab-bar",
          tags$div(class = "tab-btn active", `data-tab` = "tab-calendar",
                   onclick = "switchTab('tab-calendar')",
                   "\U0001F4C5  Handoff Calendar"),
          tags$div(class = "tab-btn", `data-tab` = "tab-pipeline",
                   onclick = "switchTab('tab-pipeline')",
                   "\U0001F9EC  Pipeline Workflows")
        ),

        # Tab 1: Handoff Calendar
        tags$div(id = "tab-calendar", class = "tab-content active",
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#C62828;"),
                    "Current & Upcoming Reminders"),
            HTML(reminders_html)),
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#2a78d6;"),
                    "Annual Handoff Timeline"),
            ho_gantt),
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#1baf7a;"),
                    "Handoff Detail Table"),
            tags$p(style = "color:#666;font-size:13px;margin:0 0 12px;",
                   "Use column filters to search and sort. Export via CSV/Excel buttons."),
            ho_dt),
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#6250d6;"),
                    "Remarks & Notes"),
            HTML(remarks_html))
        ),

        # Tab 2: Pipeline Workflows
        tags$div(id = "tab-pipeline", class = "tab-content",
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#2a78d6;"),
                    "Breeding Pipeline Timeline (Year 0 – Year 7)"),
            pipeline_gantt),
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#1baf7a;"),
                    "Pipeline Detail Table"),
            tags$p(style = "color:#666;font-size:13px;margin:0 0 12px;",
                   "Use column filters to search and sort. Export via CSV/Excel buttons."),
            pl_dt),
          tags$div(class = "section",
            tags$h2(tags$span(class = "dot", style = "background:#eda100;"),
                    "Key Notes"),
            HTML(pipeline_notes_html))
        )
      )
    )
  )
)

# ============================================================
# Write self-contained HTML (all dependencies inlined)
# ============================================================
output_file <- "Pipeline_Calendar.html"

tmp_dir  <- tempfile("report_")
dir.create(tmp_dir)
tmp_html <- file.path(tmp_dir, "report.html")
save_html(page, file = tmp_html)

html_lines <- readLines(tmp_html, warn = FALSE)
html_text  <- paste(html_lines, collapse = "\n")

inline_js <- function(html, base) {
  pattern <- '<script src="(lib/[^"]+\\.js)"[^>]*></script>'
  m <- gregexpr(pattern, html, perl = TRUE)
  matches <- regmatches(html, m)[[1]]
  for (tag in matches) {
    src <- sub('.*src="([^"]+)".*', '\\1', tag)
    fpath <- file.path(base, src)
    if (file.exists(fpath)) {
      content <- paste(readLines(fpath, warn = FALSE), collapse = "\n")
      html <- sub(tag, paste0('<script>', content, '</script>'), html, fixed = TRUE)
    }
  }
  html
}

inline_css <- function(html, base) {
  pattern <- '<link href="(lib/[^"]+\\.css)" rel="stylesheet"[^>]*/?>|<link rel="stylesheet" href="(lib/[^"]+\\.css)"[^>]*/?>'
  m <- gregexpr(pattern, html, perl = TRUE)
  matches <- regmatches(html, m)[[1]]
  for (tag in matches) {
    href <- sub('.*href="([^"]+\\.css)".*', '\\1', tag)
    fpath <- file.path(base, href)
    if (file.exists(fpath)) {
      content <- paste(readLines(fpath, warn = FALSE), collapse = "\n")
      html <- sub(tag, paste0('<style>', content, '</style>'), html, fixed = TRUE)
    }
  }
  html
}

html_text <- inline_js(html_text, tmp_dir)
html_text <- inline_css(html_text, tmp_dir)
writeLines(html_text, output_file)
unlink(tmp_dir, recursive = TRUE)

message("\n==========================================")
message("  Pipeline Calendar report generated!")
message("  Output: ", normalizePath(output_file))
message("  Size:   ", round(file.size(output_file) / 1024 / 1024, 1), " MB")
message("==========================================")
message("Input:  ", INPUT_FILE)
message("To update the dashboard:")
message("  1. Edit ", INPUT_FILE, " in Excel or any spreadsheet")
message("  2. Run:  Rscript generate_report.R")
message("  3. Share Pipeline_Calendar.html")
