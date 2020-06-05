library(DataExplorer)

browseURL("https://www.rdocumentation.org/packages/DataExplorer/versions/0.8.1/topics/create_report")

setwd("C:/Users/ANALITICA/Desktop/AG/cookbook_gallery/quick_response")

base <- read_rds("datos_ejemplo.rds")


create_report(base, y = "tomo_credito" )


report <- 
create_report(
  mtcars,
  output_format = html_document(toc = TRUE, toc_depth = 6, theme = "yeti"),
  output_file = "report.html",
  output_dir = getwd(),
  y = "cyl",
  config = configure_report(),
  report_title = "Mtcars Reporte"
)

create_report(
  data = diamonds,
  # output_format = html_document(toc = TRUE, toc_depth = 6, theme = "flatly"),
  output_file = "report.html",
  output_dir = getwd(),
  y = "price",
  report_title = "Diamantes de Shiny-Sofia",
    config = configure_report(
    add_plot_prcomp = TRUE,
    add_plot_scatterplot = FALSE,
    add_plot_missing = FALSE,
    plot_qq_args = list("by" = "cut", sampled_rows = 1000L),
    plot_bar_args = list("with" = "carat"),
    plot_correlation_args = list("cor_args" = list("use" = "pairwise.complete.obs")),
    plot_boxplot_args = list("by" = "cut"),
    global_ggtheme = quote(theme_classic())
  )
)


# configuration list pf parameters 
config <- list(
  "introduce" = list(),
  "plot_intro" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 35,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list(),
  "plot_histogram" = list(),
  "plot_density" = list(),
  "plot_qq" = list(sampled_rows = 1000L),
  "plot_bar" = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  "plot_prcomp" = list(),
  "plot_boxplot" = list(),
  "plot_scatterplot" = list(sampled_rows = 1000L)
)