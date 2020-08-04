library(magrittr)

state.claims <- readxl::read_excel(path = "data.xlsx", sheet = "injcjc", skip = 4)
fed.claims   <- readxl::read_excel(path = "data.xlsx", sheet = "injcpua", skip = 4)

claims <- state.claims %>%
  dplyr::left_join(fed.claims, by = "Dates") %>%
  set_colnames(c("dates","regular-programs","emergency-programs")) %>%
  dplyr::slice_max(dates, n = 20) %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::mutate(
    variable = variable %>% 
      stringr::str_replace_all("-", " ") %>%
      stringr::str_to_title() %>%
      factor(levels = c("Emergency Programs","Regular Programs"))
  )

p <- claims %>%
  ggplot2::ggplot(ggplot2::aes(dates, value, fill = variable)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_fill_manual(values = c("black", "#850237"))

p %>%
  pamngr::pam_plot(
    plot_title    = "Initial Unemployment Claims",
    plot_subtitle = "Thousands") %>%
  pamngr::all_output("initial-claims")

q <- p + ggplot2::facet_wrap(ggplot2::vars(variable), ncol = 2) 

q %>% pamngr::pam_plot(
  plot_title = "Initial Unemployment Claims",
  plot_subtitle = "Thousands",
  show_legend = FALSE,
  caption = FALSE
) %>%
  pamngr::all_output("reg-vs-emer-claims")
