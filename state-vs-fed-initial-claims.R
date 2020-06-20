devtools::install_github("davidallen02/pamngr")

library(magrittr)

state.claims <- readxl::read_excel(path = "data.xlsx", sheet = "injcjc", skip = 2)
fed.claims <- readxl::read_excel(path = "data.xlsx", sheet = "injcpua", skip = 2)

claims <- state.claims %>%
  dplyr::left_join(fed.claims, by = "Dates") %>%
  set_colnames(c("dates","state-claims","federal-claims")) %>%
  dplyr::slice_max(dates, n = 20) %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::mutate(
    variable = variable %>% 
      stringr::str_replace_all("-", " ") %>%
      stringr::str_to_title() %>%
      factor(levels = c("Federal Claims","State Claims"))
  )

p <- claims %>%
  ggplot2::ggplot(ggplot2::aes(dates, value, fill = variable)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_fill_manual(values = c("black", "#850237"))

p %>%
  pamngr::pam.plot(
    plot.title = "Initial Unemployment Claims",
    plot.subtitle = "Thousands"
  ) %>%
  pamngr::ppt_output("initial-claims.png")

q <- p + 
  ggplot2::facet_wrap(ggplot2::vars(variable), ncol = 2) +
  ggplot2::theme(
    legend.position = 'none',
    strip.text = ggplot2::element_text(size = ggplot2::rel(1.5))
  )

ggplot2::ggsave(
  filename = "fed-vs-state-claims.png",
  plot     = q,
  width    = 13.33,
  height   = 6.75,
  units    = 'in'
)