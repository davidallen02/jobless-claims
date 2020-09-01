p <- pamngr::get_data("injcuito") %>%
# p <- readxl::read_excel(path = "data.xlsx", sheet = "injcuito", skip = 4) %>%
  magrittr::set_colnames(c("dates","continuing_claims")) %>%
  tail(52) %>%
  # dplyr::slice_max(dates, n = 52) %>%
  ggplot2::ggplot(ggplot2::aes(dates, continuing_claims)) +
  ggplot2::geom_line(size = 2, color = "#850237") 

dat <- pamngr::get_data("injcuito") %>%
  magrittr::set_colnames(c("dates", "Continuing Claims")) %>%
  tail(20) %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::barplot() %>%
  pamngr::pam_plot(
    plot_title = "Continuing Claims",
    plot_subtitle = "Thousands"
  ) %>%
  pamngr::all_output("continuing-claims")

p %>%
  pamngr::pam_plot(
    plot_title = "Continuing Unemployment Claims",
    plot_subtitle = "Thousands"
  ) %>%
  pamngr::all_output("state-fed-continuing-claims")

injcuirs <- pamngr::get_data("injcuirs") 
injcuife <- pamngr::get_data("injcuife") 
injcuidv <- pamngr::get_data("injcuidv") 

injccpua <- pamngr::get_data("injccpua") 
injcpeuc <- pamngr::get_data("injcpeuc") 
injceuc  <- pamngr::get_data("injceuc") 
injcexb  <- pamngr::get_data("injcexb") 
injcuisb <- pamngr::get_data("injcuisb") 
injcuisw <- pamngr::get_data("injcuisw") 

regular <- injcuirs %>% 
  dplyr::left_join(injcuife, by = "dates") %>% 
  dplyr::left_join(injcuidv, by = "dates") %>%
  magrittr::set_colnames(c("dates", "injcuirs","injcuife","injcuidv")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(regular = sum(injcuirs, injcuife, injcuidv)) %>%
  dplyr::select(c(dates, regular))

emergency <- injccpua %>%
  dplyr::left_join(injcpeuc, by = "dates") %>%
  dplyr::left_join(injceuc, by = "dates") %>%
  dplyr::left_join(injcexb, by = "dates") %>%
  dplyr::left_join(injcuisb, by = "dates") %>%
  dplyr::left_join(injcuisw, by = "dates") %>%
  magrittr::set_colnames(
    c("dates", "injccpua", "injcpeuc", "injceuc", "injcexb","injcuisb","injcuisw")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    emergency = sum(injccpua, injcpeuc, injceuc, injcexb, injcuisb, injcuisw, 
                    na.rm = TRUE)
  ) %>%
  dplyr::select(c(dates, emergency))

q <- regular %>% 
  dplyr::left_join(emergency, by = "dates") %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(emergency = emergency %>% tidyr::replace_na(0)) %>%
 tail(20) %>%
  magrittr::set_colnames(c("dates", "Regular Programs","Emergency Programs")) %>%
  reshape2::melt(id.vars = "dates") %>%
  ggplot2::ggplot(ggplot2::aes(dates, value, fill = variable)) + 
  ggplot2::geom_area() +
  ggplot2::scale_fill_manual(values = pamngr::pam.pal()) 


q %>%
  pamngr::pam_plot(
    plot_title = "Continuing Unemployment Claims",
    plot_subtitle = "Thousands",
    caption = FALSE
  ) %>%
  pamngr::all_output("continuing-claims-composition")





