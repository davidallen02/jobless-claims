devtools::install_github("davidallen02/pamngr")

library(magrittr)

p <- readxl::read_excel(path = "data.xlsx", sheet = "injcuito", skip = 3) %>%
  set_colnames(c("dates","continuing_claims")) %>%
  dplyr::slice_max(dates, n = 52) %>%
  ggplot2::ggplot(ggplot2::aes(dates, continuing_claims)) +
  ggplot2::geom_line(size = 2, color = "#850237") 

p %>%
  pamngr::pam.plot(
    plot.title = "Continuing Unemployment Claims",
    plot.subtitle = "Thousands"
  ) %>%
  pamngr::ppt_output("state-fed-continuing-claims.png")

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
  set_colnames(c("dates", "injcuirs","injcuife","injcuidv")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(regular = sum(injcuirs, injcuife, injcuidv)) %>%
  dplyr::select(c(dates, regular))

emergency <- injccpua %>%
  dplyr::left_join(injcpeuc, by = "dates") %>%
  dplyr::left_join(injceuc, by = "dates") %>%
  dplyr::left_join(injcexb, by = "dates") %>%
  dplyr::left_join(injcuisb, by = "dates") %>%
  dplyr::left_join(injcuisw, by = "dates") %>%
  set_colnames(
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
  dplyr::slice_max(order_by = dates, n = 20) %>%
  set_colnames(c("dates", "Regular Programs","Emergency Programs")) %>%
  reshape2::melt(id.vars = "dates") %>%
  ggplot2::ggplot(ggplot2::aes(dates, value, fill = variable)) + 
  ggplot2::geom_area() +
  ggplot2::scale_fill_manual(values = c("#850237", "black")) 


q %>%
  pamngr::pam.plot(
    plot.title = "Continuing Unemployment Claims",
    plot.subtitle = "Thousands"
  ) %>%
  pamngr::ppt_output("continuing-claims-composition.png")





