theme_bw_2 <- function(base_size = 16, text_size = 18, title_size = 22) {
  bg_color <- "#ffffff"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(family = "Roboto Condensed"),
      plot.title = element_text(size = title_size, margin = margin(b = 10)),
      plot.subtitle = element_text(margin = margin(b = 20)),
      plot.caption = element_text(face = "italic"),
      axis.title = element_text(size = text_size),
      axis.ticks = element_blank(),
      plot.background = bg_rect,
      panel.background = bg_rect,
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "grey80", size = 0.25,
                                      linetype = 2),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0),
      legend.background = bg_rect,
      legend.key.width = unit(1.5, "line"),
      legend.key = element_blank()
    )
}

draw_boxplot <- function(df, var, by_var = NULL,
                         x_title = "", y_title = "",
                         title = NULL, subtitle = NULL, caption = NULL, ...) {
  if (is.null(by_var)) {
    ggplot(df, aes_string(x = factor(""), y = var)) +
      geom_boxplot(na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  } else {
    ggplot(df, aes_string(x = by_var, y = var)) +
      geom_boxplot(na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  }
}

draw_violin <- function(df, var, by_var = NULL,
                        x_title = "", y_title = "",
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  if (is.null(by_var)) {
    ggplot(df, aes_string(x = factor(""), y = var)) +
      geom_violin(fill = "#595959", na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  } else {
    ggplot(df, aes_string(x = by_var, y = var)) +
      geom_violin(fill = "#595959", na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  }
}

draw_histogram <- function(df, var, by_var = NULL,
                           x_title = "", y_title = "",
                           title = NULL, subtitle = NULL, caption = NULL, ...) {
  if (is.null(by_var)) {
    ggplot(df, aes_string(var)) +
      geom_histogram(na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5)) +
      theme(...)
  } else {
    ggplot(df, aes_string(var)) +
      geom_histogram(na.rm = TRUE) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      facet_grid(as.formula(paste0(". ~ ", by_var))) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      theme_bw_2() +
      theme(legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5)) +
      theme(...)
  }
}

draw_barplot <- function(df, var, by_var = NULL,
                         x_title = "", y_title = "",
                         title = NULL, subtitle = NULL, caption = NULL, ...) {
  var_sym <- rlang::sym(var)
  
  order_var <- df %>%
    group_by(!!var_sym) %>%
    count(sort = TRUE) %>%
    pull(!!var_sym)
  
  if (is.null(by_var)) {
    ggplot(df, aes_string(var)) +
      geom_bar(width = 0.6, na.rm = TRUE) +
      scale_x_discrete(limits = order_var) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      coord_flip() +
      theme_bw_2()  +
      theme(legend.position = "none") +
      theme(...)
  } else {
    ggplot(df, aes_string(var)) +
      geom_bar(width = 0.6, na.rm = TRUE) +
      scale_x_discrete(limits = order_var) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      facet_grid(as.formula(paste0(". ~ ", by_var))) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      coord_flip() +
      theme_bw_2()  +
      theme(legend.position = "none") +
      theme(...)
  }
}

draw_lollipop <- function(df, var, by_var = NULL,
                          x_title = "", y_title = "",
                          title = NULL, subtitle = NULL, caption = NULL, ...) {
  var_sym <- rlang::sym(var)
  
  if (is.null(by_var)) {
    count_var <- df %>%
      select(!!var_sym) %>%
      count(!!var_sym, sort = TRUE)
    
    order_var <- pull(count_var, !!var_sym)
    
    ggplot(data = count_var, aes_string(x = var, y = "n")) +
      geom_count(color = "#595959") +
      geom_segment(aes_string(x = var,
                              xend = var,
                              y = 0,
                              yend = "n"),
                   color = "#595959", size = 1.25) +
      scale_x_discrete(limits = order_var) +
      scale_y_continuous(labels = scales::format_format(big.mark = " "),
                         expand = c(0.02, 2)) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      coord_flip() +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  } else {
    by_var_sym <- rlang::sym(by_var)
    
    count_var <- df %>%
      select(!!var_sym, !!by_var_sym) %>%
      group_by(!!by_var_sym) %>%
      count(!!var_sym, sort = TRUE)
    
    order_var <- count_var %>%
      ungroup() %>%
      count(!!var_sym, wt = n, sort = TRUE) %>%
      pull(!!var_sym)
    
    ggplot(data = count_var, aes_string(x = var, y = "n")) +
      geom_count(color = "#595959") +
      geom_segment(aes_string(x = var,
                              xend = var,
                              y = 0,
                              yend = "n"),
                   color = "#595959", size = 1.25) +
      facet_grid(as.formula(paste0(". ~ ", by_var))) +
      scale_x_discrete(limits = order_var) +
      scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
      labs(title = title, subtitle = subtitle,
           x = x_title, y = y_title,
           caption = caption) +
      coord_flip() +
      theme_bw_2() +
      theme(legend.position = "none") +
      theme(...)
  }
}

draw_multilineplot <- function(df, var, by_var = NULL,
                               size = 1.25, ncol,
                               x_title = "", y_title = "",
                               title = NULL, subtitle = NULL,
                               caption = NULL, ...) {
  var_sym <- rlang::sym(var)
  by_var_sym <- rlang::sym(by_var)
  
  count_var <- df %>%
    group_by(!!by_var_sym) %>%
    count(!!var_sym, sort = TRUE) %>%
    ungroup()

  order_var <- count_var %>%
    group_by(!!var_sym) %>%
    count(wt = n, sort = TRUE) %>%
    pull(!!var_sym)
  
  count_var <- expand.grid(unique(df[[var]]),
                           unique(df[[by_var]]),
                           stringsAsFactors = FALSE) %>%
    setNames(c(var, by_var)) %>%
    left_join(count_var, by = c(var, by_var)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(fct = factor(!!var_sym, levels = order_var))
  
  ggplot(data = count_var, aes_string(x = by_var, y = "n")) +
    geom_line(aes(group = fct, color = fct), size = size) +
    geom_point(aes(color = fct), size = size * 2) +
    scale_x_discrete(limits = c("2011", paste0("´", 12:17))) +
    scale_y_continuous(labels = scales::format_format(big.mark = " ")) +
    facet_wrap(~ fct, ncol = ncol) +
    labs(title = title, subtitle = subtitle,
         x = x_title, y = y_title,
         caption = caption) +
    theme_bw_2() +
    theme(legend.position = "none") +
    theme(...)
}
