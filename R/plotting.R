
#' Plot 1 Basal
#'
#' @param data data
#' @param threshold numeric Value to cap y axis. 
#'
#' @return ggplot
#' @export
plot_1bas <- function(data, threshold = 3.2) {
  
  bas_df <- dplyr::tibble(bas_d = data$bas_diff[[1]],
                   time = seq_along(bas_d))
  
  
  sel_times <- if ("bas_set" %in% names(data) && !is.na(data$bas_set)) {
    data$bas_set[[1]]
    
  } else {
    data$bas_start:data$bas_stop
    
  }
  
  bas_sub <- bas_df %>% dplyr::filter(time %in% sel_times)
  
  bmean <- bas_sub %>% dplyr::pull(bas_d) %>% mean
  bsd <- bas_sub %>% dplyr::pull(bas_d) %>% sd
  
  ggplot(bas_df, aes(time, bas_d))+
    annotate(geom = "rect", xmin = min(bas_sub$time), xmax = max(bas_sub$time),
             ymin = -Inf, ymax = Inf,
             fill = "grey", alpha = 0.35)+
    geom_line(size = 0.25)+
    geom_point(size = 0.05)+
    geom_point(data = bas_sub, size = 0.4)+
    annotate("errorbarh", xmin = min(bas_sub$time), xmax = max(bas_sub$time), y = bmean, height = bsd,
             color = "blue", size = 0.25)+
    ggtitle(data$name_c,
            paste0(stringr::str_sub(data$device, 1, 3), "-", data$idSequence, " (CV = ", data$bas_cova %>% round(3), ")"))+
    labs(x = NULL, y = NULL)+
    theme(plot.title = element_text(size = 11, face = "bold"),
          plot.subtitle = element_text(size = 9))+
    theme_bw()+
    if(max(bas_df$bas_d) < threshold) {
      ylim(0, threshold)
    } else {
      list(ylim(0, NA),
           annotate(geom = "text", x = 0, y = max(bas_df$bas_d), vjust = 1, label = "*"))
    }
  
}



#' Create one plot of Cmic 
#'
#' @param data data
#'
#' @return ggplot
#' @export
plot_1cmic <- function(data) {
  # data <- summary[1,]
  
  cmic_df <- dplyr::tibble(mirr_d = data$cmic_diff[[1]],
                    time = seq_along(mirr_d),
                    cmic_d = mirr_d * 0.7 * 38) %>% 
    filter(time < 25)
  
  
  sel_times <- if ("cmic_set" %in% names(data) && !is.na(data$cmic_set)) {
    data$cmic_set[[1]]
    
  } else {
    
    mirr_means <- purrr::map_dbl(3:8, ~mean(cmic_df$cmic_d[.x:(.x+2)]))
    
    whmimi <- which.min(mirr_means) + 2
    
    whmimi:(whmimi + 2)

  }
  
  cmic_sub <- cmic_df %>% dplyr::filter(time %in% sel_times)
  
  cmic_mean <- cmic_sub$cmic_d %>% mean
  
  cmic_sd <- cmic_sub$cmic_d %>% sd
  
  ggplot(cmic_df, aes(time, cmic_d))+
    annotate(geom = "rect", xmin = 3, xmax = 10,
             ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35)+
    annotate(geom = "rect", xmin = min(cmic_sub$time), xmax = max(cmic_sub$time),
             ymin = -Inf, ymax = Inf,
             fill = "grey65", alpha = 0.35)+
    
    geom_line(size = 0.25)+
    geom_point(size = 0.05)+
    geom_point(data = cmic_sub, size = 0.4)+
    annotate("errorbarh", xmin = min(cmic_sub$time), xmax = max(cmic_sub$time), y = cmic_mean, height = cmic_sd,
             color = "blue", size = 0.25)+
    ggtitle(data$name_c,
            paste0(stringr::str_sub(data$device, 1, 3), "-", data$idSequence))+ # " (CV = ", data$cmic_cova %>% round(3), ")"
    labs(x = NULL, y = NULL)+
    theme(plot.title = element_text(size = 11, face = "bold"),
          plot.subtitle = element_text(size = 9))+
    ylim(0, NA)+
    theme_bw()
  
}




#' Create 1 mgrowth plot
#'
#' @param data data
#'
#' @return ggplot
#' @export
plot_1mgrowth <- function(data) {
  
  mgr <- data$mgrow_df[[1]]
  class(mgr)
  
  
  if (inherits(mgr, "data.frame")) {
  
  ggplot(mgr, aes(time, cmic_log)) +
    annotate(geom = "rect", xmin = min(mgr$time), xmax = max(mgr$time),
             ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35)+
    geom_line(size = 0.25) +
    geom_point(size = 0.05)+
    # geom_point(data = cmic_df0, size = 0.4)+
    # geom_line(aes(time, incr), color = "red")
    geom_smooth(data = mgr, method = "lm", size = 0.3,
                formula = y ~ x) +
    ggtitle(data$name_c,
            paste0(stringr::str_sub(data$device, 1, 3), "-", data$idSequence))+
    labs(x = NULL, y = NULL)+
    theme(plot.title = element_text(size = 11, face = "bold"),
          plot.subtitle = element_text(size = 9))+
    xlim(0, NA)+
    ylim(0, NA)+
    theme_bw()
    
  } else {
    NULL
    
  }

}


#' Basal report
#'
#' Can be used with each specific measurement of basal, cmic and mgrowth.
#' PDF format is recommended for saving multiple pages. Otherwise other `ggsave` formats should work, 
#' but will only save the first page.
#'
#' @param data 
#' @param file 
#'
#' @return data
#' @export
#'
#' @examples
#' o2meas %>% 
#'   bas_report("bas.pdf") %>% 
#'   cmic_report("cmic.pdf") %>% 
#'   mgrow_report("mgrow.pdf")
bas_report <- function(data, file) {
  
  if ("bas_plot" %in% names(data)) {
    
    plots <- data %>% dplyr::pull(bas_plot)
    
  } else {
    
    plots <- slider::slide(data, plot_1bas)
    
  }
  
  export_report(plots, file)
  
  data

}

#' Cmic report
#'
#' Can be used with each specific measurement of basal, cmic and mgrowth.
#' PDF format is recommended for saving multiple pages. Otherwise other `ggsave` formats should work, 
#' but will only save the first page.
#' 
#' @rdname reports
#' 
#' @inheritParams bas_report
#'
#' @return data
#' @export
cmic_report <- function(data, file) {
  
  if ("cmic_plot" %in% names(data)) {
    
    plots <- data %>% dplyr::pull(cmic_plot)
    
  } else {
    
    plots <- slider::slide(data, plot_1cmic)
    
  }
  
  export_report(plots, file)
  
  data
  
}

#' Microbial growth report
#'
#' Can be used with each specific measurement of basal, cmic and mgrowth.
#' PDF format is recommended for saving multiple pages. Otherwise other `ggsave` formats should work, 
#' but will only save the first page.
#'
#' @inheritParams bas_report
#'
#' @return data
#' @export
mgrow_report <- function(data, file) {
  
  if("mgro_plot" %in% names(data)) {
    
    plots <- data %>% dplyr::pull(mgro_plot)
    
  } else {
    
    plots <- slider::slide(data, plot_1mgrowth)
    plots <- plots[!purrr::map_lgl(plots, is.null)]
      
    
  }

  export_report(plots, file)
  
  data
  
}


#' Export report
#'
#' Mostly for internal use. Does the page layout and exports figures.
#'
#' @param plots list of ggplots
#' @param file path
#' @param plots_per_page 24
#'
#' @return
#' @export
export_report <- function(plots, file, plots_per_page = 24) {
  
  pl_c <- floor(sqrt(plots_per_page * 0.75))
  
  pl_r <- plots_per_page / pl_c
  
  splots <- split(plots, ceiling(seq_along(plots)/plots_per_page))
  
  sp2 <- purrr::map(splots, ~ cowplot::plot_grid(plotlist = .x, nrow = pl_r, ncol = pl_c))
  
  ggsave(filename = file,
         plot = gridExtra::marrangeGrob(grobs = sp2, nrow=1, ncol=1, left = "BAS", bottom = "time"),
         width = 210, height = 297,
         units = "mm")
  
  NULL
  
}
