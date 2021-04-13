#' @name plot
#' @title Plot `s2ts` object
#' @description Plot a `s2ts` time series, using `{ggplot2}` routines.
#' @param x Object of class `s2ts` to be plotted.
#' @param pheno (optional) Output of `cut_cycles()`
#' @param fitted (optional) Output of `fit_curve()`
#' @param plot_points (optional) Logical: should raw point values be plotted?
#'  Default: only if `pheno` and `fitted` are not provided.
#' @param plot_rawline (optional) Logical: should lines connecting raw points
#'  be plotted? Default: yes.
#'  They are represented as dark grey lines, or as almost transparent lines
#'  if smoothed values exist and are represented.
#' @param plot_smoothed (optional) Logical: should lines connecting smoothed
#'  values be plotted? Default: yes (if exist).
#'  They are represented as dark grey lines.
#' @param plot_fitted (optional) Logical: should double logistic curves be 
#'  plotted? Default: yes, if provided. They are represented as red curves.
#' @param plot_cuts (optional) Logical: should cuts between cycles be plotted?
#'  Default: yes, if provided in `pheno` or in `fitted`.
#'  They are represented as black vertical lines.
#' @param plot_cycles (optional) Logical: should existing cycles be highlighted?
#'  If TRUE (default), existing cycles are highlighted with yellow bands.
#' @param plot_dates (optional) Logical or character:
#'  if TRUE, plot the dates of cycle cuts and phenology metrics;
#'  if FALSE (default), do not plot anything;
#'  if `"cycles"` or `"pheno"`, plot only cycle cuts or phenology metrics.
#' @param pheno_metrics (optional) Character vector containing the names of
#'  the phenological metrics to be plotted. 
#'  If not provided or if `pheno_metrics = "all"`, all available metrics
#'  are plotted.
#' @param ... Not currently used.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom ggplot2 aes facet_wrap geom_line geom_point geom_rect
#'  geom_segment geom_text geom_vline ggplot scale_colour_brewer 
#'  scale_fill_viridis_c scale_x_date scale_y_continuous theme_light
#' @export
#' @examples 
#' # Plot raw time series
#' data(ts_raw) # sample raw time series
#' plot(ts_raw)
#' plot(ts_raw, plot_rawline = FALSE) # show only points
#' 
#' # Plot smoothed or filled time series
#' data(ts_smoothed) # sample smoothed time series
#' data(ts_filled) # sample filled time series
#' plot(ts_smoothed)
#' plot(ts_filled)
#' plot(ts_filled, plot_rawline = FALSE) # avoid plotting raw line
#' plot(ts_filled, plot_smoothed = FALSE) # plot only raw line
#' 
#' # Add phenological information
#' data(dt_cycles) # sample data frame with cycle cuts
#' data(cf) # sample object with curve fitting
#' data(dt_pheno) # sample data frame with phenological
#' plot(ts_filled, pheno = dt_cycles) # default plot with cycle cuts
#' plot(ts_filled, pheno = dt_cycles, plot_points = TRUE) # show also points
#' plot(ts_filled, pheno = dt_cycles, plot_dates = TRUE) # print dates
#' plot(ts_filled, pheno = dt_cycles, plot_cycles = FALSE) # avoid highlighting cycles
#' plot(ts_filled, fitted = cf) # default print with cycles (cuts and curves)
#' plot(ts_filled, fitted = cf, plot_cuts = FALSE) # show only curves
#' plot(ts_filled, fitted = cf, pheno = dt_pheno) # plot curves and metrics
#' plot(ts_filled, pheno = dt_pheno, plot_dates = TRUE) # print all dates
#' plot(ts_filled, pheno = dt_pheno, plot_dates = "cycles") # print only cut dates
#' plot(ts_filled, pheno = dt_pheno, plot_dates = "pheno") # print only phenological dates


plot.s2ts <- function(
  x, pheno, fitted, 
  plot_points, plot_rawline, plot_smoothed, plot_fitted, plot_cuts,
  plot_cycles = TRUE, plot_dates = FALSE, pheno_metrics, 
  ...
) {
  
  # Avoid check notes for data.table related variables
  value <- rawval <- id <- begin <- end <- 
    Value <- Phenology <- cycle <- QA <- NULL
  
  # Determine plot mode
  plot_mode <- if (any(!missing(pheno), !missing(fitted))) {
    "background"
  } else if (is.null(attr(x, "gen_by"))) {
    "base"
  } else if (attr(x, "gen_by") %in% c("smooth_s2ts")) {
    "smoothed"
  } else if (attr(x, "gen_by") %in% c("fill_s2ts")) {
    "filled"
  } else { # including attr(x, "gen_by") == "extract_s2ts"
    "base"
  }
  plot_elements <- list()
  plot_elements$points <- if (!missing(plot_points)) {
    plot_points
  } else {
    # by default, plot points if both `pheno` and `fitted` are missing
    all(!"background" %in% plot_mode)
  }
  plot_elements$rawline <- if (!missing(plot_rawline)) {plot_rawline} else {TRUE}
  plot_elements$smoothedline <- if (!missing(plot_smoothed)) {
    plot_smoothed
  } else {
    # by default, plot smoothed line in all cases except "base"
    any(c("smoothed", "filled", "background") %in% plot_mode)
  }
  plot_elements$fitted <- if (!missing(fitted) & !missing(plot_fitted)) {
    plot_fitted
  } else {
    !missing(fitted)
  }
  plot_elements$cuts <- if (!missing(plot_cuts)) {
    plot_cuts
  } else {
    TRUE # meaning to plot if existing
  }
  plot_elements$pheno_method <- if (!missing(pheno)) {
    attr(pheno, "info")$method
  }
  
  # Change "plot_dates" value
  if (plot_dates == TRUE) {
    plot_dates <- c("pheno", "cycles")
  }
  
  # Extract input data.table
  x_dt <- as.data.table(x)
  setnames(x_dt, "qa", "QA", skip_absent = TRUE)
  if (any(c("filled", "background") %in% plot_mode)) {
    x_dt_smooth <- x_dt
  } else {
    x_dt_smooth <- x_dt[!is.na(value),]
  }
  if ("base" %in% plot_mode || is.null(x_dt$rawval)) {
    x_dt_raw <- x_dt[!is.na(value),]
  } else {
    x_dt_raw <- x_dt[!is.na(rawval),]
    x_dt_raw[,value := rawval]
  }
  
  # Extract additional data
  if (!missing(fitted)) {
    fitted_dt <- s2fit_to_s2ts(fitted)[id %in% x_dt$id,]
  }
  if (!missing(pheno)) {
    pheno_dt <- pheno[id %in% x_dt$id,]
    pheno_dt <- pheno_dt[,c("date","value") := list(as.Date(NA),Inf)] # to plot geom_rect()
  } else if (!missing(fitted)) {
    pheno_dt <- fitted_dt[
      ,list("begin" = min(date), "end" = max(date), "date" = as.Date(NA), "value" = Inf),
      by = c("id", "year", "cycle")
    ]
    # TODO add maxval
  }
  if (!is.null(plot_elements$pheno_method)) {
    # Define which metrics are quantitative (y) and which refer to dates (x)
    metrics_y <- c(
      if (plot_elements$pheno_method %in% c("trs", "derivatives")) {
        c("mgs", "peak", "msp", "mau")
      } else if (plot_elements$pheno_method %in% c("gu")) {
        c("maxline", "baseline")
      }
    )
    metrics_x <- c(
      "maxval", #"begin", "end",
      if (plot_elements$pheno_method %in% c("trs", "derivatives")) {
        c("sos", "eos", "pop")
      } else if (plot_elements$pheno_method %in% c("gu")) {
        c("UD", "SD", "DD", "RD")
      } else if (plot_elements$pheno_method %in% c("klosterman")) {
        c("Greenup", "Maturity", "Senescence", "Dormancy")
      }
    )
    if (!missing(pheno_metrics) && 
        !(length(pheno_metrics) == 1 && "all" %in% pheno_metrics)) {
      metrics_x <- metrics_x[metrics_x %in% pheno_metrics]
      metrics_y <- metrics_y[metrics_y %in% pheno_metrics]
    }
    if (length(metrics_y) > 0) {
      pheno_dt_y <- melt(
        pheno_dt, 
        id.vars = c("id", "year", "cycle", "begin", "end"), 
        measure.vars = metrics_y,
        variable.name = "Value", value.name = "value"
      )
    }
    if (length(metrics_x) > 0) {
      pheno_dt_x <- melt(
        pheno_dt, 
        id.vars = c("id", "year", "cycle"), 
        measure.vars = metrics_x,
        variable.name = "Phenology", value.name = "date"
      )
    }
  }
  
  # Base plot
  out <- ggplot(x_dt, aes(x = date, y = value))
  
  # Add background bands
  if (exists("pheno_dt") & plot_elements$cuts == TRUE & plot_cycles == TRUE) {
    out <- out +
      geom_rect(
        data = pheno_dt,
        aes(xmin = begin, xmax = end), 
        ymin = -Inf, 
        ymax = min(x_dt$value, na.rm=TRUE) - diff(range(x_dt$value, na.rm=TRUE))*.025,
        fill = "yellow", alpha = 0.5
      )
  }
  
  # Add raw line
  if (plot_elements$rawline == TRUE) {
    out <- out + geom_line(
      data = x_dt_raw, 
      alpha = if (plot_elements$smoothedline == TRUE) {0.1} else {0.35}
    )
  }
  
  # Add cycle cuts / peaks
  if (exists("pheno_dt_y")) {
    # for (sel_cycle in pheno[, unique(cycle)]) {
    out <- out + geom_segment(
      # data = pheno_dt_y[cycle==sel_cycle,], 
      data = pheno_dt_y, 
      aes(
        x = begin, xend = end,
        y = value, yend = value,
        colour = Value
      ),
      linetype = "dashed"
    )
    # }
  }
  if (exists("pheno_dt_x")) {
    out <- out + 
      geom_vline(
        data = pheno_dt_x,
        aes(xintercept = as.numeric(date), colour = Phenology)
      )
    if ("pheno" %in% plot_dates) {
      out <- out + 
        geom_text(
          data = pheno_dt_x,
          aes(x = date, y = min(x_dt$value, na.rm=TRUE), label = date, colour = Phenology),
          angle = 90, vjust = -0.25, hjust = 0, size = 3
        )
    }
  }
  if (exists("pheno_dt") & plot_elements$cuts == TRUE) {
    out <- out + 
      geom_vline(
        data = pheno_dt,
        aes(xintercept = as.numeric(begin)), colour = "black"
      ) +
      geom_vline(
        data = pheno_dt,
        aes(xintercept = as.numeric(end)), colour = "black"
      )
    if ("cycles" %in% plot_dates) {
      out <- out + 
        geom_text(
          data = pheno_dt,
          aes(x = begin, y = max(x_dt$value, na.rm=TRUE), label = begin), colour = "black",
          angle = 90, vjust = 1.25, hjust = 1, size = 3
        ) +
        geom_text(
          data = pheno_dt,
          aes(x = end, y = max(x_dt$value, na.rm=TRUE), label = end), colour = "black",
          angle = 90, vjust = -0.25, hjust = 1, size = 3
        )
    }
  }
  
  # Add fitted line
  if (plot_elements$fitted == TRUE) {
    for (sel_year in fitted_dt[,unique(year)]) {
      for (sel_cycle in fitted_dt[year == sel_year, unique(cycle)]) {
        out <- out + geom_line(
          data = fitted_dt[year == sel_year & cycle == sel_cycle,], 
          colour = "red", alpha = 0.5
        )
      }
    }
  }
  
  # Add smoothed line
  if (plot_elements$smoothedline == TRUE) {
    out <- out + geom_line(data = x_dt_smooth[!is.na(value),], alpha = 0.5)
  }
  
  # Add points
  if (plot_elements$points == TRUE) {
    out <- out + geom_point(
      data = x_dt_raw, 
      if (!is.null(x_dt$QA)) {aes(fill = QA)}, 
      shape = 21, colour = "#00000000"
    )
  }
  
  # Facet in case of multiple IDs
  if (length(unique(x_dt$id)) > 0) {
    out <- out + facet_wrap(
      ~id, 
      ncol = round(sqrt(length(unique(x_dt$id))/2))
      # ncol = max(1, round(length(unique(x_dt$id))/4))
    )
  }
  
  # Format options
  out <- out +
    scale_x_date(name = "Date") +
    scale_y_continuous(name = NULL) +
    theme_light()
  if (plot_elements$points == TRUE) {
    out <- out + scale_fill_viridis_c(
      option = "inferno", direction = -1
    )
  }
  if (exists("pheno_dt")) {
    out <- out + scale_colour_brewer(palette = "Set2")
  }
  
  # date_range <- range(x_dt$date)
  # year_range <- as.integer(strftime(date_range,"%Y"))
  # year_seq <- seq(year_range[1]-1, year_range[2]+1)
  # dates_bands <- data.table(
  #   begin = as.Date(apply(expand.grid(year_seq,seq(1,10,3),1),1,paste,collapse="-")),
  #   end = as.Date(apply(expand.grid(year_seq,paste(seq(3,12,3),c(31,30,30,31),sep="-")),1,paste,collapse="-")),
  #   seas = rep(c("winter","spring","summer","autumn"), each = length(year_seq))
  # )[order(begin),]
  # dates_bands <- dates_bands[begin<=date_range[2] & end>=date_range[1],]
  # dates_bands[1,begin:=date_range[1]]
  # dates_bands[nrow(dates_bands),end:=date_range[2]]
  # # TODO aggiungi bande con geom_rect, limiti Y a Inf, colore blu per winter e rosso per summer
  
  out
  
}
