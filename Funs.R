eudist_2d <- function(x1,x2,y1,y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}


eudist_3d <- function(x1,x2,y1,y2,z1,z2){
  return(sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2))
}

beta <- function(a,b,c){
  return(acos(((b^2) - (a^2) - (c^2))/(-2*a*c)))
}

plot_shared_legend <- function(..., ncol = NA, nrow = NA, position = c("bottom", "right"), title = NULL, rel=NA) {
  require(cowplot)
  if(is.na(ncol)){
    if(is.na(nrow)){
      ncol = length(list(...))
      nrow = 1
    }
  }
  
  if (is.na(ncol)){
    ncol = round(length(list(...))/nrow,0)
  } else {
    nrow = round(length(list(...))/ncol,0)
  }

  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = grid::unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)))
  title <- ggdraw() + draw_label(title)
  
  
  
  return(plot_grid(title, combined, ncol=1, rel_heights=c(0.1, 1)))
}

trim <- function(x,filter=F,repl=NA){
  bs <- boxplot.stats(x)
  if(!is.na(repl))
    repl <- bs$stats[3]
  if(filter)
    return(!x %in% bs$out)
  else{
    #x[!x %in% xout]
    #x[match(bs$out,x)] <- repl
    x <- ifelse(x %in% bs$out, repl, x)
    x
  }
}


#Extract legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

tex <- function(string="$\\beta", p = T){
  if (substr(string,1,1) != "$"){
    stop("First element should be $")
  }
  if (p == T){
    return(parse(text = latex2exp::TeX(string, output='character')))
  } else{
    return(latex2exp::TeX(string, output='character'))
  }
}

##Write citation bib
getCitationBib <- function(Paper=str(),Bibs=list(),File=str()){
  
  library(readr)
  library(stringr)
  library(RefManageR)
  
  cites <- read_file(Paper)
  cites <- str_replace_all(unique(as.vector(str_extract_all(cites,"@([A-Z])\\w+", simplify = T))),"@","")
  
  Bibs <- lapply(Bibs, ReadBib)
  if (length(Bibs) >= 2) {
    for (i in 2:length(Bibs)){
      Bibs <- c(Bibs[[1]],Bibs[[2]])
    } 
  } else {
    Bibs <- c(Bibs[[1]])
  } 
  Bibs <- subset(Bibs, names(Bibs) %in% cites)
  WriteBib(Bibs,File) 
}

stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                             list(a = format(coef(m)[[1]], digits = 3), 
                                                  b = format(coef(m)[[2]], digits = 3), 
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )




# =============================================================================
# Solución numérica
# =============================================================================
xy_drag <- function(vh,vv,C=0.535,rho=1.2,m=0.057,radius=0.033,dt=0.001,g=9.807, TTC = 1, vectors = F){
  A <- pi*radius^2 
  D = (rho*C*A)/2 
  v <- sqrt(vh^2+vv^2)
  x = y = t = 0
  k = dt
  dt = 0
  vx <- vh
  vy <- vv
  x_list <- y_list <-  list()
  xv_list <- yv_list <- list()
  xa_list <- ya_list <- list()
  vv_list <- t_list <-list()
  c_list <-list()
  n <- 1
  repeat{
    #if (t > TTC*0.55)
    #  C = 0.535
    A = pi*radius^2 
    D = (rho*C*A)/2 
    
    ax <- -(D/m)*v*vx
    ay <- -g - (D/m)*v*vy
    
    vx = vx + ax * dt 
    vy = vy + ay * dt 
    
    x = x + vx * dt + 0.5*ax*dt^2
    y = y + vy * dt + 0.5*ay*dt^2
    
    dt = k
    t = t + dt  
    
    x_list[[n]] <- x
    y_list[[n]] <- y
    t_list[[n]] <- t
    xv_list[[n]] <- vx
    yv_list[[n]] <- vy
    xa_list[[n]] <- ax
    ya_list[[n]] <- ay
    ya_list[[n]] <- ay
    c_list[[n]] <- C
    
    n <- n+1
    v <- sqrt(vx^2+vy^2)
    vv_list[[n]] <- v
    
    if(y<=0 & x > 0)
      break
    
  }
  
  if (vectors == T){
    dplyr::tibble(t=do.call(c, t_list),
                  x=do.call(c, x_list),
                  y=do.call(c, y_list),                
                  vx=do.call(c, xv_list),
                  vy=do.call(c, yv_list),
                  ax=do.call(c, xa_list),
                  ay=do.call(c, ya_list),
                  vv=do.call(c, vv_list),
                  cd=do.call(c, c_list)
    ) ## Tibble para visualizar
  } else{
    dplyr::tibble(t=do.call(c, t_list),
                  x=do.call(c, x_list),
                  y=do.call(c, y_list)
    ) ## Tibble para visualizar
  }
}

xy_drag_model <- function(vh,vv,C=0.535,rho=1.2,m=0.057,radius=0.033,dt=0.001,g=9.807, vectors = F, TTC = 1){
  A <- pi*radius^2 
  D = (rho*C*A)/2 
  v <- sqrt(vh^2+vv^2)
  x = y = t = 0
  k = dt
  dt = 0
  vx <- vh
  vy <- vv
  x_list <- y_list <-  list()
  xv_list <- yv_list <- list()
  xa_list <- ya_list <- list()
  vv_list <- t_list <-list()
  c_list <-list()
  n <- 1
  repeat{
      if (t > TTC*0.3){
        C = 0.535
      }

    A = pi*radius^2 
    D = (rho*C*A)/2 
    
    ax <- -(D/m)*v*vx
    ay <- -g - (D/m)*v*vy
    
    vx = vx + ax * dt 
    vy = vy + ay * dt 
    
    x = x + vx * dt + 0.5*ax*dt^2
    y = y + vy * dt + 0.5*ay*dt^2
    
    dt = k
    t = t + dt  
    
    x_list[[n]] <- x
    y_list[[n]] <- y
    t_list[[n]] <- t
    xv_list[[n]] <- vx
    yv_list[[n]] <- vy
    xa_list[[n]] <- ax
    ya_list[[n]] <- ay
    ya_list[[n]] <- ay
    c_list[[n]] <- C
    
    n <- n+1
    v <- sqrt(vx^2+vy^2)
    vv_list[[n]] <- v
    
    if(y<=0 & x > 0)
      break
    
  }
  
  if (vectors == T){
    dplyr::tibble(t=do.call(c, t_list),
                  x=do.call(c, x_list),
                  y=do.call(c, y_list),                
                  vx=do.call(c, xv_list),
                  vy=do.call(c, yv_list),
                  ax=do.call(c, xa_list),
                  ay=do.call(c, ya_list),
                  vv=do.call(c, vv_list),
                  cd=do.call(c, c_list)
    ) ## Tibble para visualizar
  } else{
    dplyr::tibble(t=do.call(c, t_list),
                  x=do.call(c, x_list),
                  y=do.call(c, y_list)
    ) ## Tibble para visualizar
  }
}


