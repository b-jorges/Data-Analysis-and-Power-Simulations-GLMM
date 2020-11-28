# !diagnostics off

kafilter <- function(x,k=c(1,-2,1),dt)
{
  
  y1 <- as.double(stats::filter(x, filter = k))
  y1/dt^2
}

kvfilter <- function(x,k=c(3,2,1,0,-1,-2,-3)/14,dt)
{
  y1 <- as.double(stats::filter(x, filter = k))
  y1/dt
                                
}

kfilter <- function(x,k=c(1,2,3,2,1)/9) # local gaussian smoothing
{
  y1 <- as.double(stats::filter(x, filter = k))
  y1
  
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


#scale2cm(kk$time,kk$xf,kk$npos_y,kk$ax,kk$pupilTime+0.7,kk$pupilTime+0.7+kk$timeStill,kk$xinit,kk$conf1,kk$conf2)

scale2cm <- function(t,x,y,ax,fix_offset_t,motion_onset_t,tx0,c1,c2,fac=82,minconf=0.5)
{
 
  xy0 <- get_xy0(t,x,y,fix_offset_t,c1,c2,minconf=minconf) 
  if(is.na(xy0[1]))
    return(NA)
  
  # a2 <- fac*ax[t>fix_offset_t-0.7 & t<fix_offset_t]
  # n <- ifelse(length(a2)<100,50,100)
  # nsigma <- 3.4*min(RcppRoll::roll_sd(a2,n),na.rm = T) 
  ax <- ax*fac
  nsigma <- 3.4*min(RcppRoll::roll_sd(ax,400),na.rm = T) 
  if(nsigma>900)
    nsigma <- 450
  
  #print(nsigma)
  s <- msf(x[t>fix_offset_t & t<motion_onset_t],ax[t>fix_offset_t & t<motion_onset_t],min_amp = 0.45/82)
  #print(s)
  id <- which(s==-10)
  if(length(id)){
    x2 <- x[t>fix_offset_t & t<motion_onset_t][id]
   # y2 <- y[t>fix_offset_t & t<motion_onset_t]
    return(abs(tx0)/abs(x2-xy0[1]))
  }else{
    return(NA)
  }
  
}

scale2cm2 <- function(t,x,y,fix_offset_t,motion_onset_t,tx0,c1,c2,minconf=0.5)
{
  
  xy0 <- get_xy0(t,x,y,fix_offset_t-0.2,c1,c2,minconf=minconf) 
  if(is.na(xy0[1]))
    return(NA)
  
  xy1 <- get_xy0(t,x,y,motion_onset_t-0.2,c1,c2,minconf=minconf) 
  if(is.na(xy1[1]))
    return(NA)
  return(abs(tx0)/abs(xy1[1]-xy0[1]))
  
}

get_cm <- function(t,x,fix_offset_t,motion_onset_t,tx0)
{
 cm <- NA
  x2 <- x[t>fix_offset_t-0.01 & t<fix_offset_t+0.01] 
  if(length(x2)){
    xx <- mean(x2,na.rm=T)
    x2 <- x[t>motion_onset_t-0.02 & t<motion_onset_t] 
    if(length(x2)){
      xxx <- mean(x2,na.rm=T)
      #print(xxx-xx)
      cm <- abs(tx0)/abs(xxx-xx)
    }else
      return(NA)
  }else
    return(NA)
  
  return(cm)
}

get_xy0 <- function(t,x,y,fix_offset_t,c1,c2,minconf=0.5)
{
  x2 <- x[t>fix_offset_t-0.01 & t<fix_offset_t+0.01 & (c1>minconf | c2>minconf)] 
  y2 <- y[t>fix_offset_t-0.01 & t<fix_offset_t+0.01 & (c1>minconf | c2>minconf)] 
 # sc1 <- c1[t>fix_offset_t-0.01 & t<fix_offset_t+0.01][1]
#  sc2 <- c2[t>fix_offset_t-0.01 & t<fix_offset_t+0.01][1]
 # print(c(sc1,sc2))
  if(length(x2) & length(y2)){
    xx <- mean(x2,na.rm=T)
    yy <- mean(y2,na.rm=T)
    return(c(xx,yy))
  }
  return(c(NA,NA))
}

#pursuit_speed(kk$vx[kk$time>kk$pupilTime+0.7+kk$timeStill & kk$time<kk$pupilTime+0.7+kk$timeStill+kk$iTTC]*toCM,kk$sac[kk$time>kk$pupilTime+0.7+kk$timeStill & kk$time<kk$pupilTime+0.7+kk$timeStill+kk$iTTC])

pursuit_speed <- function(v,sac)
{
  
  id <- which(sac>9)# index sacaddes onset
  vv1 <- NA
  vv2 <- NA
 # print(length(id))
  if(length(id)>1){
    s <- sac[id[1]:id[2]]
    vv <- v[id[1]:id[2]]
    vv1 <- mean(vv[s==0],na.rm=T)
  }
  if(length(id)==3){
    s <- sac[id[2]:id[3]]
    vv <- v[id[2]:id[3]]
    vv2 <- mean(vv[s==0],na.rm=T)
  }
  return(c(vv1,vv2))
}

num_sac <- function(sac)
{
  length(which(sac< -8))
}

#decision making

dv <- function(x)# xend
{
  v <- c(20, 25, 32)
  x<- 19 - mean(x,na.rm=T)
  t <- x/v
  coef(lsfit(1/v,t))[2]
}

foo <- function(x,y)
  coef(lsfit(x,y))[2]


# old stuff
null2one.msi <- function(v,neg.rm=T)
{
  vmax <- max(v,na.rm=T)
  res <- 1-abs(v)/vmax
  res[v<0 & neg.rm] <- 0
  #  res[t<tpv] <- 0
  res
}

max2one.msi <- function(x)
{ 
  #  mx <- max(x)
  #norm01(x/max(x))
  norm01(x)
}

min2one.msi <- function(x)
{ 
  #  mx <- max(x)
  #norm01(min(x)/x)
  1-norm01(x)
}


dist2one.msi <- function(x,d) # distance/var(x) threshold
{
  1-abs(x-d)
}


thres2one.msi <- function(x,th)
{
  
  x[x>=th] <- 1
  x[x<th] <- 0
  x
}

findParabol <- function(p,t,x,y)
{
  p <- getXYpos(t,p[1],p[2])
  res <- sum(c(p$x-x)^2,c(p$y-y)^2) 
  res 
}


plotby <- function(df,x,y,...)
{
	
	inX <- match(x,names(df))
	inY <- match(y,names(df))
	
	for(i in unique(df$trial)){
		print(c("trial",i))
		plot(df[df$trial==i,inX],df[df$trial==i,inY[1]],...)
		if(length(inY)==2)
			points(df[df$trial==i,inX],df[df$trial==i,inY[2]],col="red")
		if(length(inY)==3)
			points(df[df$trial==i,inX],df[df$trial==i,inY[3]],col="green")

#		abline(v=df[df$trial==i,"temps"][which(df[df$trial==i,"ttc"]==1)[1]],lty=2)	
		abline(v=df[df$trial==i,"ttc"],lty=2)
#		tt <- segment.pv(df[df$trial==i,"temps"],df[df$trial==i,"velt"],vmin=15,tmin=0.5)
		tt <- segment.pos(df[df$trial==i,"temps"],df[df$trial==i,"x"],xend=df[df$trial==i,"xinit"],dthres=0.1,tmin=0.0) 
		
		abline(v=tt,col=2)
		ANSWER <- readline("another? ")
		if (substr(ANSWER, 1, 1) == "n")
	    	stop("Stop")
		
	}
	
}

fitdens <- function(x,fn="normal")
{
	x <- x[is.na(x)==FALSE]
	fit <- fitdistr(x,fn)
	gp <- fit$estimate
	fitsd <- fit$sd
	if(fn=="gamma"){
		mu <- gp[1]*(1/gp[2] )
		sig <- sqrt(gp[1]*(1/gp[2] )^2) 
		
	}
	if(fn=="normal"){
		mu <- gp[1]
		sig <- gp[2]
		mu_sd <- fitsd[1]
		sig_sd <- fitsd[2]
	}
	c(mu,sig,mu_sd,sig_sd)
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
