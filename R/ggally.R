
#' @title barchart for ggally
#' @description barchart to be used with ggally
#' @param data data frame 
#' @param mapping aes passed to ggplot
#' @param numbers display count or not, Default: F
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @return ggplot object (passed to ggally)
#' @rdname ally_bar
# trying to simplify, not exporting ally support for now. 
# #' @export 
#' @import ggplot2 

ally_bar <- function(data, mapping, numbers=F, angle=40, hjust=1){
  
  # mapping should be a x variable and should not contain y variable
  if("y" %in% names(mapping)){
    stop("stat_count() must not be used with a y aesthetic.")
  }
  if(!"x" %in% names(mapping)){
    stop("ally_bar requires an x variable")
  }
  
  # test for factor input
  x_col <- data[[deparse(mapping$x)]]
  if(!is.factor(x_col)){
    warning(paste(deparse(mapping$x), " is not a factor."))
  }

  p <- ggplot(data, mapping) + 
    geom_bar() +
    theme(axis.text.x = element_text(angle=angle, hjust=hjust),
          panel.grid = element_blank()) 
  
  if(numbers){
    # numbers does not work well in the scenario of many covariates
    p <- ggplot(data, mapping) + 
      geom_bar() +
      theme(axis.text.x = element_text(angle=angle, hjust=hjust),
            panel.grid = element_blank()) +
      geom_label(aes(label=..count.., y=(..count..)),
                 stat= "count", fontface="bold")
  }
  return(p)
}


#' @title boxplot for ggally
#' @description boxplot to be used with ggally
#' @param data data frame
#' @param mapping aes passed to ggplot
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @param x.grid panel grid on x, Default: F
#' @return ggplot object (passed to ggally)
#' @details DETAILS
#' @rdname ally_box
# #' @export 
#' @import ggplot2 

ally_box <- function(data, mapping, angle=40, hjust=1, x.grid=F){
  
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  p <- ggplot(data, mapping) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle=angle, hjust=hjust))
  
  if(!x.grid){
    p <- p +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  }
  return(p)
}


#' @title correlation plot for ggally
#' @description correlation plot to be used with ggally
#' @param data data frame
#' @param mapping aes passed to ggplot
#' @param cor_method method for correlation estimation, Default: 'pearson'
#' @param cor_use argument to cor, Default: 'pairwise.complete.obs'
#' @param cor_threshold threshold for color switch, Default: 0.4
#' @param cor_color color used at color switch, Default: 'red'
#' @return ggplot object (passed to ggally)
#' @seealso 
#'  \code{\link[vcd]{assocstats}}
#' @rdname ally_cor
# #' @export 
#' @importFrom vcd assocstats
#' @import ggplot2 

ally_cor <- function(data, mapping, 
                     cor_method='pearson',
                     cor_use = 'pairwise.complete.obs',
                     cor_threshold=0.4,
                     cor_color='red', ...) {
  
  x_col <- data[[deparse(mapping$x)]]
  y_col <- data[[deparse(mapping$y)]]
  
  ## If numeric columns 
  if(is.numeric(x_col) & is.numeric(y_col)){
    
    # Calculate correlation only if there are values different from zero
    # (important for ETA corr plots)
    if(length(unique(x_col))==1 | length(unique(y_col))==1){
      correlation <- 0
      cor_df <- data.frame(
        pos_x = (max(x_col, na.rm = TRUE)-min(x_col, na.rm = TRUE))/2,
        pos_y = (max(y_col, na.rm = TRUE)-min(y_col, na.rm = TRUE))/2,
        lab = "Corr:\nNA")
    }else{
      correlation <- cor(x_col, y_col, 
                         method=cor_method,
                         use=cor_use)
      cor_df <- data.frame(
        pos_x = (max(x_col, na.rm = TRUE)-min(x_col, na.rm = TRUE))/2,
        pos_y = (max(y_col, na.rm = TRUE)-min(y_col, na.rm = TRUE))/2,
        lab = paste0("Corr:\n", round(correlation, digits = 3)))
    }
  }
  
  ## If categorical columns 
  if(is.factor(x_col) & is.factor(y_col)){
    # Calculate correlation if more than 1 level
    if(length(levels(x_col)) > 1 & length(levels(y_col))>1){
      
      contingencyTab <- table(y_col, x_col)
      correlation <- vcd::assocstats(contingencyTab)$cramer
      
      # Correct position if vector includes NA
      if(any(is.na(x_col))){
        add_level_x <- 1 
      }else{
        add_level_x <- 0
      }
      if(any(is.na(yCol))){
        add_level_y <- 1 
      }else{
        add_level_y <- 0
      }
      cor_df <- data.frame(
        pos_x = (length(levels(x_col))+add_level_x)/2,
        pos_y = (length(levels(y_col))+add_level_y)/2,
        lab = paste0("Corr:\n", round(correlation, digits = 3)))
    }else{
      correlation <- 0
      cor_df <- data.frame(
        posX = (length(levels(x_col))+add_level_x)/2,
        posY = (length(levels(y_col))+add_level_y)/2,
        lab = "Corr:\n NA")
    }
  }
  
  # Set to red colour if more or less than threshold
  if(correlation <= -cor_threshold | correlation >= cor_threshold) {
    text_colour <- cor_color
  }else{
    text_colour <- "black"
  }
  
  # Plot
  p <- 
    ggplot(data) + 
    geom_text(data=cor_df, 
              aes(x=pos_x, y=pos_y, label=lab), 
              col=text_colour) + 
    theme(panel.grid = element_blank())
  
  return(p)
}


#' @title count plot for ggally
#' @description count plot to be used with ggally. To be used with categorical variables.
#' @param data data frame
#' @param mapping aes passed to ggplot
#' @param counts absolute number or relative, see details, Default: 'rel'
#' @param angle angle passed to theme axis.text Default: 35
#' @param hjust_x hjust passed to theme axis.text.x, Default: 1
#' @param vjust_y vjust passed to theme axis.text.y, Default:0
#' @param ... PARAM_DESCRIPTION
#' @return ggplot object (passed to ggally)
#' @details counts: one of 'abs' or 'rel'. To be expanded.
#' @rdname ally_count
# #' @export
#' @importFrom dplyr group_by mutate  
ally_count <- function(data, mapping, counts="rel", 
                       angle = 35, hjust_x=1, vjust_y=0, ...){
  
  x_col <- data[[deparse(mapping$x)]]
  y_col <- data[[deparse(mapping$y)]]
  
  if(!(is.factor(x_col) & is.factor(y_col))){
    stop(paste(deparse(mapping$x), "or", deparse(mapping$x),"is not a factor"))
  }
  
  if(counts=="abs"){
    count_n <- function(x){
      return(c( y=unique(x), label=length(x)))
    }
    # to be updated with only the number
    p <- ggplot(data, mapping) +
      geom_point(alpha=0) + # invisible layer to set the axes properly
      stat_summary(fun.data=count_n, geom = "label", size=3) + 
      theme(axis.text.x = element_text(angle = angle, hjust=hjust_x),
            axis.text.y = element_text(angle = angle, vjust=vjust_y))
  } 
  
  if(counts=="rel"){ 
    
    if(length(levels(x_col)) > 1 | length(levels(y_col))>1){
      
      # handling of NAs in plot
      if(any(is.na(x_col))){
        x_max <- length(levels(x_col))+1
      } else {
        x_max <- length(levels(x_col))
      }
      
      if(any(is.na(y_col))){
        y_max <- length(levels(y_col))+1
      }else {
        y_max <- length(levels(y_col))
      }
      
      # Get the contingency table
      contingency_tab <- table(y_col, x_col, useNA="ifany")
      contingency_tab <- as.data.frame(contingency_tab)
      
      # Summarize for x
      x_col_sum <- contingency_tab %>% 
        group_by(x_col) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               y = ifelse(is.na(y_col),
                          y_max+0.35,
                          as.numeric(y_col)+0.35)) # need to round
      
      # Summarize for y
      y_col_sum <- contingency_tab %>% 
        group_by(y_col) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               x=ifelse(is.na(x_col),
                        x_max-0.35,
                        as.numeric(x_col)-0.35)) # need to round
    } else {
      warning("All data in the same level.")
    }
    
    p <- ggplot(data, mapping) +
      geom_point(alpha=0) + # invisible layer to set the axes properly
      
      # add "anchor points" 
      geom_point(data=x_col_sum, aes(y=y_col, x=x_col), colour="gray15",
                 shape=16, size=1.5) +
      
      # Add numbers relative y-axis
      geom_text(data=x_col_sum, aes(y=y, x=x_col, label=relative), 
                size=2.8, colour="gray15") + 
      
      # Add numbers relative x-axis
      geom_text(data=y_col_sum, aes(y=y_col, x=x, label=relative), 
                size=2.8, colour="gray15") +
      
      coord_cartesian(xlim=c(0.75, seq(1, x_max, by=1)), 
                      ylim=c(seq(1, y_max, by=1), y_max+0.25)) + 
      
      theme(axis.text.x = element_text(angle = angle, hjust=hjust_x),
            axis.text.y = element_text(angle = angle, vjust=vjust_y))
  } 
  
  return(p)
}


#' @title linear regression with correlation threshold for ggally
#' @description scatter plot with linear regression and correlation threshold to be used with ggally
#' @param data data frame 
#' @param mapping aes passed to ggplot
#' @param cor_method method for correlation estimation, Default: 'pearson'
#' @param cor_use argument to cor, Default: 'pairwise.complete.obs'
#' @param cor_threshold threshold for color switch, Default: 0.4
#' @param cor_color colour for used at color switch, Default: 'red'
#' @param ... PARAM_DESCRIPTION
#' @return ggplot object (passed to ggally)
#' @rdname ally_scatter_lm_cor
# #' @export 

ally_scatter_lm_cor <- 
  function(data, mapping, 
           cor_method = "pearson",
           cor_use = 'pairwise.complete.obs',
           cor_threshold=0.4,
           cor_color="red", ...) {
    
    x_col <- data[[deparse(mapping$x)]]
    y_col <- data[[deparse(mapping$y)]]
    
    # Calculate correlation only if there are values different from zero
    # (important for ETA corr plots)
    if(length(unique(x_col))==1 | length(unique(y_col))==1) {
      correlation <- 0 
    } else {
      correlation <- cor(x_col, y_col, 
                         method=cor_method,
                         use=cor_use)
    }
    
    # Set to red colour if more or less than threshold
    if(correlation <= -cor_threshold | correlation >= cor_threshold) {
      point_color <- cor_color
    }else{
      point_color <- "black"
    }
    
    # Plot
    if(!all(x_col==0) & !all(y_col==0)){
      p <- 
        ggplot(data, mapping) + 
        geom_point(colour=point_color) + 
        geom_smooth(method="lm")
    }else{
      p <- ggplot(data, mapping) + 
        geom_point()
    }
    return(p)
  }

