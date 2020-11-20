#' Plotting function for more comprehensible gender effects
#'
#' @param
#' @keywords flat; violin
#' @export
#' @examples
#' library(AER)
#' data("PhDPublications")
#'
#' genderplot::gender_plot(PhDPublications, varname="prestige")
#' genderplot::gender_plot(PhDPublications, varname="prestige", lb=-1) #need to adjust lower bound
#' # to depict larger gender differences
#'
#' data(STAR) #data on effect of reducing class size on test scores in the early grades
#' genderplot::gender_plot(STAR, varname="read1") #read1 = reading in first grade
#' genderplot::gender_plot(STAR, varname="math3") #math3= math in 3rd grade
#'
#' data("TeachingRatings") #Data on course evaluations, course characteristics,
#' # and professor characteristics (beauty)
#' genderplot::gender_plot(TeachingRatings, varname="age") #more old professors are male
#' genderplot::gender_plot(TeachingRatings, varname="age", lb=-1, ub=2.5) #need to adjust
#' #lower and upper bound
#' genderplot::gender_plot(TeachingRatings, varname="beauty", lb=-1) #more beautiful
#' # professors are female
#' genderplot::gender_plot(TeachingRatings, varname="eval") # higher evaluated professors
#' # are more often male than female





gender_plot <- function(data, varname, gender="gender",
                        color =NA,  lb=NA, ub=NA, varname_title=NA){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {"The `ggplot2` package is not installed but required. You can install it using install.packages('ggplot2')"}
  if (!requireNamespace("dplyr", quietly = TRUE)) {"The `dplyr` package is not installed but required. You can install it using install.packages('dplyr')"}
  if (!requireNamespace("tidyr", quietly = TRUE)) {"The `tidyr` package is not installed but required. You can install it using install.packages('tidyr')"}

  requireNamespace("ggplot2"); requireNamespace("tidyr"); requireNamespace("dplyr");



  if(is.na(varname_title)) varname_title <- varname
  if(is.na(color)){color <- "#B9001EFF" }
  if(is.na(lb)){lb <- -0.5}
  if(is.na(ub)){ub <- 1 }
  data <- as.data.frame(data)



  tmp <- data.frame(gender=data[,gender])
  tmp$Sum <- data[,varname]

  if(!is.numeric(tmp$Sum)){
    stop(paste0(varname," must be a numeric vector"))
  }
  if(length(tmp$Sum) == 1){
    warning(paste0(varname," has length 1, will return NA"))
  }


  if(!all(levels(tmp$gender) %in% c("male","female"))){

    if (!requireNamespace("gendercoder", quietly = TRUE)){"The `gendercoder` package is not installed. It is optional: you can install it via  devtools::install_github('ropenscilabs/gendercodeR'), or you provide a gender column with factors labelled 'male' and 'female'"}
    tmp$gender <- recode_gender(tmp$gender, dictionary = broad)
  }
  tmp$gender <- factor(tmp$gender, levels=c("male","female"),
                       ordered=TRUE)

  breaks_left <- c(0,.10, .25, .49)  ## you could set these breaks differently
  breaks_right <- c(.51, .75,.90,1)


  tmp <- tmp %>%
    dplyr::mutate(quant= dplyr::ntile(Sum,100) / 100)


  dd <- data.frame()

  for(p in c(.10, .25, .49, .51, .75,.90)){ #c(.01, .05, .10, .25, .49, .51, .75,.90, .95, .99)
    if(p < .5){
      tmp2 <- tmp[which(tmp$quant <= p ),]
      N_boys <- length(which(tmp2$gender=="male"))
      N_girls <- length(which(tmp2$gender=="female"))
      if(is.na(sd(tmp2$Sum,na.rm=T))){cd <- 0.00; cd_low <- NA; cd_high <- NA
      } else if(sd(tmp2$Sum, na.rm=T) ==0 ){cd <- 0.00; cd_low <- NA; cd_high <- NA
      } else {
        cd <- effsize::cohen.d(Sum ~gender, tmp2)
        cd_low <- as.numeric(cd$conf.int[1])
        cd_high <- as.numeric(cd$conf.int[2])
        cd <- round(cd$estimate,2)}
    }else{
      tmp2 <- tmp[which(tmp$quant > p ), ]
      N_boys <- length(which(tmp2$gender=="male"))
      N_girls <- length(which(tmp2$gender=="female"))
      if(is.na(sd(tmp2$Sum,na.rm=T))){cd <- 0.00; cd_low <- NA; cd_high <- NA
      }else if(sd(tmp2$Sum) ==0){cd <- 0.00; cd_low <- NA; cd_high <- NA} else {
        cd <- effsize::cohen.d(Sum ~gender, tmp2)
        cd_low <- as.numeric(cd$conf.int[1])
        cd_high <- as.numeric(cd$conf.int[2])
        cd <- round(cd$estimate,2)}
    }
    row <- data.frame( #grade=g,
      percentile=p, d=cd, d_low=cd_low, d_high=cd_high,
      N_boys=N_boys, N_girls=N_girls)
    dd <- rbind(dd,row)

  }


  dd2 <- dd %>%
    dplyr::group_by(percentile) %>%
    dplyr::summarise(Nsum_boys=sum(N_boys), Nsum_girls = sum(N_girls)) %>%
    dplyr::mutate(N_total = Nsum_boys+Nsum_girls, MF_ratio = round(Nsum_boys/Nsum_girls,2)) %>%
    dplyr::ungroup()

  dd3 <- dd2 %>%
    dplyr::select(percentile, Nsum_boys, Nsum_girls) %>%
    tidyr::pivot_longer(-percentile)

  # for plotting distributions
  tmp$section1 <- NA; tmp$section2 <- NA
  tmp$section1 <- cut(tmp$quant, breaks_left)
  tmp$section2 <- cut(tmp$quant, breaks_right)
  tmp$section <- dplyr::coalesce(tmp$section1, tmp$section2)

  tmp$percentile <- car::recode(tmp$section, "'(0,0.1]' = 0.10; '(0.1,0.25]' =0.25;'(0.25,0.49]'=0.49; '(0.51,0.75]'=0.51; '(0.75,0.9]'=0.75;'(0.9,1]'=0.90")
    tmp$percentile <- as.numeric(as.character(tmp$percentile))
  tmp <- tmp[!is.na(tmp$percentile),]
  # z-score of sum
  tmp <- tmp %>% dplyr::group_by(percentile) %>% dplyr::mutate(Sum_z = scale(Sum))
  tmp$Sum_z <- tmp$Sum_z/10


  #plot
  pd <- ggplot2::position_dodge(.02)
  dodge <- ggplot2::position_dodge(1)
  dd2$label_pos <- ifelse(dd2$percentile == .49, .465,
                          ifelse(dd2$percentile == .51, .535, dd2$percentile))

  #
  ggplot2::ggplot(data=dd, ggplot2::aes(x=percentile,y=d))+
    ggplot2::geom_hline(yintercept=0)+
    ggplot2::geom_hline(yintercept=(1/4)+.5, linetype="dashed", col="grey65")+
    geom_flat_violin(data=tmp[tmp$gender== "female", ], ggplot2::aes(x=percentile, y = Sum_z, group=factor(percentile)), fill="red",  col=NA,size=.7,width=.2, alpha = 0.2)+
    geom_flat_violin(data=tmp[tmp$gender== "male", ], ggplot2::aes(x=percentile, y = Sum_z, group=factor(percentile)), fill="blue", col=NA, size=.7,width=.2, alpha = 0.2) +
    ggplot2::geom_errorbar(  ggplot2::aes( ymin=d_low, ymax=d_high),position=pd,
                    colour="grey35", width=.02, alpha=.4) +
    ggplot2::geom_line(data=dd2, ggplot2::aes(x=percentile, y=(MF_ratio/4)+.5), col="grey65" )+
    ggplot2::geom_point(col=color, size=2.5, position=pd, alpha=.7)+
    ggplot2::geom_line(col=color,size=.9,linetype="solid", position=pd, alpha=.7)+
    ggplot2::geom_text(data=dd2, ggplot2::aes(x=label_pos, y=-.4, label=Nsum_boys),
              col="#66D9DC")+
    ggplot2::geom_text(data=dd2, ggplot2::aes(x=label_pos, y=-.48, label=Nsum_girls),
              col="#FBADA7")+
    ggplot2::geom_text(data=dd2, ggplot2::aes(x=0.00, y=-.44, label="N ="))+
    ggplot2::geom_text(data=dd2, ggplot2::aes(x=percentile, y=(MF_ratio/4)+.5, label=MF_ratio),
              col="grey55", fontface="bold")+
    ggplot2::geom_label(ggplot2::aes(.22,1.25, label="Differences in favor of boys"),
               col="gray30", fill="#66D9DC", size=2.5)+
    ggplot2::geom_label(ggplot2::aes(.22,-1.25, label="Differences in favor of girls"),
               col="gray30", fill="#FBADA7", size=2.5)+
    ggplot2::theme_bw()+
    ggplot2::scale_color_manual(guide=FALSE, values=color)+
    ggplot2::ggtitle(paste("Differences across percentiles for",varname_title))+
    #   labs(subtitle="Colored points: Differences in Cohen's d and 95% CI; Grey: Male/female ratio (second y-axis); \nbottom: sample size per percentile (red:girls; blue: boys)")+
    ggplot2::scale_y_continuous(name=" \t Cohen's d", limits=c(lb, ub), breaks=seq(lb,0.5, ub-0.5),  sec.axis=ggplot2::sec_axis( ~(.*4)-2,
                                                                                                               breaks=c(0,1,2,3),
                                                                                                               name="Male:female ratio"))  +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=c(0.10, 0.25,0.49, 0.50, 0.51, 0.75, 0.90), labels=c("0.10", "0.25","", "0.50", "", "0.75", "0.90"))+
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(hjust=.33))+
    ggplot2::theme(axis.text=ggplot2::element_text(size=13),
                   axis.title = ggplot2::element_text(size=14),
                   title = ggplot2::element_text(size=14))


}
