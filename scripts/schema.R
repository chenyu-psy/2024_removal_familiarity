

r_fun <- function(t, intercept, slope) {
  
  y <- brms::inv_logit_scaled(intercept + slope * t)
  
  return(y)
}

ac_fun <- function(t, intercept, slope) {
  
  y <- intercept + slope * t
  
  return(y)
  
}

library(tidyverse)

Plot_r <- ggplot2::ggplot()+
  ggplot2::geom_function(fun = r_fun, args = list(intercept = -1, slope = 1), xlim = c(0, 4))+
  ggplot2::geom_function(fun = r_fun, args = list(intercept = 1, slope = -1), xlim = c(0, 4))+
  ylim(0,1)+
  labs(
    x = "Block",
    title = "Removal parameter"
  )+
  theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 10.5),
  )

ggplot2::ggsave(
  plot = Plot_r,
  path = "./figures/figures-Exp2/",
  filename = "Plot_Exp2_schema_r.pdf",
  width = 6, height = 4
)
  

Plot_ac <- ggplot2::ggplot()+
  ggplot2::geom_function(fun = ac_fun, args = list(intercept = 3.5, slope = 0.15), xlim = c(0, 4))+
  ggplot2::geom_function(fun = ac_fun, args = list(intercept = 3.5, slope = 0.3), xlim = c(0, 4))+
  ggplot2::geom_function(fun = ac_fun, args = list(intercept = 3.5, slope = -0.2), xlim = c(0, 4))+
  ylim(2.5,5)+
  labs(
    x = "Block",
    title = "Memory parameter"
  )+
  theme_bw()+
  theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 10.5),
  )

ggplot2::ggsave(
  plot = Plot_ac,
  path = "./figures/figures-Exp2/",
  filename = "Plot_Exp2_schema_ac.pdf",
  width = 6, height = 4
)

# Item memory and familiarity

Plot_familiarity <- ggplot2::ggplot()+
  ggplot2::geom_function(
    fun = ac_fun,
    args = list(intercept = 1, slope = 0.4),
    aes(linetype = "familiarity"))+
  ggplot2::geom_function(
    fun = ac_fun,
    args = list(intercept = 2, slope = 0.2),
    aes(linetype = "activation"))+
  scale_linetype_manual(
    values = c("familiarity" = "solid", "activation" = "dashed"))+
  xlim(0, 4)+
  ylim(0,3)+
  labs(
    x = "Block",
  )+
  theme_bw()+
  theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 10.5),
  )

ggplot2::ggsave(
  plot = Plot_familiarity,
  path = "./figures/figures-Exp2/",
  filename = "Plot_Exp2_schema_familiarity.pdf",
  width = 6, height = 4
)
