library(dplyr)
library(specr)

# dat <- readRDS("cleaned_data_full.RDS")
# 
# #create two gaming variables
# 
# if (!"game_hours" %in% names(dat) && "game_minutes" %in% names(dat)) {
#   dat <- mutate(dat, game_hours = game_minutes / 60)
# }
# #linear models with a two-part gaming structure
# # Two-part encoding of gaming
# dat <- dat %>%
#   mutate(
#     any_game       = as.integer(game_hours > 0),
#     game_hours_pos = ifelse(game_hours > 0, game_hours, NA_real_)
#   )
# 
# #keep wellbeing as outcome, keep model = "lm"
# 
# wellbeing_vars_01 <- c(
#   "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
#   "Satis_01","Worth_01","Happy_01",
#   "Anxious_rev_01","GenHlth_rev_01",
#   "Rushed_01","Stressed_01"
# )
# 
# controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")
# 
# y_present <- intersect(wellbeing_vars_01, names(dat))
# c_present <- intersect(controls_all,      names(dat))
# 
# message("SCA with ", length(y_present), " outcomes and ", length(c_present), " controls.")
# 
# ##Original spec: simple linear effect of game_hours (full sample)
# 
# 
# # 1 Simple linear effect: game_hours
# specs_linear <- setup(
#   data     = dat,
#   y        = y_present,
#   x        = "game_hours",
#   model    = "lm",
#   controls = c_present,
#   subsets  = NULL  # or age_band subset list if desired
# )
# 
# res_linear <- specr(specs_linear)
# 
# #Any vs none: binary gamer indicator
# # 2 Gamer vs non-gamer: any_game
# specs_any <- setup(
#   data     = dat,
#   y        = y_present,
#   x        = "any_game",
#   model    = "lm",
#   controls = c_present,
#   subsets  = NULL
# )
# 
# res_any <- specr(specs_any)
# 
# #Dose among gamers only
# 
# # 3 Dose among gamers only: restrict to players, use game_hours_pos
# dat_gamers <- dat %>% filter(game_hours > 0)
# 
# specs_dose <- setup(
#   data     = dat_gamers,
#   y        = y_present,
#   x        = "game_hours_pos",
#   model    = "lm",
#   controls = c_present,
#   subsets  = NULL
# )
# 
# res_dose <- specr(specs_dose)
# 
# #res_any tells about “whether playing at all”.
# #res_dose tells about “how much, among those who play”.
# #res_linear is the “single-equation” version (everyone in one regression).
# 
# #Each  plotted and saved:
# p_linear <- plot(res_linear)
# p_any    <- plot(res_any)
# p_dose   <- plot(res_dose)
# 
# ggplot2::ggsave("sca_plot_full_linear.png", p_linear, width = 11, height = 7, dpi = 300)
# ggplot2::ggsave("sca_plot_full_any.png",    p_any,    width = 11, height = 7, dpi = 300)
# ggplot2::ggsave("sca_plot_full_dose.png",   p_dose,   width = 11, height = 7, dpi = 300)
# 
# saveRDS(res_linear, "sca_results_full_linear.RDS")
# saveRDS(res_any,    "sca_results_full_any.RDS")
# saveRDS(res_dose,   "sca_results_full_dose.RDS")
# 
# #This is conceptually aligns with the two-process but keeps everything in a familiar linear modelling framework suited to the outcomes.


#HURDLE STARTS HERE
library(pscl)

if (!"game_hours" %in% names(dat) && "game_minutes" %in% names(dat)) {
  dat <- mutate(dat, game_hours = game_minutes / 60)
}

# Two-part encoding of gaming
dat <- dat %>%
  mutate(
    any_game       = as.integer(game_hours > 0),
    game_hours_pos = ifelse(game_hours > 0, game_hours, NA_real_)
  )

# assume game_minutes is an integer count
dat <- readRDS("cleaned_data_full.RDS")
dat_hurdle <- dat %>%
  filter(!is.na(game_minutes))

# Basic hurdle model of gaming time
m_hurdle <- hurdle(
  game_minutes ~ SatInc_01 + Happy_01 + Worth_01 + DVAge + DMSex + Educ_bin,
  data      = dat_hurdle,
  dist      = "negbin",
  zero.dist = "binomial"
)

summary(m_hurdle)
