# script to perform dagnostics for Stock Synthesis models

# set the directory for the ss files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
dir.path <- getwd()
ss3lobter = r4ss::SS_output(file.path(dir.path))
wd <- getwd()
mod_path <- file.path(wd)

# create a new directory
new_mod_path <- file.path(mod_path, "base_diag")

# copy over the model files
r4ss::copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path, overwrite = TRUE)

# read in input files
start <- r4ss::SS_readstarter(file = file.path(new_mod_path, "starter.ss"),  verbose = FALSE)
dat <- r4ss::SS_readdat(file = file.path(new_mod_path, start$datfile), verbose = FALSE)
ctl <- r4ss::SS_readctl(file = file.path(new_mod_path, start$ctlfil), verbose = FALSE, use_datlist = TRUE, datlist = dat)
fore <- r4ss::SS_readforecast(file = file.path(new_mod_path, "forecast.ss"), verbose = FALSE)

# run the modified model
setwd(mod_path)
r4ss::run(dir = new_mod_path, exe = "ss3_win")

# investigate the model run
myreplist <- r4ss::SS_output(dir = mod_path)
r4ss::SS_plots(replist = myreplist)

# retrospectives 
# do a retrospective
retro_dir <- file.path(mod_path, "retro_dir")
r4ss::copy_SS_inputs(dir.old = mod_path, dir.new = retro_dir, overwrite = TRUE)
dir_retro <- file.path(tempdir(check = TRUE), "retrospectives")
dir.create(dir_retro, showWarnings = FALSE)
list.files(new_mod_path)
file.copy(from = list.files(new_mod_path, full.names = TRUE), to = dir_retro)
r4ss::retro(dir = dir_retro, exe = "ss3_win", years = 0:-5, verbose = FALSE)

# Visualizing Output
retro_mods <- r4ss::SSgetoutput(dirvec = file.path(dir_retro, "retrospectives", 
                                                   paste0("retro", seq(0, -5, by = -1))), verbose = FALSE)
retroSummary <- r4ss::SSsummarize(retro_mods, verbose = FALSE)
ss3diags::SSplotRetro(retroSummary, subplots = "SSB", add = TRUE) # SSB or F
ss3diags::SSplotRetro(retroSummary, subplots = "F", add = TRUE) # SSB or F


# Likelihood Profile
dir_tmp <- tempdir(check = TRUE)
likelihood_dir <- file.path(mod_path, "likelihood_dir")
r4ss::copy_SS_inputs(dir.old = mod_path, dir.new = likelihood_dir, overwrite = TRUE) 
dir_profile <- file.path(likelihood_dir, "profile")
dir.create(dir_profile, showWarnings = FALSE, recursive = TRUE)
list.files(likelihood_dir)
file.copy(from = list.files(likelihood_dir, full.names = TRUE), to = dir_tmp)

# Run a profile on R0
r4ss::run(dir = dir_profile, exe = "ss3_win", verbose = FALSE)
files <- c("data.ss", "control.ss_new", "starter.ss", "forecast.ss", "ss3.par", "ss3_win.exe")
file.copy(from = file.path(likelihood_dir, files), to = dir_profile)
CTL <- r4ss::SS_readctl_3.30(file = file.path(dir_profile, "control.ss_new"), datlist = file.path(dir_profile, "data.ss"))
CTL$SR_parms

# get the estimated r0 value
r0 <- CTL$SR_parms$INIT[1]
r0_vec <- seq(r0 - 1, r0 + 1, by = 0.2)
r0_vec

# modify the starter file to making sure the likelihood is calculated for non-estimated quantities.
START <- r4ss::SS_readstarter(file = file.path(dir_profile, "starter.ss"), verbose = FALSE)
START$prior_like <- 1
START$ctlfile <- "control_modified.ss"
r4ss::SS_writestarter(START, dir = dir_profile, overwrite = TRUE, verbose = F)
r4ss::profile(
  dir = dir_profile,
  newctlfile = "control_modified.ss",
  string = "SR_LN",
  profilevec = r0_vec,
  conv_criteria = 0.0001,
  exe = "ss3_win",
  verbose = FALSE)

# plot the profile
profile_mods <- r4ss::SSgetoutput(dirvec = dir_profile, keyvec = 1:length(r0_vec), verbose = FALSE)
profile_mods_sum <- r4ss::SSsummarize(profile_mods, verbose = FALSE)
r4ss::SSplotProfile(profile_mods_sum,
                    profile.string = "SR_LN",
                    profile.label = "SR_LN(R0)")

# plot data-type and fleet-specific profiles 
r4ss::sspar(mfrow = c(1, 2))
r4ss::PinerPlot(profile_mods_sum,
                component = "Length_like",
                main = "Length")
r4ss::PinerPlot(profile_mods_sum,
                component = "Surv_like",
                main = "Survey")


# residual analyses
# runs test
files_path <- system.file("extdata", "simple_small", package = "r4ss")
report <- r4ss::SS_output(dir = files_path, verbose = FALSE, printstats = FALSE)
r4ss::sspar(mfrow = c(1, 2))
ss3diags::SSplotRunstest(report, add = TRUE)

# plot
r4ss::sspar(mfrow = c(2, 2))
ss3diags::SSplotRunstest(report, subplots = "len", indexselect = 1, add = TRUE)
ss3diags::SSplotRunstest(report, subplots = "age", indexselect = 2, add = TRUE)
ss3diags::SSplotRunstest(report, subplots = "age", indexselect = 2, add = TRUE, ylim = c(-0.5, 0.5))
ss3diags::SSplotRunstest(report, subplots = "age", indexselect = 2, add = TRUE, ylim = c(-0.5, 0.5), ylimAdj = 1)

# p-value for the runs test
rcpue <- ss3diags::SSrunstest(report, quants = "cpue")
rlen <- ss3diags::SSrunstest(report, quants = "len")
rbind(rcpue, rlen)

# rmse
r4ss::sspar(mfrow = c(2, 2))
ss3diags::SSplotJABBAres(report, subplots = "cpue", add = TRUE)
ss3diags::SSplotJABBAres(report, subplots = "age", add = TRUE)
ss3diags::SSplotJABBAres(report, subplots = "len", add = TRUE, ylim = c(-0.2, 0.2))


# jittering
jitter_dir <- file.path(mod_path, "retro_dir")
r4ss::copy_SS_inputs(dir.old = mod_path, dir.new = jitter_dir, overwrite = T) # copy over the stock synthesis model
r4ss::get_ss3_exe(dir = jitter_dir)
jitter_loglike <- r4ss::jitter(
  dir = jitter_dir,
  #model = "ss3_win", 
  Njitter = 100,
  verbose = F,
  jitter_fraction = 0.1 
)



# tune composition data
# copy model input files
r4ss::copy_SS_inputs(dir.old = mod_path, dir.new = new_mod_path, verbose = F, overwrite = T)

# copy over the Report file 
file.copy(
  from = file.path(mod_path, "Report.sso"),
  to = file.path(new_mod_path, "Report.sso")
)

# copy comp report file
file.copy(
  from = file.path(mod_path, "CompReport.sso"),
  to = file.path(new_mod_path, "CompReport.sso"))

# run the model
tune_info <- r4ss::tune_comps(
  option = "MI",
  niters_tuning = 1,
  dir = mod_path,
  allow_up_tuning = TRUE,
  model = "ss_win",
  verbose = FALSE
)
tune_info

# run the  model using Dirichlet-multinomial parameters to weight
mod_path_dm <- file.path(tempdir(), "base_mod_new_dm")
copy_SS_inputs(dir.old = mod_path, dir.new = mod_path_dm, verbose = FALSE, copy_exe = F)

# copy over the Report file
file.copy(
  from = file.path(mod_path, "Report.sso"),
  to = file.path(mod_path_dm, "Report.sso")
)

# copy comp report file
file.copy(
  from = file.path(mod_path, "CompReport.sso"),
  to = file.path(mod_path_dm, "CompReport.sso")
)

# add Dirichlet-multinomial parameters and rerun
DM_parm_info <- SS_tune_comps(
  option = "DM",
  niters_tuning = 1, 
  dir = mod_path_dm,
  model = "ss_win",
  extras = "-nohess",
  verbose = FALSE
)
DM_parm_info[["tuning_table_list"]]
