tar_plan(
  # Load the required paths
  input_folders = list(
    ForestClim_12 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_12/",
    ForestClim_15 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_15/",
    ForestClim_05 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_05/",
    ForestClim_06 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_06/",
    cec = "/lustre1/scratch/348/vsc34871/output/tiles/cec/",
    clay = "/lustre1/scratch/348/vsc34871/output/tiles/clay/"
  ),
  tar_target(mdl_paths,
             list.files(
               "/lustre1/scratch/348/vsc34871/input/Models/",
               full.names = TRUE
             )),
  # Make future species distributions
  tar_target(futureSDMs,
             predict_futSDM(input_folders, mdl_paths),
             pattern = map(mdl_paths)
  )
)

#Test#
mdl_paths <- list.files(
  "I:/SDMs_France/Results/models/",
  full.names = TRUE
)

input_folders = list(
  cec = "I:/SDMs_France/pred_tiles/cec/",
  CHELSA_bio12_EU_2000.2019 = "I:/SDMs_France/pred_tiles/CHELSA_bio12_EU_2000.2019/",
  CHELSA_bio15_EU_2000.2019 = "I:/SDMs_France/pred_tiles/CHELSA_bio15_EU_2000.2019/",
  clay = "I:/SDMs_France/pred_tiles/clay/",
  Elevation = "I:/SDMs_France/pred_tiles/Elevation/",
  Micro_BIO5_EU_CHELSAbased_2000.2020 = "I:/SDMs_France/pred_tiles/Micro_BIO5_EU_CHELSAbased_2000.2020/",
  Micro_BIO6_EU_CHELSAbased_2000.2020 = "I:/SDMs_France/pred_tiles/Micro_BIO6_EU_CHELSAbased_2000.2020/",
  Slope = "I:/SDMs_France/pred_tiles/Slope/"
)
