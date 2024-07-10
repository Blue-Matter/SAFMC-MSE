# This script sources the individual scripts for building the operating models
# and running the analyses


# 1. Build Base Case OMs
source('Run_Analysis/1. Build_Base_Case_OMs.R')

# 2. Build Sensitivity OMs
source('Run_Analysis/2. Build_Sensitivity_OMs.R')

# 3. Run Historical Simulations
source('Run_Analysis/3. Simulate_Historical_Fisheries.R')

# 4. Report Stock Status from OMs
source('Run_Analysis/4. Generate_Figures.R')

# 5. Create MPs
source('Run_Analysis/5. create_MPs.R')

# 6. Run Base Case Projections
source('Run_Analysis/6. Projections_Base_Case.R')

# 7. Base Case Performance Results
source('Run_Analysis/7. Results_Base_Case.R')

# 8. Run Sensitivity Projections
# source('Run_Analysis/8. Projections_Sensitivities.R')

# 9. Sensitivity Performance Results
# source('Run_Analysis/9. Results_Sensitivities.R')

