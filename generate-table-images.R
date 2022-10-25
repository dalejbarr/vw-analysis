## FUNCTIONS
source("codebook.R")
library("tidyverse")

write_table_image <- function(x, file, 
                              caption = NULL, density = 600L) {
  x %>%
    knitr::kable(caption = caption) %>%
    kableExtra::kable_styling(full_width = FALSE,
                              bootstrap_options = "striped",
                              position = "left") %>%
    kableExtra::column_spec(1, bold = TRUE, monospace = TRUE) %>%
    kableExtra::column_spec(2, monospace = TRUE) %>%
    kableExtra::save_kable(file = file, density = density)
}

#######################################################
## main script

if (!dir.exists(img_path)) dir.create(img_path)

write_table_image(subjects_cb, file = file.path(img_path, "subjects.png"))

write_table_image(locations_cb, file = file.path(img_path, "locations.png"))

write_table_image(stimuli_cb, file = file.path(img_path, "stimuli.png"))

write_table_image(screens_cb, file.path(img_path, "screens.png"))

write_table_image(speech_cb, file.path(img_path, "speech-timings.png"))

write_table_image(trials_cb, file.path(img_path, "trials.png"))

write_table_image(eye_cb, file.path("table-images", "eye-data.png"))
