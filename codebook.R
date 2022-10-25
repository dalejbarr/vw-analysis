## FUNCTIONS

codebook_table <- function(x, descriptions = NULL) {
  types <- c("integer" = "<int>",
             "numeric" = "<dbl>",
             "character" = "<chr>",
             "factor" = "<fct>")
  
  dat_cb <- tibble::tibble(variable = names(x),
                   type = types[sapply(x, class)])
  
  if (!is.null(descriptions)) {
    if (length(descriptions) != nrow(dat_cb)) {
      stop("length of 'descriptions' (", length(descriptions),
           ") does not match number of variables in table (",
           nrow(dat_cb), ")")
    }
    dplyr::mutate(dat_cb, description = descriptions)
  } else {
    dat_cb
  }
}

#######################################################
## globals

img_path <- "table-images"
sub_id_desc <- "arbitrary value uniquely identifying each subject"
iv_id_desc <- "arbitary value uniquely identifying each version of each stimulus"
i_id_desc <- "arbitrary value uniquely identifying each stimulus set"
s_id_desc <- "arbitrary value uniquely identifying each display screen"
loc_desc <- "arbitrary integer identifying each rectangle"
sound_desc <- "name of the sound file containing the speech"
t_id_desc <- "arbitrary value uniquely identifying each trial within a subject"
ignored <- "(ignored)"

#######################################################
## main script

.subjects <- readr::read_csv("data-raw/subjects.csv",
                            col_types = "ic")

subjects_cb <- codebook_table(.subjects,
                              c(sub_id_desc,
                                "whether subject is in 'adult' or 'child' group"))

.locations <- readr::read_csv("data-raw/locations.csv",
                             col_types = "iiiii")

locations_cb <- codebook_table(.locations,
                               c(loc_desc,
                                 "horizontal coordinate of top-left corner in pixels",
                                 "vertical coordinate of top-left corner in pixels",
                                 "horizontal coordinate of bottom-right corner in pixels",
                                 "vertical coordinate of bottom-right corner in pixels"))


.stimuli <- readr::read_csv("data-raw/stimuli.csv", col_types = "iiciccc")

stimuli_cb <- codebook_table(.stimuli,
                             c(iv_id_desc,
                               i_id_desc,
                               "human-friendly identifier of the stimulus set",
                               s_id_desc,
                               "type of competitor, existing or novel",
                               "type of critical image: competitor, unrelated, untrained",
                               sound_desc))

.screens <- readr::read_csv("data-raw/screens.csv",
                           col_types = "iicc")

screens_cb <- codebook_table(.screens,
                             c(s_id_desc,
                               loc_desc,
                               "image's role in the set (target, critical, existing novel)",
                               "name of bitmap file"))

.speech <- readr::read_csv("data-raw/speech-timings.csv",
                           col_types = "ciii")

speech_cb <- codebook_table(.speech,
                            c(sound_desc,
                              "onset of the definite article [the] in milliseconds from file start",
                              "onset of the noun in milliseconds from file start",
                              "disambiguation point in milliseconds from file start"))

.trials <- readr::read_csv("data-raw/trials.csv",
                           col_types = "iiiiii")

trials_cb <- codebook_table(.trials,
                            c(sub_id_desc,
                              t_id_desc,
                              iv_id_desc,
                              "was the response accurate? (0: no, 1: yes)",
                              "response time from playback of sound file",
                              "location number that was clicked (1-4)"))

.eye <- readr::read_tsv("data-raw/adult/sub_001.gazedata",
                        col_types = "idiiiiiddddddiddddddiic")

eye_cb <- codebook_table(.eye,
                         c("arbitrary value uniquely identifying each frame within subject",
                           ignored,
                           ignored,
                           "horizontal point of gaze in pixels",
                           "vertical point of gaze in pixels",
                           "timestamp in seconds",
                           "millisecond portion of timestamp (cycles around)",
                           ignored,
                           ignored,
                           ignored,
                           ignored,
                           ignored, # DiameterPupilLeftEye
                           ignored,
                           ignored,
                           ignored,
                           ignored,
                           ignored, # XCameraPosRightEye
                           ignored,
                           ignored,
                           ignored,
                           ignored,
                           paste(t_id_desc, " (same as t_id)"),
                           "phase of the trial (Fixation, Preview, StimSlide)"))
