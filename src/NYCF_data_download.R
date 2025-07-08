require("googledrive")

## Run the line below to download the study's data. 
## You will be required to sign up with a Google Drive account and grant access to the tidyverse library
## You will also be asked for permission to cache your google drive credentials, to which you may answer "No" ("2") in order to prevent them from being saved in your R profile.

drive_download(as_id("1BGL2lXki-ZY6_WyqblaU4Gf0zFUZ3Jhb"), path = "data_2.zip")

unzip("data.zip")
