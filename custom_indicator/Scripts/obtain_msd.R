
load_secrets()

# set_paths(folderpath_msd = "Data",
#                     folderpath_datim =  "Data",
#                     folderpath_downloads =  "Data")
## comment in/out the above after setting initially


#create active session

sess <- grabr::pano_session(username = pano_user(), password = pano_pwd())


# Extract data items details
url <- "https://pepfar-panorama.org/forms/downloads/"

cont <- grabr::pano_content(page_url = url, session = sess)


# Download most recent PSNUxIM MSD ------------------------------------------------
# Extract data items details
dirs <- grabr::pano_elements(page_html = cont)

dir_mer_path <- dirs %>%
  filter(str_detect(item, "^MER")) %>%
  pull(path)

mer_items <- grabr::pano_content(page_url = dir_mer_path, session = sess) %>%
  grabr::pano_elements(page_url = dir_mer_path)
# Extract MER data items details from HTML CODE
dest_path <- paste0(si_path(),"/Temp/")


# pull latest psnuXim MSD ---------------------------------------------------------
url_ou_im <- mer_items %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_OU_IM_FY2.*.zip$")) %>%
  pull(path) %>%
  first() 



# quick fix to filepaths --------------------------------------------------
grabr::pano_download(item_url = url_ou_im, session = sess)
