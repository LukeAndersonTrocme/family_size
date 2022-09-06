# the following depends on "get_extended_family.R"
get_relative_family_size <- function(pedigree, list_of_parents){
  
  # three columns for extended family function
  three_col_ped <- pedigree %>% select(ind, mother, father)
  
  # get family size of mothers
  mother_family_size <-
    three_col_ped %>%
    filter(!is.na(mother), mother != 0) %>%
    group_by(mother) %>%
    tally(name = "family_size") %>%
    dplyr::rename(ind = mother)
  
  # get family size of fathers
  father_family_size <-
    three_col_ped %>%
    filter(!is.na(father), mother != 0) %>%
    group_by(father) %>%
    tally(name = "family_size") %>%
    dplyr::rename(ind = father)
  
  # get the grand parents for each individual
  extended_families <- get_extended_family(three_col_ped, list_of_parents)
  
  relative_family_size <-
    extended_families %>% 
    
    # STEP 1 : reshape data
    # first we pivot longer to : ind, relative, ind_id
    tidyr::pivot_longer(!ind, names_to = "relative", values_to = "ind_id") %>%
    filter(!is.na(ind_id)) %>%
    # remove unnecessary suffixes
    mutate_all(~gsub(".mom|.mom.x|.mom.y|.dad|.dad.x|.dad.y", "", .)) %>%
    # split columns (relevant for grouping downstream)
    tidyr::separate(., relative, into = c("great","grand","parents"), sep = "_") %>%
    # exception handling to keep columns sensible
    mutate(parents = case_when(great %in% c("mother", "father") ~ great,
                               grand %in% c("mother", "father") ~ grand,
                               TRUE ~ parents),
           grand = case_when(great == "grand" ~ great,
                             TRUE ~ grand),
           great = case_when(great == "great" ~ great)) %>% 
    # for some reason these columns are characters
    mutate_at('ind', as.numeric) %>% 
    mutate_at('ind_id', as.numeric) %>%
    
    # STEP 2 : get family size of relatives
    # first we get the size of the focal individuals family
    left_join(mother_family_size, by = "ind") %>%
    left_join(father_family_size, by = "ind") %>%
    # make it to a single column
    mutate(focal_family_size = case_when(
      !is.na(family_size.x) ~ family_size.x,
      !is.na(family_size.y) ~ family_size.y)) %>%
    dplyr::select(-family_size.y, -family_size.x) %>%
    dplyr::rename(focal_ind = ind, ind = ind_id) %>%
    # then we get the size of the focal individuals family
    left_join(mother_family_size, by = "ind") %>%
    left_join(father_family_size, by = "ind") %>%
    # make it to a single column
    dplyr::rename(relative_ind = ind) %>%
    mutate(relative_family_size = case_when(
      !is.na(family_size.x) ~ family_size.x,
      !is.na(family_size.y) ~ family_size.y)) %>%
    dplyr::select(-family_size.y, -family_size.x) %>%
    dplyr::rename(ind = focal_ind) %>%
    # now we add relevant metadata for downstream analyses
    left_join(pedigree) %>%
    dplyr::select(ind, focal_family_size, 
                  relative_ind, relative_family_size,
                  great, grand, parents,
                  decade, a, t, min_wts_name
                  )
  
  return(relative_family_size)
}