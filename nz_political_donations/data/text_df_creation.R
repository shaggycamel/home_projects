
library(tidyverse)
library(tidytext)

st_suffixes <- "road|rd|street|st|avenue|ave|boulevard|blvd|lane|ln|drive|dr|way|court|ct|plaza|plz|terrace|ter|place|pl|bay|crescent|cres|cr|trail|trl|turnpike|tpke|parkway|pkwy|causeway|cswy|row|beltway|bltway|crossing|xing|alley|aly|point|pt|pike|square|sq|landing|lndg|circle|cir|valley|val"


# -------------------------------------------------------------------------

x <- image_text[str_detect(names(image_text), "^ACT")]
df_raw <- tibble(image=names(x), text=unlist(x)) |> 
    mutate(
        party = str_remove(str_extract(image, "[:graph:]*-Annual"), "-Annual")
        , image = str_remove(str_extract(image, "\\d{2}.png"), ".png") |> as.numeric()
        , .before = everything()
    ) |> 
    unnest_lines(text, text) 


# -------------------------------------------------------------------------


df <- filter(df_raw, image > 1, image < 10) |> 
    mutate(tags = case_when(
        str_detect(text, "^(.{2,3})?part [a-i]") 
            & lead(image, 5) != image + 1 
                ~ str_extract(text, "part [a-i]")
        , str_detect(str_remove(text, "[:punct:]"), "^((donor|contributor|overseas contributor)s )?name and") 
            | str_detect(text, "(donation|payment) received")
                ~ "table_start"
        , lead(image) == image + 1 
            | is.na(lead(image)) 
            | lead(image) < image
                ~ "table_end"
    )) |> 
    mutate(filt_ass = tags) |> 
    fill(filt_ass) |> 
    mutate(filt_ass = if_else(filt_ass == "table_start" & is.na(tags), "table", filt_ass)) |> 
    filter(!(is.na(tags) & str_detect(filt_ass, "part"))) |> 
    mutate(benefactor = case_when(
       str_detect(text, glue::glue("\\d+( st)? \\w+ {st_suffixes}"))
        & str_count(text, ",") >= 2 
            ~ TRUE
       , TRUE ~ FALSE
    ))
               


