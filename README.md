# Band History App

This repo contains code for building a small web application for exploring the bands and shows I've seen.

## The Data

The Data is all housed in a publicly published Google Sheet CSV, which the app downloads anew at start-up.

If you want to use the template to explore your own data, you can build your own file and replace the value in the `R/fetch_band_info()` function, like so:

```{r}
fetch_band_info <- function() {
  # Published CSV of the Bands
  bands <- data.table::fread('http://your/file/here.csv')
  # Slight cleaning
  bands <- bands[!is.na(BandID),]
  return(bands)
}

```

The file is fairly simple -- here's a `head` of the `data.frame` from the file, so you can see the format.

           ShowID BandID                  Band                   Venue Year       Date   Notes
    1: 38836-1026   1077               Chromeo        Hamilton College 2006 2006-04-29 May Day
    2: 38836-1026   1112             Elf Power        Hamilton College 2006 2006-04-29 May Day
    3: 38836-1026   1380 The New Pornographers        Hamilton College 2006 2006-04-29 May Day
    4: 40105-1040   1339        Sunset Rubdown Logan Square Auditorium 2009 2009-10-19        
    5: 41406-1050   1232                  Metz            Lincoln Hall 2013 2013-05-12        
    6: 41406-1050   1250                No Joy            Lincoln Hall 2013 2013-05-12        

Note -- ShowID and BandID can be whatever you want, as long as they are unique.

## Installing It for yourself

The `renv.lock` file should allow you to use `renv` to get the proper library set up to run the app.

## The Application

The application itself is fairly basic, and definitely *NOT* mobile optimized (it, in fact, looks pretty bad on mobile).

You can see it in action by visiting <https://ashook.shinyapps.io/liveshowdash/>
