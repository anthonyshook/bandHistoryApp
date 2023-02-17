fetch_band_info <- function() {
# Published CSV of the Bands
bands <- data.table::fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ9bGvpClTiv8tu-hJxDtZiHsbuoQLcXNdRNPWZwTiGPxWBscDw0z_d0PMmcrJLA81hPmnaVpSJIHgE/pub?gid=0&single=true&output=csv')
# Slight cleaning
bands <- bands[!is.na(BandID),]
return(bands)
}
