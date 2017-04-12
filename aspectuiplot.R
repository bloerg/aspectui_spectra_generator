library(FITSio)

aui_make_tile<-function(spectra_ids, plot_width, plot_height) {
    if (
        ! is.data.frame(spectra_ids) |
        ! is.integer(spectra_ids$mjd) |
        ! is.integer(spectra_ids$plate) |
        ! is.integer(spectra_ids$fiberid) |
          sqrt(nrow(spectra_ids)) != round(sqrt(nrow(spectra_ids)))
    ) {
        stop("'spectra_ids' has to be a data frame with quadratic nrow and mjd, plate, fiberid columns.")
    } else if (
        trunc(plot_width) != plot_width |
        trunc(plot_height) != plot_height |
        plot_height < 1 | plot_width < 1
    ) {
        stop("'plot_width' and 'plot_height' have to be integers > 0")
    } else {
        edge_length <- sqrt(nrow(spectra_ids))
        png(filename="/var/tmp/bla.png", width=plot_width, height=plot_height, units="px", bg="transparent")
        par(
            mfrow=c(edge_length, edge_length),
            mar=c(0,0,0,0)
        )
        for (X in 1:nrow(spectra_ids)) {
            aui_plot_spec(
                spectrum = aui_extract_spec(
                    mjd=spectra_ids$mjd[X], 
                    plate=spectra_ids$plate[X], 
                    fiberid=spectra_ids$fiberid[X],
                    pow10xaxis=0
                ),
                plot_width=plot_width, 
                plot_height=plot_height
            )
        }
        dev.off()
    }
}

aui_plot_spec<-function(spectrum, plot_width, plot_height) {
    if (
        ! is.data.frame(spectrum) |
        ! is.vector(spectrum$x) | ! is.vector(spectrum$y) |
        ! is.numeric(spectrum$x[1]) | ! is.numeric(spectrum$y[1])
    ) {
        stop("The plotted spectrum must be a data frame consisting of two numeric vectors x and y.")

    } else if (
        trunc(plot_width) != plot_width |
        trunc(plot_height) != plot_height |
        plot_height < 1 | plot_width < 1
    ) {
        stop("'plot_width' and 'plot_height' have to be integers > 0")
    } else {
        
        temp_margins<-par()$mar
        temp_margins[1]<-0.1 # bottom
        temp_margins[2]<-0.1 # left
        temp_margins[3]<-0.1 # top
        temp_margins[4]<-0.1 # right
        par(
            mar=temp_margins,
            lwd=0.6,
            cex=0.5, # general font size
            cex.axis=1.8, # axis tick label size
            cex.lab=0.8 # Achsen label (einheiten usw.)
    #            ,las=1 # y-axis labels horizontal
        )
        plot(
            spectrum$x,
            spectrum$y, 
            type="l",
            xaxt="n",
            yaxt="n",
            xlab="",
            ylab="",
            axes=F
        )
    }
}

aui_extract_spec <- function(mjd,plate,fiberid,pow10xaxis) {
  
    error <- 0
  
    # check wether mjd is a number larger than 0
    if (is.numeric(mjd) & mjd > 0) {
        # check whether mjd is an integer
        if (trunc(mjd) != mjd) {
            stop("'mjd' must be an integer")
         }
    } else {
        stop("'mjd' must be a positive integer")
    }
    
    # check wether plate is a number larger than 0
    if (is.numeric(plate) & plate > 0) {
        # check whether plate is an integer
        if (trunc(plate) != plate) {
              stop("'plate' must be an integer")
         }
    } else {
            stop("'plate' must be a positive integer")
    }
    
    # check wether fiberid is a number larger than 0
    if (is.numeric(fiberid) & fiberid > 0) {
        # check whether mjd is an integer
        if (trunc(fiberid) != fiberid) {
            stop("'fiberid' must be an integer")
         }
    } else {
        stop("'fiberid' must be a positive integer")
    }

    # check wether pow10axis is a boolean or a number equal to 0 or equal to 1
    if (pow10xaxis > 1 | pow10xaxis < 0) {
        stop("'pow10xaxis' must be True or False or 0 or 1")
    }
    
    spec_file_content<-data.frame(flux=numeric(), loglam=numeric())
    tryCatch(
        expr = {
            spec_file_content <- readFrameFromFITS(
                paste(
                    "/var/tmp/spec-", 
                    sprintf("%04d", plate),
                    "-",
                    mjd,
                    "-",
                    sprintf("%04d", fiberid),
                    ".fits",
                    sep=""
                )
            )
            return(
                data.frame(
                    x=spec_file_content$loglam,
                    y=spec_file_content$flux
                )
            )
        },
        error = function(err) {
           # print(err)
            return(data.frame(x=numeric(), y=numeric()))
        },
        finally = {
            # nothing
        }
    )

    
}
