#' Palette for color blindness
#' @description The palette could be used for heatmap or pie graph
#' @source  
#' http://mkweb.bcgsc.ca/biovis2012/
#' @details The names of the palette is approximal color name.
#' @export paletteMartin
#' @rdname colorPalette
#' @examples 
#' paletteMartin
paletteMartin <- c("Black"="#000000",
                   "SherpaBlue"="#004949",
                   "PersianGreen"="#009292",
                   "HotPink"="#ff6db6",
                   "CottonCandy"="#ffb6db",
                   "PigmentIndigo"="#490092",
                   "ScienceBlue"="#006ddb",
                   "Heliotrope"="#b66dff",
                   "Malibu"="#6db6ff",
                   "FrenchPass"="#b6dbff",
                   "RedBerry"="#920000",
                   "Brown"="#924900",
                   "MangoTango"="#db6d00",
                   "Harlequin"="#24ff24",
                   "LaserLemon"="#ffff6d")

#' @references 
#' Light A, Bartlein PJ (2004). The End of the Rainbow? Color Schemes for Improved Data Graphics.
#' EOS Transactions of the American Geophysical Union, 85(40), 385.
#' @source
#' http://geog.uoregon.edu/datagraphics/color_scales.htm
#' @details 
#' Green2Magenta16Steps: Useful for generic diverging data. 
#' @export Green2Magenta16Steps
#' @rdname colorPalette
#' @examples 
#' Green2Magenta16Steps
Green2Magenta16Steps = c("#005000", "#008600", "#00BB00", "#00F100", 
                         "#50FF50", "#86FF86", "#BBFFBB", "#FFFFFF",
                         "#FFF1FF", "#FFBBFF", "#FF86FF", "#FF50FF", 
                         "#F100F1", "#BB00BB", "#860086", "#500050")

#' @details 
#' Blue2DarkRed12/18Steps: Useful for temperature-like data, 
#' with a subjective interpretation (blue=cold, red=hot)
#' Blue2OrangeRed14Steps: Useful as an alternative to the red/blue temperature scale.
#' @export Blue2DarkRed12Steps
#' @rdname colorPalette
#' @examples 
#' Blue2DarkRed12Steps
Blue2DarkRed12Steps = c("#290AD8", "#264DFF", "#3FA0FF", "#72D9FF",
                        "#AAF7FF", "#E0FFFF", "#FFFFBF", "#FFE099", 
                        "#FFAD72", "#F76D5E", "#D82632", "#A50021")
#' @export Blue2DarkRed18Steps
#' @rdname colorPalette
#' @examples 
#' Blue2DarkRed18Steps
Blue2DarkRed18Steps = c("#2400D8", "#181CF7", "#2857FF", "#3D87FF",
                        "#56B0FF", "#75D3FF", "#99EAFF", "#BCF9FF", 
                        "#EAFFFF", "#FFFFEA", "#FFF1BC", "#FFD699", 
                        "#FFAC75", "#FF7856", "#FF3D3D", "#F72735",
                        "#D8152F", "#A50021")
#' @export Blue2OrangeRed14Steps
#' @rdname colorPalette
#' @examples Blue2OrangeRed14Steps
Blue2OrangeRed14Steps = c("#075AFF", "#3276FF", "#5990FF", "#8CB2FF", 
                          "#BFD4FF", "#E5EEFF", "#F7F9FF", "#FFFFCC",
                          "#FFFF99", "#FFFF00", "#FFCC00", "#FF9900",
                          "#FF6600", "#FF0000")

#' @details 
#' Blue2DarkOrange12/18Steps: Useful for data without a specific subjective color association.
#' @export Blue2DarkOrange12Steps
#' @rdname colorPalette
#' @examples 
#' Blue2DarkOrange12Steps
Blue2DarkOrange12Steps = c("#1E8E99", "#51C3CC", "#99F9FF", "#B2FCFF", 
                           "#CCFEFF", "#E5FFFF", "#FFE5CC", "#FFCA99", 
                           "#FFAD65", "#FF8E32", "#CC5800", "#993F00")
#' @export Blue2DarkOrange18Steps
#' @rdname colorPalette
#' @examples 
#' Blue2DarkOrange18Steps
Blue2DarkOrange18Steps = c("#006666", "#009999", "#00CCCC", "#00FFFF", 
                           "#33FFFF", "#65FFFF", "#99FFFF", "#B2FFFF", 
                           "#CBFFFF", "#E5FFFF", "#FFE5CB", "#FFCA99", 
                           "#FFAD65", "#FF8E33", "#FF6E00", "#CC5500", 
                           "#993D00", "#662700")
#' @details 
#' Blue2Green14Steps: Useful for data with a winter (blue) vs. summer (green) association.
#' @export Blue2Green14Steps
#' @rdname colorPalette
#' @examples 
#' Blue2Green14Steps
Blue2Green14Steps = c("#0000FF", "#3333FF", "#6565FF", "#9999FF", 
                      "#B2B2FF", "#CBCBFF", "#E5E5FF", "#E5FFE5", 
                      "#CBFFCB", "#B2FFB2", "#99FF99", "#65FF65", 
                      "#33FF33", "#00FF00")

#' @details 
#' Brown2Blue10/12Steps: Useful for data with a dry (brown) vs. wet (blue) association.
#' @export Brown2Blue10Steps 
#' @rdname colorPalette
#' @examples 
#' Brown2Blue10Steps
Brown2Blue10Steps = c("#662F00", "#996035", "#CC9B7A", "#D8AF97", 
                      "#F2DACD", "#CCFDFF", "#99F8FF", "#65EFFF", 
                      "#32E3FF", "#00A9CC")
#' @export Brown2Blue12Steps
#' @rdname colorPalette
#' @examples 
#' Brown2Blue12Steps
Brown2Blue12Steps = c("#331900", "#662F00", "#996035", "#CC9B7A",
                      "#D8AF97", "#F2DACD", "#CCFDFF", "#99F8FF", 
                      "#65EFFF", "#32E3FF", "#00A9CC", "#007A99")


#' @details 
#' Blue2Gray8Steps: Useful in particular for diverging data like cloudiness anomalies.
#' @export Blue2Gray8Steps
#' @rdname colorPalette
#' @examples 
#' Blue2Gray8Steps
Blue2Gray8Steps = c("#0099CC", "#66E5FF", "#99FFFF", "#CCFFFF", 
                    "#E5E5E5", "#999999", "#666666", "#333333")


#' @details 
#' Blue2Orange8/10/12Steps: Useful for data like sea-level pressure, 
#' with ansubjective association (blue=low, wet, orange=high, dry) 
#' @export Blue2Orange8Steps
#' @rdname colorPalette
#' @examples 
#' Blue2Orange8Steps
Blue2Orange8Steps = c("#007FFF", "#4CC3FF", "#99EDFF", "#CCFFFF", 
                      "#FFFFCC", "#FFEE99", "#FFC34C", "#FF7F00")
#' @export Blue2Orange10Steps
#' @rdname colorPalette
#' @examples 
#' Blue2Orange10Steps
Blue2Orange10Steps = c("#0054FF", "#3299FF", "#65CCFF", "#99EDFF", 
                       "#CCFFFF", "#FFFFCC", "#FFEE99", "#FFCC65",
                       "#FF9932", "#FF5500")
#' @export Blue2Orange12Steps
#' @rdname colorPalette
#' @examples 
#' Blue2Orange12Steps
Blue2Orange12Steps = c("#002AFF", "#1965FF", "#3299FF", "#65CCFF", 
                       "#99EDFF", "#CCFFFF", "#FFFFCC", "#FFEE99", 
                       "#FFCC65", "#FF9932", "#FF6619", "#FF2A00")


#' @details 
#' ModifiedSpectralScheme11Steps:	An alternative to the spectral scheme (no green)
#' @export ModifiedSpectralScheme11Steps
#' @rdname colorPalette
#' @examples 
#' ModifiedSpectralScheme11Steps
ModifiedSpectralScheme11Steps = c("#A50021", "#D82632", "#F76D5E", "#FFAD72",
                                  "#FFE099", "#FFFFBF", "#E0FFFF", "#AAF7FF", 
                                  "#72D8FF", "#3FA0FF", "#264CFF")


#' @details 
#' LightBlue2DarkBlue7/10Steps: Useful for precipitation-like data.
#' @export LightBlue2DarkBlue7Steps
#' @rdname colorPalette
#' @examples 
#' LightBlue2DarkBlue7Steps
LightBlue2DarkBlue7Steps = c("#FFFFFF", "#CCFDFF", "#99F8FF", "#66F0FF",
                             "#33E4FF", "#00AACC", "#007A99")
#' @export LightBlue2DarkBlue10Steps
#' @rdname colorPalette
#' @examples 
#' LightBlue2DarkBlue10Steps
LightBlue2DarkBlue10Steps = c("#E5FFFF", "#CCFAFF", "#B2F2FF", "#99E5FF", 
                              "#7FD4FF", "#65BFFF", "#4CA5FF", "#3288FF", 
                              "#1965FF", "#003FFF")

#' @details 
#' PairedColor12Steps: Attempt at a categorical color scale with colors
#' that may be distinguishable to all viewers
#' @export PairedColor12Steps
#' @rdname colorPalette
#' @examples 
#' PairedColor12Steps
PairedColor12Steps = c("#FFBF7F", "#FF7F00", "#FFFF99", "#FFFF32", 
                       "#B2FF8C", "#32FF00", "#A5EDFF", "#19B2FF", 
                       "#CCBFFF", "#654CFF", "#FF99BF", "#E51932")


#' @details 
#' SteppedSequential5Steps: Useful for portraying levels-within-categories
#' @export SteppedSequential5Steps
#' @rdname colorPalette
#' @examples 
#' SteppedSequential5Steps
SteppedSequential5Steps = c("#990F0F", "#B22C2C", "#CC5151", "#E57E7E", "#FFB2B2",
                            "#99540F", "#B26F2C", "#CC8E51", "#E5B17E", "#FFD8B2",
                            "#6B990F", "#85B22C", "#A3CC51", "#C3E57E", "#E5FFB2", 
                            "#0F6B99", "#2C85B2", "#51A3CC", "#7EC3E5", "#B2E5FF",
                            "#260F99", "#422CB2", "#6551CC", "#8F7EE5", "#BFB2FF")



#' Available color palette
#' @description List all the available color palettes.
#' @return a character vector contain available color palettes.
#' @export
#' @examples 
#' availablePalette()
availablePalette<- function(){
  c("paletteMartin", "Blue2DarkOrange12Steps","Blue2DarkOrange18Steps",
    "Blue2DarkRed12Steps","Blue2DarkRed18Steps","Blue2Gray8Steps",
    "Blue2Green14Steps","Blue2Orange10Steps","Blue2Orange12Steps",
    "Blue2Orange8Steps","Blue2OrangeRed14Steps","Brown2Blue10Steps",
    "Brown2Blue12Steps","Green2Magenta16Steps","LightBlue2DarkBlue10Steps",
    "LightBlue2DarkBlue7Steps","ModifiedSpectralScheme11Steps",
    "PairedColor12Steps","SteppedSequential5Steps")
}
#' Display available palette
#' @description Display all the available color palettes.
#' @return an \link[ggplot2:ggplot]{ggplot} object
#' @param ... parameters could be used by \link[ggplot2:geom_raster]{geom_tile}.
#' @export
#' @examples 
#' displayAvailablePalette()
displayAvailablePalette<- function(...){
  cols <- sapply(availablePalette(), get, simplify = FALSE)
  len <- lengths(cols)
  cols <- lapply(cols, function(.ele) c(.ele, rep("#FFFFFF", max(len)))[seq.int(max(len))])
  l <- unlist(cols, use.names = FALSE)
  d <- data.frame(x=unlist(sapply(cols, seq_along, simplify = FALSE), use.names = FALSE), 
                  y=factor(rep(names(cols), lengths(cols)), levels = rev(names(cols))), 
                  col=factor(l, levels=unique(l)))
  g <- ggplot(d, aes_string(x='x', y='y', fill='col')) + 
    geom_tile(...) + coord_equal() +
    scale_fill_manual(guide = FALSE, values = unique(l)) +
    theme(axis.line        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "Helvetica"))
  return(g)
}
