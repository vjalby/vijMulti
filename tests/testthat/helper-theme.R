# Renders a results' plot (an Image) through jmvcore (as jamovi does) and returns the ggplot drawn
renderedPlot <- function(image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(image)
    ggplot2::last_plot()
}

# Text layers of a ggplot (ggplot2's and ggrepel's)
textLayers <- function(p) {
    Filter(function(l) inherits(l$geom, c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel")), p$layers)
}

# Checks a plot drawn with plotTheme = "ggBw" and fontFamily = "serif": the theme's frame,
# the serif font in the theme and in every text layer (at least nText of them)
expectBwSerif <- function(p, nText = 0) {
    border <- ggplot2::calc_element("panel.border", p$theme)
    expect_true(inherits(border, "element_rect"))
    expect_equal(border$colour, "#333333FF")
    expect_equal(ggplot2::calc_element("plot.title", p$theme)$family, "serif")
    expect_equal(ggplot2::calc_element("axis.text.x", p$theme)$family, "serif")
    layers <- textLayers(p)
    expect_gte(length(layers), nText)
    for (l in layers)
        expect_equal(l$aes_params$family, "serif")
}

# Checks the module's black frame of the factorial maps (default and jamovi themes)
expectBlackFrame <- function(p) {
    border <- ggplot2::calc_element("panel.border", p$theme)
    expect_equal(border$colour, "black")
    expect_equal(border$linewidth, 1)
}
