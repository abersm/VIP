# VIP
Vaccine Integrity Project


![logo](https://github.com/user-attachments/assets/fe0c57af-d075-409f-8977-80619a3bc280)
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->

<svg
   width="18.892548mm"
   height="23.016367mm"
   viewBox="0 0 18.892548 23.016367"
   version="1.1"
   id="svg1"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:svg="http://www.w3.org/2000/svg">
  <defs
     id="defs1" />
  <g
     id="layer1"
     transform="translate(-64.734094,-112.5866)">
    <g
       id="g4"
       transform="matrix(0.02833503,0,0,0.02833503,17.382863,94.432849)">
      <g
         id="g1"
         transform="matrix(0.62975179,0,0,0.62975179,618.72918,267.19722)">
        <path
           d="m 2200.5,1860.5 -508,-254 v -737 l 508,-254 508,254 v 737 z"
           fill="#d0e4ec"
           fill-rule="evenodd"
           fill-opacity="1"
           id="path1" />
        <path
           d="m 2200.5,1860.5 -508,-254 v -737 l 508,-254 508,254 v 737 z m 508,-254 v -737 l -508,-254 -508,254 v 737 l 508,254 z"
           fill="#287697"
           fill-rule="nonzero"
           fill-opacity="1"
           id="path2" />
        <path
           d="M 2200.5,1844.5 1705.5,1597 V 879 l 495,-247.5 495,247.5 v 718 z"
           fill="#d0e4ec"
           fill-rule="evenodd"
           fill-opacity="1"
           id="path3" />
        <path
           d="M 2200.5,1882.93 1671.12,1618.24 V 857.755 l 529.38,-264.688 529.38,264.688 v 760.485 z m 506.46,-278.85 V 871.918 L 2200.5,618.689 1694.04,871.918 v 732.162 l 506.46,253.23 z M 2200.5,1831.69 1716.96,1589.92 V 886.082 l 483.54,-241.771 483.54,241.771 v 703.838 z m 460.62,-255.94 V 900.245 L 2200.5,669.932 1739.88,900.245 v 675.505 l 460.62,230.32 z"
           fill="#287697"
           fill-rule="nonzero"
           fill-opacity="1"
           id="path4" />
        <text
           fill="#a63b86"
           fill-opacity="1"
           font-family="Phosphate, Phosphate_MSFontService, sans-serif"
           font-style="normal"
           font-variant="normal"
           font-weight="400"
           font-stretch="normal"
           font-size="550px"
           text-anchor="start"
           direction="ltr"
           writing-mode="lr-tb"
           unicode-bidi="normal"
           text-decoration="none"
           transform="translate(1782.63,1424)"
           id="text4"
           style="text-decoration:none;text-decoration-line:none">VIP</text>
      </g>
    </g>
  </g>
</svg>

``` r
# Make sure devtools is installed (skip next line if devtools already installed)
install.packages("devtools")

# Install VIP package from github repository
devtools::install_github("abersm/VIP")
```

## Running shiny app
After installing package (see above), initialize the application by running the following: 

``` r
# Load VIP package
library(VIP)

# Run shiny app
vip_shiny()
```
