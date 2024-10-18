#### See: http://research.stowers.org/mcm/efg/R/Color/Chart/
### The function col2rgb can be used to extract the RGB (red-green-blue) components of a color, e.g.,
col2rgb("yellow")


## Each of the three RGB color components ranges from 0 to 255, which is interpreted to be 0.0 to 1.0 in RGB colorspace.  With each of the RGB components having 256 possible discrete values, this results in 256*256*256 possible colors, or 16,777,216 colors.

##While the RGB component values range from 0 to 255 in decimal, they range from hex 00 to hex FF.  Black, which is RGB = (0,0,0) can be represented in hex as #000000, and white, which is RGB = (255,255,255), can represented in hex as #FFFFFF.

##5.  R provides a way to define an RGB triple with each of the color components ranging from 0.0 to 1.0 using the rgb function.  For example, yellow can be defined:

rgb(1.0, 1.0, 0.0)
[1] "#FFFF00"
