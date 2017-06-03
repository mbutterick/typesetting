#lang br


gif-file : header logical-screen-descriptor global-color-table middle trailer
header : signature version
signature : #"GIF"
version : u1{3} => bytes->string
logical-screen-descriptor : width height packed bgcolor-idx aspect
width : u2
height : u2
packed : color-table-flag color-res sort-flag color-table-size
bgcolor-idx : u1
aspect : u1

color-table-flag : b{1} => ->integer
color-res : b{3}
sort-flag : b{1}
color-table-size : b{3}

color-table-colors (size->colors color-table-size)

global-color-table : color-rec{color-table-flag * color-table-colors}
color-rec : red green blue
red : u1
blue : u1
green : u1

middle: gfx-control-ext img-descriptor lct? image-data

gfx-control-ext : u1{8}
img-descriptor : img-separator left top width height img-packed

img-separator : #"2C"
left : u2
top : u2
width : u2
height : u2
img-packed : lct-flag interlace sort res lct-size
lct-flag : b
interlace : b
sort : b
res : b{2}
lct-size : b{3}

lct: color-rec{lct-flag * (size->colors lct-size)}

image-data : lzw-size data-subblock