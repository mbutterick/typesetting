#lang brag

gif-file : header logical_screen u1*

header : magic version
magic : GIF-HEADER
version : u1 u1 u1

logical_screen : image_width image_height flags bg_color_index pixel_aspect_ratio
image_width : u2le
image_height :u2le
flags : u1
bg_color_index : u1
pixel_aspect_ratio : u1
u2le : BYTE BYTE
u1 : BYTE

color_table : color_table_entry*
color_table_entry : red green blue
red : u1
green : u1
blue : u1

#block : block_type body
#block_type : u1

