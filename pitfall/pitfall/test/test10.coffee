PDFDocument = require 'pdfkit'
tiger = require './assets/tiger'
fs = require 'fs'

make = (doc) -> 
  doc.translate(220, 300)
  # Render each path that makes up the tiger image
  for part in tiger
      doc.path(part.path) # render an SVG path
      
      if part['stroke-width']
          doc.lineWidth part['stroke-width']
      
      if part.fill isnt 'none' and part.stroke isnt 'none'
          doc.fillAndStroke(part.fill, part.stroke)
      else
          unless part.fill is 'none'
              doc.fill(part.fill)
              
          unless part.stroke is 'none'
              doc.stroke(part.stroke)
                 
  doc.end()

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test10c.pdf'))
make doc

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test10.pdf'))
make doc