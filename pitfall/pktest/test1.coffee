PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test1.pdf'))

# Draw a triangle and a circle
doc.save()
   .moveTo(100, 150)
   .lineTo(100, 250)
   .lineTo(200, 250)
   .fill("#FF3300")
   
doc.circle(280, 200, 50)
   .fill("#6600FF")

doc.scale(0.6)
   .translate(470, -380)
   .path('M 250,75 L 323,301 131,161 369,161 177,301 z') # render an SVG path
   .fill('red', 'even-odd') # fill using the even-odd winding rule
   .restore()

doc.end()