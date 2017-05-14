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

doc.end()