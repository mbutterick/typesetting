PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 

  # Register a font name for use later
  doc.registerFont('Charter', 'assets/charter.ttf')

  # Set the font, draw some text
  doc.font('Charter')
     .fontSize(25)
     .text('Some text with an embedded font', 100, 100, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test12.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test12c.pdf'))
make doc
