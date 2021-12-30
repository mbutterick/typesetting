PDFDocument = require 'pdfkit'
fs = require 'fs'

# embed otf

make = (doc) -> 

  # Register a font name for use later
  doc.registerFont('fira', 'assets/fira.otf')
  doc.registerFont('charter', 'assets/charter.otf')

  # Set the font, draw some text
  doc.font('fira')
     .fontSize(40)
     .text('Fira OTF rifle fire', 100, 100, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test20.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test20c.pdf'))
make doc

