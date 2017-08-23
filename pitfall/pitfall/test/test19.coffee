PDFDocument = require 'pdfkit'
fs = require 'fs'

# test ss03 (alternate ampersand form)

make = (doc) -> 

  # Register a font name for use later
  doc.registerFont('the-font', 'assets/fira.ttf')

  # Set the font, draw some text
  doc.font('the-font')
     .fontSize(100)
     .text('A&B', 100, 100, {width: false})
     .text('X&Y', 100, 200, {width: false, features: ["ss03"]})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test19.pdf'))
make doc

