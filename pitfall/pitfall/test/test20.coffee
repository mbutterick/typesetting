PDFDocument = require 'pdfkit'
fs = require 'fs'

# embed otf

make = (doc) -> 

  # Register a font name for use later
  doc.registerFont('the-font', 'assets/fira.otf')

  # Set the font, draw some text
  doc.font('the-font')
     .fontSize(40)
     .text('Embedded OTF', 100, 100, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test20.pdf'))
make doc

