PDFDocument = require 'pdfkit'
fs = require 'fs'

# embed otf

make = (doc) -> 

  # Use a font with 2048 em (noto)
  doc.registerFont('fira', 'assets/fira.otf')
  doc.registerFont('noto', 'assets/noto-emoji/NotoEmoji-Regular.ttf')

  # Set the font, draw some text
  doc.font('noto')
     .fontSize(40)
     .text('😂😂😂', 100, 150, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test22.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test22c.pdf'))
make doc

