PDFDocument = require 'pdfkit'
fs = require 'fs'

# embed otf

make = (doc) -> 

  doc.registerFont('sarabun', 'assets/sarabun.ttf')
  doc.registerFont('fira', 'assets/fira.otf')

  # Set the font, draw some text
  doc.font('sarabun')
     .fontSize(40)
     .text('hello สวัสดีชาวโลก world', 100, 150, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test23.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test23c.pdf'))
make doc

