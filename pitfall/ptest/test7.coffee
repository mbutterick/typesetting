PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
    # Set the font, draw some text, and embed an image
    doc.font('Times-Italic')
       .fontSize(25)
       .text('Here comes a JPEG!', 100, 100, lineBreak: no)
       .image('assets/test.jpeg', 100, 160, width: 412)

    doc.end()

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test7.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test7c.pdf'))
make doc

