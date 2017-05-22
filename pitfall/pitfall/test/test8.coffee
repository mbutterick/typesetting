PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
    # Set the font, draw some text, and embed an image
    doc.font('Helvetica-Bold')
       .fontSize(25)
       .text('Another fantastic pic', 100, 100, lineBreak: no)
       .image('assets/test.png', 100, 160, width: 412)

    doc.end()

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test8.pdf'))
make doc

#doc = new PDFDocument({compress: yes})
#doc.pipe(fs.createWriteStream('test8c.pdf'))
#make doc

