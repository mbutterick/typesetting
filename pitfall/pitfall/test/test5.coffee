PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
    # Set the font, draw some text, and embed an image
    doc.font('Times-Italic')
       .fontSize(25)
       .text('Some text with an embedded font!', 100, 100, lineBreak: no)
       .image('death.png', 100, 160, width: 412)

    doc.end()

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test5.pdf'))
make doc

#doc = new PDFDocument({compress: yes})
#doc.pipe(fs.createWriteStream('test5c.pdf'))
#make doc

