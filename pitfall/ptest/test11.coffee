PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
  # Add some text with annotations            
  doc.fillColor("blue")
     .translate(50,50)
     .font('Helvetica', 30)
     .text('Here is a link!', 100, 100, { link: 'http://google.com/', underline: true, width: false})
                 
  doc.end()

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test11.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test11c.pdf'))
make doc