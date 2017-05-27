PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
  # Add some text with annotations            
  doc.fillColor("blue")
     .font('Helvetica', 30)
     .text('Here is a link!', 100, 100, { link: 'http://google.com/', underline: true })
                 
  doc.end()

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test11c.pdf'))
make doc