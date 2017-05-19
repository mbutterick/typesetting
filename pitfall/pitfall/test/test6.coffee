PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
    doc.text('Page 1')
   .addPage()
   .text('Page 2')
   .addPage()
   .text('Page 3')
   .addPage()
   .text('Page 4')
   .addPage()
   .text('Page 5')
   .addPage()
   .text('Page 6')

   doc.end()

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test6.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test6c.pdf'))
make doc

