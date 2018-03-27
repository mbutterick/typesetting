PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
    doc.text("Hello world")
    doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test3.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test3c.pdf'))
make doc

