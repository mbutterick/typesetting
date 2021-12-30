PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test0.pdf'))
doc.end()

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test0c.pdf'))
doc.end()