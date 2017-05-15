PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test3.pdf'))

doc.text("Hello world")

doc.end()