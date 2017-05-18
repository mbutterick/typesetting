PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test3c.pdf'))

doc.text("Hello world")

doc.end()