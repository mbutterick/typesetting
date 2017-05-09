PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument
doc.pipe(fs.createWriteStream('out0.pdf'))

doc.end()