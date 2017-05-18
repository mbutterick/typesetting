PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
# uncompressed obj order: 5 4 3 6 2 1
# compressed obj order: 5 4 6 2 1 3
doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test0c.pdf'))

doc.end()