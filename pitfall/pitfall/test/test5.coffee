PDFDocument = require 'pdfkit'
tiger = require './assets/tiger'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument
doc.pipe fs.createWriteStream('test5.pdf')

# Set the font, draw some text, and embed an image
doc.font('Times-Italic')
   .fontSize(25)
   .text('Some text with an embedded font!', 100, 100, lineBreak: no)
   .image('assets/test.png', 100, 160, width: 412)
   .image('assets/test.jpeg', 190, 400, height: 300)
   
doc.end()