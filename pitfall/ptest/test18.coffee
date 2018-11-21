PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 

  # Register a font name for use later
  doc.registerFont('the-font', 'assets/fira.ttf')

  # Set the font, draw some text
  doc.font('the-font')
     .fontSize(25)
     .text('In Xanadu did Kubla Khan', 100, 100, {width: false})
     .text('A stately pleasure dome decree:', 100, 140, {width: false})
     .text('Where Alph, the sacred river, ran', 100, 180, {width: false})
     .text('Through caverns measureless to man', 100, 220, {width: false})
     .text('Down to a sunless sea.', 100, 260, {width: false})
                 
  doc.end()


doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('test18.pdf'))
make doc

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test18c.pdf'))
make doc
