PDFDocument = require 'pdfkit'
fs = require 'fs'

doc = new PDFDocument({compress: no})
doc.pipe(fs.createWriteStream('minion-test.pdf'))
doc.registerFont('the-font', '/Library/Fonts/MinionPro-Regular.OTF')
doc.font('the-font')
 .fontSize(25)
 .text('Minion Pro Regular OTF', 50, 50, {width: false})
doc.end()