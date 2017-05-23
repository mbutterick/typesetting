PDFDocument = require 'pdfkit'
fs = require 'fs'

make = (doc) -> 
   doc.moveTo(0, 20)
      .lineTo(100, 160)
      .quadraticCurveTo(130, 200, 150, 120)
      .bezierCurveTo(190, -40, 200, 200, 300, 150) # draw a bezier curve
      .lineTo(400, 90)                             # draw another line
      .stroke()                                    # stroke the path

   doc.translate(0, 200)

   doc.path('M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90')
      .stroke()

   doc.end()

doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test09c.pdf'))
make doc