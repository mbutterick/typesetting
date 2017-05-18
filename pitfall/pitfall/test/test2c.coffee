PDFDocument = require 'pdfkit'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument({compress: yes})
doc.pipe(fs.createWriteStream('test2c.pdf'))

doc.moveTo(0, 20)
   .lineTo(100, 160)
   .quadraticCurveTo(130, 200, 150, 120)
   .bezierCurveTo(190, -40, 200, 200, 300, 150) # draw a bezier curve
   .lineTo(400, 90)                             # draw another line
   .stroke()                                    # stroke the path

doc.translate(0, 200)

doc.polygon [100, 0], [50, 100], [150, 100]
doc.stroke()

doc.save()
doc.translate(200, 0)
doc.circle(100, 50, 50)
   .dash(5, space: 10)
   .stroke()
doc.restore()

doc.save()
doc.translate(400, 0)
doc.circle(100, 50, 50)
   .lineWidth(3)
   .fillOpacity(0.8)
   .fillAndStroke("red", "#900")
doc.restore()

doc.translate(0, 200)


# these examples are easier to see with a large line width
doc.lineWidth(25)
# line cap settings
doc.lineCap('butt')
   .moveTo(50, 20)
   .lineTo(100, 20)
   .stroke()
doc.lineCap('round')
   .moveTo(150, 20)
   .lineTo(200, 20)
   .stroke()
# square line cap shown with a circle instead of a line so you can see it
doc.lineCap('square')
   .moveTo(250, 20)
   .circle(275, 30, 15)
   .stroke()
# line join settings
doc.lineJoin('miter')
   .rect(50, 100, 50, 50)
   .stroke()
doc.lineJoin('round')
   .rect(150, 100, 50, 50)
   .stroke()
doc.lineJoin('bevel')
   .rect(250, 100, 50, 50)
   .stroke()


doc.end()