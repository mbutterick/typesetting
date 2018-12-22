#lang br

#|
PDFDocument = require 'pdfkit'
tiger = require './tiger'
fs = require 'fs'

# Create a new PDFDocument
doc = new PDFDocument
doc.pipe fs.createWriteStream('out.pdf')

# Set some meta data
doc.info['Title'] = 'Test Document'
doc.info['Author'] = 'Devon Govett'

# Register a font name for use later
doc.registerFont('Charter', 'charter.ttf')

# Set the font, draw some text, and embed an image
doc.font('Charter')
   .fontSize(25)
   .text('Some text with an embedded font!', 100, 100)
   .fontSize(18)
   .text('PNG and JPEG images:')
   .image('test.png', 100, 160, width: 412)
   .image('test.jpeg', 190, 400, height: 300)

# Add another page
doc.add-page()
   .fontSize(25)
   .text 'Here is some vector graphics...', 100, 100

# Draw a triangle and a circle
doc.save()
   .move-to(100, 150)
   .line-to(100, 250)
   .line-to(200, 250)
   .fill("#FF3300")
   
doc.circle(280, 200, 50)
   .fill("#6600FF")

doc.scale(0.6)
   .translate(470, -380)
   .path('M 250,75 L 323,301 131,161 369,161 177,301 z') # render an SVG path
   .fill('red', 'even-odd') # fill using the even-odd winding rule
   .restore()
   
loremIpsum = '''
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam in suscipit purus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Vivamus nec hendrerit felis. Morbi aliquam facilisis risus eu lacinia. Sed eu leo in turpis fringilla hendrerit. Ut nec accumsan nisl. Suspendisse rhoncus nisl posuere tortor tempus et dapibus elit porta. Cras leo neque, elementum a rhoncus ut, vestibulum non nibh. Phasellus pretium justo turpis. Etiam vulputate, odio vitae tincidunt ultricies, eros odio dapibus nisi, ut tincidunt lacus arcu eu elit. Aenean velit erat, vehicula eget lacinia ut, dignissim non tellus. Aliquam nec lacus mi, sed vestibulum nunc. Suspendisse potenti. Curabitur vitae sem turpis. Vestibulum sed neque eget dolor dapibus porttitor at sit amet sem. Fusce a turpis lorem. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae;
Mauris at ante tellus. Vestibulum a metus lectus. Praesent tempor purus a lacus blandit eget gravida ante hendrerit. Cras et eros metus. Sed commodo malesuada eros, vitae interdum augue semper quis. Fusce id magna nunc. Curabitur sollicitudin placerat semper. Cras et mi neque, a dignissim risus. Nulla venenatis porta lacus, vel rhoncus lectus tempor vitae. Duis sagittis venenatis rutrum. Curabitur tempor massa tortor.
'''

# Draw some text wrapped to 412 points wide
doc.text('And here is some wrapped text...', 100, 300)
   .font('Charter', 13)
   .moveDown() # move down 1 line
   .text(loremIpsum, width: 412, align: 'justify', indent: 30, paragraphGap: 5)

# Add another page, and set the font back   
doc.add-page()
   .font('Charter', 25)
   .text('Rendering some SVG paths...', 100, 100)
   .translate(220, 300)

# Render each path that makes up the tiger image
for part in tiger
    doc.save()
    doc.path(part.path) # render an SVG path
    
    if part['stroke-width']
        doc.line-width part['stroke-width']
    
    if part.fill isnt 'none' and part.stroke isnt 'none'
        doc.fill-and-stroke(part.fill, part.stroke)
    else
        unless part.fill is 'none'
            doc.fill(part.fill)
            
        unless part.stroke is 'none'
            doc.stroke(part.stroke)
            
    doc.restore()

# Add some text with annotations            
doc.add-page()
   .fill-color("blue")
   .text('Here is a link!', 100, 100, { link: 'http://google.com/', underline: true })
doc.end()
|#