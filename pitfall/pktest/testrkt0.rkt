#lang br

(require pitfall/kit/document)

(define doc (new PDFDocument [options (hasheq 'out "testrkt0.pdf")]))
(send doc end)

#|
  var PDFDocument, doc, fs;

  PDFDocument = require('pdfkit');

  fs = require('fs');

  doc = new PDFDocument;

  doc.pipe(fs.createWriteStream('out.pdf'));

  doc.end();
|#