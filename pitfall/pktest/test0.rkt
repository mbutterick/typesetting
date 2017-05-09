#lang br

(require pitfall/kit/document)

(define doc (make-object PDFDocument "outrkt0.pdf"))
(send doc end)

#|
  var PDFDocument, doc, fs;

  PDFDocument = require('pdfkit');

  fs = require('fs');

  doc = new PDFDocument;

  doc.pipe(fs.createWriteStream('out.pdf'));

  doc.end();
|#