#lang br

(require pitfall/kit/document)

(define doc (new PDFDocument))
(send doc pipe (open-output-file "testrkt0.pdf" #:exists 'replace))
(send doc end)

#|
  var PDFDocument, doc, fs;

  PDFDocument = require('pdfkit');

  fs = require('fs');

  doc = new PDFDocument;

  doc.pipe(fs.createWriteStream('out.pdf'));

  doc.end();
|#