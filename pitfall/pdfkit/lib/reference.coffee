###
PDFReference - represents a reference to another object in the PDF object heirarchy
By Devon Govett
###

zlib = require 'zlib'
stream = require 'stream'

class PDFReference extends stream.Writable
  constructor: (@document, @id, @data = {}) ->
    super decodeStrings: no
    @gen = 0
    @deflate = null
    @compress = no
    @uncompressedLength = 0
    @chunks = []
    
  initDeflate: ->
    @data.Filter = 'FlateDecode'
    
    @deflate = zlib.createDeflate()

    @deflate.on 'data', (chunk) =>
      console.log("got data event for ref " + @id + " from " + this.toString())
      @chunks.push chunk
      @data.Length += chunk.length
      
    @deflate.on 'end', () => 
      console.log("got end event for ref " + @id + " from " + this.toString())
      @finalize()
    
  _write: (chunk, encoding, callback) ->
    unless Buffer.isBuffer(chunk)
      chunk = new Buffer(chunk + '\n', 'binary')
      
    @uncompressedLength += chunk.length
    @data.Length ?= 0
    
    if @compress
      @initDeflate() if not @deflate
      console.log("chunk = " + chunk)
      @deflate.write chunk
      console.log("wrote chunk")
    else
      @chunks.push chunk
      @data.Length += chunk.length
      
    callback()
    
  end: (chunk) ->
    super
    
    if @deflate
      @deflate.end()
    else
      @finalize()
    
  finalize: =>
    @offset = @document._offset
    
    @document._write "#{@id} #{@gen} obj"
    @document._write PDFObject.convert(@data)
    
    if @chunks.length
      @document._write 'stream'
      for chunk in @chunks
        @document._write chunk
        
      @chunks.length = 0 # free up memory
      @document._write '\nendstream'
      
    @document._write 'endobj'
    @document._refEnd(this)
    
  toString: ->
    return "#{@id} #{@gen} R"
      
module.exports = PDFReference
PDFObject = require './object'
