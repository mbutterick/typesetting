zlib = require 'zlib'
PNG = require 'png-js'

class PNGImage
  constructor: (data, @label) ->
    #console.log("raw data")
    #console.log(data.slice(0, 20))
    @image = new PNG(data)
    @width = @image.width
    @height = @image.height
    @imgData = @image.imgData
    #console.log("result from png-js")
    #console.log(@imgData.slice(0, 20))
    #console.log(@image)
    @obj = null

  embed: (@document) ->
    return if @obj

    @obj = @document.ref
      Type: 'XObject'
      Subtype: 'Image'
      BitsPerComponent: @image.bits
      Width: @width
      Height: @height
      Filter: 'FlateDecode'

    unless @image.hasAlphaChannel
      params = @document.ref
        Predictor: 15
        Colors: @image.colors
        BitsPerComponent: @image.bits
        Columns: @width

      @obj.data['DecodeParms'] = params
      params.end()

    if @image.palette.length is 0
      @obj.data['ColorSpace'] = @image.colorSpace
    else
      # embed the color palette in the PDF as an object stream
      palette = @document.ref()
      palette.end new Buffer @image.palette

      # build the color space array for the image
      @obj.data['ColorSpace'] = ['Indexed', 'DeviceRGB', (@image.palette.length / 3) - 1, palette]

    # For PNG color types 0, 2 and 3, the transparency data is stored in
    # a dedicated PNG chunk.
    if @image.transparency.grayscale  
      console.log("transparency.grayscale")
      # Use Color Key Masking (spec section 4.8.5)
      # An array with N elements, where N is two times the number of color components.
      val = @image.transparency.greyscale
      @obj.data['Mask'] = [val, val]

    else if @image.transparency.rgb
      console.log("transparency.rgb")
      # Use Color Key Masking (spec section 4.8.5)
      # An array with N elements, where N is two times the number of color components.
      rgb = @image.transparency.rgb
      mask = []
      for x in rgb
        mask.push x, x

      @obj.data['Mask'] = mask

    else if @image.transparency.indexed
      console.log("transparency.indexed")
      # Create a transparency SMask for the image based on the data
      # in the PLTE and tRNS sections. See below for details on SMasks.
      @loadIndexedAlphaChannel()

    else if @image.hasAlphaChannel
      console.log("got alphachannel " + @image.colorType + " in png.coffee")
      # For PNG color types 4 and 6, the transparency data is stored as a alpha
      # channel mixed in with the main image data. Separate this data out into an
      # SMask object and store it separately in the PDF.
      @splitAlphaChannel()

    else
      @finalize()

  finalize: ->
    if @alphaChannel
      sMask = @document.ref
        Type: 'XObject'
        Subtype: 'Image'
        Height: @height
        Width: @width
        BitsPerComponent: 8
        Filter: 'FlateDecode'
        ColorSpace: 'DeviceGray'
        Decode: [0, 1]

      sMask.end @alphaChannel
      @obj.data['SMask'] = sMask

    # add the actual image data
    #console.log(@imgData)
    @obj.end @imgData

    # free memory
    @image = null
    @imgData = null

  splitAlphaChannel: ->
    console.log("start splitAlphaChannel in png.coffee")
    @image.decodePixels (pixels) =>
      console.log("decoded pixels length=" + pixels.length);
      console.log("decoded pixels=");
      console.log(pixels);

      colorByteSize = @image.colors * @image.bits / 8
      pixelCount = @width * @height
      imgData = new Buffer(pixelCount * colorByteSize)
      alphaChannel = new Buffer(pixelCount)

      i = p = a = 0
      len = pixels.length
      while i < len
        imgData[p++] = pixels[i++]
        imgData[p++] = pixels[i++]
        imgData[p++] = pixels[i++]
        alphaChannel[a++] = pixels[i++]

      console.log("uncompressed imgData length=" + imgData.length)
      console.log("uncompressed imgData=")
      console.log(imgData)

      console.log("uncompressed alphaChannel length=" + alphaChannel.length)
      console.log("uncompressed alphaChannel=")
      console.log(alphaChannel)

      done = 0
      zlib.deflate imgData, (err, @imgData) =>
        throw err if err
        console.log("compressed @imgData length=" + @imgData.length)
        console.log("compressed @imgData=")
        console.log(@imgData)

        @finalize() if ++done is 2

      zlib.deflate alphaChannel, (err, @alphaChannel) =>
        throw err if err
        console.log("compressed @alphaChannel length=" + @alphaChannel.length)
        console.log("compressed @alphaChannel=")
        console.log(@alphaChannel)
        @finalize() if ++done is 2

  loadIndexedAlphaChannel: (fn) ->
    transparency = @image.transparency.indexed
    @image.decodePixels (pixels) =>
      alphaChannel = new Buffer(@width * @height)

      i = 0
      for j in [0...pixels.length] by 1
        alphaChannel[i++] = transparency[pixels[j]]

      zlib.deflate alphaChannel, (err, @alphaChannel) =>
        throw err if err
        @finalize()

module.exports = PNGImage
