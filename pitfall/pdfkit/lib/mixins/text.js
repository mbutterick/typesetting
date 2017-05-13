// Generated by CoffeeScript 1.12.5
(function() {
  var LineWrapper, number;

  LineWrapper = require('../line_wrapper');

  number = require('../object').number;

  module.exports = {
    initText: function() {
      this.x = 0;
      this.y = 0;
      return this._lineGap = 0;
    },
    lineGap: function(_lineGap) {
      this._lineGap = _lineGap;
      return this;
    },
    moveDown: function(lines) {
      if (lines == null) {
        lines = 1;
      }
      this.y += this.currentLineHeight(true) * lines + this._lineGap;
      return this;
    },
    moveUp: function(lines) {
      if (lines == null) {
        lines = 1;
      }
      this.y -= this.currentLineHeight(true) * lines + this._lineGap;
      return this;
    },
    _text: function(text, x, y, options, lineCallback) {
      var j, len, line, ref, wrapper;
      options = this._initOptions(x, y, options);
      text = '' + text;
      if (options.wordSpacing) {
        text = text.replace(/\s{2,}/g, ' ');
      }
      if (options.width) {
        wrapper = this._wrapper;
        if (!wrapper) {
          wrapper = new LineWrapper(this, options);
          wrapper.on('line', lineCallback);
        }
        this._wrapper = options.continued ? wrapper : null;
        this._textOptions = options.continued ? options : null;
        wrapper.wrap(text, options);
      } else {
        ref = text.split('\n');
        for (j = 0, len = ref.length; j < len; j++) {
          line = ref[j];
          lineCallback(line, options);
        }
      }
      return this;
    },
    text: function(text, x, y, options) {
      return this._text(text, x, y, options, this._line.bind(this));
    },
    widthOfString: function(string, options) {
      if (options == null) {
        options = {};
      }
      return this._font.widthOfString(string, this._fontSize, options.features) + (options.characterSpacing || 0) * (string.length - 1);
    },
    heightOfString: function(text, options) {
      var height, lineGap, ref, x, y;
      if (options == null) {
        options = {};
      }
      ref = this, x = ref.x, y = ref.y;
      options = this._initOptions(options);
      options.height = 2e308;
      lineGap = options.lineGap || this._lineGap || 0;
      this._text(text, this.x, this.y, options, (function(_this) {
        return function(line, options) {
          return _this.y += _this.currentLineHeight(true) + lineGap;
        };
      })(this));
      height = this.y - y;
      this.x = x;
      this.y = y;
      return height;
    },
    list: function(list, x, y, options, wrapper) {
      var flatten, i, indent, itemIndent, items, level, levels, midLine, r;
      options = this._initOptions(x, y, options);
      midLine = Math.round((this._font.ascender / 1000 * this._fontSize) / 2);
      r = options.bulletRadius || Math.round((this._font.ascender / 1000 * this._fontSize) / 3);
      indent = options.textIndent || r * 5;
      itemIndent = options.bulletIndent || r * 8;
      level = 1;
      items = [];
      levels = [];
      flatten = function(list) {
        var i, item, j, len, results;
        results = [];
        for (i = j = 0, len = list.length; j < len; i = ++j) {
          item = list[i];
          if (Array.isArray(item)) {
            level++;
            flatten(item);
            results.push(level--);
          } else {
            items.push(item);
            results.push(levels.push(level));
          }
        }
        return results;
      };
      flatten(list);
      wrapper = new LineWrapper(this, options);
      wrapper.on('line', this._line.bind(this));
      level = 1;
      i = 0;
      wrapper.on('firstLine', (function(_this) {
        return function() {
          var diff, l;
          if ((l = levels[i++]) !== level) {
            diff = itemIndent * (l - level);
            _this.x += diff;
            wrapper.lineWidth -= diff;
            level = l;
          }
          _this.circle(_this.x - indent + r, _this.y + midLine, r);
          return _this.fill();
        };
      })(this));
      wrapper.on('sectionStart', (function(_this) {
        return function() {
          var pos;
          pos = indent + itemIndent * (level - 1);
          _this.x += pos;
          return wrapper.lineWidth -= pos;
        };
      })(this));
      wrapper.on('sectionEnd', (function(_this) {
        return function() {
          var pos;
          pos = indent + itemIndent * (level - 1);
          _this.x -= pos;
          return wrapper.lineWidth += pos;
        };
      })(this));
      wrapper.wrap(items.join('\n'), options);
      return this;
    },
    _initOptions: function(x, y, options) {
      var key, margins, ref, val;
      if (x == null) {
        x = {};
      }
      if (options == null) {
        options = {};
      }
      if (typeof x === 'object') {
        options = x;
        x = null;
      }
      options = (function() {
        var k, opts, v;
        opts = {};
        for (k in options) {
          v = options[k];
          opts[k] = v;
        }
        return opts;
      })();
      if (this._textOptions) {
        ref = this._textOptions;
        for (key in ref) {
          val = ref[key];
          if (key !== 'continued') {
            if (options[key] == null) {
              options[key] = val;
            }
          }
        }
      }
      if (x != null) {
        this.x = x;
      }
      if (y != null) {
        this.y = y;
      }
      if (options.lineBreak !== false) {
        margins = this.page.margins;
        if (options.width == null) {
          options.width = this.page.width - this.x - margins.right;
        }
      }
      options.columns || (options.columns = 0);
      if (options.columnGap == null) {
        options.columnGap = 18;
      }
      return options;
    },
    _line: function(text, options, wrapper) {
      var lineGap;
      if (options == null) {
        options = {};
      }
      this._fragment(text, this.x, this.y, options);
      lineGap = options.lineGap || this._lineGap || 0;
      if (!wrapper) {
        return this.x += this.widthOfString(text);
      } else {
        return this.y += this.currentLineHeight(true) + lineGap;
      }
    },
    _fragment: function(text, x, y, options) {
      var addSegment, align, base, characterSpacing, commands, d, encoded, encodedWord, flush, hadOffset, i, j, last, len, len1, lineWidth, lineY, m, mode, name, pos, positions, positionsWord, ref, ref1, renderedWidth, scale, spaceWidth, textWidth, word, wordSpacing, words;
      text = ('' + text).replace(/\n/g, '');
      if (text.length === 0) {
        return;
      }
      align = options.align || 'left';
      wordSpacing = options.wordSpacing || 0;
      characterSpacing = options.characterSpacing || 0;
      if (options.width) {
        switch (align) {
          case 'right':
            textWidth = this.widthOfString(text.replace(/\s+$/, ''), options);
            x += options.lineWidth - textWidth;
            break;
          case 'center':
            x += options.lineWidth / 2 - options.textWidth / 2;
            break;
          case 'justify':
            words = text.trim().split(/\s+/);
            textWidth = this.widthOfString(text.replace(/\s+/g, ''), options);
            spaceWidth = this.widthOfString(' ') + characterSpacing;
            wordSpacing = Math.max(0, (options.lineWidth - textWidth) / Math.max(1, words.length - 1) - spaceWidth);
        }
      }
      renderedWidth = options.textWidth + (wordSpacing * (options.wordCount - 1)) + (characterSpacing * (text.length - 1));
      if (options.link) {
        this.link(x, y, renderedWidth, this.currentLineHeight(), options.link);
      }
      if (options.underline || options.strike) {
        this.save();
        if (!options.stroke) {
          this.strokeColor.apply(this, this._fillColor);
        }
        lineWidth = this._fontSize < 10 ? 0.5 : Math.floor(this._fontSize / 10);
        this.lineWidth(lineWidth);
        d = options.underline ? 1 : 2;
        lineY = y + this.currentLineHeight() / d;
        if (options.underline) {
          lineY -= lineWidth;
        }
        this.moveTo(x, lineY);
        this.lineTo(x + renderedWidth, lineY);
        this.stroke();
        this.restore();
      }
      this.save();
      this.transform(1, 0, 0, -1, 0, this.page.height);
      y = this.page.height - y - (this._font.ascender / 1000 * this._fontSize);
      if ((base = this.page.fonts)[name = this._font.id] == null) {
        base[name] = this._font.ref();
      }
      this.addContent("BT");
      this.addContent("1 0 0 1 " + (number(x)) + " " + (number(y)) + " Tm");
      this.addContent("/" + this._font.id + " " + (number(this._fontSize)) + " Tf");
      mode = options.fill && options.stroke ? 2 : options.stroke ? 1 : 0;
      if (mode) {
        this.addContent(mode + " Tr");
      }
      if (characterSpacing) {
        this.addContent((number(characterSpacing)) + " Tc");
      }
      if (wordSpacing) {
        words = text.trim().split(/\s+/);
        wordSpacing += this.widthOfString(' ') + characterSpacing;
        wordSpacing *= 1000 / this._fontSize;
        encoded = [];
        positions = [];
        for (j = 0, len = words.length; j < len; j++) {
          word = words[j];
          ref = this._font.encode(word, options.features), encodedWord = ref[0], positionsWord = ref[1];
          encoded.push.apply(encoded, encodedWord);
          positions.push.apply(positions, positionsWord);
          positions[positions.length - 1].xAdvance += wordSpacing;
        }
      } else {
        ref1 = this._font.encode(text, options.features), encoded = ref1[0], positions = ref1[1];
      }
      scale = this._fontSize / 1000;
      commands = [];
      last = 0;
      hadOffset = false;
      addSegment = (function(_this) {
        return function(cur) {
          var advance, hex;
          if (last < cur) {
            hex = encoded.slice(last, cur).join('');
            advance = positions[cur - 1].xAdvance - positions[cur - 1].advanceWidth;
            commands.push("<" + hex + "> " + (number(-advance)));
          }
          return last = cur;
        };
      })(this);
      flush = (function(_this) {
        return function(i) {
          addSegment(i);
          if (commands.length > 0) {
            _this.addContent("[" + (commands.join(' ')) + "] TJ");
            return commands.length = 0;
          }
        };
      })(this);
      for (i = m = 0, len1 = positions.length; m < len1; i = ++m) {
        pos = positions[i];
        if (pos.xOffset || pos.yOffset) {
          flush(i);
          this.addContent("1 0 0 1 " + (number(x + pos.xOffset * scale)) + " " + (number(y + pos.yOffset * scale)) + " Tm");
          flush(i + 1);
          hadOffset = true;
        } else {
          if (hadOffset) {
            this.addContent("1 0 0 1 " + (number(x)) + " " + (number(y)) + " Tm");
            hadOffset = false;
          }
          if (pos.xAdvance - pos.advanceWidth !== 0) {
            addSegment(i + 1);
          }
        }
        x += pos.xAdvance * scale;
      }
      flush(i);
      this.addContent("ET");
      return this.restore();
    }
  };

}).call(this);
