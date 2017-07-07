fontkit = require '../pdfkit/node_modules/fontkit'

fira_path = "../pitfall/test/assets/fira.ttf"
f = fontkit.openSync(fira_path)
console.log "*************************** start decode"
thing = f.GSUB.lookupList.get(19)
console.log thing
