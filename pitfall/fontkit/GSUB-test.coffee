fontkit = require '../pdfkit/node_modules/fontkit'

fira_path = "../pitfall/test/assets/fira.ttf"
f = fontkit.openSync(fira_path)
console.log f.GSUB.lookupList.get(30).subTables
console.log f.GSUB.lookupList.get(30).subTables[0].ligatureSets.get(0)