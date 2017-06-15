var fs = require('fs');
var orig = JSON.parse(fs.readFileSync(process.argv[2], 'utf-8'));
var patch = JSON.parse(fs.readFileSync(process.argv[3], 'utf-8'));
for(var k in patch.cmap) {
	if(!orig.cmap[k]) { orig.cmap[k] = patch.cmap[k] }
}
for(var k in patch.glyf) {
	if(!orig.glyf[k]) { orig.glyf[k] = patch.glyf[k] }
}
for(var j = 0; j < orig.name.length; j++){
	orig.name[j].nameString = orig.name[j].nameString.replace(/Iosevka/g, 'Imposevka');
}
fs.writeFileSync(process.argv[4], JSON.stringify(orig))