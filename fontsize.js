var min=.5;
var max=2;
var elt = new Array();
var defsiz = new Array();
elt[0]='p';
defsiz[0]=100;
elt[1]='li';
defsiz[1]=100;
elt[2]='blockquote';
defsiz[2]=100;
elt[3]='h1';
defsiz[3]=130;
elt[4]='h2';
defsiz[4]=120;
elt[5]='h3';
defsiz[5]=110;
elt[6]='address';
defsiz[6]=100;
function increaseFontSize() {
    for(j=0;j<elt.length;j++) {
	var p = document.getElementsByTagName(elt[j]);
	for(i=0;i<p.length;i++) {
	    if(p[i].style.fontSize) {
		var s = parseInt(p[i].style.fontSize.replace("%",""));
	    } else {
		var s = defsiz[j];
	    }
	    if(s<=max*defsiz[j]) {
		s *= 1.1;
	    }
	    p[i].style.fontSize = s+"%"
		}
    }
}
function decreaseFontSize() {
    for(j=0;j<elt.length;j++) {
	var p = document.getElementsByTagName(elt[j]);
	for(i=0;i<p.length;i++) {
	    if(p[i].style.fontSize) {
		var s = parseInt(p[i].style.fontSize.replace("%",""));
	    } else {
		var s = defsiz[j];
	    }
	    if(s>=min*defsiz[j]) {
		s /= 1.1;
	    }
	    p[i].style.fontSize = s+"%"
		}
    }
}
function resetFontSize() {
    for(j=0;j<elt.length;j++) {
	var p = document.getElementsByTagName(elt[j]);
	for(i=0;i<p.length;i++) {
	    if(p[i].style.fontSize) {
		var s = parseInt(p[i].style.fontSize.replace("%",""));
	    } else {
		var s = defsiz[j];
	    }
	    s = defsiz[j];
	    p[i].style.fontSize = s+"%"
		}
    }
}
