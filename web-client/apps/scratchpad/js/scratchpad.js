var env = {};

env.dirty = false;
env.history = [];
env.maxHistoryLength = 15;

env.editor = ace.edit("editor");
env.editor.setTheme("ace/theme/textmate");
env.editor.getSession().setMode("ace/mode/html");
env.editor.setHighlightActiveLine(false);
env.editor.setDisplayIndentGuides(false);
env.editor.renderer.setShowPrintMargin(false);
env.editor.session.setFoldStyle("manual");
env.editor.renderer.setVScrollBarAlwaysVisible(true);


//

function clearContent() {
    var presentation = document.getElementById("presentation");
    presentation.src = "about:blank";
}

function runProgram() {
    var presentation = document.getElementById("presentation").contentWindow;
    if (presentation.Pengine) presentation.Pengine.destroy_all(true);
    presentation.document.open();
    presentation.document.write(getProgram());
    presentation.document.close();
}


// Getting and setting program

function getProgram() {
    return env.editor.getValue()
}

function setProgram(src) {
	env.editor.setValue(src, -1);
	env.dirty = false;
	// $('#run-btn').prop('disabled', false);
}

// Printing

function print_editor_content() {
	var iframe = document.createElement("iframe");
	iframe.style.display = "none"
	document.body.appendChild(iframe)
	var windw = iframe.contentWindow;
	windw.document.open();
    windw.document.write('</head><body><pre>');
    windw.document.write(escapeHtml(getProgram()));
    windw.document.write('</pre></body></html>');
    windw.print();
    windw.document.close();
    document.body.removeChild(iframe);
}

var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
  };

function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
      return entityMap[s];
    });
  }


// Handling programs

function maybeLoadSrc() {
    var file = window.location.hash.slice(1);
    if (file) {
        loadSrc("/storage/"+ encodeURIComponent(file));
    }
}

function loadSrc(url) {
    $.get(url)
    .done(function(program) {
		setProgram(program);
	})
	.fail(function() {
		alert('Error: ' + url + ' does not exist.');
	})
}

function saveProgram() {
    var program = encodeURIComponent(getProgram());
    if (program) {
        $.post("/storage/store", "type=html&program=" + program, function(response) {
            var url = response.url;
            var file = response.file;
            window.location.hash = file;
            $("#url").val(url + "/apps/scratchpad/index.html#" + file);
            // $('#run-btn').prop('disabled', false);
            env.dirty = false;
        });
    }
}

function updateProgram() {
	var file = window.location.hash.slice(1);
    var program = encodeURIComponent(getProgram());
    if (program) {
         $.post("/storage/update", "file=" + file + "&program=" + program, function() {
            // $('#run-btn').prop('disabled', false);
            env.dirty = false;
        });
    }
}


// Event handlers: Editor

env.editor.getSession().on('change', function() {
	if (!env.dirty) {
	    env.dirty = true;
		// $('#run-btn').prop('disabled', true);
	}
});


// Event handlers: Menus

$("#file-menu").on("click", "a#new", function(evt) {
	evt.preventDefault();
	window.location.hash = "";
	setProgram("<html>\n"+
		   "  <head>\n"+
		   "    <script src=\"/vendor/jquery/jquery-2.0.3.min.js\"></script>\n"+
		   "    <script src=\"/pengine/pengines.js\"></script>\n"+
		   "    <script type=\"text/x-prolog\">\n\n"+
		   "      % Your Prolog code goes here\n\n"+
		   "	</script>\n"+
		   "    <script>\n"+
		   "      var pengine = new Pengine({\n"+
		   "        ask: \"member(X,[pippi])\",         // Your goal goes here\n"+
		   "        template: 'X',                    // The variables you are interested in\n"+
		   "        onsuccess: handleSuccess,\n"+
		   "      });\n"+
		   "	  function handleSuccess() {\n"+
		   "	    alert(JSON.stringify(this.data)); // handle results\n"+
		   "	  }\n"+
		   "    </script>\n"+
		   "  </head>\n"+
		   "  <body>\n"+
		   "  </body>\n"+
		   "</html>\n");
});

$("#file-menu").on("click", "a#save", function(evt) {
	evt.preventDefault();
	if (window.location.hash == "") {
	    saveProgram();
	} else {
	    updateProgram();
	}
});

$("#file-menu").on("click", "a#share", function(evt) {
	evt.preventDefault();
	if (window.location.hash == "") {
	    saveProgram();
	} else {
	    updateProgram();
	}
    $('#share-dialog').modal();
});

$("#file-menu").on("click", "a#collaborate", function(evt) {
	evt.preventDefault();
	TogetherJS(this);
});

$("#file-menu").on("click", "a#prefs", function(evt) {
	evt.preventDefault();
	$("#preferences").modal({backdrop:false});
});

$("#file-menu").on("click", "a#print", function(evt) {
	evt.preventDefault();
	print_editor_content();
});

$("#edit-menu").on("click", "a#undo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.undo.exec(env.editor)
});

$("#edit-menu").on("click", "a#redo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.redo.exec(env.editor)
});

$("#edit-menu").on("click", "a#indent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.indent.exec(env.editor)
});

$("#edit-menu").on("click", "a#outdent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.outdent.exec(env.editor)
});

$("#edit-menu").on("click", "a#comment", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.toggleBlockComment.exec(env.editor)
});

$("#edit-menu").on("click", "a#find", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.replace.exec(env.editor, "left")
});

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
	clearContent();
	window.location.hash = "";
	loadSrc(evt.target.href);
});


// Non-menu controls

$("#run-btn").on("click", runProgram);


$("#slider").on("input", function() {
    var val = this.value;
    changeWindow(val);
});

function changeWindow(val) {
    $("#editor").css("width", val+"%");
    $("#console").css("width", (100-val)+"%");
    if (val > 69) {
        $("#console").css("display","none");
        $("#editor").css("width", "100%");
    } else {
        $("#console").css("display","block");
    }
    if (val < 31) {
        $("#editor").css("display","none");
        $("#console").css("width", "100%");
    } else {
        $("#editor").css("display","block");
    }
}


// GUI preferences

function setTheme(theme) {
	env.editor.setTheme("ace/theme/" + theme);
	$("#theme-menu option:selected").prop("selected", false);
	$("#theme-menu").find("option[value='" + theme +"']").prop("selected", true);
}

function setFontFamily(family) {
	$('#editor').css('fontFamily', family);
	$("#font-family-menu option:selected").prop("selected", false);
	$("#font-family-menu").find("option[value='" + family +"']").prop("selected", true);
}

function setFontSize(size) {
	$('#editor').css('fontSize', size + 'px');
	$("#font-size-menu option:selected").prop("selected", false);
	$("#font-size-menu").find("option[value=" + size +"]").prop("selected", true);
}

function setTabSize(n) {
	env.editor.getSession().setTabSize(n);
	$("#tab-size-menu option:selected").prop("selected", false);
	$("#tab-size-menu").find("option[value=" + n +"]").prop("selected", true);
}

function setUseSoftTabs(bool) {
	env.editor.getSession().setUseSoftTabs(bool);
	$("#tab-soft-checkbox").prop('checked', bool);
}

function setLineWrap(bool) {
	env.editor.getSession().setUseWrapMode(bool);
	$("#line-wrap-checkbox").prop('checked', bool);
}

function setLineHighlight(bool) {
	env.editor.setHighlightActiveLine(bool);
	$("#line-highlight-checkbox").prop('checked', bool);
}

function setShowGutter(bool) {
	env.editor.renderer.setShowGutter(bool);
	$("#line-numbering-checkbox").prop('checked', bool);
}


// Event handlers: Preferences

$("#theme-menu").on("change", function() {
	var value = $("#theme-menu option:selected").val();
	setTheme(value);
	if (localStorage) {
		localStorage['scratchpad-theme'] = value;
	}
});

$("#font-family-menu").on("change", function() {
	var value = $("#font-family-menu option:selected").val();
	setFontFamily(value);
	if (localStorage) {
		localStorage['scratchpad-font-family'] = value;
	}
});

$("#font-size-menu").on("change", function() {
	var value = $("#font-size-menu option:selected").val();
	setFontSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['scratchpad-font-size'] = value;
	}
});

$("#tab-size-menu").on("change", function() {
	var value = $("#tab-size-menu option:selected").val();
	setTabSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['scratchpad-tab-size'] = value;
	}
});

$("#tab-soft-checkbox").on("change", function() {
	var value = $("#tab-soft-checkbox").prop('checked');
	setUseSoftTabs(value);
	if (localStorage) {
		localStorage['scratchpad-tab-soft'] = value;
	}
});

$("#line-wrap-checkbox").on("change", function() {
	var value = $("#line-wrap-checkbox").prop('checked');
	setLineWrap(value);
	if (localStorage) {
		localStorage['scratchpad-line-wrap'] = value;
	}
});

$("#line-highlight-checkbox").on("change", function() {
	var value = $("#line-highlight-checkbox").prop('checked');
	setLineHighlight(value);
	if (localStorage) {
		localStorage['scratchpad-line-highlight'] = value;
	}
});

$("#line-numbering-checkbox").on("change", function() {
	var value = $("#line-numbering-checkbox").prop('checked');
	setShowGutter(value);
	if (localStorage) {
		localStorage['scratchpad-line-numbering'] = value;
	}
});



function parseBoolean(value) {
	return value == "true" ? true : false;
}

$(document).ready(function() {
	if (localStorage && localStorage.length > 0) {
		setTheme(localStorage['scratchpad-theme']);
	    setFontFamily(localStorage['scratchpad-font-family']);
	    setFontSize(localStorage['scratchpad-font-size']);
	    setTabSize(parseInt(localStorage['scratchpad-tab-size'], 10));
	    setLineWrap(parseBoolean(localStorage['scratchpad-line-wrap']));
	    setLineHighlight(parseBoolean(localStorage['scratchpad-line-highlight']));
	    setShowGutter(parseBoolean(localStorage['scratchpad-line-numbering']));
	    setUseSoftTabs(parseBoolean(localStorage['scratchpad-tab-soft']));
	}
    maybeLoadSrc();
});


