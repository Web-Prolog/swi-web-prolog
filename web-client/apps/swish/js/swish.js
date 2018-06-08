var env = {};

env.dirty = false;
env.history = [];
env.maxHistoryLength = 15;

env.editor = ace.edit("editor");
env.editor.setTheme("ace/theme/brain");
env.editor.getSession().setMode("ace/mode/prolog");
env.editor.setHighlightActiveLine(false);
env.editor.setDisplayIndentGuides(false);
env.editor.renderer.setShowPrintMargin(false);
env.editor.session.setFoldStyle("manual");
env.editor.renderer.setVScrollBarAlwaysVisible(true);


// Getting and setting program and goal

function getProgram() {
    return env.editor.getValue()
}

function setProgram(src) {
	env.editor.setValue(src, -1);
}


function theme() {
    return env.editor.renderer.theme.cssClass;
}


// Printing

function print_editor_content() {
	var iframe = document.createElement("iframe");
	iframe.style.display = "none"
	document.body.appendChild(iframe)
	var windw = iframe.contentWindow;
	windw.document.open();
    windw.document.write('</head><body><pre>');
    windw.document.write(getProgram());
    windw.document.write('</pre></body></html>');
    windw.print();
    windw.document.close();
    document.body.removeChild(iframe);
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
		env.dirty = true;
	})
	.fail(function() {
		alert("Error loading program.")
	})
}

function saveProgram() {
    var program = encodeURIComponent(getProgram());
    if (program) {
        $.post("/storage/store", "program=" + program, function(response) {
            var url = response.url;
            var file = response.file;
            window.location.hash = file;
            $("#url").val(url + "/apps/swish/index.html#" + file);
            env.dirty = false;
        });
    }
}

function updateProgram() {
	var file = window.location.hash.slice(1);
    var program = encodeURIComponent(getProgram());
    if (program) {
         $.post("/storage/update", "file=" + file + "&program=" + program, function() {
            env.dirty = false;
        });
    }
}



function extractExamples() {
    var Search = ace.require("./search").Search;
    var search = new Search();
    search.setOptions({
        needle: /\/\*\*\s*Examples/,
        range: null,
        caseSensitive: false,
        regExp: true
    });
    var ranges = search.findAll(env.editor.session)
    var doc = env.editor.session.getDocument();
    var examples = []
    for (var i in ranges) {
        var examplegroup = [];
        var row = ranges[i].start.row;
        for (var j = row + 1; ; j++) {
            var ex = doc.getLine(j).trim();
            if (ex == "*/") {
                break;
            } else {
                if (ex != "") {
                    examplegroup.push(ex);
                }
            }
        }
        examples.push(examplegroup);
    }
    return examples;
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
  
function examplesToHTML(examples) {
    var html = [];
    for (var i in examples) {
        var examplegroup = examples[i];
        for (var j in examplegroup) {
            var ex = examplegroup[j];
            ex = escapeHtml(ex);
            ex = "<li><a href='#' onclick='paste(\"" + ex + "\")'>?- " + ex + "</a></li>";
            html.push(ex);
        }
        html.push("<li class='divider'></li>")
    }
    html.pop(); // get rid of the last divider
    return html.join("");
}


function populateExampleMenu() {
    var html = examplesToHTML(extractExamples());
    $("#examples").html(html);
}


function updateHistory(query) {
	var history = env.history;
	var index = history.indexOf(query);
	if (index != -1) history.splice(index, 1);
	if (history.length >= env.maxHistoryLength) history.shift();
	env.history.push(query);
}

function populateHistoryMenu() {
	var html = "";
	var history = env.history;
	for (var i in history) {
    	var hist = history[i];
        hist = escapeHtml(hist);
		html += "<li><a href='#' onclick='paste(\"" + hist + ".\")'>?- " + hist + ".</a></li>";
	}
    $("#history").html(html);}
    
    
function disableButtons(ask, next, stop, abort) {
    $("#ask-btn").prop("disabled", ask);
    $("#next-btn").prop("disabled", next);
    $("#stop-btn").prop("disabled", stop);
    $("#abort-btn").prop("disabled", abort);
}


// Event handlers: Editor

env.editor.getSession().on('change', function() {
	if (!env.dirty) {
	    env.dirty = true;
	}
});


// Event handlers: Menus

$("#file-menu").on("click", "a#new", function(evt) {
	evt.preventDefault();
	window.location.hash = "";
	setProgram("% Your program goes here\n\n\n\n/** Examples\n\n\n*/\n");
	env.dirty = false;
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
    if (evt.target.id == "tut") {
        $("#editor").css("display","none");
        $("#tutorial").css("display","block");
        $("#examples-btn").prop("disabled", true);
    } else {
        $("#editor").css("display","block");
        $("#tutorial").css("display","none");
        $("#examples-btn").prop("disabled", false);
        window.location.hash = "";
        loadSrc(evt.target.href);  
    }
});

function load_example(url) {
    $("#editor").css("display","block");
    $("#tutorial").css("display","none");
    $("#examples-btn").prop("disabled", false);
    window.location.hash = "";
	loadSrc(url);
	env.dirty = true;     
}


// Event handlers: Preferences

$("#theme-menu").on("change", function() {
	var value = $("#theme-menu option:selected").val();
	setTheme(value);
	if (localStorage) {
		localStorage['swish-theme'] = value;
	}
});

$("#font-family-menu").on("change", function() {
	var value = $("#font-family-menu option:selected").val();
	setFontFamily(value);
	if (localStorage) {
		localStorage['swish-font-family'] = value;
	}
});

$("#font-size-menu").on("change", function() {
	var value = $("#font-size-menu option:selected").val();
	setFontSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-font-size'] = value;
	}
});

$("#tab-size-menu").on("change", function() {
	var value = $("#tab-size-menu option:selected").val();
	setTabSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-tab-size'] = value;
	}
});

$("#tab-soft-checkbox").on("change", function() {
	var value = $("#tab-soft-checkbox").prop('checked');
	setUseSoftTabs(value);
	if (localStorage) {
		localStorage['swish-tab-soft'] = value;
	}
});

$("#line-wrap-checkbox").on("change", function() {
	var value = $("#line-wrap-checkbox").prop('checked');
	setLineWrap(value);
	if (localStorage) {
		localStorage['swish-line-wrap'] = value;
	}
});

$("#line-highlight-checkbox").on("change", function() {
	var value = $("#line-highlight-checkbox").prop('checked');
	setLineHighlight(value);
	if (localStorage) {
		localStorage['swish-line-highlight'] = value;
	}
});

$("#line-numbering-checkbox").on("change", function() {
	var value = $("#line-numbering-checkbox").prop('checked');
	setShowGutter(value);
	if (localStorage) {
		localStorage['swish-line-numbering'] = value;
	}
});

$("#slider").on("input", function() {
    var val = this.value;
    $("#editor").css("width", val+"%");
    $("#tutorial").css("width", val+"%");
    $("#shell").css("width", (100-val)+"%");
    $("#controls").css("width", (100-val)+"%");
});



// Event handlers: Console


$("#ask-btn").on("click", function() {
    gterm.exec(gterm.before_cursor().trim());
    gterm.set_command("");
    setTimeout(gterm.enable, 0);
});

$("#next-btn").on("click", function() {
    gterm.exec(";");
    setTimeout(gterm.enable, 0);
});

$("#stop-btn").on("click", function() {
    gterm.exec("");
    setTimeout(gterm.enable, 0);
});

$("#abort-btn").on("click", function(evt) {
    gterm.resume();
    gmysend({
	 command:"pengine_abort", 
	 pid:pid
    });
    setTimeout(gterm.enable, 0);
});

$("#examples-btn").on("click", function() {
	if (env.dirty) {
		populateExampleMenu();
	}
});

$("#history-btn").on("click", populateHistoryMenu);

$("#json-trace-checkbox").on('click', function(e) {
    e.stopImmediatePropagation();
    trace = checked = (e.currentTarget.checked) ? false : true;
    e.currentTarget.checked=(checked) ? false : checked.toString();
});

$("#clear-btn-query").on("click", function() {
	gterm.clear();
    setTimeout(gterm.enable, 0);
});

function presentation_mode(bool) {
    if (bool) {
        $("p:not(.ask-buttons)").css('display','none');
        $(".alert").css('display','none');
    } else {
        $("p:not(.ask-buttons)").css('display','inline');
        $(".alert").css('display','block');        
    }
}


function parseBoolean(value) {
	return value == "true" ? true : false;
}

// Initialisation

$(document).ready(function() {
    env.editor.setTheme("ace/theme/brain");
	if (localStorage && localStorage.length > 0) {
		//setTheme(localStorage['swish-theme']);
		setFontFamily(localStorage['swish-font-family']);
		setFontSize(localStorage['swish-font-size']);
		setTabSize(parseInt(localStorage['swish-tab-size'], 10));
		setLineWrap(parseBoolean(localStorage['swish-line-wrap']));
		setLineHighlight(parseBoolean(localStorage['swish-line-highlight']));
		setShowGutter(parseBoolean(localStorage['swish-line-numbering']) || true);
		setUseSoftTabs(parseBoolean(localStorage['swish-tab-soft']));
	}
    maybeLoadSrc();
});

