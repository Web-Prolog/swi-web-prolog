            function settings_to_table(arr) {
                var html = '<table class="table table-bordered table-condensed"><tbody>';
                for (var i in arr) {
                    var module = arr[i].module;
                    var settings = arr[i].settings;
                    html += '<tr><th colspan="4">'+module+'</th></tr>';
                    for (var i in settings) {
                        var comment = settings[i].comment;
                        var name = settings[i].name;
                        var type = settings[i].type;
                        var value = settings[i].value;
                        var defaul = settings[i]['default'];
                        var button = name + "-btn";
                        var disabled = (value===defaul) ? "disabled" : "";
                        html += '<tr id="'+name+'-row"><td>'+comment+'</td><td class="widget-cell">'+widget(name, type, value)+'</td><td class="btn-cell"><button class="btn btn-xs btn-primary" id="'+button+'" disabled onclick="set_setting(\''+name+'\')">Apply</button></td><td class="btn-cell"><button id="'+name+'-def" class="btn btn-xs btn-default" onclick="set_default(\''+name+'\',\''+defaul+'\')" '+disabled+'>Default</button></td></tr>';
                    }
                }
                html += '</tbody></table'
                return html;
            }

            function widget(name, type, value) {
                if (type === 'atom' || type === 'number' || type === 'integer' || type === 'nonneg') {
                    return "<input id='" + name + "' style='width:100px' type='text' value='" + value + "'>"
                } else if (type === 'boolean') {
                    return "<input id='" + name + "' style='width:100px' type='checkbox' value='" + value + "'>"
                } else if (type.kind === 'oneof') {
                    var options = type.options;
                    var html = ""
                    for (var i in options) {
                        if (options[i] === value) {
                            html += "<option selected value='"+options[i]+"'>"+options[i]+"</option>";
                        } else {
                            html += "<option value='"+options[i]+"'>"+options[i]+"</option>";
                        }
                    }
                    return "<select id='" + name + "' style='width:100px' >" + html + "</select>"
                } else if (type.kind === 'atomlist') {
                    return "<input id='" + name + "' style='width:100px' type='text' value='" + value + "'>"
                } else if (type.kind === 'compoundlist') {
                    return "<input id='" + name + "' style='width:100px' type='text' value='" + value + "'>"
                } else {
                    return "<input id='" + name + "' style='width:100px' type='text' value='" + value + "'>"
                }
            }

            function install_handlers(arr) {
                for (var i in arr) {
                    var settings = arr[i].settings;
                    for (var i in settings) {
                        var name = settings[i].name;
                        $("#"+name).keyup(function(e) {
                            $("#"+e.target.id+"-btn").prop('disabled', false);
                            $("#"+e.target.id+"-def").prop('disabled', false);
                       });
                        $("#"+name).change(function(e) {
                            $("#"+e.target.id+"-btn").prop('disabled', false);
                            $("#"+e.target.id+"-def").prop('disabled', false);
                        })
                    }
                }
            }

            function set_setting(name) {
                $("#alert").remove();
                var value = $("#"+name).val();
                value = value.trim();
                if (value) {
                    var mname = name.replace("-",":");
                    $.get('/admin/set_settings?name='+mname+'&value='+value, function(obj) {
                        var name = obj.name;
                        var value = obj.value;
                        if (!obj.error && value) {
                            $("#"+name+"-row").after('<tr id="alert"><td colspan="4"><div class="alert alert-success">Success! Setting <em>'+mname+ '</em> has been set to <code>' + value + '</code>.</div></td></tr>')
                            $("#"+name+"-btn").prop('disabled', true);
                            if (value.toString() === obj['default'].toString() || obj['default'] === '*') {// This is a mystery!!! Should really be "[*]". Bug in jQuery?
                                $("#"+name+"-def").prop('disabled', true)
                            } else {
                                $("#"+name+"-def").prop('disabled', false)
                            }
                            setTimeout(function() {$("#alert").remove()}, 3000)
                        } else {
                            $("#"+name+"-row").after('<tr id="alert"><td colspan="4"><div class="alert alert-danger">'+obj.msg+'.</div></td></tr>');
                        }
                    })
                } else {
                    $("#"+name+"-row").after('<tr id="alert"><td colspan="4"><div class="alert alert-danger">Error: The empty string is not a permissible value.</div></td></tr>')
                }
            }


            function set_default(name, defaul) {
                if (defaul == "*") { // This is a mystery!!! Should really be "[*]". Bug in jQuery?
                    var defaul = "["+defaul+"]";
                }
                $("#"+name).val(defaul);
                $("#"+name+"-btn").prop('disabled', false);
            }
