// URL input binding
// This input binding is very similar to textInputBinding from
// shiny.js
var treeTableInputBinding = new Shiny.InputBinding();


// An input binding must implement these methods
$.extend(treeTableInputBinding, {

  // This returns a jQuery object with the DOM element
  find: function(scope) {
    return $(scope).find('div[type="treeTable"]');
  },

  // return the ID of the DOM element
  getId: function(el) {
    //console.log('get the id');
    return el.id;
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var jtree = $(el).jstree(true);
    if (!jtree) { return undefined; }
    var val = jtree.get_top_selected(full=true);
    if (val[0]===undefined) { return undefined; }
    return {
      id:$(val[0]).attr('id'),
      text:$(val[0]).attr('text'),
      value: val[0].a_attr.value
    };
  },

  // Given the DOM element for the input, set the value
  //setValue: function(el, value) {
  //  console.log('set the value');
  //  el.value = value;
  //},

  // Set up the event listeners so that interactions with the
  // input will result in data being sent to server.
  // callback is a function that queues data to be sent to
  // the server.
  subscribe: function(el, callback) {
    //console.log('subscribe function');
    $(el).on('keyup.treeTableInputBinding input.treeTableInputBinding', function(event) {
      callback(true);
      // When called with true, it will use the rate policy,
      // which in this case is to debounce at 500ms.
    });
    $(el).on('change.treeTableInputBinding', function(event) {
      callback(false);
      // When called with false, it will NOT use the rate policy,
      // so changes will be sent immediately
    });
  },

  // Remove the event listeners
  unsubscribe: function(el) {
    $(el).off('.treeTableInputBinding');
  },

  // Receive messages from the server.
  // Messages sent by updateTreeTableInput() are received by this function.
  receiveMessage: function(el, message) {
    //console.log(message);
    var selected = message.selected;
    //console.log(message.data);
    var json = messageToData(message.data);
    //console.log(json);
    //console.log('selected');
    //console.log(selected);
    if (selected) {
      json = selectTreeByID(json,selected[0]);
    }
    //$(el).jstree({
    //'core' : {
    //  'data' : json,
  //    'multiple': false,
    //  'check_callback':true
    // }
    //});
    $(el).jstree(true).settings.core.data = json;
    //var myj = $(el).jstree(true).get_json();
    //setTimeout(function(){ $(el).jstree(true).refresh(); },100);    
    $(el).jstree(true).refresh();
    //$(el).jstree(true).redraw(true);
    $(el).jstree(true).hide_icons();

    if (selected) {
      json = selectTreeByID(json,selected[0]);
    }
    $(el).jstree(true).settings.core.data = json;
    $(el).jstree(true).refresh();
    //$(el).click();
    //var sel = $(el).jstree(true).get_top_selected(full);
    //var sel2 = $(el).find(sel[0]);
    //console.log($(sel2));
    setTimeout(function(){ $(el).click(); },100);    
  },

  // This returns a full description of the input's state.
  // Note that some inputs may be too complex for a full description of the
  // state to be feasible.
  //getState: function(el) {
  //  console.log('get state function');
  //  return {
  //    label: $(el).parent().find('div[for="' + $escape(el.id) + '"]').text(),
  //    value: el.value
  //  };
  //},

  // The input rate limiting policy
  getRatePolicy: function() {
    //console.log('get rate policy');
    return {
      // Can be 'debounce' or 'throttle'
      policy: 'debounce',
      delay: 500
    };
  }
});

Shiny.inputBindings.register(treeTableInputBinding, 'shiny.treeTableInput');

$(document).on("click","div[type='treeTable']",function(evt){
  var el = $(evt.target);
  //console.log('clicked');
  //console.log(el);
  el.jstree(true).hide_icons();
  el.trigger("change");
});

// From message json to a good data json
var messageToData = function(json) {
    // Modify json if text is missing
    for (let i in json) {
      // If ID is not set, use text
      // loose typing is intential to catch undefined and null
      if(json[i].id == null) {json[i].id = json[i].text;}
      if (json[i].parent == null) {json[i].parent = '#';}
      // If text is not set get it from ID
      if (!json[i].text) { json[i].text = json[i].id;}
      // Set them open
      if (json[i].state == null) {json[i].state = {};}
      if (json[i].state.opened == null) {json[i].state.opened = true;}
      // Set the value attribute
      if (json[i].a_attr == null) {json[i].a_attr={};}
      if (json[i].value != null) {
        json[i].a_attr.value = json[i].value;
      }
      else if (json[i].text != null) {
        json[i].a_attr.value = json[i].text;
      } else { 
        json[i].a_attr.value = json[i].id; 
      }
    }
  return json;
};

var selectTreeByID = function (data,selected) {
  for (let x in data) {
    if(data[x].id === selected) {
      data[x].state.selected = true;
    }
  }
  return data;
};
var initTreeTable = function (inputID,message,selected) {
  var data = messageToData(JSON.parse(message));
  if(selected) {
    selected = JSON.parse(selected)[0];
    selectTreeByID(data,selected);
  }
  $(inputID).jstree({
    'core' : {
      'data' : data,
      'multiple': false,
      'check_callback':true
    }
  });
  $(inputID).jstree(true).hide_icons();
  $(document).on('shiny:sessioninitialized', function() {
    //console.log('session initialized');
    $(inputID).trigger("change");
    setTimeout(function() {$(inputID).click();},100);
  });
};

