// Generated by BUCKLESCRIPT VERSION 1.9.2, PLEASE EDIT WITH CARE
'use strict';

var List        = require("bs-platform/lib/js/list.js");
var $$Array     = require("bs-platform/lib/js/array.js");
var Block       = require("bs-platform/lib/js/block.js");
var Curry       = require("bs-platform/lib/js/curry.js");
var React       = require("react");
var Js_boolean  = require("bs-platform/lib/js/js_boolean.js");
var Pervasives  = require("bs-platform/lib/js/pervasives.js");
var ReasonReact = require("reason-react/lib/js/src/reasonReact.js");

function se(prim) {
  return prim;
}

var component = ReasonReact.statelessComponent("TodoItem");

function make(item, onToggle, _) {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function () {
      return React.createElement("div", {
                  className: "item"
                }, React.createElement("input", {
                      checked: Js_boolean.to_js_boolean(item[/* completed */2]),
                      type: "checkbox",
                      onClick: onToggle
                    }), item[/* title */1]);
    });
  return newrecord;
}

var TodoItem = /* module */[
  /* component */component,
  /* make */make
];

function valueFromEvent(evt) {
  return evt.target.value;
}

var component$1 = ReasonReact.reducerComponent("Input");

function make$1(onSubmit, _) {
  var newrecord = component$1.slice();
  newrecord[/* render */9] = (function (param) {
      var reduce = param[/* reduce */3];
      return React.createElement("input", {
                  placeholder: "Write something",
                  type: "text",
                  value: param[/* state */4],
                  onKeyDown: Curry._1(reduce, (function (evt) {
                          return /* KeyDown */Block.__(1, [evt.key]);
                        })),
                  onChange: Curry._1(reduce, (function (evt) {
                          return /* UpdateText */Block.__(0, [evt.target.value]);
                        }))
                });
    });
  newrecord[/* initialState */10] = (function () {
      return "";
    });
  newrecord[/* reducer */12] = (function (action, state) {
      if (action.tag) {
        if (action[0] === "Enter") {
          Curry._1(onSubmit, state);
          return /* Update */Block.__(0, [""]);
        } else {
          return /* NoUpdate */0;
        }
      } else {
        return /* Update */Block.__(0, [action[0]]);
      }
    });
  return newrecord;
}

var Input = /* module */[
  /* valueFromEvent */valueFromEvent,
  /* component */component$1,
  /* make */make$1
];

var lastId = [0];

function newItem(text) {
  lastId[0] = lastId[0] + 1 | 0;
  return /* record */[
          /* id */lastId[0],
          /* title */text,
          /* completed : false */0
        ];
}

function toggleItem(items, id) {
  return List.map((function (item) {
                var match = +(item[/* id */0] === id);
                if (match !== 0) {
                  return /* record */[
                          /* id */item[/* id */0],
                          /* title */item[/* title */1],
                          /* completed */1 - item[/* completed */2]
                        ];
                } else {
                  return item;
                }
              }), items);
}

var component$2 = ReasonReact.reducerComponent("TodoApp");

function make$2(className, _) {
  var newrecord = component$2.slice();
  newrecord[/* render */9] = (function (param) {
      var items = param[/* state */4][/* items */0];
      var reduce = param[/* reduce */3];
      var numItems = List.length(items);
      return React.createElement("div", {
                  className: className + " app"
                }, React.createElement("div", {
                      className: "title"
                    }, "What to do", ReasonReact.element(/* None */0, /* None */0, make$1(Curry._1(reduce, (function (text) {
                                    return /* AddItem */Block.__(0, [text]);
                                  })), /* array */[]))), React.createElement("div", {
                      className: "items"
                    }, $$Array.of_list(List.map((function (item) {
                                return ReasonReact.element(/* Some */[Pervasives.string_of_int(item[/* id */0])], /* None */0, make(item, Curry._1(reduce, (function () {
                                                      return /* ToggleItem */Block.__(1, [item[/* id */0]]);
                                                    })), /* array */[]));
                              }), items))), React.createElement("div", {
                      className: "footer"
                    }, Pervasives.string_of_int(numItems) + " items"));
    });
  newrecord[/* initialState */10] = (function () {
      return /* record */[/* items : :: */[
                /* record */[
                  /* id */lastId[0],
                  /* title */"Make things",
                  /* completed : false */0
                ],
                /* [] */0
              ]];
    });
  newrecord[/* reducer */12] = (function (action, state) {
      if (action.tag) {
        return /* Update */Block.__(0, [/* record */[/* items */toggleItem(state[/* items */0], action[0])]]);
      } else {
        return /* Update */Block.__(0, [/* record */[/* items : :: */[
                      newItem(action[0]),
                      state[/* items */0]
                    ]]]);
      }
    });
  return newrecord;
}

exports.se         = se;
exports.TodoItem   = TodoItem;
exports.Input      = Input;
exports.lastId     = lastId;
exports.newItem    = newItem;
exports.toggleItem = toggleItem;
exports.component  = component$2;
exports.make       = make$2;
/* component Not a pure module */
