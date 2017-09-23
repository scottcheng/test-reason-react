let se = ReasonReact.stringToElement;

type item = {id: int, title: string, completed: bool};

module TodoItem = {
  let component = ReasonReact.statelessComponent "TodoItem";
  let make ::item ::onToggle _children => {
    ...component,
    render: fun _self =>
      <div className="item">
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean item.completed)
          onClick=onToggle
        />
        (se item.title)
      </div>
  };
};

module Input = {
  let valueFromEvent evt :string => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target evt))##value;
  type actions =
    | UpdateText string
    | KeyDown string;
  let component = ReasonReact.reducerComponent "Input";
  let make ::onSubmit _children => {
    ...component,
    initialState: fun () => "",
    reducer: fun action state =>
      switch action {
      | UpdateText text => ReasonReact.Update text
      | KeyDown key =>
        if (key == "Enter") {
          onSubmit state;
          ReasonReact.Update ""
        } else {
          ReasonReact.NoUpdate
        }
      },
    render: fun {state: text, reduce} =>
      <input
        value=text
        _type="text"
        placeholder="Write something"
        onChange=(reduce (fun evt => UpdateText (valueFromEvent evt)))
        onKeyDown=(reduce (fun evt => KeyDown (ReactEventRe.Keyboard.key evt)))
      />
  };
};

let lastId = ref 0;

let newItem text => {
  lastId := !lastId + 1;
  {id: !lastId, title: text, completed: false}
};

let toggleItem items id =>
  items |> List.map (fun item => item.id === id ? {...item, completed: not item.completed} : item);

type actions =
  | AddItem string
  | ToggleItem int;

type state = {items: list item};

let component = ReasonReact.reducerComponent "TodoApp";

let make ::className _children => {
  ...component,
  initialState: fun () => {items: [{id: !lastId, title: "Make things", completed: false}]},
  reducer: fun action state =>
    switch action {
    | AddItem text => ReasonReact.Update {...state, items: [newItem text, ...state.items]}
    | ToggleItem id => ReasonReact.Update {...state, items: toggleItem state.items id}
    },
  render: fun {state: {items}, reduce} => {
    let numItems = List.length items;
    <div className=(className ^ " app")>
      <div className="title">
        (se "What to do")
        <Input onSubmit=(reduce (fun text => AddItem text)) />
      </div>
      <div className="items">
        (
          items |>
          List.map (
            fun item =>
              <TodoItem
                key=(string_of_int item.id)
                item
                onToggle=(reduce (fun _ => ToggleItem item.id))
              />
          ) |> Array.of_list |> ReasonReact.arrayToElement
        )
      </div>
      <div className="footer"> (se (string_of_int numItems ^ " items")) </div>
    </div>
  }
};
