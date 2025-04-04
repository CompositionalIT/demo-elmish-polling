open Feliz


open Browser.Dom

module BrokenElmish =
    open Feliz.UseElmish
    open Elmish

    type Model = { counter: int }

    type Msg =
        | Loop
        | PollAction

    let init _ =
        printfn "initializing broken loop"
        { counter = 0 }, Cmd.ofMsg Loop

    let update sendMessage msg model =
        printfn "calling update on the broken loops"

        match msg with
        | PollAction ->
            sendMessage $"this message came from the broken loop: {model.counter}"

            {
                model with
                    counter = model.counter + 1
            },
            Cmd.none

        | Loop ->
            model,
            Cmd.batch [
                Cmd.ofMsg PollAction
                Cmd.OfAsync.perform (fun _ -> Async.Sleep 1000) () (fun _ -> Loop)
            ]

    [<ReactComponent>]
    let View sendMessage =
        let model, _ = React.useElmish (init, update sendMessage, [])

        Html.div [
            prop.key "broken-elmish"
            prop.children [ Html.h2 "Broken Elmish loop"; Html.p $"inner counter: {model.counter}" ]
        ]



module FixedElmish =
    open Feliz.UseElmish
    open Elmish

    type Model = {
        counter: int
        mutable disposed: bool
    } with

        interface System.IDisposable with
            member this.Dispose() = this.disposed <- true

    type Msg =
        | Loop
        | PollAction

    let init _ =
        { counter = 0; disposed = false }, Cmd.ofMsg Loop

    let update sendMessage msg model =
        match msg with
        | PollAction ->
            sendMessage $"this message came from the fixed loop {model.counter}"

            {
                model with
                    counter = model.counter + 1
            },
            Cmd.none

        | Loop ->

            let nextPoll =
                match model.disposed with
                | true -> Cmd.none
                | false -> Cmd.OfAsync.perform (fun _ -> Async.Sleep 1000) () (fun _ -> Loop)

            model, Cmd.batch [ nextPoll; Cmd.ofMsg PollAction ]

    [<ReactComponent>]
    let View sendMessage =
        let model, _ = React.useElmish (init, update sendMessage, [])

        Html.div [
            prop.key "fixed-loop"
            prop.children [

                Html.h2 "Fixed elmish loop"
                Html.p $"inner counter: {model.counter}"
            ]
        ]

[<ReactComponent>]
let Body () =
    let showingElmish, setShowingElmish = React.useStateWithUpdater false
    let messages, updateMessages = React.useStateWithUpdater []

    let saveMessage =
        fun m ->
            printfn $"saving message {m}"
            updateMessages (fun messages -> m :: messages)

    React.fragment [
        Html.button [ prop.text "toggle elmish"; prop.onClick (fun v -> setShowingElmish (not)) ]
        if showingElmish then
            yield! [ BrokenElmish.View saveMessage; FixedElmish.View saveMessage ]
        Html.ul [
            for message: string in messages do
                Html.li message
        ]
    ]



let root = ReactDOM.createRoot (document.getElementById "app")
root.render (Body())