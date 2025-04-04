open Feliz


open Browser.Dom
open Elmish
open Elmish.React


module Composed =

    type Model = { counter: int }

    type Msg =
        | Loop
        | PollAction

    let init _ = { counter = 0 }, Cmd.ofMsg Loop

    let update msg model =

        match msg with
        | PollAction ->
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
    let View model _ =

        Html.p $"Counter: {model.counter}"

module BrokenElmish =
    open Feliz.UseElmish

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
            sendMessage $"this message came from the broken loop"

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

        Html.p $"Counter: {model.counter}"



module FixedElmish =
    open Feliz.UseElmish

    type Model = {
        counter: int
        mutable disposed: bool
    } with

        interface System.IDisposable with
            member this.Dispose() = this.disposed <- true

        member this.looping = not this.disposed

    type Msg =
        | Loop
        | PollAction

    let init _ =
        { counter = 0; disposed = false }, Cmd.ofMsg Loop

    let update sendMessage msg model =
        match msg with
        | PollAction ->
            sendMessage $"this message came from the fixed loop"

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

        Html.p $"Counter: {model.counter}"


[<RequireQualifiedAccess>]
type View =
    | FixedUseElmish
    | BrokenUseElmish
    | Composed

    member this.Name =
        match this with
        | FixedUseElmish -> "Fixed use Elmish"
        | BrokenUseElmish -> "Broken use Elmish"
        | Composed -> "Composed"

[<RequireQualifiedAccess>]
type Page =
    | FixedUseElmish
    | BrokenUseElmish
    | Composed of Composed.Model

    member this.View =
        match this with
        | FixedUseElmish -> View.FixedUseElmish
        | BrokenUseElmish -> View.BrokenUseElmish
        | Composed _ -> View.Composed

type model = { Page: Page; Messages: string list }

type Msg =
    | SwitchView of View
    | ComposedMessage of Composed.Msg
    | ReceiveMessage of string

let init () =
    {
        Page = Page.FixedUseElmish
        Messages = []
    },
    Cmd.none

let update msg model =
    match msg with
    | ReceiveMessage m ->
        {
            model with
                Messages = m :: model.Messages
        },
        Cmd.none
    | SwitchView v ->
        let page, cmd =
            match v with
            | View.FixedUseElmish -> Page.FixedUseElmish, Cmd.none
            | View.BrokenUseElmish -> Page.BrokenUseElmish, Cmd.none
            | View.Composed ->
                let composedModel, composedMessage = Composed.init ()

                Page.Composed composedModel, Cmd.map ComposedMessage composedMessage

        { model with Page = page }, cmd
    | ComposedMessage message ->
        let outerCmd =
            match message with
            | Composed.Msg.PollAction -> Cmd.ofMsg (ReceiveMessage "This message came from the child loop")
            | _ -> Cmd.none

        let page, cmd =
            match model.Page with
            | Page.Composed model ->
                let updatedMessage, newCommand = Composed.update message model

                Page.Composed updatedMessage, Cmd.map ComposedMessage newCommand
            | other -> other, Cmd.none

        { model with Page = page }, Cmd.batch [ outerCmd; cmd ]




[<ReactComponent>]
let view model dispatch =


    React.fragment [
        Html.p [
            Html.text "This demo accompanies a "
            Html.a [
                prop.text "blog post"
                prop.href "" //TODO blog post permalink
            ]
            Html.text
                " about unwanted behaviour when polling using the React.UseElmish hook without implementing iDisposable on the model."
        ]
        Html.p "Select a different mode to see the behaviors."
        Html.p [
            Html.a [
                prop.text "Source code"
                prop.href "https://github.com/CompositionalIT/demo-elmish-polling"
            ]
        ]

        for view in [ View.BrokenUseElmish; View.Composed; View.FixedUseElmish ] do
            Html.button [ prop.text view.Name; prop.onClick (fun _ -> view |> SwitchView |> dispatch) ]


        Html.h2 model.Page.View.Name

        match model.Page with
        | Page.BrokenUseElmish -> BrokenElmish.View(ReceiveMessage >> dispatch)
        | Page.FixedUseElmish -> FixedElmish.View(ReceiveMessage >> dispatch)
        | Page.Composed model -> Composed.View model ()

        Html.h2 "Messages"
        Html.ul [
            for message: string in model.Messages do
                Html.li message
        ]
    ]


Program.mkProgram init update view
|> Program.withReactSynchronous "app"
|> Program.run