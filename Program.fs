open Feliz


open Browser.Dom

module Elmish =
    open Feliz.UseElmish
    open Elmish

    type Model =
        { counter: int
          mutable disposed: bool }

        interface System.IDisposable with
            member this.Dispose() = this.disposed <- true

    type Msg =
        | Poll
        | IncrementCounters

    let init _ =
        { counter = 0; disposed = false }, Cmd.ofMsg Poll

    let update updateCounter msg model =
        match msg with
        | IncrementCounters ->
            updateCounter ((+) 1)

            { model with
                counter = model.counter + 1 },
            Cmd.none

        | Poll ->
            printfn "polling"

            let nextPoll =
                match model.disposed with
                | true -> Cmd.none
                | false ->
                    Cmd.OfAsync.perform
                        (fun _ ->
                            async {
                                do! Async.Sleep 1000

                            })
                        ()
                        (fun _ -> Poll)

            model, Cmd.batch [ nextPoll; Cmd.ofMsg IncrementCounters ]

    [<ReactComponent>]
    let View updateCounter =
        let model, _ = React.useElmish (init, update updateCounter, [])

        Html.p $"inner counter: {model.counter}"

[<ReactComponent>]
let Body () =
    let showingElmish, setShowingElmish = React.useStateWithUpdater false
    let counter, setCounter = React.useStateWithUpdater 0

    React.fragment
        [ Html.p $"counter {counter}"
          Html.button [ prop.text "toggle elmish"; prop.onClick (fun v -> setShowingElmish (not)) ]
          if showingElmish then
              Elmish.View setCounter
          else
              Html.p "not showing elmish loop" ]



let root = ReactDOM.createRoot (document.getElementById "app")
root.render (Body())
