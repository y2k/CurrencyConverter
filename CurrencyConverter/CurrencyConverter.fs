namespace CurrencyConverter

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module App =
    type Api = FSharp.Data.XmlProvider<"http://www.cbr.ru/scripts/XML_daily.asp", Encoding="1251">

    type Model =
      { currencies : Api.Valute []
        fromCurrency : Api.Valute option
        toCurrency : Api.Valute option
        amount : string
        convertedAmount : string }

    type Msg =
        | GetCurrencies of Choice<Api.ValCurs, exn>
        | FromChanged of int
        | ToChanged of int
        | AmountChanged of string
        | Convert

    let init() =
        { currencies = [||]; fromCurrency = None; toCurrency = None; amount = ""; convertedAmount = "" },
        Api.AsyncLoad("http://www.cbr.ru/scripts/XML_daily.asp") |> (Async.Catch >> Cmd.ofAsyncMsg >> Cmd.map GetCurrencies)

    let update msg model =
        match msg with
        | GetCurrencies(Choice1Of2 x) -> { model with currencies = x.Valutes }, Cmd.none
        | FromChanged x -> { model with fromCurrency = model.currencies |> Array.tryItem x }, Cmd.none
        | ToChanged x -> { model with toCurrency = model.currencies |> Array.tryItem x }, Cmd.none
        | AmountChanged x -> { model with amount = x }, Cmd.none
        | Convert ->
            let convertedAmount =
                (model.fromCurrency, model.toCurrency, try Some(float model.amount) with _ -> None)
                |||> Option.map3 (fun src dst amount -> string <| amount * (float src.Value / float dst.Value) * (float dst.Nominal / float src.Nominal))
            { model with convertedAmount = Option.defaultValue "" convertedAmount }, Cmd.none
        | _ -> model, Cmd.none

    let view (model : Model) dispatch =
        let viewCurrentyPicker target f =
            View.Picker (
                title = sprintf "Select '%s' currency:" target,
                itemsSource = (model.currencies |> Array.map (fun x -> sprintf "%s (%s)" x.Name x.CharCode)),
                selectedIndexChanged = fun (i, _) -> dispatch (f i))
        View.ContentPage(
          content = View.StackLayout(padding = 20.0,
            children = [
                viewCurrentyPicker "source" FromChanged
                viewCurrentyPicker "destination" ToChanged
                View.Editor (
                    text = model.amount,
                    placeholder = "Enter amount",
                    keyboard = Keyboard.Numeric,
                    textChanged = (fun t -> dispatch (AmountChanged t.NewTextValue)))
                View.Button (text = "Convert", command = fun () -> dispatch Convert)
                View.Label(text = model.convertedAmount) ]))

    let program = Program.mkProgram init update view

type App() as app =
    inherit Application()

    let runner =
        App.program
        |> Program.withConsoleTrace
        |> XamarinFormsProgram.run app

    let modelId = "model"
    override __.OnSleep() =
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        app.Properties.[modelId] <- json
    override __.OnResume() =
        try
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) ->
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)
                runner.SetCurrentModel(model, Cmd.none)
            | _ -> ()
        with ex ->
            App.program.onError ("Error while restoring model found in app.Properties", ex)
    override this.OnStart() = this.OnResume()
