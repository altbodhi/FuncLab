module AvaloniaGame
open System
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Themes.Default
open Avalonia.Styling
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.Input
open Game
open Util
open Game


let uiCommand key =
    match key with
    | Key.Up -> Some Up
    | Key.Down -> Some Down
    | Key.Left -> Some Left
    | Key.Right -> Some Right
    | Key.Escape -> Some Exit
    | _ -> None

type FsApp() = class
    inherit Application()
    override this.Initialize() =
        this.Styles.Add(new DefaultTheme())
        let loader = new AvaloniaXamlLoader()
        let baseLight = loader.Load(new Uri("resm:Avalonia.Themes.Default.Accents.BaseLight.xaml?assembly=Avalonia.Themes.Default"))
        let style  = downcast baseLight : IStyle
        this.Styles.Add(style)
end

type MsgBox (message: string) as it = class
    inherit Window()
    
    let button = new Button (Content = message)
    
    do
        it.Title <- message
        button.FontWeight <- FontWeight.Bold
        button.FontSize <- 72. 
        button.Foreground <- Brushes.Yellow
        button.Background <- Brushes.Blue
        button.Click.Add (fun _ -> it.Close())
        it.Content <- button

    static member ShowMe mess = 
        let msg = new MsgBox(mess)
        msg.ShowDialog() |> ignore
end

let getBrush = function
    | Docker _ -> Brushes.Blue
    | Block _ -> Brushes.GreenYellow
    | Brick _ -> Brushes.SandyBrown
    | Target _ -> Brushes.OrangeRed


type MainWindow() as it = class
    inherit Window()
    let initSize = 1000.
   
    let canvas = new Canvas( Focusable = true)
  
    do
        it.Title <- "Грузчик"
        canvas.Background <- Brushes.Gray
        it.Content <- canvas
        canvas.Focus()
    
    member it.Draw state rs = 
        canvas.Children.Clear()
        let maxX = state |> List.map getPosition |> List.map (fun (x,y) -> x) |> List.max
        let maxY = state |> List.map getPosition |> List.map (fun (x,y) -> y) |> List.max
        let dy = initSize / (1.0 + float maxY) 
        let dx = initSize / (1.0 + float maxX)
        let de = if dy < dx then dy else dx

        if rs then
            it.Width <- de * (1.0 + float maxX)
            it.Height <- de * (1.0 + float maxY)
            

        for cell in state do
            let (x,y) = getPosition cell
            let rect = new Rectangle( Width = de,  Height = de,   Fill = getBrush cell)
            Canvas.SetLeft(rect,de * float x);
            Canvas.SetTop(rect,de * float y);
            canvas.Children.Add rect
        it.InvalidateVisual()


    member it.ShowWin isWin =
        if isWin then
            MsgBox.ShowMe "Поздравляю!\n\nГруз доставлен!"
        else ()            

    member it.Play (zero) =
        it.Draw zero true

        let gameOver = new Progress<unit>()
        gameOver.ProgressChanged.Add(fun _ -> printfn "exit"; it.Close())
        
        let progress = new Progress<Element list>()
        progress.ProgressChanged.Add ( fun e -> it.Draw e false; it.ShowWin (win e))

        let commander = 
            MailboxProcessor.Start( fun inbox -> 
                let rec loop (state) = async { 
                    let! cmd = inbox.Receive()
                    match cmd with
                    | None -> 
                        return! loop state
                    | Some Exit -> (gameOver:> IProgress<unit>).Report()
                    | Some command -> 
                        let newState = gamePlay state command
                        (progress :> IProgress<Element list>).Report(newState)
                        return! loop newState
                    } 
                loop (zero))

       
        canvas.KeyDown.Add (fun k -> commander.Post (uiCommand (k.Key)))
end

let start() =
    let zero = loadMap "map.txt" 
    let app = new FsApp()
    AppBuilder.Configure(app).UsePlatformDetect().SetupWithoutStarting() |> ignore
    let mainWindow = new MainWindow()    

    mainWindow.Show()
    mainWindow.Play zero

    app.Run(mainWindow) 

    