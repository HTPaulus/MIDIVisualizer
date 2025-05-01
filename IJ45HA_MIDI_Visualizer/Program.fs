open System
open System.Drawing
open System.Windows.Forms
open System.Threading
open NAudio.Midi
open System.Collections.Generic

let ColorFromHSV(hue: float, saturation: float, value: float) =
    let hi = int (hue / 60.0) % 6
    let f = hue / 60.0 - float hi
    let v = value * 255.0
    let p = v * (1.0 - saturation)
    let q = v * (1.0 - f * saturation)
    let t = v * (1.0 - (1.0 - f) * saturation)
    match hi with
    | 0 -> Color.FromArgb(int v, int t, int p)
    | 1 -> Color.FromArgb(int q, int v, int p)
    | 2 -> Color.FromArgb(int p, int v, int t)
    | 3 -> Color.FromArgb(int p, int q, int v)
    | 4 -> Color.FromArgb(int t, int p, int v)
    | _ -> Color.FromArgb(int v, int p, int q)

let noteToColor (note: int) (velocity: int) (hueOffset: float) =
    let hueMap = dict [
        0, 0.0     // C
        2, 60.0    // D
        4, 120.0   // E
        5, 180.0   // F
        7, 240.0   // G
        9, 300.0   // A
        11, 360.0  // H/C
    ]

    let n = note % 12

    let lower, upper =
        [0..11]
        |> List.pairwise
        |> List.tryFind (fun (a, b) -> a <= n && n <= b && hueMap.ContainsKey a && hueMap.ContainsKey b)
        |> Option.defaultValue (0, 11)

    let h1 = hueMap.[lower]
    let h2 = hueMap.[upper]
    let interp =
        if upper = lower then 0.0
        else float (n - lower) / float (upper - lower)

    let hue = ((1.0 - interp) * h1 + interp * h2 + hueOffset) % 360.0
    let saturation = 0.6
    let minValue = 0.3
    let maxValue = 0.7
    let brightness = minValue + (float velocity / 127.0) * (maxValue - minValue)
    ColorFromHSV(hue, saturation, brightness)

[<CLIMutable>]
type Note = {
    Note: int
    Velocity: int
    Start: int
    Duration: int
    Channel: int
}

let parseMidiFile (path: string) =
    let midi = new MidiFile(path, false)
    let notes = ResizeArray<Note>()
    for track in midi.Events do
        let activeNotes = Dictionary<(int * int), (int64 * int)>()
        for e in track do
            match e with
            | :? NoteOnEvent as noteOn when noteOn.Velocity > 0 ->
                let key = (noteOn.NoteNumber, noteOn.Channel)
                if not (activeNotes.ContainsKey(key)) then
                    activeNotes.Add(key, (noteOn.AbsoluteTime, noteOn.Velocity))
            | :? NoteEvent as noteOff when
                  noteOff.CommandCode = MidiCommandCode.NoteOff ||
                  (noteOff :? NoteOnEvent && (noteOff :?> NoteOnEvent).Velocity = 0) ->
                let key = (noteOff.NoteNumber, noteOff.Channel)
                match activeNotes.TryGetValue(key) with
                | true, (start, velocity) ->
                    let duration = int (noteOff.AbsoluteTime - start)
                    notes.Add({ Note = noteOff.NoteNumber; Velocity = velocity; Start = int start; Duration = duration; Channel = noteOff.Channel })
                    activeNotes.Remove(key) |> ignore
                | _ -> ()
            | _ -> ()
    notes |> List.ofSeq

let drawVisualization (notes: Note list) (hueOffset: float) =
    let width = 1000
    let height = 1000
    let sortedNotes = notes |> List.sortBy (fun n -> n.Start)

    let maxRowWidth = width
    let mutable x = 0
    let mutable rowCount = 1

    for note in sortedNotes do
        let length = Math.Min(note.Duration / 10, 100)
        if x + length > maxRowWidth then
            rowCount <- rowCount + 1
            x <- 0
        x <- x + length

    let rowHeight = height / rowCount
    let bmp = new Bitmap(width, height)
    use g = Graphics.FromImage(bmp)
    g.Clear(Color.Black)

    x <- 0
    let mutable y = 0
    for note in sortedNotes do
        let length = Math.Min(note.Duration / 10, 100)
        if x + length > maxRowWidth then
            x <- 0
            y <- y + rowHeight

        let color = noteToColor note.Note note.Velocity hueOffset
        let rect = new Rectangle(x, y, length, rowHeight)
        use brush = new SolidBrush(color)
        g.FillRectangle(brush, rect)

        x <- x + length

    bmp

[<STAThread>]
[<EntryPoint>]
let main argv =
    let form = new Form(Text = "MIDI Visualizer", Width = 1000, Height = 900)
    let pictureBox = new PictureBox(Width = 1000, Height = 768, Top = 0, Left = 0, BorderStyle = BorderStyle.FixedSingle)
    let importButton = new Button(Text = "Import MIDI", Left = 10, Top = 780, Width = 100)
    let exportButton = new Button(Text = "Export Image", Left = 120, Top = 780, Width = 100)
    let playButton = new Button(Text = "Play", Left = 230, Top = 780, Width = 100)
    let stopButton = new Button(Text = "Stop", Left = 340, Top = 780, Width = 100)
    let slider = new TrackBar(Left = 450, Top = 780, Width = 200, Minimum = 0, Maximum = 100, TickFrequency = 10, Value = 0)

    let mutable currentImage: Bitmap option = None
    let mutable currentNotes: Note list = []

    let mutable midiOut: MidiOut option = None
    let mutable playThread: Thread option = None
    let mutable isPlaying = false
    let mutable isPaused = false
    let mutable currentNoteIndex = 0

    let refreshImage () =
        if currentNotes.Length > 0 then
            let hueOffset = float slider.Value / 100.0 * 360.0
            let bmp = drawVisualization currentNotes hueOffset
            pictureBox.Image <- bmp :> Image
            currentImage <- Some bmp

    let startPlayback () =
        if currentNotes.Length > 0 then
            let notesArray = currentNotes |> List.sortBy (fun n -> n.Start) |> List.toArray
            midiOut <- Some (new MidiOut(0))
            playThread <- Some (new Thread(fun () ->
                let startTime = System.Diagnostics.Stopwatch.StartNew()
                while currentNoteIndex < notesArray.Length do
                    if isPaused then
                        Thread.Sleep(10)
                    else
                        let note = notesArray.[currentNoteIndex]
                        let waitTime = note.Start * 2
                        while startTime.ElapsedMilliseconds < int64 waitTime do
                            Thread.Sleep(1)
                        let message = (0x90 ||| (note.Channel &&& 0x0F)) ||| (note.Note <<< 8) ||| (note.Velocity <<< 16)
                        midiOut.Value.Send(message)
                        let duration = note.Duration / 2
                        async {
                            do! Async.Sleep(duration)
                            let offMessage = (0x80 ||| (note.Channel &&& 0x0F)) ||| (note.Note <<< 8)
                            midiOut.Value.Send(offMessage)
                        } |> Async.Start
                        currentNoteIndex <- currentNoteIndex + 1
                isPlaying <- false
                isPaused <- false
                currentNoteIndex <- 0
            ))
            isPlaying <- true
            isPaused <- false
            playThread.Value.Start()

    let stopPlayback () =
        match playThread with
        | Some thread when thread.IsAlive -> thread.Abort()
        | _ -> ()
    
        if midiOut.IsSome then
            try
                midiOut.Value.Dispose()
            with _ -> ()
    
        midiOut <- None
        playThread <- None
        isPlaying <- false
        isPaused <- false
        currentNoteIndex <- 0

    importButton.Click.Add(fun _ ->
        use ofd = new OpenFileDialog()
        ofd.Filter <- "MIDI files (*.mid)|*.mid"
        if ofd.ShowDialog() = DialogResult.OK then
            currentNotes <- parseMidiFile ofd.FileName
            refreshImage()
    )

    exportButton.Click.Add(fun _ ->
        match currentImage with
        | Some bmp ->
            use sfd = new SaveFileDialog()
            sfd.Filter <- "PNG files (*.png)|*.png"
            if sfd.ShowDialog() = DialogResult.OK then
                bmp.Save(sfd.FileName)
        | None -> MessageBox.Show("Please load a MIDI file.") |> ignore
    )

    playButton.Click.Add(fun _ ->
        if not isPlaying then
            startPlayback()
            playButton.Text <- "Pause"
        elif isPlaying && not isPaused then
            isPaused <- true
            playButton.Text <- "Play"
        elif isPlaying && isPaused then
            isPaused <- false
            playButton.Text <- "Pause"
    )

    stopButton.Click.Add(fun _ ->
        stopPlayback()
        playButton.Text <- "Play"
    )

    slider.Scroll.Add(fun _ -> refreshImage())

    form.Controls.Add(pictureBox)
    form.Controls.Add(importButton)
    form.Controls.Add(exportButton)
    form.Controls.Add(playButton)
    form.Controls.Add(stopButton)
    form.Controls.Add(slider)
    Application.Run(form)
    0