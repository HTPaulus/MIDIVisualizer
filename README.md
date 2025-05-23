# MIDIVisualizer
This F# application can import, analyze, visualize, and play MIDI files.

How does it work?
- Click on the Import MIDI button
- Select a file with any .mid extension and click open

- The program will display a visual model of the file and you can adjust the color shift to your liking using the slider on the right. You can also save it as an image by clicking on the Export Image button.

- Click on the play button to play the midi file. You can then stop it by pressing the pause or stop button.

![Screenshot](screenshot.png)


The project needs the following to compile:

- .NET Framework version 4.8.1
- NAudio version 2.1.0

Build and run:

```bash
git clone https://github.com/HTPaulus/MIDIVisualizer.git
cd MIDIVisualizer
dotnet build IJ45HA_MIDI_Visualizer.sln
dotnet run --project IJ45HA_MIDI_Visualizer/IJ45HA_MIDI_Visualizer.fsproj
```
