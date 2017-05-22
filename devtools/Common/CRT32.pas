{$R-}
unit crt32;
{# freeware}
{# version 1.0.0127}
{# Date 18.01.1997}
{# Author Frank Zimmer}
{# description
 Copyright © 1997, Frank Zimmer, 100703.1602@compuserve.com
 Fixes Copyright © 2001 Juancarlo Añez, juanco@suigeneris.org
 Version: 1.0.0119
 Date:    18.01.1997

 an Implementation of Turbo Pascal CRT-Unit for Win32 Console Subsystem
 tested with Windows NT 4.0
 At Startup you get the Focus to the Console!!!!

 ( with * are not in the original Crt-Unit):
 Procedure and Function:
   ClrScr
   ClrEol
   WhereX
   WhereY
   GotoXY
   InsLine
   DelLine
   HighVideo
   LowVideo
   NormVideo
   TextBackground
   TextColor
   Delay             // use no processtime
   KeyPressed
   ReadKey           // use no processtime
   Sound             // with Windows NT your could use the Variables SoundFrequenz, SoundDuration
   NoSound
   *TextAttribut     // Set TextBackground and TextColor at the same time, usefull for Lastmode
   *FlushInputBuffer // Flush the Keyboard and all other Events
   *ConsoleEnd       // output of 'Press any key' and wait for key input when not pipe
   *Pipe             // True when the output is redirected to a pipe or a file

 Variables:
   WindMin           // the min. WindowRect
   WindMax           // the max. WindowRect
   *ViewMax          // the max. ConsoleBuffer start at (1,1);
   TextAttr          // Actual Attributes only by changing with this Routines
   LastMode          // Last Attributes only by changing with this Routines
   *SoundFrequenz    // with Windows NT your could use these Variables
   *SoundDuration    // how long bells the speaker  -1 until ??, default = -1
   *HConsoleInput    // the Input-handle;
   *HConsoleOutput   // the Output-handle;
   *HConsoleError    // the Error-handle;

 This Source is freeware, have fun :-)

 History
   18.01.97   the first implementation
   23.01.97   Sound, delay, Codepage inserted and setfocus to the console
   24.01.97   Redirected status
}

interface
uses
  Windows, Messages;

{$IFDEF win32}
const
  Intense = FOREGROUND_INTENSITY or BACKGROUND_INTENSITY;
  Black = 0;
  Blue = FOREGROUND_BLUE or BACKGROUND_BLUE;
  Green = FOREGROUND_GREEN or BACKGROUND_GREEN;
  Cyan = Blue and Green;
  Red = FOREGROUND_RED or BACKGROUND_RED;
  Magenta = Blue or Red;
  Brown = Green or Red;
  LightGray = Blue or Green or Red;
  DarkGray = LightGray;
  LightBlue = Blue or Intense;
  LightGreen = Green or Intense;
  LightCyan = Cyan or Intense;
  LightRed = Red or Intense;
  LightMagenta = Magenta or Intense;
  Yellow = Brown or Intense;
  White = LightGray or Intense;

  BackgroundMask = BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
  ForegroundMask = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;

function WhereX: integer;
function WhereY: integer;
procedure ClrEol;
procedure ClrScr;
procedure InsLine;
procedure DelLine;
procedure GotoXY(const x, y: integer);
procedure HighVideo;
procedure LowVideo;
procedure NormVideo;
procedure TextBackground(const Color: word);
procedure TextColor(const Color: word);
procedure TextAttribut(const Color, Background: word);
procedure Delay(const ms: integer);
function KeyPressed: boolean;
function ReadKey: Char;
procedure Sound;
procedure NoSound;
procedure ConsoleEnd;
procedure FlushInputBuffer;
function Pipe: boolean;

procedure Restore;

procedure SetWindowTo(R: TSmallRect);

procedure More(const Text: string);

var
  HConsoleInput: tHandle;
  HConsoleOutput: thandle;
  HConsoleError: Thandle;
  WindMin: tcoord;
  WindMax: tcoord;
  ViewMax: tcoord;
  TextAttr: Word;
  LastMode: Word;
  SoundFrequenz: Integer;
  SoundDuration: Integer;

type
  PD3InputRecord = ^TD3InputRecord;
  TD3InputRecord = record
    EventType: Word;
    case Integer of
      0: (KeyEvent: TKeyEventRecord);
      1: (MouseEvent: TMouseEventRecord);
      2: (WindowBufferSizeEvent: TWindowBufferSizeRecord);
      3: (MenuEvent: TMenuEventRecord);
      4: (FocusEvent: TFocusEventRecord);
  end;

{$ENDIF win32}

implementation
{$IFDEF win32}
uses Classes, SysUtils;

var
  StartAttr: word;
  OldCP: integer;
  CrtPipe: Boolean;
  German: boolean;

procedure ClrEol;
var
  tC: tCoord;
  Len, Nw: LongWord;
  Cbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(HConsoleOutput, cbi);
  len := cbi.dwsize.x - cbi.dwcursorposition.x;
  tc.x := cbi.dwcursorposition.x;
  tc.y := cbi.dwcursorposition.y;
  FillConsoleOutputAttribute(HConsoleOutput, textattr, len, tc, nw);
  FillConsoleOutputCharacter(HConsoleOutput, #32, len, tc, nw);
end;

procedure ClrScr;
var
  tc: tcoord;
  nw: LongWord;
  cbi: TConsoleScreenBufferInfo;
begin
  getConsoleScreenBufferInfo(HConsoleOutput, cbi);
  tc.x := 0;
  tc.y := 0;
  FillConsoleOutputAttribute(HConsoleOutput, textattr, cbi.dwsize.x * cbi.dwsize.y, tc, nw);
  FillConsoleOutputCharacter(HConsoleOutput, #32, cbi.dwsize.x * cbi.dwsize.y, tc, nw);
  setConsoleCursorPosition(hconsoleoutput, tc);
end;

function WhereX: integer;
var
  cbi: TConsoleScreenBufferInfo;
begin
  getConsoleScreenBufferInfo(HConsoleOutput, cbi);
  result := tcoord(cbi.dwCursorPosition).x + 1
end;

function WhereY: integer;
var
  cbi: TConsoleScreenBufferInfo;
begin
  getConsoleScreenBufferInfo(HConsoleOutput, cbi);
  result := tcoord(cbi.dwCursorPosition).y + 1
end;

procedure GotoXY(const x, y: integer);
var
  coord: tcoord;
begin
  coord.x := x - 1;
  coord.y := y - 1;
  setConsoleCursorPosition(hconsoleoutput, coord);
end;

procedure InsLine;
var
  cbi: TConsoleScreenBufferInfo;
  ssr: tsmallrect;
  coord: tcoord;
  ci: tcharinfo;
  nw: LongWord;
begin
  getConsoleScreenBufferInfo(HConsoleOutput, cbi);
  coord := cbi.dwCursorPosition;
  ssr.left := 0;
  ssr.top := coord.y;
  ssr.right := cbi.srwindow.right;
  ssr.bottom := cbi.srwindow.bottom;
  ci.asciichar := #32;
  ci.attributes := cbi.wattributes;
  coord.x := 0;
  coord.y := coord.y + 1;
  ScrollConsoleScreenBuffer(HconsoleOutput, ssr, nil, coord, ci);
  coord.y := coord.y - 1;
  FillConsoleOutputAttribute(HConsoleOutput, textattr, cbi.dwsize.x * cbi.dwsize.y, coord, nw);
end;

procedure DelLine;
var
  cbi: TConsoleScreenBufferInfo;
  ssr: tsmallrect;
  coord: tcoord;
  ci: tcharinfo;
  nw: LongWord;
begin
  getConsoleScreenBufferInfo(HConsoleOutput, cbi);
  coord := cbi.dwCursorPosition;
  ssr.left := 0;
  ssr.top := coord.y + 1;
  ssr.right := cbi.srwindow.right;
  ssr.bottom := cbi.srwindow.bottom;
  ci.asciichar := #32;
  ci.attributes := cbi.wattributes;
  coord.x := 0;
  coord.y := coord.y;
  ScrollConsoleScreenBuffer(HconsoleOutput, ssr, nil, coord, ci);
  FillConsoleOutputAttribute(HConsoleOutput, textattr, cbi.dwsize.x * cbi.dwsize.y, coord, nw);
end;

procedure TextBackground(const Color: word);
begin
  LastMode := TextAttr;
  textattr := Color and BackgroundMask;
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure TextColor(const Color: word);
begin
  LastMode := TextAttr;
  textattr := color and ForegroundMask;
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure TextAttribut(const Color, Background: word);
begin
  LastMode := TextAttr;
  textattr := (color and ForegroundMask) or (Background and BackgroundMask);
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure Restore;
begin
  SetConsoleTextAttribute(hconsoleoutput, LastMode);
  textattr := LastMode;
end;

procedure HighVideo;
begin
  LastMode := TextAttr;
  textattr := textattr or $8;
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure LowVideo;
begin
  LastMode := TextAttr;
  textattr := textattr and $F7;
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure NormVideo;
begin
  LastMode := TextAttr;
  textattr := startAttr;
  SetConsoleTextAttribute(hconsoleoutput, textattr);
end;

procedure FlushInputBuffer;
begin
  FlushConsoleInputBuffer(hconsoleinput)
end;

function keypressed: boolean;
var
  InputRec: array[1..32] of TInputRecord;
  i,
    NumRead: LongWord;
  NEvents: DWORD;
begin
  Result := False;
  if GetNumberOfConsoleInputEvents(HConsoleInput, NEvents)
    and (NEvents > 0)
    and PeekConsoleInput(HConsoleInput, InputRec[1], 32, NumRead)
    and (NumRead > 0) then
    for i := 1 to NumRead do
      if (InputRec[i].EventType = KEY_EVENT)
        and (PD3InputRecord(@InputRec[i]).KeyEvent.bKeyDown) then
      begin
        Result := True;
        Exit
      end
end;

function ReadKey: Char;
var
  NumRead: DWORD;
  InputRec: TInputRecord;
begin
  while not ReadConsoleInput(HConsoleInput, InputRec, 1, NumRead)
    or (InputRec.EventType <> KEY_EVENT)
    or (not PD3InputRecord(@InputRec).KeyEvent.bKeyDown) do
  begin
  end;
  Result := PD3InputRecord(@InputRec).KeyEvent.AsciiChar
end;

procedure delay(const ms: integer);
begin
  sleep(ms);
end;

procedure Sound;
begin
  windows.beep(SoundFrequenz, soundduration);
end;

procedure NoSound;
begin
  windows.beep(soundfrequenz, 0);
end;

procedure ConsoleEnd;
begin
  if isconsole and not crtpipe then
  begin
    if wherex > 1 then writeln;
    textcolor(green);
    setfocus(GetCurrentProcess);
    if german then
      write('Bitte eine Taste drücken!')
    else
      write('Press any key!');
    normvideo;
    FlushInputBuffer;
    ReadKey;
    FlushInputBuffer;
  end;
end;

function Pipe: boolean;
begin
  result := crtpipe;
end;

procedure init;
var
  cbi: TConsoleScreenBufferInfo;
  tc: tcoord;
begin
  SetActiveWindow(0);
  HConsoleInput := GetStdHandle(STD_InPUT_HANDLE);
  if (HConsoleInput = INVALID_HANDLE_VALUE)
    or (HConsoleInput = 0) then
  begin
    AllocConsole;
    HConsoleInput := GetStdHandle(STD_InPUT_HANDLE);
  end;
  HConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  HConsoleError := GetStdHandle(STD_Error_HANDLE);
  if getConsoleScreenBufferInfo(HConsoleOutput, cbi) then
  begin
    TextAttr := cbi.wAttributes;
    StartAttr := cbi.wAttributes;
    lastmode := cbi.wAttributes;
    tc.x := cbi.srwindow.left + 1;
    tc.y := cbi.srwindow.top + 1;
    windmin := tc;
    ViewMax := cbi.dwsize;
    tc.x := cbi.srwindow.right + 1;
    tc.y := cbi.srwindow.bottom + 1;
    windmax := tc;
    crtpipe := false;
  end
  else
    crtpipe := true;
  SoundFrequenz := 1000;
  SoundDuration := -1;
  oldCp := GetConsoleoutputCP;
 //SetConsoleoutputCP(1252);
  german := $07 = (LoWord(GetUserDefaultLangID) and $3FF);
end;

procedure SetWindowTo(R: TSmallRect);
begin
  SetConsoleWindowInfo(hConsoleOutput, TRUE, R)
end;

procedure More(const Text :string);
var
  S :TStrings;
  i :Integer;
begin
  S := TStringList.Create;
  try
    S.Text := Text;
    i := 0;
    while i < S.Count do
    begin
      if (Pos('---', S[i]) <> 1) then
      begin
        Writeln(S[i]);
        Inc(i);
      end
      else
      begin
        Write(Format('-- More (%d%%) --'#13, [100*(i+2) div S.Count]));
        Inc(i);
        repeat until ReadKey in [' ',#13,#10, 'q'];
        writeln(#13' ': 70);
      end;
    end;
  finally
    S.Free;
  end;
end;

initialization
  init;
finalization
  SetConsoleoutputCP(oldcp);
{$ENDIF win32}
end.

