{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBasethb.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer@Excite.com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):


Last Modified: 2002-07-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvBaseThumbnail;

interface
uses Classes, Controls, ExtCtrls, Windows, SysUtils, Messages, Forms;

type

{ The TfileName Object has been created to handle the first field of a Thumb
  Wich is the Thumbs actual FileName complite with the path because no
  duplicates are alowed in the final list.
  It Has the following properties
    01) Filename : it Keeps the filename as gived by the user
    02) LongName : It always returns the longName of the file
    03) ShortName : It Always returns the short name of the file
    04) Size      : It returns the size in byte that it will occupy if saved in a stream
    05) Length    : The "Filename" property Length;
  and the following Methods
    01) LoadFromStream(Astream:TStream;Apos:Integer); It loads a filename from a stream
        if Apos < 0 then don't change the cursors position in the stream
        else AStream.seek(Apos,0);
    02) SaveToStream(Astream:TStream;Apos:Integer); Save the Filename to AStream
        If APos>-1 then AStream.seek(Apos,0);
        SaveData;
}
  TProgressNotify = procedure (Sender: TObject; Position: integer; var Break: boolean) of object;
  TGRF_type =(GR_BMP,GR_JPG,GR_WMF,GR_EMF,GR_ICO,{$IFNDEF COMPILER6_UP}GR_GIF,{$ENDIF}GR_PNG);//,GR_PCX,GR_TGA);
  TPercent = -100..100;

  TFileName = Class(TObject)
    Private
      VLongName     : String;
      VShortName    : String;
      VFileName     : String;
      VCreated,
      VAccessed,
      VModified     : TDatetime;
      VFileSize     : LongInt;
    Protected
      Procedure SetName(NewName:String);Virtual;
      Function  GetLength : integer;
      Procedure SetLength (NewLength:integer);
      Procedure Init;
    Public
      Constructor Create;
      Destructor Destroy; override;
      Procedure LoadFromStream(Astream:TStream;Apos:Integer);//Load From stream;
      // both of this routines are inserting extract data to the stream its self
      // like a header and data end string;
      Procedure SaveToStream(AStream:TStream;Apos:Integer);// Save to a Stream;
    Published
      Property LongName  : string read VLongName;// The LongName of this filenam;
      Property ShortName : String Read VShortName; // shortname of this filename
      Property Filename  : string read VFileName Write SetName; // The Filename AS given by the user;
      Property Length    : Integer read GetLength Write SetLength;
  end;

{ The Following classes are declared here so I can handle interaction of the mouse
  between the three components.
}
PJvThumbtitle=^TJvThumbTitle;
TJvThumbTitle = class(TPanel)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Click;override;
    procedure DblClick;override;
    procedure MouseDown(button:Tmousebutton;shift:Tshiftstate;
                         x,y:integer);override;
    procedure MouseUp(button:Tmousebutton;shift:tshiftstate;
                       x,y:integer);override;
    procedure MouseMove(Shift:TShiftState;x,y:integer);override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                          MousePos: TPoint): Boolean; Override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); Override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); Override;
    procedure KeyPress(var Key: Char); Override;
  public
    constructor Create(AOwner: TComponent); override;
  published
end;{}

TJvBaseThumbImage = class(TImage)
  Private
    VIgnoreMouse : Boolean;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  Protected
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState;
                        X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState;
                      X, Y: Integer);override;
    procedure Click; override;
    procedure DblClick;override;
  Public
    Constructor Create(Aowner:TComponent);override;
  Published
    Property IgnoreMouse : Boolean Read VIgnoreMouse Write VIgnoreMouse;
end;

TJvBaseTHumbNail = class(TPanel)
  Private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    Procedure MouseDown(Button: TMouseButton;Shift: TShiftState;
                        X, Y: Integer);override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    Procedure MouseUp(Button: TMouseButton;Shift: TShiftState;
                      X, Y: Integer);override;
    procedure Click; override;
    procedure DblClick;override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; Override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); Override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); Override;
    procedure KeyPress(var Key: Char); Override;
  public
    Constructor Create(Aowner:Tcomponent);override;
  published
end;

TJvBaseThumbView = class(TScrollBox)
  Private
//    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  Protected
  Public
    Constructor Create(Aowner:TComponent);override;
  published
end;


function BoundByte(min,max,value:integer):byte;
Procedure InsertStr(Var Str:String;Const NewStr:String;Pos:Longint);
function ProportionalSize(phisicalsize,newsize:tpoint):Tpoint;
Function CompletePath(Const Path : String):String;
Function IncompletePath(APath:String):String;
Function ReplaceChar(Const AStr : string; Const CharToFind,NewChar:Char;
                     ReplaceNo:longint;CaseSensitive:boolean):String;
Function Jkceil(i:extended):longint;
function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
                       CaseSensitive:boolean): string;

implementation
Const
{$IFDEF COMPILER5_UP}
  PathEnd ='\';
{$ENDIF}
{$IFDEF LINUX}
  PathEnd ='/';
{$ENDIF}

function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
                       CaseSensitive:boolean): string;
var
  Cnt         : Integer;
  S1, S2, SF  : string;

begin
  S1 := Str;
  If CaseSensitive then
  begin
    S2 := S1;
    SF := SearchFor;
  end
  else
  begin
    S2 := UpperCase(S1);
    SF := UpperCase(SearchFor);
  end;
  Result := '';
  repeat
    Cnt := Pos(SF, S2);
    if Cnt > 0 then
    begin
      Result := Result + Copy(S1, 1, Cnt - 1) + ReplaceWith;
      S1 := Copy(S1, Cnt + Length(SF), Length(S1));
      if CaseSensitive then S2:=S1 else S2 := UpperCase(S1);
    end
    else
      Result := Result + S1;
  until Cnt <= 0;
end;
Function Jkceil(i:extended):longint;
var
  t:longint;
begin
  t:=trunc(i);
  if t<>i then
   if i>0 then t:=t+1
   else t:=t-1;
  result:=t;
end;

Function ReplaceChar( Const AStr:string;Const CharToFind,NewChar:Char;
                      ReplaceNo:Longint;CaseSensitive:boolean):String;
var
  Count : Longint;
  RepCount : longint;
  res : string;
begin
  res := Astr;
  if replaceNo >0 then repcount :=0 else repcount:=-1;
  Count := 1;
  if length(Res)>0 then repeat
    if res[count] = chartofind then
    begin
      res[count]:=newchar;
      if repcount >=0 then inc(repcount,1);
    end;
    inc(Count,1);
  until (Count > length(Res)) or (RepCount>=ReplaceNo);
  result := res;
end;

// add the slash character at the end of the path.
Function CompletePath(Const Path:String):String;
var
 res:String;
begin
  If Length(Path)>0 then
  begin
    if path[length(Path)] <> PathEnd then  res := path + PathEnd
    else res:= path;
  end
  else res :=path;
  result := res;
end;
// Remove the slash character from the end of the path.
Function IncompletePath(APath:String):String;
begin
  IF Length(APath)>0 then
  begin
    If Apath[length(Apath)] = PathEnd then
      result := Copy(Apath,1,Length(Apath)-1)
    else
      Result := APath;
  end
  else Result := APath;
end;

function ProportionalSize(phisicalsize,newsize:tpoint):Tpoint;
var
  Percent      : single;
  tempx,tempy  : Single;
  fs:tpoint;
begin
  // Υπολογισμός ποσοστού  επί της εκατό που θα επιδοθεί στην τιμή προς
  // αλλαγή.
  if phisicalsize.x<>0 then
    tempx:=((newsize.x)/phisicalsize.x)*100
  else tempx:=0;
  If Phisicalsize.y<>0 then
    tempy:=((newsize.y)/phisicalsize.y)*100
  else Tempy:=0;
  //Ευρεση μικρότερου ποσοστού αλαγής και χρήση αυτού.
  if tempx<=tempy then
    percent:=tempx
  else
    percent:=tempy;
  //fs.x:=round((phisicalsize.x/100)*percent);
  //fs.y:=round((phisicalsize.y/100)*percent);
  fs.x:=trunc((phisicalsize.x/100)*percent);
  fs.y:=trunc((phisicalsize.y/100)*percent);
  result:=fs;
end;

Procedure InsertStr(Var Str:String; Const NewStr:String;Pos:Longint);
begin
  SetLength(Str,Length(str)+Length(NewStr));
  CopyMemory(@Str[pos+Length(NewStr)+1],@Str[Pos+1],Length(Str)-Pos-Length(NewStr));
  CopyMemory(@str[pos+1],@NewStr[1],Length(newStr));
end;

function BoundByte(min,max,value:integer):byte;
begin
  if value<min then result:=min
  else
  if value>max then result:=max
  else result:=value;
end;

//******************* Thumbtitle Procedure and functions ******************
procedure TJvThumbTitle.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result:=1;
end;

procedure TJvThumbTitle.Mousedown(Button:TMouseButton;Shift: TShiftState;
                                X, Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mousedown(button,shift,x+left,y+top)
  else inherited;
end;

procedure TJvThumbTitle.mouseup(Button:TMouseButton;Shift: TShiftState;
                              X, Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mouseup(button,shift,x+left,y+top)
  else inherited;
end;

procedure TJvThumbTitle.click;
begin
  if parent is TJvBaseTHumbNail then TJvBaseTHumbNail(parent).click
  else inherited;
end;

procedure TJvThumbTitle.Dblclick;
begin
  if parent is TJvBaseTHumbNail then TJvBaseTHumbNail(parent).Dblclick
  else inherited;
end;

procedure TJvThumbTitle.mousemove(Shift: TShiftState;X,Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mousemove(shift,x+left,y+top)
  else inherited;
end;

function TJvThumbTitle.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).DoMouseWheel(shift,WheelDelta,MousePos)
  else Result:= inherited DoMouseWheel(Shift,WheelDelta,MousePos);
  Result := true;
End;

function TJvThumbTitle.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).DoMouseWheelDown(shift,MousePos)
  else   Result:=Inherited DoMouseWheelDown(Shift,MousePos);
end;

function TJvThumbTitle.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).DoMouseWheelUp(shift,MousePos)
  else Result := Inherited DoMouseWheelUp(Shift,MousePos);
end;

procedure TJvThumbTitle.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).KeyDown(Key,Shift)
  else Inherited;
end;

procedure TJvThumbTitle.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).KeyUp(Key,Shift)
  else Inherited;
end;

procedure TJvThumbTitle.KeyPress(var Key: Char);
begin
  if parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).KeyPress(Key)
  else Inherited;
end;

Constructor TJvThumbTitle.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  If AOwner is TJvBaseTHumbNail then controlstyle:=controlstyle-[csSetCaption,csCaptureMouse,csClickEvents,csDoubleClicks]
  Else controlstyle:=controlstyle-[csSetCaption];
end;

//******************* TJvBaseThumbImage Procedure and functions ******************

procedure TJvBaseThumbImage.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result:=1;
end;

procedure TJvBaseThumbImage.Mousedown(Button:TMouseButton;Shift: TShiftState;
                                    X, Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mousedown(button,shift,x+left,y+top)
  else inherited mousedown(button,shift,x,y);
end;

procedure TJvBaseThumbImage.mouseup(Button:TMouseButton;Shift: TShiftState;
                              X, Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mouseup(button,shift,x+left,y+top)
  else inherited mouseup(button,shift,x,y);
end;

procedure TJvBaseThumbImage.mousemove(Shift: TShiftState;X,Y: Integer);
begin
  if Parent is TJvBaseTHumbNail then
    TJvBaseTHumbNail(parent).mousemove(shift,x+left,y+top)
  else inherited mousemove(shift,x,y);
end;

procedure TJvBaseThumbImage.click;
begin
  if parent is TJvBaseTHumbNail then TJvBaseTHumbNail(parent).click
  else inherited click;
end;

procedure TJvBaseThumbImage.dblclick;
begin
  if parent is TJvBaseTHumbNail then TJvBaseTHumbNail(parent).Dblclick
  else inherited dblclick;
end;

Constructor TJvBaseThumbImage.Create(AOwner:TComponent);
begin
  Inherited;
  ControlStyle:=controlstyle-[csSetCaption];
{  If AOwner is TJvBaseTHumbNail then begin
    ControlStyle := ControlStyle-[csCaptureMouse];
    VIgnoreMouse := True
  end Else{} VIgnoreMouse := False;
end;

procedure TJvBaseThumbImage.CMHitTest(var Message: TCMHitTest);
begin
  If VIgnoreMouse then Message.Result := HTNOWHERE //  0;
  else Message.Result := HTCLIENT;// 1;
end;

//******************* TJvBaseTHumbNail Procedure and functions *******************

Procedure TJvBaseTHumbNail.mousedown(Button: TMouseButton;Shift: TShiftState;
                                   X, Y: Integer);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).mousedown(button,shift,left+x,top+y)
  else inherited;
end;

procedure TJvBaseTHumbNail.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result:=1;
end;

procedure TJvBaseTHumbNail.mousemove(Shift: TShiftState; X,Y: Integer);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).MouseMove(shift,left+x,top+y)
  else inherited;
end;

function TJvBaseTHumbNail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).DoMouseWheel(shift,WheelDelta,MousePos)
  else Result:= inherited DoMouseWheel(Shift,WheelDelta,MousePos);
End;

function TJvBaseTHumbNail.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).DoMouseWheelDown(shift,MousePos)
  else   Result:=Inherited DoMouseWheelDown(Shift,MousePos);
end;

function TJvBaseTHumbNail.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).DoMouseWheelUp(shift,MousePos)
  else Result := Inherited DoMouseWheelUp(Shift,MousePos);
end;

procedure TJvBaseTHumbNail.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).KeyDown(Key,Shift)
  else Inherited;
end;

procedure TJvBaseTHumbNail.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).KeyUp(Key,Shift)
  else Inherited;
end;

procedure TJvBaseTHumbNail.KeyPress(var Key: Char);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).KeyPress(Key)
  else Inherited;
end;

Constructor TJvBaseTHumbNail.Create(Aowner:Tcomponent);
begin
  inherited;
  If AOwner is TJvBaseTHumbview then
    controlstyle:=controlstyle-[csSetCaption,csCaptureMouse]//,
//                                csClickEvents,csDoubleClicks]
  Else{} controlstyle:=controlstyle-[csSetCaption];
end;

procedure TJvBaseTHumbNail.mouseup(Button: TMouseButton;Shift: TShiftState;
                                 X, Y: Integer);
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).MouseUp(Button,shift,left+x,top+y)
  else inherited;
end;

procedure TJvBaseTHumbNail.click;
begin
  if parent is TJvBaseTHumbview then TJvBaseTHumbview(parent).click
  else inherited click;
end;

procedure TJvBaseTHumbNail.dblclick;
begin
  if parent is TJvBaseTHumbview then
    TJvBaseTHumbview(parent).Dblclick
  else{} inherited dblclick;
end;

Constructor TJvBaseTHumbview.Create(Aowner:Tcomponent);
begin
  inherited;
  controlstate:=controlstate+[csFocusing];
  controlstyle:=controlstyle+[csopaque]-[csSetCaption];
end;
{
procedure TJvBaseTHumbview.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 0;
end;{}

//******************* TFileName Procedure and functions *******************
Procedure TFileName.SetName (NewName:string);
begin
  VFileName := NewName;
  IF (NewName <> LongName) and (NewName <> ShortName) then init;
end;
Procedure TFileName.Init;
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
  dft:dword;
  lft:tfiletime;
begin
  SearchHandle := FindFirstFile(PChar(VFileName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    VLongName := string(Temp.cFileName);
    VShortName := String(Temp.cAlternateFileName);
    if VLongName = '' then VLongName:= VShortName;
    If VShortName ='' then VShortName:=VLongName;
    //fdFileAccessed
    filetimetolocalfiletime(Temp.ftLastAccessTime,lft);
    filetimetodosdatetime(lft,longrec(dft).hi,longrec(dft).lo);
    VAccessed := Dft;
    //fdFilechanged
    filetimetolocalfiletime(Temp.ftLastwriteTime,lft);
    filetimetodosdatetime(lft,longrec(dft).hi,longrec(dft).lo);
    VModified :=Dft;
    //fdFilecreated
    filetimetolocalfiletime(Temp.ftCreationTime,lft);
    filetimetodosdatetime(lft,longrec(dft).hi,longrec(dft).lo);
    VCreated := DFT;
    VFileSize:=(Temp.nfilesizehigh * maxdword)+Temp.nfilesizelow;
    //VFilename:=NewName;
  end
  else VFileName:= '';
  Windows.FindClose(SearchHandle);
end;


Procedure TFileName.LoadFromStream(Astream:TStream;Apos:Integer);
begin
  // Uner Construction;
end;

Procedure TFileName.SaveToStream(AStream:TStream;Apos:Integer);
begin
  //Under Construction
end;

Function TFileName.GetLength : Integer;
begin
  Result := System.Length(VFileName);
end;

Procedure TFileName.SetLength (NewLength:integer);
begin
  System.SetLength(VfileName,NewLength);
end;

Constructor TFileName.Create;
begin
  inherited;
end;

Destructor TFileName.Destroy;
begin
  inherited;
end;

end.
