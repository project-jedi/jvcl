{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbView.PAS, released on 2002-07-03.

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

unit JvThumbviews;

interface
uses
  Windows, Classes, Controls, Forms, dialogs, ExtCtrls, JvThumbNails, Filectrl,
  stdctrls, sysutils, messages, graphics, JvBaseThumbnail;


type
  TPercent = 0..100;
  TScrollMode = (SMHorizontal, SMVertical, SMboth);
  TViewType = (VTNormal, VTCenter, VTFitToScreen);
  TBufferAction = (BFCancel, BFCreate, BFOpen, BFInsert, BFReplace, BFDelete);
  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbnailTitle: string;
    var ThumbnailFont: TFont;
    var ThumbnailColor: TColor) of object;
  TProgressStartNotify = procedure(Sender: TObject; Max: integer) of object;

  TJvThumbList = class(TStringList) // declare a new type of Thumblist and try not to break the old code;
  private
  protected
    function GetThumb(Apos: longint): TJvThumbNail;
  public
    property Thumbnail[index: longint]: TJvThumbNail read GetThumb; default;
  published
  end;

  TJvThumbView = class(TJvBaseThumbView)
  private
    { Private declarations }
    FMaxsize: TPoint;
    FThumbSize: TPoint;
//    Dum: string;
    FPercent: tPercent;
    FDirectory: string;
    FScrollMode: TScrollMode;
    FScroll: boolean;
    FSelected: longint;
    PViewType: TViewType;
    FSpace: byte;
    FMaxX: word;
    FMinMemory: boolean;
    FOnGetTitle: TTitleNotify;
    FOnChange: TNotifyEvent;
    FOnStartScanning: TProgressStartNotify;
    FOnStopScanning: TNotifyEvent;
    FProgressNotify: TProgressNotify;
    FWaitUntilFull: boolean;
    FPainted: boolean;
    FFileList: TStringList;
    FFileListSorted: TStringList;
    FSorted: boolean;
    FFilling: boolean;
    FFilter: string;
//    FBufferFile: string;
    FThumbColor: TColor;
    FAsButtons: Boolean;
    FTitlePos: TTitlePos;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    PAutohandle: Boolean;
    FGraphicExtensions: TstringList;
    FShowShadow: Boolean;
    FShadowColor: TColor;
    FThumbList: TJvThumbList;
    FOnInvalidImage: TInvalidImageEvent;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure GetFiles(ADirectory: string);
    procedure SetSorted(const Value: boolean);
    procedure CalculateMaxX;
    procedure Calculatesize;
    procedure WmGetDlgCode(var Message: TWmGetDlgCode); message WM_GETDLGCODE;
    function CalculatexPos(num: word): longint;
    function Calculateypos(num: word): longint;
    procedure ScrollTo(const number: longint);
    procedure SetViewType(VType: TViewType);
    procedure Reposition(Start: integer);
    procedure GoLeft;
    procedure GoRight;
    procedure GoDown;
    procedure GoUp;
    procedure SetAsButton(const NewVal: Boolean);
    procedure SetTitlePos(const NewVal: TTitlePos);
    function CreateFilter: string;
    procedure SetFilters;
    function GetBufferName(AName: string): string;
    function GetMaxThumbHeight: longint;
    function GetMaxThumbWidth: longint;
    procedure DoInvalidImage(Sender: TObject; const Filename: String);
    //    Procedure WMLoadWhenReady(var Message:TMessage); Message WM_LoadWhenReady;
  protected
    { Protected declarations }
//  {$IFDEF DEBUG}
//    procedure WndProc(var Message: TMessage); Override;
//  {$ENDIF}
    procedure SetScrollMode(Amode: TscrollMode);
    procedure SetSelected(number: longint);
    //    Procedure SetBufferFile(NewName:String);
    procedure Resize; override;
    procedure SetThumbWidth(W: longint);
    procedure SetDirectory(Value: string);
    procedure SetThumbHeight(H: longint);
    procedure Keypress(var key: char); override;
    procedure Keydown(var Key: word; ShiftState: TshiftState); override;
    procedure KeyUp(var Key: word; ShiftState: TshiftState); override;
    procedure SetSpace(SP: byte);
    procedure SetPercent(P: TPercent);
    procedure SetFile(Afile: string);
    function GetFile: string;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    //    Function GetBufferFile:string;
  public
    { Public declarations }
    DiskSize: dword;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure AddThumb(atitle: string; Redraw: boolean);
    procedure AddFromFile(Afile: string);
    procedure AddFromStream(Astream: Tstream; Gr_type: Tgrf_type);
    procedure Delete(no: longint);
    procedure EmptyList;
    procedure SortList;
    procedure Refresh;
    function GetCount: word;
    property ThumbList: TJvThumbList read FThumbList write FThumbList;
  published
    { Published declarations }
    property SelectedFile: string read Getfile write Setfile;
    property AlignView: TViewtype read Pviewtype write setviewtype;
    property AutoScrolling: boolean read FScroll write FScroll;
    property ThumbGap: byte read FSpace write setspace;
    property AutoHandleKeyb: boolean read Pautohandle write Pautohandle;
    property MinMemory: Boolean read FMinMemory write FMinMemory;
    property Count: word read getcount default 0;
    property MaxWidth: longint read GetMaxThumbWidth write SetThumbWidth;
    property MaxHeight: longint read GetMaxThumbHeight write SetThumbheight;
    property Size: TPercent read FPercent write SetPercent;
    property ScrollMode: TScrollMode read FScrollMode write SetScrollMode;
    property Directory: string read FDirectory write Setdirectory;
    property Sorted: boolean read FSorted write SetSorted;
    property Selected: longint read FSelected write setselected default -1;
    property OnStartScanning: TProgressStartNotify read FOnStartScanning write FOnStartScanning;
    property OnStopScanning: TNotifyEvent read FOnStopScanning write FOnStopScanning;
    property OnScanProgress: TProgressNotify read FProgressNotify write FProgressNotify;
    property OnGetTitle: TTitleNotify read FOnGetTitle write FOnGetTitle;
    property OnInvalidImage:TInvalidImageEvent read FOnInvalidImage write FOnInvalidImage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Onkeyup: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property AsButtons: Boolean read FAsButtons write SetAsButton;
    property TitlePlacement: TTitlePos read FTitlePos write SetTitlePos default T_UP;
    property Filter: string read FFilter write FFilter;
    //    Property BufferFile : String Read FBufferFile write SetBufferFile;
    property ThumbColor: TColor read FThumbColor write FThumbColor;
    property ShowShadow: Boolean read FShowShadow write FShowShadow;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property AutoScroll;
    property PopupMenu;
    property BorderStyle;
    property Align;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
  end;


implementation
{const
  FGraphicExtensions  : array[1..9] of string = ('*.BMP','*.JPG','*.WMF','*.EMF',
                                             '*.ICO','*.GIF','*.PCX',
                                             '*.TGA','*.PNG'); {}
const
  CRLF = #13 + #10;

procedure TJvThumbview.DoInvalidImage(Sender:TObject;const Filename:String);
begin
  if Assigned(FOnInvalidImage) then FOnInvalidImage(Sender,Filename);
end;

procedure TJvThumbview.addThumb(atitle: string; Redraw: boolean);
var
  Thb: TJvThumbNail;
begin
  Thb := TJvThumbNail.create(self);
  Thb.left := Calculatexpos(count + 1);
  Thb.top := Calculateypos(count + 1);
  Thb.width := FThumbSize.x;
  Thb.height := FThumbSize.y;
  thb.Asbutton := FAsButtons;
  thb.TitlePlacement := FTitlePos;
  thb.ShadowColor := FShadowColor;
  thb.ShowShadow := FShowShadow;
  Thb.onclick := onclick;
  Thb.photo.onclick := onclick;
  Thb.Photo.OnInvalidImage := DoInvalidImage;
  Thb.ondblclick := ondblclick;
  Thb.photo.ondblclick := ondblclick;
  Thb.minimizememory := MinMemory;
  Thb.Color := self.Color;
  Thb.title := atitle;
  if FThumbColor = clNone then
  begin
    Thb.Color := Self.Color;
    Thb.ParentColor := true;
    Thb.TitleColor := Self.Color;
  end
  else
    Thb.Color := FThumbColor;
  FThumbList.AddObject(thb.Title, thb);
  thb.parent := self;
  if redraw then
  begin
    CalculateSize;
    Reposition(0);
  end;
end;

procedure TJvThumbview.GetFiles(ADirectory: string);
var
  SearchRec: TSearchRec;
  FResult: Integer;
  NumExtensions: integer;

  function FindFirstGraphic(GRF_Extension: string): integer;
  begin
    FindFirstGraphic :=
      FindFirst(ADirectory + GRF_Extension, faArchive, SearchRec);
  end;
begin
  FFileList.Clear;
  FFileListSorted.Clear;
  setFilters;
  if not DirectoryExists(ADirectory) then
    Exit;
  if ADirectory[Length(ADirectory)] <> '\' then
    ADirectory := ADirectory + '\';
  for NumExtensions := 0 to FGraphicExtensions.Count - 1 do
  begin
    if (FindFirstGraphic(FGraphicExtensions[NumExtensions]) = 0) and (FFileList.IndexOf(ADirectory + SearchRec.Name) < 0) then
    begin
      FFileList.Add(ADirectory + SearchRec.Name);
      FFileListSorted.Add(ADirectory + SearchRec.Name);
      repeat
        FResult := FindNext(SearchRec);
        if (FResult = 0) and (FFileList.IndexOf(ADirectory + SearchRec.Name) < 0) then
        begin
          FFileList.Add(ADirectory + SearchRec.Name);
          FFileListSorted.Add(ADirectory + SearchRec.Name);
        end;
      until FResult <> 0;
      FindClose(SearchRec);
    end;
  end;
  FFileListSorted.Sort;
  if Assigned(FGraphicExtensions) then FreeAndNil(FGraphicExtensions);
end;

procedure TJvThumbview.Setviewtype(Vtype: Tviewtype);
begin
  if Vtype <> Pviewtype then
  begin
    Pviewtype := Vtype;
    reposition(0);
  end;
end;

procedure TJvThumbview.ScrollTo(const number: longint);
begin
  // if AutoScrolling then if (number>-1) then
  if (number < 0) or (number > FThumbList.count - 1) then exit;
  case scrollmode of
    smvertical:
      begin
        if TJvThumbNail(FThumbList.objects[number]).top < 0 then
          vertscrollbar.position := vertscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).top -
            (TJvThumbNail(FThumbList.objects[number]).width div 2));
        if TJvThumbNail(FThumbList.objects[number]).top +
          TJvThumbNail(FThumbList.objects[number]).height > height then
          vertscrollbar.position := vertscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).top -
            (height - TJvThumbNail(FThumbList.objects[number]).height -
            (TJvThumbNail(FThumbList.objects[number]).height div 2)));
      end;
    smhorizontal:
      begin
        if TJvThumbNail(FThumbList.objects[number]).left < 0 then
          horzscrollbar.position := Horzscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).left -
            (TJvThumbNail(FThumbList.objects[number]).width div 2));
        if TJvThumbNail(FThumbList.objects[number]).left +
          TJvThumbNail(FThumbList.objects[number]).width > width then
          Horzscrollbar.position := Horzscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).left -
            (width - TJvThumbNail(FThumbList.objects[number]).width -
            (TJvThumbNail(FThumbList.objects[number]).width div 2)));
      end;
    smBoth:
      begin
        if TJvThumbNail(FThumbList.objects[number]).top < 0 then
          vertscrollbar.position := vertscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).top -
            (TJvThumbNail(FThumbList.objects[number]).width div 2));
        if TJvThumbNail(FThumbList.objects[number]).top +
          TJvThumbNail(FThumbList.objects[number]).height > height then
          vertscrollbar.position := vertscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).top -
            (TJvThumbNail(FThumbList.objects[number]).height -
            (TJvThumbNail(FThumbList.objects[number]).height div 2)));
        if TJvThumbNail(FThumbList.objects[number]).left < 0 then
          horzscrollbar.position := Horzscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).left -
            (TJvThumbNail(FThumbList.objects[number]).width div 2));
        if TJvThumbNail(FThumbList.objects[number]).left +
          TJvThumbNail(FThumbList.objects[number]).width > width then
          Horzscrollbar.position := Horzscrollbar.position +
            (TJvThumbNail(FThumbList.objects[number]).left -
            (width - TJvThumbNail(FThumbList.objects[number]).width -
            (TJvThumbNail(FThumbList.objects[number]).width div 2)));
      end;
  end;
end;

function TJvThumbview.GetBufferName(AName: string): string;
var
  tst: string;
  FN: string;
  Res: string;
begin
  tst := completepath(extractFiledir(AName));
  if tst = AName then
  begin // no filename included only a directory;
    // the user wants us to create a seperate file for each
    // directory it opens in a pre-specified path
    FN := ReplaceChar(FDirectory, '\', '_', 0, False); //create the filename from the path
    FN := ReplaceChar(FN, ':', '_', 0, False); //create the filename from the path
    res := AName + fn;
  end
  else
  begin // the user has specified either a full path and a name or just a name
    if tst = '' then
      // the user has specified only a name to use
      // in each directory that is opened by the component there will be created
      // a file with name <ANAME> where the thumbs are been saved;
      Res := CompletePath(FDirectory) + AName
    else
      // the user has specified a full path and a file name weach is the same
      // for all the directories he/she opens.
      res := AName;
  end;
  result := res;
end;

//Procedure TJvThumbview.SetBufferFile(NewName:String);
//var
//  tst : string;
//begin
//  If NewName <> FBufferFile then
//    tst := GetBufferName(NewName);
//  End;
//end;

//---------TJvThumbview OBJECT PROCS AND FUNCTIONS------------------

procedure TJvThumbview.SetSelected(Number: longint);
begin
  if FThumbList.count > 0 then
  begin
    if (FSelected <> -1) then
    begin
      TJvThumbNail(FThumbList.objects[FSelected]).titlecolor :=
        TJvThumbNail(FThumbList.objects[FSelected]).Color;
      TJvThumbNail(FThumbList.objects[FSelected]).titlefont.color :=
        TJvThumbNail(FThumbList.objects[FSelected]).Font.Color;
    end;
    if number <> -1 then
    begin
      TJvThumbNail(FThumbList.objects[Number]).titlecolor := clhighlight;
      TJvThumbNail(FThumbList.objects[Number]).titlefont.color := clhighlighttext;
      if autoscrolling then
      begin
        if (TJvThumbNail(FThumbList.objects[Number]).top +
          TJvThumbNail(FThumbList.objects[Number]).height > height) or
          (TJvThumbNail(FThumbList.objects[Number]).top < 0) then
          Scrollto(number);
        if (TJvThumbNail(FThumbList.objects[Number]).left +
          TJvThumbNail(FThumbList.objects[Number]).width > width) or
          (TJvThumbNail(FThumbList.objects[Number]).left < 0) then
          scrollto(number);
      end
    end;
    if (FSelected <> number) and (assigned(FOnChange)) then FOnChange(self);
    FSelected := number;
  end;
end;

function TJvThumbview.GetFile;
begin
  if selected <> -1 then
    result := TJvThumbNail(FThumbList.objects[selected]).Filename;
end;

procedure TJvThumbview.Setfile(Afile: string);
var
  i: longint;
  dir: string;
begin
  dir := extractfiledir(afile);
  if dir[length(dir)] = '\' then dir := copy(dir, 0, length(dir) - 1);
  directory := dir;
  for i := 0 to FThumbList.count - 1 do
    if TJvThumbNail(FThumbList.objects[I]).filename = afile then
    begin
      selected := i;
      if not FScroll then scrollto(i);
      exit;
    end;
end;

procedure TJvThumbview.Setdirectory(Value: string);
var
  Counter1,FStartTime: longint;
  Cancel: boolean;
  ReadFileList: TStringList;
  OldCursor: TCursor;
//  Pic: TPicture;
begin
  FSelected := -1;
  //  If Not FPainted then begin
  //    postMessage(Self.Handle,WM_LoadWhenReady,0,0);
  //    Exit;
  //  end;
  DiskSize := 0;
  if FFilling then Exit;
  if Value <> '' then
  begin
    ReadFileList := TStringList.Create;
    OldCursor := Cursor;
    try
      FFilling := True;
    //    if assigned(ReadFileList) then FreeAndNil(ReadFileList);
      FStartTime := GetTickCount;
      GetFiles(Value);
      if FSorted then
        ReadFileList.Assign(FFileListSorted)
      else
        ReadFileList.Assign(FFileList);
      EmptyList;
      FDirectory := Value;
      if ReadFileList.Count > 0 then
      begin
        if Assigned(FOnStartScanning) then // raise the event if it has been requested
          FOnStartScanning(Self, ReadFileList.Count - 1);
        Cancel := false;
        for Counter1 := 0 to ReadFileList.Count - 1 do
        begin
          if Assigned(FProgressNotify) then
            FProgressNotify(Self, Counter1 + 1, Cancel);
          if Cancel then Break;
          AddThumb(ExtractFilename(ReadFileList.Strings[Counter1]), true);
          TJvThumbNail(FThumbList.objects[Counter1]).filename := ReadFileList.Strings[Counter1];
          Inc(DiskSize, TJvThumbNail(FThumbList.objects[Counter1]).FileSize);
          if (Cursor <> crHourGlass) and (GetTickCount - FStartTime > 1000) then
            Cursor := crHourGlass;
        end;
        if Assigned(FOnStopScanning) then
          FOnStopScanning(Self);
      end;
    finally
      FreeandNil(ReadFileList);
      FFilling := False;
      Cursor := OldCursor;
    end
  end
  else
  begin
    Emptylist;
  end;
  FDirectory := Value;
  if (FThumbList.Count > 0) and (Selected < 0) then SetSelected(0);
  Invalidate;
end;

procedure TJvThumbview.Reposition;
var
  I: word;
  Tmp1: longint;
  tmp2: longint;
begin
  Tmp2 := HorzScrollBar.Position;
  HorzScrollBar.position := 0;
  Tmp1 := VertScrollBar.position;
  VertScrollBar.position := 0;
  if (FThumbList.Count > 0) and (start < FThumbList.count) then
    for I := start to FThumbList.Count - 1 do
    begin
      if TJvThumbNail(FThumbList.objects[I]) <> nil then
      begin
        TJvThumbNail(FThumbList.objects[I]).left := Calculatexpos(I + 1);
        TJvThumbNail(FThumbList.objects[I]).top := Calculateypos(I + 1);
        TJvThumbNail(FThumbList.objects[I]).width := FThumbSize.x;
        TJvThumbNail(FThumbList.objects[I]).height := FThumbSize.y;
      end;
    end;
  HorzScrollBar.Position := tmp2;
  VertScrollBar.position := Tmp1;
end;

procedure TJvThumbview.CalculateMaxX;
var
  a: longint;
begin
  case FScrollMode of
    SMVertical: a := (width - 20) div (FThumbSize.x + FSpace);
    SmHorizontal: a := (height - 20) div (FThumbSize.y + FSpace);
    Smboth: a := jkceil(sqrt(FThumbList.count));
  else A := 1;
  end;
  if A < 1 then a := 1;
  if a <> FMaxX then
  begin
    FMaxX := a;
  end
end;

procedure TJvThumbview.Calculatesize;
begin
  FThumbSize.x := trunc((Maxwidth / 100) * size);
  FThumbSize.y := trunc((Maxheight / 100) * size);
  CalculateMaxX;
end;

function TJvThumbview.CalculateXPos(num: word): longint;
var
  vpos, hpos: longint;
  temp: longint;
  Tmp: longint;
  Spact: longint;
begin
  if num > 0 then
  begin
    spact := FSpace;
    case FScrollMode of
      SmVertical, SMBoth:
        begin
          if (Pviewtype = VTFitToScreen) and (FScrollMode = SmVertical) then
          begin
            spact := ((width - 20) - (FThumbSize.x * FMaxX)) div (FMaxX + 1);
          end;
          vpos := jkceil(num / FMaxX);
          hpos := (num - (vpos * FMaxX)) + FMaxX;
          temp := (FThumbSize.x * (hpos - 1)) + (Hpos * SpAct);
          if (Pviewtype = VTCenter) and (FScrollMode = SMVertical) then
          begin
            tmp := ((width - 20) div 2) - (((FThumbSize.x + FSpace) * FMaxX) div 2);
            Temp := temp + tmp;
          end;
        end;
      smhorizontal:
        begin
          vpos := jkceil(num / FMaxX);
          temp := (FThumbSize.y * (vpos - 1)) + (vpos * SpAct);
        end
    else temp := 0
    end;
  end
  else temp := 0;
  result := temp;
end;

function TJvThumbview.Calculateypos(num: word): longint;
var
  vpos, hpos: longint;
  temp: longint;
  tmp: longint;
  spact: Longint;
begin
  if num > 0 then
  begin
    spact := FSpace;
    case FScrollMode of
      smVertical, SmBoth:
        begin
          vpos := jkceil(num / FMaxX);
          temp := (FThumbSize.y * (vpos - 1)) + (vpos * SpAct);
        end;
      SMHorizontal:
        begin
          if Pviewtype = VTFitToScreen then
          begin
            spact := ((height - 20) - ((FThumbSize.y + FSpace) * FMaxX)) div (FMaxX + 1);
          end;
          hpos := jkceil(num / FMaxX);
          vpos := (num - (hpos * FMaxX)) + FMaxX;
          temp := (FThumbSize.x * (vpos - 1)) + (vpos * SpAct);
          if Pviewtype = VTCenter then
          begin
            tmp := ((height - 20) div 2) - ((FThumbSize.y * FMaxX) div 2);
            Temp := temp + tmp;
          end;
        end;
    else temp := 0;
    end;
  end
  else temp := 0;
  result := temp;
end;

procedure TJvThumbview.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  no: word;
  tempx, tempy: longint;
begin
  // Check to see if there are any problems removing the following
  // For sure it solves a focus problem I'm having in an application
 //  setfocus;
  if Count > 0 then
    case scrollmode of
      smVertical, SMBoth:
        begin
          tempx := jkceil((x + horzscrollbar.Position) / (FThumbSize.x + FSpace));
          tempy := jkceil((y + vertscrollbar.Position) / (FThumbSize.y + FSpace));
          if tempx > FMaxX then tempx := FMaxX;
          if tempy < 1 then tempy := 1;
          no := ((tempy - 1) * FMaxX + tempx) - 1;
          if no < count then
            if TJvThumbNail(FThumbList.objects[No]) <> nil then
              if (x > TJvThumbNail(FThumbList.objects[No]).left) and
                (x < TJvThumbNail(FThumbList.objects[No]).left +
                TJvThumbNail(FThumbList.objects[No]).width) and
                (y > TJvThumbNail(FThumbList.objects[No]).top) and
                (y < TJvThumbNail(FThumbList.objects[No]).top +
                TJvThumbNail(FThumbList.objects[No]).height)
                then setselected(no)
              else
                setselected(-1)
            else
              setselected(-1)
          else
            setselected(-1);
        end;
      smHorizontal:
        begin
          tempx := JKCEIL((x + horzscrollbar.Position) / (FThumbSize.x + FSpace));
          tempy := JKCEIL((y + vertscrollbar.Position) / (FThumbSize.y + FSpace));
          if tempy > FMaxX then tempy := FMaxX;
          if tempx < 1 then tempx := 1;
          no := ((tempx - 1) * FMaxX + tempy) - 1;
          if no < count then
            if TJvThumbNail(FThumbList.objects[No]) <> nil then
              if (x > TJvThumbNail(FThumbList.objects[No]).left) and
                (x < TJvThumbNail(FThumbList.objects[No]).left +
                TJvThumbNail(FThumbList.objects[No]).width)
                and (y > TJvThumbNail(FThumbList.objects[No]).top)
                and (y < TJvThumbNail(FThumbList.objects[No]).top +
                TJvThumbNail(FThumbList.objects[No]).height)
                then setselected(no)
              else
                setselected(-1)
            else
              setselected(-1)
          else
            setselected(-1);
        end;
    else setselected(-1);
    end;
  inherited;
end; {}

constructor TJvThumbview.create(Aowner: tcomponent);
begin
  inherited create(Aowner);
  TabStop := true;
  FPainted := False;
  width := 600;
  height := 480;
  FMaxsize.x := 200;
  FMaxsize.y := 200;
  FPercent := 100;
  FSpace := 4;
  vertscrollbar.tracking := true;
  Horzscrollbar.tracking := true;
  FScrollMode := smhorizontal;
  caption := '';
  Calculatesize;
  FWaitUntilFull := false;
  FFilling := False;
  FSorted := True;
  FMinMemory := true;
  FSelected := -1;
  AutoScrolling := true;
  DiskSize := 0;
  Pautohandle := true;
  FFilter := CreateFilter;
  FThumbList := TJvThumbList.Create;
  FThumbList.Sorted := Sorted;
  FFileList := TStringList.Create;
  FFileList.Clear;
  FFileListSorted := TStringList.Create;
  FFileListSorted.Clear;
  FThumbColor := clNone;
end;

procedure TJvThumbview.addfromstream(Astream: Tstream; gr_type: Tgrf_type);
var
  THB: TJvThumbNail;
begin
  THb := TJvThumbNail.create(self);
  THb.streamfiletype := gr_type;
  THb.left := Calculatexpos(count + 1);
  THb.top := Calculateypos(count + 1);
  THb.width := FThumbSize.x;
  THb.height := FThumbSize.y;
  THb.onclick := onclick;
  THb.photo.onclick := onclick;
  THb.ondblclick := ondblclick;
  THb.photo.ondblclick := ondblclick;
  //  THb.Buffer := Vbuffer;
  thb.Photo.LoadFromStream(astream, thb.StreamFileType);
  FThumbList.AddObject(thb.title, thb);
  insertcontrol(THb);
  Calculatesize;
end;

procedure TJvThumbview.addfromfile(Afile: string);
var
  ThumbnailTitle: string;
  FFont: TFont;
  FColor: TColor;
  thb: TJvThumbNail;
begin
  THb := TJvThumbNail.create(self);
  if Assigned(FOnGetTitle) then
  begin
    ThumbnailTitle := ExtractFilename(Afile);
    FFont := TFont.Create;
    FColor := clBtnFace;
    if assigned(FOnGetTitle) then
      FOnGetTitle(Self, afile, ThumbnailTitle, FFont, FColor);
    thb.SetTitlePanel(ThumbnailTitle, FFont, FColor);
    FreeAndNil(FFont);
  end;
  THb.onclick := onclick;
  THb.photo.onclick := onclick;
  THb.ondblclick := ondblclick;
  THb.photo.ondblclick := ondblclick;
  THb.minimizememory := MinMemory;
  //  thb.Buffer := VBuffer;
  FThumbList.AddObject(Afile, thb);
  insertcontrol(thb);
  Calculatesize;
  Reposition(0);
  TJvThumbNail(FThumbList.objects[FThumbList.IndexOf(Afile)]).filename := afile;
end;

procedure TJvThumbview.Delete(no: longint);
var
  dummy: longint;
begin
  if no >= FThumbList.Count then
  begin
  end //Raise an exception
  else
  begin
    dummy := FFileList.IndexOf(selectedFile);
    if dummy >= 0 then FFileList.delete(dummy);
    dummy := FFileListSorted.IndexOf(Selectedfile);
    if dummy >= 0 then FFileListSorted.Delete(dummy);
    TJvThumbNail(FThumbList.objects[no]).Free;
    FThumbList.Delete(no);
    FSelected := -1;
    //Calculatesize;
    Dec(no, 1);
    if no < 0 then no := 0;
    Reposition(no);
    refresh;
    Repaint;
  end
end;

procedure TJvThumbview.Setspace(SP: Byte);
begin
  case Pviewtype of
    VTNormal, VTCenter:
      begin
        FSpace := sp;
        calculateMaxx;
        reposition(0);
      end;
    VTFitToScreen:
      begin
        FSpace := SP;
      end;
  end;
end;

function TJvThumbview.GetCount: word;
begin
  result := FThumbList.count;
end;

procedure TJvThumbview.SortList;
begin
  // add code to resort the list
  FThumbList.Sort;
  CalculateSize;
  Reposition(0);
end;

procedure TJvThumbview.Refresh;
var
  I: Longint;
begin
  Calculatesize;
  Reposition(0);
  for I := 0 to FThumbList.Count - 1 do
    FThumbList.Thumbnail[i].refresh;
  inherited;
end;

procedure TJvThumbview.emptylist;
var
  metr: integer;
begin
  if count > 0 then
    for metr := 0 to count - 1 do
      if assigned(FThumbList.objects[0]) then
      begin
        TJvThumbNail(FThumbList.objects[0]).parent := nil;
        TJvThumbNail(FThumbList.objects[0]).Free;
        FThumbList.delete(0);
      end;
end;

procedure TJvThumbview.SetThumbwidth(w: longint);
begin
  FMaxsize.x := w;
  Calculatesize;
  Reposition(0);
end;

procedure TJvThumbview.SetThumbheight(H: longint);
begin
  // if FMaxsize.y<h then
  FMaxsize.y := h;
  Calculatesize;
  Reposition(0);
end;

procedure TJvThumbview.resize;
begin
  CalculateMaxX;
  Reposition(0);
  inherited;
end;

procedure TJvThumbview.SetPercent(P: TPercent);
begin
  FPercent := p;
  Calculatesize;
  Reposition(0);
end;

destructor TJvThumbview.destroy;
begin
  if Assigned(FFileListSorted) then FreeAndNil(FFileListSorted);
  if Assigned(FFileList) then FreeAndNil(FFileList);
  if Assigned(FThumbList) then FreeAndNil(FThumbList);
  //If Assigned(FFilter) then FreeAndNil(FFilter);
  inherited destroy;
end;

procedure TJvThumbview.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if not FPainted then
  begin
    FPainted := true;
    SetDirectory(FDirectory);
  end;
end;

procedure TJvThumbview.SetScrollMode(AMode: TScrollMode);
begin
  if FScrollMode <> Amode then
  begin
    FScrollMode := Amode;
    Calculatesize;
    Reposition(0);
    if selected > -1 then
      scrollto(selected);
  end
end;

//{$IFDEF DEBUG}
//procedure TJvThumbview.WndProc(var Message: TMessage);
//Var
//  Test : TStringList;
//begin
//    Test:= Tstringlist.create;
//    try
//      Test.LoadFromFile('c:\Log.Tx');
//      Test.Add(IntTostr(Message.Msg) +','+IntToStr(Message.LParam)+','+IntToStr(Message.WParam));
//      Test.SaveToFile('C:]Log.txt');
//    Finally
//      Test.free;
//    end;
//  Inherited
//end;
//{$ENDIF}

procedure TJvThumbview.Keyup(var Key: word; ShiftState: TshiftState);
begin
  if assigned(Onkeyup) then onkeyup(self, key, Shiftstate);
  inherited;
end;

procedure TJvThumbview.KeyDown(var Key: word; ShiftState: TshiftState);
begin
  if (autohandlekeyb) and (FThumbList.Count > 0) then
    case key of
      39:
        begin
          goright;
          scrollto(selected);
        end; // Right arrow pressed
      40:
        begin
          godown;
          scrollto(selected);
        end; // down arrow pressed
      37:
        begin
          goleft;
          scrollto(selected);
        end; // left arrow pressed
      38:
        begin
          goUp;
          scrollto(selected);
        end; // Up arrow pressed
      46:
        begin
          //delete(FSelected);
        end; // Delete key pressed;
      33:
        begin // pageup;
          showmessage('Page Up');
          //tscrollbox
        end;
      34:
        begin // PageDown
          showmessage('Page Down');
        end;
      35:
        begin // End Pressed;
          //showmessage('End');
          selected := Count - 1;
          ScrollTo(Selected);
        end;
      36:
        begin // Home Pressed
          Selected := 0;
          ScrollTo(Selected);
        end;
      {      else
              Showmessage(inttostr(key));{}
    end;
  inherited;
end;

procedure TJvThumbview.keypress(var key: char);
begin
  if assigned(Onkeypress) then onkeypress(self, key);
  inherited;
end;

procedure TJvThumbview.SetSorted(const Value: boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    if not FPainted then
      Exit;
    FThumbList.Sorted := FSorted;
    SetDirectory(FDirectory); // force reread
    Invalidate;
  end;
end;

procedure TJvThumbview.WmGetDlgCode(var Message: Twmgetdlgcode);
begin
  Message.Result := DLGC_WantArrows or DLGC_WANTALLKEYS;
end;

procedure TJvThumbview.GoRight;
var
  actual: Longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected + FMaxX;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth) then Actual := Selected + 1;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJvThumbview.GoLeft;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected - FMaxX;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth) then Actual := Selected - 1;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJvThumbview.GoDown;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected + 1;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth)
    then Actual := Selected + FMaxX;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJvThumbview.GoUp;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected - 1;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth)
    then Actual := Selected - FMaxX;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;


procedure TJvThumbview.SetAsButton(const NewVal: Boolean);
var
  Dum: Longint;
begin
  if (NewVal <> FAsButtons) then
  begin
    if (FThumbList.Count > 0) then
      for dum := 0 to FThumbList.Count - 1 do
        FThumbList.Thumbnail[dum].Asbutton := newVal;
    FAsButtons := NewVal;
  end;
end;

procedure TJvThumbview.SetTitlePos(const NewVal: TTitlePos);
var
  Dum: Longint;
begin
  if newVal <> FTitlePos then
  begin
    if FThumbList.Count > 0 then
      for Dum := 0 to FThumbList.count - 1 do
        FThumbList.Thumbnail[Dum].TitlePlacement := NewVal;
    FTitlePos := newVal;
  end;
end;

function TJvThumbview.CreateFilter: string;
//var
//  Res: string;
//  Pos: Longint;
begin
  Result := GraphicFilter(TGraphic);
end;

procedure TJvThumbview.SetFilters;
var
  CP1 {, CP2}: Integer; // CurrentPosition;
//  Md: Byte; // Mode
  Res: string;
//  Sub: string;
  Final: string;
begin
  if not Assigned(FGraphicExtensions) then FGraphicExtensions := TStringList.Create;
//  CP1 := 0;
//  CP2 := 0;
  Res := FFilter;
  Final := '';
  repeat
    CP1 := Pos('|', Res);
    if CP1 > 0 then
    begin
      system.Delete(Res, 1, CP1);
      CP1 := Pos('|', Res);
      if CP1 > 0 then
      begin
        Final := Final + ';' + copy(res, 1, CP1 - 1);
        system.delete(res, 1, CP1);
      end
      else
        Final := Final + ';' + Res;
    end
    else
      Final := Final + ';' + Res;
  until CP1 = 0;
  final := ReplaceAllstr(Final, ';', crlf, false);
  FGraphicExtensions.Text := Final;

  CP1 := 0;
  repeat
    if FGraphicExtensions[CP1] = '' then FGraphicExtensions.Delete(cp1) else inc(CP1);
  until CP1 = FGraphicExtensions.Count;
end;

function TJvThumbList.GetThumb(Apos: longint): TJvThumbNail;
begin
  Result := TJvThumbNail(objects[apos]);
end;

function TJvThumbView.GetMaxThumbHeight: longint;
begin
  Result := FMaxsize.Y;
end;

function TJvThumbView.GetMaxThumbWidth: longint;
begin
  Result := FMaxsize.X;
end;

end.

