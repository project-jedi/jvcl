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

unit JvThumbViews;

interface
uses
  Windows, Classes, Controls, Forms, dialogs, ExtCtrls, JvThumbnails, Filectrl,
  stdctrls, sysutils, messages, graphics, JvBaseThumbnail;


type
  TPercent = 0..100;
  TScrollMode = (SMHorizontal, SMVertical, SMboth);
  TViewType = (VTNormal, VTCenter, VTFitToScreen);
  TBufferAction = (BFCancel, BFCreate, BFOpen, BFInsert, BFReplace, BFDelete);
  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbNailTitle: string;
    var ThumbNailFont: TFont;
    var ThumbNailColor: TColor) of object;
  TProgressStartNotify = procedure(Sender: TObject; Max: integer) of object;

  TjvThumbList = class(TStringList) // declare a new type of Thumblist and try not to break the old code;
  private
  protected
    function GetThumb(Apos: longint): TJvThumbNail;
  public
    property THumbnail[index: longint]: TJvTHumbnail read GetThumb; default;
  published
  end;

  TJvThumbView = class(TJvBaseThumbView)
  private
    { Private declarations }
    Maxsize: TPoint;
    Thumbsize: TPoint;
//    Dum: string;
    Percent: tPercent;
    VDirectory: string;
    PScrollMode: TScrollMode;
    TScroll: boolean;
    TSelected: longint;
    PViewType: TViewType;
    Tspace: byte;
    MaxX: word;
    P_minmemory: boolean;
    V_OnGetTitle: TTitleNotify;
    V_OnChange: TNotifyEvent;
    V_OnStartScanning: TProgressStartNotify;
    V_OnStopScanning: TNotifyEvent;
    V_ProgressNotify: TProgressNotify;
    V_WaitUntilFull: boolean;
    V_Painted: boolean;
    V_FileList: TStringList;
    V_FileListSorted: TStringList;
    V_Sorted: boolean;
    V_Filling: boolean;
    V_Filter: string;
//    V_BufferFile: string;
    VThumbColor: TColor;
    VAsButtons: Boolean;
    VTitlePos: TTitlePos;
    P_OnKeyDown: Tkeyevent;
    P_OnKeyUp: TkeyEvent;
    P_OnKeyPress: TKeyPressEvent;
    PAutohandle: Boolean;
    Grf_Extensions: TstringList;
    VShowShadow: Boolean;
    VShadowColor: TColor;
    vThumbList: Tjvthumblist;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure GetFiles(V_Directory: string);
    procedure PSetV_Sorted(const Value: boolean);
    procedure CalculateMaxX;
    procedure Calculatesize;
    procedure wmgetdlgCode(var Message: Twmgetdlgcode); message WM_GetDlgCode;
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
    procedure SetDirectory(V_Directory: string);
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
    disksize: dword;
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
    property ThumbList: TjvThumbList read vThumbList write vThumbList;
  published
    { Published declarations }
    property SelectedFile: string read Getfile write Setfile;
    property AlignView: TViewtype read Pviewtype write setviewtype;
    property AutoScrolling: boolean read Tscroll write Tscroll;
    property ThumbGap: byte read Tspace write setspace;
    property AutoHandleKeyb: boolean read Pautohandle write Pautohandle;
    property MinMemory: Boolean read P_Minmemory write P_Minmemory;
    property Count: word read getcount default 0;
    property MaxWidth: longint read Maxsize.X write SetThumbWidth;
    property MaxHeight: longint read Maxsize.y write SetThumbheight;
    property Size: TPercent read Percent write SetPercent;
    property ScrollMode: TScrollMode read PScrollMode write SetScrollMode;
    property Directory: string read VDirectory write Setdirectory;
    property Sorted: boolean read V_Sorted write PSetV_Sorted;
    property Selected: longint read Tselected write setselected default -1;
    property OnStartScanning: TProgressStartNotify read V_OnStartScanning write V_OnStartScanning;
    property OnStopScanning: TNotifyEvent read V_OnStopScanning write V_OnStopScanning;
    property OnScanProgress: TProgressNotify read V_ProgressNotify write V_ProgressNotify;
    property OnGetTitle: TTitleNotify read V_OnGetTitle write V_OnGetTitle;
    property OnChange: TNotifyEvent read V_OnChange write V_OnChange;
    property Onkeyup: Tkeyevent read P_onkeyup write P_Onkeyup;
    property OnKeyDown: Tkeyevent read P_onkeydown write P_Onkeydown;
    property OnKeyPress: Tkeypressevent read P_onkeyPress write P_OnkeyPress;
    property AsButtons: Boolean read VAsButtons write SetAsButton;
    property TitlePlacement: TTitlePos read VTitlePos write SetTitlePos default T_UP;
    property Filter: string read V_Filter write V_Filter;
    //    Property BufferFile : String Read V_BufferFile write SetBufferFile;
    property ThumbColor: TColor read VThumbColor write VThumbColor;
    property ShowShadow: Boolean read VShowShadow write VShowShadow;
    property ShadowColor: TColor read VShadowColor write VShadowColor;
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
  GRF_Extensions  : array[1..9] of string = ('*.BMP','*.JPG','*.WMF','*.EMF',
                                             '*.ICO','*.GIF','*.PCX',
                                             '*.TGA','*.PNG'); {}
const
  CRLF = #13 + #10;

procedure TJVTHumbview.addThumb(atitle: string; Redraw: boolean);
var
  Thb: TJvThumbnail;
begin
  Thb := TJvThumbnail.create(self);
  Thb.left := Calculatexpos(count + 1);
  Thb.top := Calculateypos(count + 1);
  Thb.width := Thumbsize.x;
  Thb.height := Thumbsize.y;
  thb.Asbutton := VAsButtons;
  thb.TitlePlacement := VTitlePos;
  thb.ShadowColor := VShadowColor;
  thb.ShowShadow := VShowShadow;
  Thb.onclick := onclick;
  Thb.photo.onclick := onclick;
  Thb.ondblclick := ondblclick;
  Thb.photo.ondblclick := ondblclick;
  Thb.minimizememory := MinMemory;
  Thb.Color := self.Color;
  Thb.title := atitle;
  if VThumbColor = clNone then
  begin
    Thb.Color := Self.Color;
    Thb.ParentColor := true;
    Thb.TitleColor := Self.Color;
  end
  else
    Thb.Color := VThumbColor;
  vThumbList.AddObject(thb.Title, thb);
  thb.parent := self;
  if redraw then
  begin
    CalculateSize;
    Reposition(0);
  end;
end;

procedure TJVTHumbview.GetFiles(V_Directory: string);
var
  SearchRec: TSearchRec;
  V_Result: Integer;
  NumExtensions: integer;

  function FindFirstGraphic(GRF_Extension: string): integer;
  begin
    FindFirstGraphic :=
      FindFirst(V_Directory + GRF_Extension, faArchive, SearchRec);
  end;
begin
  V_FileList.Clear;
  V_FileListSorted.Clear;
  setFilters;
  if not DirectoryExists(V_Directory) then
    exit;
  if V_Directory[Length(V_Directory)] <> '\' then
    V_Directory := V_Directory + '\';
  for NumExtensions := 0 to GRF_Extensions.Count - 1 do
  begin
    if FindFirstGraphic(GRF_Extensions[NumExtensions]) = 0 then
    begin
      V_FileList.Add(V_Directory + SearchRec.Name);
      V_FileListSorted.Add(V_Directory + SearchRec.Name);
      repeat
        V_Result := FindNext(SearchRec);
        if V_Result = 0 then
        begin
          V_FileList.Add(V_Directory + SearchRec.Name);
          V_FileListSorted.Add(V_Directory + SearchRec.Name);
        end;
      until V_Result <> 0;
      FindClose(SearchRec);
    end;
  end;
  V_FileListSorted.Sort;
  if Assigned(Grf_Extensions) then FreeAndNil(Grf_Extensions);
end;

procedure TJVTHumbview.Setviewtype(Vtype: Tviewtype);
begin
  if Vtype <> Pviewtype then
  begin
    Pviewtype := Vtype;
    reposition(0);
  end;
end;

procedure TJVTHumbview.ScrollTo(const number: longint);
begin
  // if AutoScrolling then if (number>-1) then
  if (number < 0) or (number > vthumblist.count - 1) then exit;
  case scrollmode of
    smvertical:
      begin
        if TJvTHumbnail(vthumblist.objects[number]).top < 0 then
          vertscrollbar.position := vertscrollbar.position +
            (TJVThumbNail(vthumblist.objects[number]).top -
            (TJVThumbNail(vthumblist.objects[number]).width div 2));
        if TJVThumbNail(vthumblist.objects[number]).top +
          TJVThumbNail(vthumblist.objects[number]).height > height then
          vertscrollbar.position := vertscrollbar.position +
            (TJVThumbNail(vthumblist.objects[number]).top -
            (height - TJVThumbNail(vThumbList.objects[number]).height -
            (TJVThumbNail(vthumblist.objects[number]).height div 2)));
      end;
    smhorizontal:
      begin
        if TJVThumbNail(vthumblist.objects[number]).left < 0 then
          horzscrollbar.position := Horzscrollbar.position +
            (TJVThumbNail(vthumblist.objects[number]).left -
            (TJVThumbNail(vthumblist.objects[number]).width div 2));
        if TJVThumbNail(vthumblist.objects[number]).left +
          TJVThumbNail(vThumbList.objects[number]).width > width then
          Horzscrollbar.position := Horzscrollbar.position +
            (TJVThumbNail(vThumbList.objects[number]).left -
            (width - TJVThumbNail(vThumbList.objects[number]).width -
            (TJVThumbNail(vThumbList.objects[number]).width div 2)));
      end;
    smBoth:
      begin
        if TJVThumbNail(vThumbList.objects[number]).top < 0 then
          vertscrollbar.position := vertscrollbar.position +
            (TJVThumbNail(vThumbList.objects[number]).top -
            (TJVThumbNail(vThumbList.objects[number]).width div 2));
        if TJVThumbNail(vThumbList.objects[number]).top +
          TJVThumbNail(vThumbList.objects[number]).height > height then
          vertscrollbar.position := vertscrollbar.position +
            (TJVThumbNail(vThumbList.objects[number]).top -
            (TJVThumbNail(vThumbList.objects[number]).height -
            (TJVThumbNail(vThumbList.objects[number]).height div 2)));
        if TJVThumbNail(vThumbList.objects[number]).left < 0 then
          horzscrollbar.position := Horzscrollbar.position +
            (TJVThumbNail(vThumbList.objects[number]).left -
            (TJVThumbNail(vThumbList.objects[number]).width div 2));
        if TJVThumbNail(vThumbList.objects[number]).left +
          TJVThumbNail(vThumbList.objects[number]).width > width then
          Horzscrollbar.position := Horzscrollbar.position +
            (TJVThumbNail(vThumbList.objects[number]).left -
            (width - TJVThumbNail(vThumbList.objects[number]).width -
            (TJVThumbNail(vThumbList.objects[number]).width div 2)));
      end;
  end;
end;

function TJVTHumbview.GetBufferName(AName: string): string;
var
  tst: string;
  FN: string;
  Res: string;
begin
  tst := completepath(extractFiledir(AName));
  if tst = AName then
  begin // no filename included only a directory;
    // the user wants us to create a separate file for each
    // directory it opens in a pre-specified path
    FN := ReplaceChar(VDirectory, '\', '_', 0, False); //create the filename from the path
    FN := ReplaceChar(FN, ':', '_', 0, False); //create the filename from the path
    res := AName + fn;
  end
  else
  begin // the user has specified either a full path and a name or just a name
    if tst = '' then
      // the user has specified only a name to use
      // in each directory that is opened by the component there will be created
      // a file with name <ANAME> where the thumbs are been saved;
      Res := CompletePath(VDirectory) + AName
    else
      // the user has specified a full path and a file name weach is the same
      // for all the directories he/she opens.
      res := AName;
  end;
  result := res;
end;

//Procedure TJVTHumbview.SetBufferFile(NewName:String);
//var
//  tst : string;
//begin
//  If NewName <> V_BufferFile then
//    tst := GetBufferName(NewName);
//  End;
//end;

//---------TJVTHumbview OBJECT PROCS AND FUNCTIONS------------------

procedure TJVTHumbview.SetSelected(Number: longint);
begin
  if vThumbList.count > 0 then
  begin
    if (Tselected <> -1) then
    begin
      TJVThumbNail(vThumbList.objects[Tselected]).titlecolor :=
        TJVThumbNail(vThumbList.objects[Tselected]).Color;
      TJVThumbNail(vThumbList.objects[Tselected]).titlefont.color :=
        TJVThumbNail(vThumbList.objects[Tselected]).Font.Color;
    end;
    if number <> -1 then
    begin
      TJVThumbNail(vThumbList.objects[Number]).titlecolor := clhighlight;
      TJVThumbNail(vThumbList.objects[Number]).titlefont.color := clhighlighttext;
      if autoscrolling then
      begin
        if (TJVThumbNail(vThumbList.objects[Number]).top +
          TJVThumbNail(vThumbList.objects[Number]).height > height) or
          (TJVThumbNail(vThumbList.objects[Number]).top < 0) then
          Scrollto(number);
        if (TJVThumbNail(vThumbList.objects[Number]).left +
          TJVThumbNail(vThumbList.objects[Number]).width > width) or
          (TJVThumbNail(vThumbList.objects[Number]).left < 0) then
          scrollto(number);
      end
    end;
    if (tselected <> number) and (assigned(V_Onchange)) then V_onchange(self);
    TSelected := number;
  end;
end;

function TJVTHumbview.GetFile;
begin
  if selected <> -1 then
    result := TJVThumbNail(vThumbList.objects[selected]).Filename;
end;

procedure TJVTHumbview.Setfile(Afile: string);
var
  i: longint;
  dir: string;
begin
  dir := extractfiledir(afile);
  if dir[length(dir)] = '\' then dir := copy(dir, 0, length(dir) - 1);
  directory := dir;
  for i := 0 to vThumbList.count - 1 do
    if TJVThumbNail(vThumbList.objects[I]).filename = afile then
    begin
      selected := i;
      if not tscroll then scrollto(i);
      exit;
    end;
end;

procedure TJVTHumbview.Setdirectory(V_Directory: string);
var
  Counter1: longint;
  Break: boolean;
  ReadFileList: TStringList;
//  Pic: TPicture;
begin
  Tselected := -1;
  //  If Not v_Painted then begin
  //    postMessage(Self.Handle,WM_LoadWhenReady,0,0);
  //    Exit;
  //  end;
  if V_Filling then exit;
  if V_Directory <> '' then
  try
    V_Filling := True;
    //    if assigned(ReadFileList) then FreeAndNil(ReadFileList);
    ReadFileList := TStringList.Create;
    GetFiles(V_Directory);
    if V_Sorted then
      ReadFileList.Assign(V_FileListSorted)
    else
      ReadFileList.Assign(V_FileList);
    EmptyList;
    VDirectory := V_Directory;
    if ReadFileList.Count > 0 then
    begin
      if Assigned(V_OnStartScanning) then // raise the event if it has been requested
        V_OnStartScanning(Self, ReadFileList.Count - 1);
      Counter1 := 0;
      Break := False; // dont stop the loop
      repeat
        try
          if counter1 < readfilelist.count then
            addTHumb(extractfilename(ReadFileList.Strings[Counter1]), false)
          else addTHumb(extractfilename(ReadFileList.Strings[Counter1]), True);
        except
          raise;
        end;
        inc(Counter1);
        application.processmessages;
      until (Counter1 = ReadFileList.Count) or (Break);
      Counter1 := 0;
      if not break then repeat
          if Assigned(V_ProgressNotify) then
            V_ProgressNotify(Self, Counter1 + 1, Break);
          TJVThumbNail(vThumbList.objects[Counter1]).filename :=
            ReadFileList.Strings[Counter1];
          disksize := disksize + TJVThumbNail(vThumbList.objects[vThumbList.Count - 1]).filesize;
          inc(Counter1);
          Application.processMessages;
        until (Counter1 = ReadFileList.Count) or (Break); {}
      if Assigned(V_OnStopScanning) then
        V_OnStopScanning(Self);
    end;
  finally
    FreeandNil(ReadFileList);
    V_Filling := False;
  end
  else
  begin
    Emptylist;
  end;
  VDirectory := V_Directory;
  if (vThumbList.count > 0) and (selected < 0) then setselected(0);
end;

procedure TJVTHumbview.Reposition;
var
  I: word;
  Tmp1: longint;
  tmp2: longint;
begin
  Tmp2 := HorzScrollBar.Position;
  HorzScrollBar.position := 0;
  Tmp1 := VertScrollBar.position;
  VertScrollBar.position := 0;
  if (vThumbList.Count > 0) and (start < vThumbList.count) then
    for I := start to vThumbList.Count - 1 do
    begin
      if TJVThumbNail(vThumbList.objects[I]) <> nil then
      begin
        TJVThumbNail(vThumbList.objects[I]).left := Calculatexpos(I + 1);
        TJVThumbNail(vThumbList.objects[I]).top := Calculateypos(I + 1);
        TJVThumbNail(vThumbList.objects[I]).width := Thumbsize.x;
        TJVThumbNail(vThumbList.objects[I]).height := Thumbsize.y;
      end;
    end;
  HorzScrollBar.Position := tmp2;
  VertScrollBar.position := Tmp1;
end;

procedure TJVTHumbview.CalculateMaxX;
var
  a: longint;
begin
  case PscrollMode of
    SMVertical: a := (width - 20) div (Thumbsize.x + TSpace);
    SmHorizontal: a := (height - 20) div (thumbsize.y + TSpace);
    Smboth: a := jkceil(sqrt(vThumbList.count));
  else A := 1;
  end;
  if A < 1 then a := 1;
  if a <> MaxX then
  begin
    Maxx := a;
  end
end;

procedure TJVTHumbview.Calculatesize;
begin
  Thumbsize.x := trunc((Maxwidth / 100) * size);
  Thumbsize.y := trunc((Maxheight / 100) * size);
  CalculateMaxX;
end;

function TJVTHumbview.CalculateXPos(num: word): longint;
var
  vpos, hpos: longint;
  temp: longint;
  Tmp: longint;
  Spact: longint;
begin
  if num > 0 then
  begin
    spact := Tspace;
    case PscrollMode of
      SmVertical, SMBoth:
        begin
          if (Pviewtype = VTFitToScreen) and (Pscrollmode = SmVertical) then
          begin
            spact := ((width - 20) - (thumbsize.x * maxx)) div (maxx + 1);
          end;
          vpos := jkceil(num / Maxx);
          hpos := (num - (vpos * Maxx)) + Maxx;
          temp := (Thumbsize.x * (hpos - 1)) + (Hpos * SpAct);
          if (Pviewtype = VTCenter) and (Pscrollmode = SMVertical) then
          begin
            tmp := ((width - 20) div 2) - (((thumbsize.x + Tspace) * maxx) div 2);
            Temp := temp + tmp;
          end;
        end;
      smhorizontal:
        begin
          vpos := jkceil(num / Maxx);
          temp := (Thumbsize.y * (vpos - 1)) + (vpos * SpAct);
        end
    else temp := 0
    end;
  end
  else temp := 0;
  result := temp;
end;

function TJVTHumbview.Calculateypos(num: word): longint;
var
  vpos, hpos: longint;
  temp: longint;
  tmp: longint;
  spact: Longint;
begin
  if num > 0 then
  begin
    spact := Tspace;
    case Pscrollmode of
      smVertical, SmBoth:
        begin
          vpos := jkceil(num / Maxx);
          temp := (Thumbsize.y * (vpos - 1)) + (vpos * SpAct);
        end;
      SMHorizontal:
        begin
          if Pviewtype = VTFitToScreen then
          begin
            spact := ((height - 20) - ((thumbsize.y + Tspace) * maxx)) div (maxx + 1);
          end;
          hpos := jkceil(num / Maxx);
          vpos := (num - (hpos * Maxx)) + Maxx;
          temp := (Thumbsize.x * (vpos - 1)) + (vpos * SpAct);
          if Pviewtype = VTCenter then
          begin
            tmp := ((height - 20) div 2) - ((thumbsize.y * Maxx) div 2);
            Temp := temp + tmp;
          end;
        end;
    else temp := 0;
    end;
  end
  else temp := 0;
  result := temp;
end;

procedure TJVTHumbview.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
          tempx := jkceil((x + horzscrollbar.Position) / (thumbsize.x + TSpace));
          tempy := jkceil((y + vertscrollbar.Position) / (thumbsize.y + TSpace));
          if tempx > maxx then tempx := maxx;
          if tempy < 1 then tempy := 1;
          no := ((tempy - 1) * maxx + tempx) - 1;
          if no < count then
            if TJVThumbNail(vThumbList.objects[No]) <> nil then
              if (x > TJVThumbNail(vThumbList.objects[No]).left) and
                (x < TJVThumbNail(vThumbList.objects[No]).left +
                TJVThumbNail(vThumbList.objects[No]).width) and
                (y > TJVThumbNail(vThumbList.objects[No]).top) and
                (y < TJVThumbNail(vThumbList.objects[No]).top +
                TJVThumbNail(vThumbList.objects[No]).height)
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
          tempx := JKCEIL((x + horzscrollbar.Position) / (thumbsize.x + TSpace));
          tempy := JKCEIL((y + vertscrollbar.Position) / (thumbsize.y + TSpace));
          if tempy > maxx then tempy := maxx;
          if tempx < 1 then tempx := 1;
          no := ((tempx - 1) * maxx + tempy) - 1;
          if no < count then
            if TJVThumbNail(vThumbList.objects[No]) <> nil then
              if (x > TJVThumbNail(vThumbList.objects[No]).left) and
                (x < TJVThumbNail(vThumbList.objects[No]).left +
                TJVThumbNail(vThumbList.objects[No]).width)
                and (y > TJVThumbNail(vThumbList.objects[No]).top)
                and (y < TJVThumbNail(vThumbList.objects[No]).top +
                TJVThumbNail(vThumbList.objects[No]).height)
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

constructor TJVTHumbview.create(Aowner: tcomponent);
begin
  inherited create(Aowner);
  TabStop := true;
  V_Painted := False;
  width := 600;
  height := 480;
  Maxsize.x := 200;
  Maxsize.y := 200;
  Percent := 100;
  Tspace := 4;
  vertscrollbar.tracking := true;
  Horzscrollbar.tracking := true;
  PscrollMode := smhorizontal;
  caption := '';
  Calculatesize;
  V_WaitUntilFull := false;
  V_Filling := False;
  V_Sorted := True;
  P_minmemory := true;
  Tselected := -1;
  AutoScrolling := true;
  disksize := 0;
  Pautohandle := true;
  V_Filter := CreateFilter;
  vThumbList := TjvThumblist.Create;
  vThumbList.Sorted := Sorted;
  V_FileList := TStringList.Create;
  V_FileList.Clear;
  V_FileListSorted := TStringList.Create;
  V_FileListSorted.Clear;
  VThumbColor := clNone;
end;

procedure TJVTHumbview.addfromstream(Astream: Tstream; gr_type: Tgrf_type);
var
  THB: TJVThumbNail;
begin
  THb := TJVThumbNail.create(self);
  THb.streamfiletype := gr_type;
  THb.left := Calculatexpos(count + 1);
  THb.top := Calculateypos(count + 1);
  THb.width := Thumbsize.x;
  THb.height := Thumbsize.y;
  THb.onclick := onclick;
  THb.photo.onclick := onclick;
  THb.ondblclick := ondblclick;
  THb.photo.ondblclick := ondblclick;
  //  THb.Buffer := Vbuffer;
  thb.Photo.LoadFromStream(astream, thb.StreamFileType);
  vThumbList.AddObject(thb.title, thb);
  insertcontrol(THb);
  Calculatesize;
end;

procedure TJVTHumbview.addfromfile(Afile: string);
var
  ThumbNailTitle: string;
  V_Font: TFont;
  V_Color: TColor;
  thb: TJVThumbNail;
begin
  THb := TJVThumbNail.create(self);
  if Assigned(V_OnGetTitle) then
  begin
    ThumbNailTitle := ExtractFilename(Afile);
    V_Font := TFont.Create;
    V_Color := clBtnFace;
    if assigned(V_OnGetTitle) then
      V_OnGetTitle(Self, afile, ThumbNailTitle, V_Font, V_Color);
    thb.SetTitlePanel(ThumbNailTitle, V_Font, V_Color);
    FreeAndNil(V_Font);
  end;
  THb.onclick := onclick;
  THb.photo.onclick := onclick;
  THb.ondblclick := ondblclick;
  THb.photo.ondblclick := ondblclick;
  THb.minimizememory := MinMemory;
  //  thb.Buffer := VBuffer;
  vThumbList.AddObject(Afile, thb);
  insertcontrol(thb);
  Calculatesize;
  Reposition(0);
  TJVThumbNail(vThumbList.objects[vThumbList.IndexOf(Afile)]).filename := afile;
end;

procedure TJVTHumbview.Delete(no: longint);
var
  dummy: longint;
begin
  if no >= vThumbList.Count then
  begin
  end //Raise an exception
  else
  begin
    dummy := V_Filelist.IndexOf(selectedFile);
    if dummy >= 0 then V_fileList.delete(dummy);
    dummy := V_FilelistSorted.IndexOf(Selectedfile);
    if dummy >= 0 then V_fileListSorted.Delete(dummy);
    TJVThumbNail(vThumbList.objects[no]).Free;
    vThumbList.Delete(no);
    Tselected := -1;
    //Calculatesize;
    Dec(no, 1);
    if no < 0 then no := 0;
    Reposition(no);
    refresh;
    Repaint;
  end
end;

procedure TJVTHumbview.Setspace(SP: Byte);
begin
  case Pviewtype of
    VTNormal, VTCenter:
      begin
        Tspace := sp;
        calculateMaxx;
        reposition(0);
      end;
    VTFitToScreen:
      begin
        Tspace := SP;
      end;
  end;
end;

function TJVTHumbview.GetCount: word;
begin
  result := vThumbList.count;
end;

procedure TJVTHumbview.SortList;
begin
  // add code to resort the list
  vThumbList.Sort;
  CalculateSize;
  Reposition(0);
end;

procedure TJVTHumbview.Refresh;
var
  I: Longint;
begin
  Calculatesize;
  Reposition(0);
  for I := 0 to vThumbList.Count - 1 do
    vThumbList.THumbnail[i].refresh;
  inherited;
end;

procedure TJVTHumbview.emptylist;
var
  metr: integer;
begin
  if count > 0 then
    for metr := 0 to count - 1 do
      if assigned(vThumbList.objects[0]) then
      begin
        TJVThumbNail(vThumbList.objects[0]).parent := nil;
        TJVThumbNail(vThumbList.objects[0]).Free;
        vThumbList.delete(0);
      end;
end;

procedure TJVTHumbview.SetThumbwidth(w: longint);
begin
  Maxsize.x := w;
  Calculatesize;
  Reposition(0);
end;

procedure TJVTHumbview.SetThumbheight(H: longint);
begin
  // if maxsize.y<h then
  Maxsize.y := h;
  Calculatesize;
  Reposition(0);
end;

procedure TJVTHumbview.resize;
begin
  CalculateMaxX;
  Reposition(0);
  inherited;
end;

procedure TJVTHumbview.SetPercent(P: TPercent);
begin
  Percent := p;
  Calculatesize;
  Reposition(0);
end;

destructor TJVTHumbview.destroy;
begin
  if Assigned(V_FileListSorted) then FreeAndNil(V_FileListSorted);
  if Assigned(V_FileList) then FreeAndNil(V_FileList);
  if Assigned(vThumbList) then FreeAndNil(vThumbList);
  //If Assigned(V_Filter) then FreeAndNil(V_Filter);
  inherited destroy;
end;

procedure TJVTHumbview.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if not V_Painted then
  begin
    V_Painted := true;
    SetDirectory(VDirectory);
  end;
end;

procedure TJVTHumbview.SetScrollMode(AMode: TScrollMode);
begin
  if PscrollMode <> Amode then
  begin
    PScrollMode := Amode;
    Calculatesize;
    Reposition(0);
    if selected > -1 then
      scrollto(selected);
  end
end;

//{$IFDEF DEBUG}
//procedure TJVTHumbview.WndProc(var Message: TMessage);
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

procedure TJVTHumbview.Keyup(var Key: word; ShiftState: TshiftState);
begin
  if assigned(Onkeyup) then onkeyup(self, key, Shiftstate);
  inherited;
end;

procedure TJVTHumbview.KeyDown(var Key: word; ShiftState: TshiftState);
begin
  if (autohandlekeyb) and (vThumbList.Count > 0) then
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
          //delete(Tselected);
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

procedure TJVTHumbview.keypress(var key: char);
begin
  if assigned(Onkeypress) then onkeypress(self, key);
  inherited;
end;

procedure TJVTHumbview.PSetV_Sorted(const Value: boolean);
var
  I: Integer;
  Current: integer;

  function indexofFile(Afilename: string): Integer;
  var
    I: Longint;
  begin
    for i := 0 to vThumbList.count - 1 do
      if TJVThumbNail(vThumbList.objects[i]).filename = AfileName then
        break;
    if (I <= vThumbList.count) and
      (TJVThumbNail(vThumbList.objects[i]).filename = AFilename) then
      result := i
    else
      result := -1;
  end;

var
  UnSorted: Tjvthumblist;
begin
  if not V_Painted then
    exit;
  if Value <> V_Sorted then
  begin
    V_Sorted := Value;
    vThumbList.Sorted := V_Sorted;
    if (VDirectory <> '') and (not Value) then
    begin
      unsorted := tjvthumblist.create;
      for i := 0 to v_FileList.count - 1 do
      begin
        current := indexoffile(V_filelist.strings[i]);
        if current > -1 then
          unsorted.AddObject(vThumbList.strings[current], vThumbList.objects[current]);
      end;
      FreeAndNil(vthumblist);
      FreeAndNil(Unsorted);
      CalculateSize;
      Reposition(0);
    end
    else
      SortList;
  end;
end;

procedure TJVTHumbview.wmgetdlgCode(var Message: Twmgetdlgcode);
begin
  message.result := DLGC_WantArrows or DLGC_WANTALLKEYS;
end;

procedure TJVTHumbview.GoRight;
var
  actual: Longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected + maxX;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth) then Actual := Selected + 1;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJVTHumbview.GoLeft;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected - maxX;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth) then Actual := Selected - 1;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJVTHumbview.GoDown;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected + 1;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth)
    then Actual := Selected + MaxX;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;

procedure TJVTHumbview.GoUp;
var
  Actual: longint;
begin
  actual := 0;
  if ScrollMode = SMHorizontal then Actual := selected - 1;
  if (scrollmode = SmVertical) or (scrollmode = SmBoth)
    then Actual := Selected - MaxX;
  if (actual > count - 1) or (actual < 0) then actual := selected;
  selected := actual;
end;


procedure TJVTHumbview.SetAsButton(const NewVal: Boolean);
var
  Dum: Longint;
begin
  if (NewVal <> VAsButtons) then
  begin
    if (vThumbList.Count > 0) then
      for dum := 0 to vThumbList.Count - 1 do
        vThumbList.THumbnail[dum].Asbutton := newVal;
    VAsButtons := NewVal;
  end;
end;

procedure TJVTHumbview.SetTitlePos(const NewVal: TTitlePos);
var
  Dum: Longint;
begin
  if newVal <> VtitlePos then
  begin
    if vThumbList.Count > 0 then
      for Dum := 0 to vThumbList.count - 1 do
        vThumbList.THumbnail[Dum].TitlePlacement := NewVal;
    VTitlePos := newVal;
  end;
end;

function TJVTHumbview.CreateFilter: string;
//var
//  Res: string;
//  Pos: Longint;
begin
  Result := GraphicFilter(TGraphic);
end;

procedure TJVTHumbview.SetFilters;
var
  CP1{, CP2}: Integer; // CurrentPosition;
//  Md: Byte; // Mode
  Res: string;
//  Sub: string;
  Final: string;
begin
  if not Assigned(Grf_Extensions) then Grf_Extensions := TStringList.Create;
//  CP1 := 0;
//  CP2 := 0;
  Res := V_Filter;
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
  Grf_Extensions.Text := Final;

  CP1 := 0;
  repeat
    if Grf_Extensions[CP1] = '' then Grf_Extensions.Delete(cp1) else inc(CP1);
  until CP1 = Grf_Extensions.Count;
end;

function TjvTHumblist.GetThumb(Apos: longint): TJVThumbNail;
begin
  Result := TJVThumbNail(objects[apos]);
end;

end.

