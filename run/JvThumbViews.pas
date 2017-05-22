{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain A Copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbView.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThumbViews;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Controls, Forms, ExtCtrls,
  SysUtils, Messages, Graphics,
  JvThumbnails, JvBaseThumbnail, JvExControls;

type
  // (rom) already in JvBaseThumbnail
  //TPercent = 0..100;
  TScrollMode = (smHorizontal, smVertical, smBoth);
  TViewType = (vtNormal, vtCenter, vtFitToScreen);
  // (rom) obviously unused
  //TBufferAction = (bfCancel, bfCreate, bfOpen, bfInsert, bfReplace, bfDelete);
  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbnailTitle: string; var ThumbnailFont: TFont;
    var ThumbnailColor: TColor) of object;
  TProgressStartNotify = procedure(Sender: TObject; Max: Integer) of object;

  TJvThumbList = class(TStringList) // declare A new type of Thumblist and try not to Break the old code;
  protected
    function GetThumbnail(Index: Longint): TJvThumbnail;
  public
    property Thumbnail[Index: Longint]: TJvThumbnail read GetThumbnail; default;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvThumbView = class(TJvBaseThumbView)
  private
    FMaxSize: TPoint;
    FThumbSize: TPoint;
//    Dummy: string;
    FPercent: TPercent;
    FDirectory: string;
    FScrollMode: TScrollMode;
    FAutoScrolling: Boolean;
    FSelected: Longint;
    FAlignView: TViewType;
    FThumbGap: Byte;
    FMaxX: Word;
    FMinMemory: Boolean;
    FOnGetTitle: TTitleNotify;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnStartScanning: TProgressStartNotify;
    FOnStopScanning: TNotifyEvent;
    FOnScanProgress: TProgressNotify;
    FWaitUntilFull: Boolean;
    FPainted: Boolean;
    FFileList: TStringList;
    FFileListSorted: TStringList;
    FSorted: Boolean;
    FFilling: Boolean;
    FFilter: string;
//    FBufferFile: string;
    FThumbColor: TColor;
    FAsButtons: Boolean;
    FTitlePlacement: TTitlePos;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FAutoHandleKeyb: Boolean;
    FGraphicExtensions: TStringList;
    FShowShadow: Boolean;
    FShadowColor: TColor;
    FThumbList: TJvThumbList;
    FOnInvalidImage: TInvalidImageEvent;
    FDiskSize: DWORD;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure GetFiles(ADirectory: string);
    procedure SetSorted(const Value: Boolean);
    procedure CalculateMaxX;
    procedure CalculateSize;
    function CalculateXPos(Num: Word): Longint;
    function CalculateYPos(Num: Word): Longint;
    procedure ScrollTo(const Number: Longint);
    procedure SetAlignView(AType: TViewType);
    procedure Reposition(Start: Integer);
    procedure GoLeft;
    procedure GoRight;
    procedure GoDown;
    procedure GoUp;
    procedure SetAsButton(const NewVal: Boolean);
    procedure SetTitlePos(const NewVal: TTitlePos);
    function CreateFilter: string;
    procedure SetFilters;
    //function GetBufferName(AName: string): string;
    function GetMaxHeight: Longint;
    function GetMaxWidth: Longint;
    procedure DoInvalidImage(Sender: TObject; const FileName: string);
    //    procedure WMLoadWhenReady(var Msg: TMessage); message WM_LOADWHENREADY;
  protected
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure SetScrollMode(AMode: TScrollMode);
    procedure SetSelected(Number: Longint);
    //    procedure SetBufferFile(NewName: string);
    procedure Resize; override;
    procedure SetMaxWidth(W: Longint);
    procedure SetDirectory(Value: string);
    procedure SetMaxHeight(H: Longint);
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetThumbGap(Sp: Byte);
    procedure SetPercent(P: TPercent);
    procedure SetSelectedFile(AFile: string);
    function GetSelectedFile: string;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //    function GetBufferFile: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddThumb(ATitle: string; Redraw: Boolean);
    function AddFromFile(AFile: string) : Integer;
    procedure AddFromStream(AStream: TStream; AType: TGRFKind); overload;
    function AddFromStream(AStream: TStream; AType: TGRFKind; const aTitle: string): Integer; overload;

    procedure Delete(No: Longint);
    procedure EmptyList;
    procedure SortList;
    procedure Refresh;
    function GetCount: Word;
    property ThumbList: TJvThumbList read FThumbList write FThumbList;
  published
    property SelectedFile: string read GetSelectedFile write SetSelectedFile;
    property AlignView: TViewType read FAlignView write SetAlignView;
    property AutoScrolling: Boolean read FAutoScrolling write FAutoScrolling;
    property ThumbGap: Byte read FThumbGap write SetThumbGap;
    property AutoHandleKeyb: Boolean read FAutoHandleKeyb write FAutoHandleKeyb;
    property MinMemory: Boolean read FMinMemory write FMinMemory;
    property Count: Word read GetCount default 0;
    property MaxWidth: Longint read GetMaxWidth write SetMaxWidth;
    property MaxHeight: Longint read GetMaxHeight write SetMaxHeight;
    property Size: TPercent read FPercent write SetPercent;
    property ScrollMode: TScrollMode read FScrollMode write SetScrollMode;
    property Directory: string read FDirectory write SetDirectory;
    property Sorted: Boolean read FSorted write SetSorted;
    property Selected: Longint read FSelected write SetSelected default -1;
    property OnStartScanning: TProgressStartNotify read FOnStartScanning write FOnStartScanning;
    property OnStopScanning: TNotifyEvent read FOnStopScanning write FOnStopScanning;
    property OnScanProgress: TProgressNotify read FOnScanProgress write FOnScanProgress;
    property OnGetTitle: TTitleNotify read FOnGetTitle write FOnGetTitle;
    property OnInvalidImage: TInvalidImageEvent read FOnInvalidImage write FOnInvalidImage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property AsButtons: Boolean read FAsButtons write SetAsButton;
    property TitlePlacement: TTitlePos read FTitlePlacement write SetTitlePos default tpUp;
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
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvConsts;

{const
  FGraphicExtensions  : array[1..9] of string = ('*.BMP','*.JPG','*.WMF','*.EMF',
                                             '*.ICO','*.GIF','*.PCX',
                                             '*.TGA','*.PNG'); {}

constructor TJvThumbView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FPainted := False;
  Width := 600;
  Height := 480;
  FMaxSize.X := 200;
  FMaxSize.Y := 200;
  FPercent := 100;
  FThumbGap := 4;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Tracking := True;
  FScrollMode := smHorizontal;
  Caption := '';
  CalculateSize;
  FWaitUntilFull := False;
  FFilling := False;
  FSorted := True;
  FMinMemory := True;
  FSelected := -1;
  AutoScrolling := True;
  FDiskSize := 0;
  FAutoHandleKeyb := True;
  FFilter := CreateFilter;
  FThumbList := TJvThumbList.Create;
  FThumbList.Sorted := Sorted;
  FFileList := TStringList.Create;
  FFileList.Clear;
  FFileListSorted := TStringList.Create;
  FFileListSorted.Clear;
  FThumbColor := clNone;
end;

destructor TJvThumbView.Destroy;
begin
  FreeAndNil(FFileListSorted);
  FreeAndNil(FFileList);
  FreeAndNil(FThumbList);
  //FreeAndNil(FFilter);
  inherited Destroy;
end;

procedure TJvThumbView.DoInvalidImage(Sender: TObject; const FileName: string);
begin
  if Assigned(FOnInvalidImage) then
    FOnInvalidImage(Sender, FileName);
end;

procedure TJvThumbView.AddThumb(ATitle: string; Redraw: Boolean);
var
  Thb: TJvThumbnail;
begin
  Thb := TJvThumbnail.Create(Self);
  Thb.Left := CalculateXPos(Count + 1);
  Thb.Top := CalculateYPos(Count + 1);
  Thb.Width := FThumbSize.X;
  Thb.Height := FThumbSize.Y;
  Thb.AsButton := FAsButtons;
  Thb.TitlePlacement := FTitlePlacement;
  Thb.ShadowColor := FShadowColor;
  Thb.ShowShadow := FShowShadow;
  Thb.OnClick := OnClick;
  Thb.Photo.OnClick := OnClick;
  Thb.Photo.OnInvalidImage := DoInvalidImage;
  Thb.OnDblClick := OnDblClick;
  Thb.Photo.OnDblClick := OnDblClick;
  Thb.MinimizeMemory := MinMemory;
  Thb.Color := Self.Color;
  Thb.Title := ATitle;
  if FThumbColor = clNone then
  begin
    Thb.Color := Self.Color;
    Thb.ParentColor := True;
    Thb.TitleColor := Self.Color;
  end
  else
    Thb.Color := FThumbColor;
  FThumbList.AddObject(Thb.Title, Thb);
  Thb.Parent := Self;
  if Redraw then
  begin
    CalculateSize;
    Reposition(0);
  end;
end;

procedure TJvThumbView.GetFiles(ADirectory: string);
var
  SearchRec: TSearchRec;
  FResult: Integer;
  NumExtensions: Integer;

  function FindFirstGraphic(AExtension: string): Integer;
  begin
    // (rom) strange flag faArchive
    FindFirstGraphic :=
      FindFirst(ADirectory + AExtension, faArchive, SearchRec);
  end;

begin
  FFileList.Clear;
  FFileListSorted.Clear;
  SetFilters;
  if not DirectoryExists(ADirectory) then
    Exit;
  if ADirectory[Length(ADirectory)] <> PathDelim then
    ADirectory := ADirectory + PathDelim;
  for NumExtensions := 0 to FGraphicExtensions.Count - 1 do
  begin
    if (FindFirstGraphic(FGraphicExtensions[NumExtensions]) = 0) then
    begin
      try
        if (FFileList.IndexOf(ADirectory + SearchRec.Name) < 0) then
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
        end;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
  FFileListSorted.Sort;
  if Assigned(FGraphicExtensions) then
    FreeAndNil(FGraphicExtensions);
end;

procedure TJvThumbView.SetAlignView(AType: TViewType);
begin
  if AType <> FAlignView then
  begin
    FAlignView := AType;
    Reposition(0);
  end;
end;

procedure TJvThumbView.ScrollTo(const Number: Longint);
var
  TN: TJvThumbnail;
begin
// if AutoScrolling then
  if (Number < 0) or (Number >= FThumbList.Count) then
    Exit;
  TN := TJvThumbnail(FThumbList.Objects[Number]);
  case ScrollMode of
    smVertical:
      begin
        if TN.Top < 0 then
          VertScrollBar.Position := VertScrollBar.Position +
            (TN.Top - (TN.Width div 2));
        if TN.Top + TN.Height > Height then
          VertScrollBar.Position := VertScrollBar.Position +
            (TN.Top - (Height - TN.Height - (TN.Height div 2)));
      end;
    smHorizontal:
      begin
        if TN.Left < 0 then
          HorzScrollBar.Position := HorzScrollBar.Position +
            (TN.Left - (TN.Width div 2));
        if TN.Left + TN.Width > Width then
          HorzScrollBar.Position := HorzScrollBar.Position +
            (TN.Left - (Width - TN.Width - (TN.Width div 2)));
      end;
    smBoth:
      begin
        if TN.Top < 0 then
          VertScrollBar.Position := VertScrollBar.Position +
            (TN.Top - (TN.Width div 2));
        if TN.Top + TN.Height > Height then
          VertScrollBar.Position := VertScrollBar.Position +
            (TN.Top - (TN.Height - (TN.Height div 2)));
        if TN.Left < 0 then
          HorzScrollBar.Position := HorzScrollBar.Position +
            (TN.Left - (TN.Width div 2));
        if TN.Left + TN.Width > Width then
          HorzScrollBar.Position := HorzScrollBar.Position +
            (TN.Left - (Width - TN.Width - (TN.Width div 2)));
      end;
  end;
  if FSelected <> Number then
  begin
    FSelected := Number;
    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

(*
function TJvThumbView.GetBufferName(AName: string): string;
var
  tst: string;
  FN: string;
  Res: string;
begin
  tst := completepath(extractFiledir(AName));
  if tst = AName then
  begin // No FileName included only A Directory;
    // the user wants us to Create A seperate file for each
    // Directory it opens in A pre-specified path
    FN := ReplaceChar(FDirectory, '\', '_', 0, False); //Create the FileName from the path
    FN := ReplaceChar(FN, ':', '_', 0, False); //Create the FileName from the path
    Res := AName + fn;
  end
  else
  begin // the user has specified either A full path and A name or just A name
    if tst = '' then
      // the user has specified only A name to use
      // in each Directory that is opened by the component there will be created
      // A file with name <ANAME> where the thumbs are been saved;
      Res := CompletePath(FDirectory) + AName
    else
      // the user has specified A full path and A file name weach is the same
      // for all the directories he/she opens.
      Res := AName;
  end;
  Result := Res;
end;
*)

//Procedure TJvThumbView.SetBufferFile(NewName: string);
//var
//  tst: string;
//begin
//  If NewName <> FBufferFile then
//    tst := GetBufferName(NewName);
//  End;
//end;

procedure TJvThumbView.SetSelected(Number: Longint);
var
  TN: TJvThumbnail;
begin
  if (Number < 0) or (Number >= FThumbList.Count) then
    Number := -1;

  if FThumbList.Count > 0 then
  begin
    if FSelected <> -1 then
    begin
      TN := TJvThumbnail(FThumbList.Objects[FSelected]);
      TN.TitleColor := TN.Color;
      TN.TitleFont.Color := TN.Font.Color;
    end;
    if Number <> -1 then
    begin
      TN := TJvThumbnail(FThumbList.Objects[Number]);
      TN.TitleColor := clHighlight;
      TN.TitleFont.Color := clHighlightText;
      if AutoScrolling then
      begin
        if (TN.Top + TN.Height > Height) or (TN.Top < 0) then
          ScrollTo(Number);
        if (TN.Left + TN.Width > Width) or (TN.Left < 0) then
          ScrollTo(Number);
      end
    end;
    if FSelected <> Number then
    begin
      if Assigned(FOnChanging) then
        FOnChanging(Self);

      FSelected := Number;

      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end
  else
    FSelected := -1;
end;

function TJvThumbView.GetSelectedFile;
begin
  if Selected <> -1 then
    Result := TJvThumbnail(FThumbList.Objects[Selected]).FileName;
end;

procedure TJvThumbView.SetSelectedFile(AFile: string);
var
  I: Longint;
  Dir: string;
begin
  Dir := ExtractFileDir(AFile);
  if Dir[Length(Dir)] = PathDelim then
    Dir := Copy(Dir, 0, Length(Dir) - 1);
  Directory := Dir;
  for I := 0 to FThumbList.Count - 1 do
    if TJvThumbnail(FThumbList.Objects[I]).FileName = AFile then
    begin
      Selected := I;
      if not FAutoScrolling then
        ScrollTo(I);
      Exit;
    end;
end;

procedure TJvThumbView.SetDirectory(Value: string);
var
  Counter1, FStartTime: DWORD;
  Cancel: Boolean;
  ReadFileList: TStringList;
  OldCursor: TCursor;
//  Pic: TPicture;
begin
  FSelected := -1;
  //  if not FPainted then
  //  begin
  //    postMessage(Self.Handle, WM_LOADWHENREADY, 0, 0);
  //    Exit;
  //  end;
  FDiskSize := 0;
  if FFilling then
    Exit;
  if Value <> '' then
  begin
    ReadFileList := TStringList.Create;
    OldCursor := Cursor;
    try
      FFilling := True;
    //    if Assigned(ReadFileList) then FreeAndNil(ReadFileList);
      FStartTime := GetTickCount;
      GetFiles(Value);
      if FSorted then
        ReadFileList.Assign(FFileListSorted)
      else
        ReadFileList.Assign(FFileList);
      EmptyList;
      FDirectory := Value;
      if Assigned(FOnStartScanning) then
        FOnStartScanning(Self, ReadFileList.Count - 1);
      if ReadFileList.Count > 0 then
      begin
        Cancel := False;
        for Counter1 := 0 to ReadFileList.Count - 1 do
        begin
          if Assigned(FOnScanProgress) then
            FOnScanProgress(Self, Counter1 + 1, Cancel);
          if Cancel then
            Break;
          AddThumb(ExtractFilename(ReadFileList.Strings[Counter1]), True);
          TJvThumbnail(FThumbList.Objects[Counter1]).FileName := ReadFileList.Strings[Counter1];
          Inc(FDiskSize, TJvThumbnail(FThumbList.Objects[Counter1]).FileSize);
          if (Cursor <> crHourGlass) and (GetTickCount - FStartTime > 1000) then
            Cursor := crHourGlass;
        end;
      end;
      if Assigned(FOnStopScanning) then
        FOnStopScanning(Self);
    finally
      FreeAndNil(ReadFileList);
      FFilling := False;
      Cursor := OldCursor;
    end
  end
  else
    EmptyList;
  FDirectory := Value;
  if (FThumbList.Count > 0) and (Selected < 0) then
    SetSelected(0);
  Invalidate;
end;

procedure TJvThumbView.Reposition(Start: Integer);
var
  I: Integer;
  Tmp1: Longint;
  Tmp2: Longint;
  TN: TJvThumbnail;
begin
  Tmp2 := HorzScrollBar.Position;
  HorzScrollBar.Position := 0;
  Tmp1 := VertScrollBar.Position;
  VertScrollBar.Position := 0;
  for I := Start to FThumbList.Count - 1 do
  begin
    TN := TJvThumbnail(FThumbList.Objects[I]);
    if TN <> nil then
    begin
      TN.Left := CalculateXPos(I + 1);
      TN.Top := CalculateYPos(I + 1);
      TN.Width := FThumbSize.X;
      TN.Height := FThumbSize.Y;
    end;
  end;
  HorzScrollBar.Position := Tmp2;
  VertScrollBar.Position := Tmp1;
end;

procedure TJvThumbView.CalculateMaxX;
var
  A: Longint;
begin
  case FScrollMode of
    smVertical:
      A := (Width - 20) div (FThumbSize.X + FThumbGap);
    smHorizontal:
      A := (Height - 20) div (FThumbSize.Y + FThumbGap);
    smBoth:
      A := JkCeil(Sqrt(FThumbList.Count));
  else
    A := 1;
  end;
  if A < 1 then
    A := 1;
  if A <> FMaxX then
    FMaxX := A;
end;

procedure TJvThumbView.CalculateSize;
begin
  FThumbSize.X := Trunc((MaxWidth / 100.0) * Size);
  FThumbSize.Y := Trunc((MaxHeight / 100.0) * Size);
  CalculateMaxX;
end;

function TJvThumbView.CalculateXPos(Num: Word): Longint;
var
  VPos, HPos: Longint;
  Temp: Longint;
  Tmp: Longint;
  Spact: Longint;
begin
  if Num > 0 then
  begin
    Spact := FThumbGap;
    case FScrollMode of
      smVertical, smBoth:
        begin
          if (FAlignView = vtFitToScreen) and (FScrollMode = smVertical) then
            Spact := ((Width - 20) - (FThumbSize.X * FMaxX)) div (FMaxX + 1);
          VPos := JkCeil(Num / FMaxX);
          HPos := (Num - (VPos * FMaxX)) + FMaxX;
          Temp := (FThumbSize.X * (HPos - 1)) + (HPos * Spact);
          if (FAlignView = vtCenter) and (FScrollMode = smVertical) then
          begin
            Tmp := ((Width - 20) div 2) - (((FThumbSize.X + FThumbGap) * FMaxX) div 2);
            Temp := Temp + Tmp;
          end;
        end;
      smHorizontal:
        begin
          VPos := JkCeil(Num / FMaxX);
          Temp := (FThumbSize.Y * (VPos - 1)) + (VPos * Spact);
        end
    else
      Temp := 0
    end;
  end
  else
    Temp := 0;
  Result := Temp;
end;

function TJvThumbView.CalculateYPos(Num: Word): Longint;
var
  VPos, HPos: Longint;
  Temp: Longint;
  Tmp: Longint;
  Spact: Longint;
begin
  if Num > 0 then
  begin
    Spact := FThumbGap;
    case FScrollMode of
      smVertical, smBoth:
        begin
          VPos := JkCeil(Num / FMaxX);
          Temp := (FThumbSize.Y * (VPos - 1)) + (VPos * Spact);
        end;
      smHorizontal:
        begin
          if FAlignView = vtFitToScreen then
            Spact := ((Height - 20) - ((FThumbSize.Y + FThumbGap) * FMaxX)) div (FMaxX + 1);
          HPos := JkCeil(Num / FMaxX);
          VPos := (Num - (HPos * FMaxX)) + FMaxX;
          Temp := (FThumbSize.X * (VPos - 1)) + (VPos * Spact);
          if FAlignView = vtCenter then
          begin
            Tmp := ((Height - 20) div 2) - ((FThumbSize.Y * FMaxX) div 2);
            Temp := Temp + Tmp;
          end;
        end;
    else
      Temp := 0;
    end;
  end
  else
    Temp := 0;
  Result := Temp;
end;

procedure TJvThumbView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  No: Word;
  TempX, TempY: Longint;
begin
  // Check to see if there are any problems removing the following
  // For sure it solves A focus problem I'm having in an application
 //  setfocus;
  if Count > 0 then
    case ScrollMode of
      smVertical, smBoth:
        begin
          TempX := JkCeil((X + HorzScrollBar.Position) / (FThumbSize.X + FThumbGap));
          TempY := JkCeil((Y + VertScrollBar.Position) / (FThumbSize.Y + FThumbGap));
          if TempX > FMaxX then
            TempX := FMaxX;
          if TempY < 1 then
            TempY := 1;
          No := ((TempY - 1) * FMaxX + TempX) - 1;
          if No < Count then
            if TJvThumbnail(FThumbList.Objects[No]) <> nil then
              if (X > TJvThumbnail(FThumbList.Objects[No]).Left) and
                (X < TJvThumbnail(FThumbList.Objects[No]).Left +
                TJvThumbnail(FThumbList.Objects[No]).Width) and
                (Y > TJvThumbnail(FThumbList.Objects[No]).Top) and
                (Y < TJvThumbnail(FThumbList.Objects[No]).Top +
                TJvThumbnail(FThumbList.Objects[No]).Height) then
                SetSelected(No)
              else
                SetSelected(-1)
            else
              SetSelected(-1)
          else
            SetSelected(-1);
        end;
      smHorizontal:
        begin
          TempX := JkCeil((X + HorzScrollBar.Position) / (FThumbSize.X + FThumbGap));
          TempY := JkCeil((Y + VertScrollBar.Position) / (FThumbSize.Y + FThumbGap));
          if TempY > FMaxX then
            TempY := FMaxX;
          if TempX < 1 then
            TempX := 1;
          No := ((TempX - 1) * FMaxX + TempY) - 1;
          if No < Count then
            if TJvThumbnail(FThumbList.Objects[No]) <> nil then
              if (X > TJvThumbnail(FThumbList.Objects[No]).Left) and
                (X < TJvThumbnail(FThumbList.Objects[No]).Left +
                TJvThumbnail(FThumbList.Objects[No]).Width) and
                (Y > TJvThumbnail(FThumbList.Objects[No]).Top) and
                (Y < TJvThumbnail(FThumbList.Objects[No]).Top +
                TJvThumbnail(FThumbList.Objects[No]).Height) then
                SetSelected(No)
              else
                SetSelected(-1)
            else
              SetSelected(-1)
          else
            SetSelected(-1);
        end;
    else
      SetSelected(-1);
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvThumbView.AddFromStream(AStream: TStream; AType: TGRFKind);
begin
  AddFromStream(AStream, AType, '');
end;

function TJvThumbView.AddFromStream(AStream: TStream; AType: TGRFKind; const aTitle: string): Integer;
var
  Thb: TJvThumbnail;
begin
  Thb := TJvThumbnail.Create(Self);
  Thb.StreamFileType := AType;
  Thb.Left := CalculateXPos(Count + 1);
  Thb.Top := CalculateYPos(Count + 1);
  Thb.Width := FThumbSize.X;
  Thb.Height := FThumbSize.Y;
  Thb.OnClick := OnClick;
  Thb.Photo.OnClick := OnClick;
  Thb.OnDblClick := OnDblClick;
  Thb.Title := aTitle;
  Thb.Photo.OnDblClick := OnDblClick;
  //  Thb.Buffer := Vbuffer;
  Thb.Photo.LoadFromStream(AStream, Thb.StreamFileType);
  Result := FThumbList.AddObject(Thb.Title, Thb);
  InsertControl(Thb);
  CalculateSize;
  Reposition(Result);
end;

function  TJvThumbView.AddFromFile(AFile: string) : Integer;
var
  ThumbnailTitle: string;
  FFont: TFont;
  FColor: TColor;
  Thb: TJvThumbnail;
begin
  Thb := TJvThumbnail.Create(Self);
  if Assigned(FOnGetTitle) then
  begin
    ThumbnailTitle := ExtractFilename(AFile);
    FFont := TFont.Create;
    FColor := clBtnFace;
    if Assigned(FOnGetTitle) then
      FOnGetTitle(Self, AFile, ThumbnailTitle, FFont, FColor);
    Thb.SetTitlePanel(ThumbnailTitle, FFont, FColor);
    FreeAndNil(FFont);
  end;
  Thb.OnClick := OnClick;
  Thb.Photo.OnClick := OnClick;
  Thb.OnDblClick := OnDblClick;
  Thb.Photo.OnDblClick := OnDblClick;
  Thb.MinimizeMemory := MinMemory;
  //  Thb.Buffer := VBuffer;
  FThumbList.AddObject(AFile, Thb);
  InsertControl(Thb);
  CalculateSize;
  Reposition(0);
  TJvThumbnail(FThumbList.Objects[FThumbList.IndexOf(AFile)]).FileName := AFile;
  result := FThumbList.IndexOf(AFile);
end;

procedure TJvThumbView.Delete(No: Longint);
var
  Dummy: Longint;
begin
  if No >= FThumbList.Count then
  begin
  end //Raise an exception
  else
  begin
    Dummy := FFileList.IndexOf(SelectedFile);
    if Dummy >= 0 then
      FFileList.Delete(Dummy);
    Dummy := FFileListSorted.IndexOf(SelectedFile);
    if Dummy >= 0 then
      FFileListSorted.Delete(Dummy);
    TJvThumbnail(FThumbList.Objects[No]).Free;
    FThumbList.Delete(No);
    FSelected := -1;
    //CalculateSize;
    Dec(No, 1);
    if No < 0 then
      No := 0;
    Reposition(No);
    Refresh;
    Repaint;
  end
end;

procedure TJvThumbView.SetThumbGap(Sp: Byte);
begin
  case FAlignView of
    vtNormal, vtCenter:
      begin
        FThumbGap := Sp;
        CalculateMaxX;
        Reposition(0);
      end;
    vtFitToScreen:
      FThumbGap := Sp;
  end;
end;

function TJvThumbView.GetCount: Word;
begin
  Result := FThumbList.Count;
end;

procedure TJvThumbView.SortList;
begin
  // add code to resort the list
  FThumbList.Sort;
  CalculateSize;
  Reposition(0);
end;

procedure TJvThumbView.Refresh;
var
  I: Longint;
begin
  CalculateSize;
  Reposition(0);
  for I := 0 to FThumbList.Count - 1 do
    FThumbList.Thumbnail[I].Refresh;
  inherited Refresh;
end;

procedure TJvThumbView.EmptyList;
var
  Metr: Integer;
begin
  for Metr := Count - 1 downto 0 do
    if FThumbList.Objects[Metr] <> nil then
    begin
      TJvThumbnail(FThumbList.Objects[Metr]).Parent := nil;
      TJvThumbnail(FThumbList.Objects[Metr]).Free;
      FThumbList.Delete(Metr);
    end;
  FSelected := -1; // Mantis #5140
end;

procedure TJvThumbView.SetMaxWidth(W: Longint);
begin
  FMaxSize.X := W;
  CalculateSize;
  Reposition(0);
end;

procedure TJvThumbView.SetMaxHeight(H: Longint);
begin
  // if FMaxSize.Y<H then
  FMaxSize.Y := H;
  CalculateSize;
  Reposition(0);
end;

procedure TJvThumbView.Resize;
begin
  CalculateMaxX;
  Reposition(0);
  inherited Resize;
end;

procedure TJvThumbView.SetPercent(P: TPercent);
begin
  FPercent := P;
  CalculateSize;
  Reposition(0);
end;

procedure TJvThumbView.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if not FPainted then
  begin
    FPainted := True;
    SetDirectory(FDirectory);
  end;
end;

procedure TJvThumbView.SetScrollMode(AMode: TScrollMode);
begin
  if FScrollMode <> AMode then
  begin
    FScrollMode := AMode;
    CalculateSize;
    Reposition(0);
    if Selected > -1 then
      ScrollTo(Selected);
  end
end;

procedure TJvThumbView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
  inherited KeyUp(Key, Shift);
end;

procedure TJvThumbView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if AutoHandleKeyb and (FThumbList.Count > 0) then
    case Key of
      VK_RIGHT:
        begin
          GoRight;
          ScrollTo(Selected);
        end;
      VK_DOWN:
        begin
          GoDown;
          ScrollTo(Selected);
        end;
      VK_LEFT:
        begin
          GoLeft;
          ScrollTo(Selected);
        end;
      VK_UP:
        begin
          GoUp;
          ScrollTo(Selected);
        end;
      VK_DELETE:
        begin
        end;
      VK_PRIOR:
        begin
        end;
      VK_NEXT:
        begin
        end;
      VK_END:
        begin
          Selected := Count - 1;
          ScrollTo(Selected);
        end;
      VK_HOME:
        begin
          Selected := 0;
          ScrollTo(Selected);
        end;
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvThumbView.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
  inherited KeyPress(Key);
end;

procedure TJvThumbView.SetSorted(const Value: Boolean);
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

procedure TJvThumbView.GetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows, dcWantAllKeys];
end;

procedure TJvThumbView.GoRight;
var
  Actual: Longint;
begin
  Actual := 0;
  if ScrollMode = smHorizontal then
    Actual := Selected + FMaxX;
  if (ScrollMode = smVertical) or (ScrollMode = smBoth) then
    Actual := Selected + 1;
  if (Actual > Count - 1) or (Actual < 0) then
    Actual := Selected;
  Selected := Actual;
end;

procedure TJvThumbView.GoLeft;
var
  Actual: Longint;
begin
  Actual := 0;
  if ScrollMode = smHorizontal then
    Actual := Selected - FMaxX;
  if (ScrollMode = smVertical) or (ScrollMode = smBoth) then
    Actual := Selected - 1;
  if (Actual > Count - 1) or (Actual < 0) then
    Actual := Selected;
  Selected := Actual;
end;

procedure TJvThumbView.GoDown;
var
  Actual: Longint;
begin
  Actual := 0;
  if ScrollMode = smHorizontal then
    Actual := Selected + 1;
  if (ScrollMode = smVertical) or (ScrollMode = smBoth) then
    Actual := Selected + FMaxX;
  if (Actual > Count - 1) or (Actual < 0) then
    Actual := Selected;
  Selected := Actual;
end;

procedure TJvThumbView.GoUp;
var
  Actual: Longint;
begin
  Actual := 0;
  if ScrollMode = smHorizontal then
    Actual := Selected - 1;
  if (ScrollMode = smVertical) or (ScrollMode = smBoth) then
    Actual := Selected - FMaxX;
  if (Actual > Count - 1) or (Actual < 0) then
    Actual := Selected;
  Selected := Actual;
end;

procedure TJvThumbView.SetAsButton(const NewVal: Boolean);
var
  I: Longint;
begin
  if NewVal <> FAsButtons then
  begin
    for I := 0 to FThumbList.Count - 1 do
      FThumbList.Thumbnail[I].AsButton := NewVal;
    FAsButtons := NewVal;
  end;
end;

procedure TJvThumbView.SetTitlePos(const NewVal: TTitlePos);
var
  I: Longint;
begin
  if NewVal <> FTitlePlacement then
  begin
    for I := 0 to FThumbList.Count - 1 do
      FThumbList.Thumbnail[I].TitlePlacement := NewVal;
    FTitlePlacement := NewVal;
  end;
end;

function TJvThumbView.CreateFilter: string;
//var
//  Res: string;
//  Pos: Longint;
begin
  Result := GraphicFilter(TGraphic);
end;

procedure TJvThumbView.SetFilters;
var
  Cp1 {, CP2}: Integer; // CurrentPosition;
//  Md: Byte; // Mode
  Res: string;
//  Sub: string;
  Final: string;
begin
  if not Assigned(FGraphicExtensions) then
    FGraphicExtensions := TStringList.Create;
//  Cp1 := 0;
//  CP2 := 0;
  Res := FFilter;
  Final := '';
  repeat
    Cp1 := Pos('|', Res);
    if Cp1 > 0 then
    begin
      System.Delete(Res, 1, Cp1);
      Cp1 := Pos('|', Res);
      if Cp1 > 0 then
      begin
        Final := Final + ';' + Copy(Res, 1, Cp1 - 1);
        System.Delete(Res, 1, Cp1);
      end
      else
        Final := Final + ';' + Res;
    end
    else
      Final := Final + ';' + Res;
  until Cp1 = 0;
  Final := ReplaceAllstr(Final, ';', sLineBreak, False);
  FGraphicExtensions.Text := Final;

  Cp1 := 0;
  repeat
    if FGraphicExtensions[Cp1] = '' then
      FGraphicExtensions.Delete(Cp1)
    else
      Inc(Cp1);
  until Cp1 = FGraphicExtensions.Count;
end;

function TJvThumbList.GetThumbnail(Index: Longint): TJvThumbnail;
begin
  Result := TJvThumbnail(Objects[Index]);
end;

function TJvThumbView.GetMaxHeight: Longint;
begin
  Result := FMaxSize.Y;
end;

function TJvThumbView.GetMaxWidth: Longint;
begin
  Result := FMaxSize.X;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
