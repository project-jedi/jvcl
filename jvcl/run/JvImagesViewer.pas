{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImagesViewer.PAS, released on 2003-12-01.

The Initial Developer of the Original Code is: Peter Thörnqvist
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImagesViewer;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Messages, Controls, Graphics, StdCtrls, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QWindows, QControls, QGraphics, QStdCtrls, QComCtrls,
  {$ENDIF VisualCLX}
  JvCustomItemViewer;

type
  TJvImageItem = class(TJvViewerItem)
  private
    FFilename: string;
    FPicture: TPicture;
    FCaption: TCaption;
    procedure SetFilename(const Value: string);
    procedure SetCaption(const Value: TCaption);
    procedure SetPicture(const Value: TPicture);
    function GetPicture: TPicture;
    procedure CreatePicture;
  protected
    procedure DoPictureChange(Sender: TObject); virtual;
    procedure DoLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string); virtual;
    procedure ReduceMemoryUsage; override;
  public
    destructor Destroy; override;
  public
    property Filename: string read FFilename write SetFilename;
    property Picture: TPicture read GetPicture write SetPicture;
    property Caption: TCaption read FCaption write SetCaption;
  end;

  TJvImageViewerOptions = class(TJvCustomItemViewerOptions)
  private
    FImagePadding: integer;
    FFrameColor: TColor;
    FHotFrameSize: integer;
    FHotColor: TColor;
    FTransparent: boolean;
    procedure SetImagePadding(const Value: integer);
    procedure SetFrameColor(const Value: TColor);
    procedure SetHotColor(const Value: TColor);
    procedure SetHotFrameSize(const Value: integer);
    procedure SetTransparent(const Value: boolean);
  public
    constructor Create(AOwner: TJvCustomItemViewer); override;
  published
    property AutoCenter;
    property Alignment;
    property BrushPattern;
    property DragAutoScroll;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clGray;
    property Height;
    property HorzSpacing;
    property HotColor: TColor read FHotColor write SetHotColor default clHighlight;
    property HotFrameSize: integer read FHotFrameSize write SetHotFrameSize default 2;
    property HotTrack;
    property ImagePadding: integer read FImagePadding write SetImagePadding default 8;
    property Layout;
    property LazyRead;
    property MultiSelect;
    property ReduceMemoryUsage;
    property RightClickSelect;
    property Transparent:boolean read FTransparent write SetTransparent default false;
    property ScrollBar;
    property ShowCaptions default true;
    property Tracking;
    property VertSpacing;
    property Width;
  end;

  TJvImageLoadEvent = procedure(Sender: TObject; Item: TJvImageItem) of object;
  TJvImageLoadErrorEvent = procedure(Sender: TObject; E: Exception;
    const Filename: string; var Handled: boolean) of object;
  TJvImageViewerLoadProgress = procedure(Sender: TObject; Item: TJvImageItem; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  TJvImagesViewer = class(TJvCustomItemViewer)
  private
    FFileMask: string;
    FDirectory: string;
    FOnLoadError: TJvImageLoadErrorEvent;
    FOnLoadProgress: TJvImageViewerLoadProgress;
    FOnLoadBegin: TNotifyEvent;
    FOnLoadEnd: TNotifyEvent;
    procedure SetDirectory(const Value: string);
    procedure SetFileMask(const Value: string);
    function GetItems(Index: integer): TJvImageItem;
    procedure ExpandFileMask(const Mask: string; Strings: TStrings);
    function ScaleRect(ARect, RefRect: TRect): TRect;
    function GetOptions: TJvImageViewerOptions;
    procedure SetOptions(const Value: TJvImageViewerOptions);
  protected
    function GetItemClass: TJvViewerItemClass; override;
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
    function LoadErrorHandled(E: Exception; const Filename: string): boolean;
    procedure DoLoadBegin; virtual;
    procedure DoLoadProgress(Item: TJvImageItem; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R:
      TRect; const Msg: string);
    procedure DoLoadEnd; virtual;
    procedure DrawItem(Index: Integer; State: TCustomDrawState; Canvas: TCanvas; ItemRect, TextRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadImages: boolean;
    property Items[Index: integer]: TJvImageItem read GetItems;
    property Count;
  published
    property Directory: string read FDirectory write SetDirectory;
    property FileMask: string read FFileMask write SetFileMask;
    property Options: TJvImageViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
    property OnScroll;
    property OnLoadBegin: TNotifyEvent read FOnLoadBegin write FOnLoadBegin;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TJvImageLoadErrorEvent read FOnLoadError write FOnLoadError;
    property OnLoadProgress: TJvImageViewerLoadProgress read FOnLoadProgress write FOnLoadProgress;
    property OnDrawItem;
    property OnOptionsChanged;
    property OnItemChanging;
    property OnItemChanged;
    property OnItemHint;
    property OnInsertion;
    property OnDeletion;

    property Align;
    property Anchors;
    //    property BiDiMode;
    property Color;
    property Constraints;
    {$IFDEF VCL}
    property DockSite;
    property DragCursor;
    property DragKind;
    {$ENDIF VCL}
    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    {$IFDEF VCL}
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF VCL}
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnStartDrag;
  end;

implementation

uses
  JvJCLUtils;
  

{ TJvImageViewerOptions }

constructor TJvImageViewerOptions.Create(AOwner: TJvCustomItemViewer);
begin
  inherited Create(AOwner);
  FImagePadding := 20;
  FFrameColor := clGray;
  FHotColor := clHighlight;
  FHotFrameSize := 2;
  ShowCaptions := true;
end;

procedure TJvImageViewerOptions.SetFrameColor(const Value: TCOlor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TJvImageViewerOptions.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

procedure TJvImageViewerOptions.SetHotFrameSize(const Value: integer);
begin
  FHotFrameSize := Value;
end;

procedure TJvImageViewerOptions.SetImagePadding(const Value: integer);
begin
  if (FImagePadding <> Value) then
  begin
    FImagePadding := Value;
    Change;
  end;
end;

procedure TJvImageViewerOptions.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Change;
  end;
end;


{ TJvImageItem }

procedure TJvImageItem.CreatePicture;
var S:string;
begin
  if FPicture = nil then
  begin
    FPicture := TPicture.Create;
    FPicture.OnChange := DoPictureChange;
    FPicture.OnProgress := DoLoadProgress;
    S := ExpandUNCFilename(Filename);
    if (S <> '') and FileExists(S) then
    try
      FPicture.LoadFromFile(S);
      if FPicture.Graphic <> nil then
        FPicture.Graphic.Transparent := TJvImagesViewer(Owner).Options.Transparent;
    except
      on E: Exception do
        if not TJvImagesViewer(Owner).LoadErrorHandled(E, Filename) then
          Delete
        else
        begin
          FreeAndNil(FPicture);
          raise;
        end;
    end;
  end;
end;

destructor TJvImageItem.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TJvImageItem.DoPictureChange(Sender: TObject);
begin
  Changed;
end;

procedure TJvImageItem.DoLoadProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Owner is TJvImagesViewer then
    TJvImagesViewer(Owner).DoLoadProgress(Self, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TJvImageItem.GetPicture: TPicture;
begin
  CreatePicture;
  Result := FPicture;
end;

procedure TJvImageItem.SetFilename(const Value: string);
begin
  if (AnsiCompareFilename(FFilename, Value) <> 0) and Changing then
  begin
    FFilename := Value;
    // don't load image unless necessary
    FreeAndNil(FPicture);
  end;
end;

procedure TJvImageItem.SetPicture(const Value: TPicture);
begin
  if Value <> nil then
    GetPicture.Assign(Value)
  else
    FreeAndNil(FPicture);
  Changed;
end;

procedure TJvImageItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) and Changing then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvImageItem.ReduceMemoryUsage;
begin
  inherited ReduceMemoryUsage;
  if Filename <> '' then // release image if we can recreate it from it's filename
    Picture := nil;
end;

{ TJvImagesViewer }

constructor TJvImagesViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  FDirectory := GetCurrentDir;
  FFileMask := Graphics.GraphicFileMask(TGraphic);
  Color := clWindow;
end;

function TJvImagesViewer.ScaleRect(ARect, RefRect: TRect): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := ARect.Right - ARect.Left;
  h := ARect.Bottom - ARect.Top;
  cw := RefRect.Right - RefRect.Left;
  ch := RefRect.Bottom - RefRect.Top;

  if (w > cw) or (h > ch) then
  begin
    if (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;
end;

procedure TJvImagesViewer.DrawItem(Index: Integer; State: TCustomDrawState; Canvas: TCanvas; ItemRect, TextRect:
  TRect);
var
  ImageRect: TRect;
  TotalPadding, BottomRightShift: integer;
  AItem: TJvImageItem;
  S: string;
  procedure ModifyRect(var R:TRect; ALeft, ATop, ARight, ABottom:integer);
  begin
    Inc(R.Left, ALeft);
    Inc(R.Top, ATop);
    Inc(R.Right, ARight);
    Inc(R.Bottom, ABottom);
  end;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    BottomRightShift := 1
  else
  {$ENDIF MSWINDOWS}
    BottomRightShift := 0;
  AItem := Items[Index];
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Font.Color;
  TotalPadding := Options.ImagePadding;
  if Options.ShowCaptions then
  begin
    Dec(ImageRect.Bottom, 2);
    Inc(TextRect.Top, 2);
    S := AItem.Caption;
    if (S = '') then
      S := ExtractFileName(AItem.Filename);
  end;

  if cdsHot in State then
  begin
    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
    Canvas.Font.Color := clHighlight;
    Canvas.Pen.Color := Options.HotColor;
    Canvas.Pen.Width := Options.HotFrameSize;
    Canvas.Brush.Style := bsClear;
    ModifyRect(ItemRect,Options.HotFrameSize div 2,Options.HotFrameSize div 2,
      -Options.HotFrameSize div 2 + BottomRightShift,-Options.HotFrameSize div 2 + BottomRightShift);
    Canvas.Rectangle(ItemRect);
    ModifyRect(ItemRect,-Options.HotFrameSize div 2,-Options.HotFrameSize div 2,
      Options.HotFrameSize div 2 - BottomRightShift,Options.HotFrameSize div 2 - BottomRightShift);
    Canvas.Brush.Style := bsSolid;
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    Canvas.Pen.Width := 1;
  end;
  if cdsSelected in State then
  begin
    Canvas.Pen.Color := clBtnFace;
    Canvas.Brush.Color := clHighlight;
    if Options.BrushPattern.Active then
      Canvas.Brush.Bitmap := Options.BrushPattern.GetBitmap
    else
      Canvas.Brush.Color := Options.BrushPattern.OddColor;
    Canvas.Rectangle(ItemRect);
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Options.HotColor;
    Canvas.Pen.Width := Options.HotFrameSize;
    ModifyRect(ItemRect,Options.HotFrameSize div 2,Options.HotFrameSize div 2,
      -Options.HotFrameSize div 2 + BottomRightShift,-Options.HotFrameSize div 2 + BottomRightShift);
    Canvas.Rectangle(ItemRect);
    ModifyRect(ItemRect,-Options.HotFrameSize div 2,-Options.HotFrameSize div 2,
      Options.HotFrameSize div 2 - BottomRightShift,Options.HotFrameSize div 2 - BottomRightShift);
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Width := 1;
  end
  else
  if (Options.FrameColor <> clNone) and not (cdsHot in State) then
  begin
    Canvas.Brush.Color := Options.FrameColor;
    Canvas.FrameRect(ItemRect);
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
  end;
  // make space around image
  InflateRect(ItemRect, -TotalPadding, -TotalPadding);
  if AItem.Picture <> nil then // access Picture to load image
  begin
    ImageRect := Rect(0, 0, AItem.Picture.Width, AItem.Picture.Height);
    ImageRect := CenterRect(ScaleRect(ImageRect, ItemRect), ItemRect);
    if (RectWidth(ImageRect) > 0) and (RectHeight(ImageRect) > 0) then
    begin
      if (AItem.Picture.Graphic is TIcon) then
        //        and (RectWidth(ImageRect) < RectWidth(R)) and (RectHeight(ImageRect) < RectHeight(R))  then
        with ImageRect do // TIcon doesn't scale it's content
          DrawIconEx(Canvas.Handle, Left, Top, AItem.Picture.Icon.Handle, Right - Left, Bottom - Top, 0, 0, DI_NORMAL)
      else
        Canvas.StretchDraw(ImageRect, AItem.Picture.Graphic);
    end;
  end;

  if Options.ShowCaptions and (S <> '') then
  begin
    if Options.Layout = tlCenter then
      S := ' ' + S + ' ';
    ViewerDrawText(Canvas, PChar(S), Length(S),
      TextRect, DT_END_ELLIPSIS or DT_EDITCONTROL, Options.Alignment, tlCenter, false);
  end;
end;

function TJvImagesViewer.GetItems(Index: integer): TJvImageItem;
begin
  Result := TJvImageItem(inherited Items[Index]);
end;

function TJvImagesViewer.GetItemClass: TJvViewerItemClass;
begin
  Result := TJvImageItem;
end;

function TJvImagesViewer.LoadImages: boolean;
var
  i, j: integer;
  F: TSearchRec;
  Files, FileMasks: TStringList;
  tmpDir: string;
begin
  BeginUpdate;
  try
    Count := 0;
    tmpDir := ExpandUNCFileName(Directory);
    FileMasks := TStringList.Create;
    try
      FileMasks.Sorted := true; // make sure no duplicates are added
      ExpandFileMask(Filemask, FileMasks);
      if tmpDir <> '' then
        tmpDir := IncludeTrailingPathDelimiter(tmpDir);
      DoLoadBegin;
      Files := TStringlist.Create;
      try
        Files.Sorted := true;
        for i := 0 to FileMasks.Count - 1 do
        begin
          if SysUtils.FindFirst(tmpDir + FileMasks[i], faAnyFile, F) = 0 then
          try
            repeat
              if F.Attr and faDirectory = 0 then
                Files.Add(tmpDir + F.Name);
            until SysUtils.FindNext(F) <> 0;
            Count := Files.Count;
            j := 0;
            while j < Files.Count do
            begin
              Items[j].Filename := Files[j];
              Inc(j);
            end;
          finally
            SysUtils.FindClose(F);
          end;
        end;
      finally
        Files.Free;
      end;
      DoLoadEnd;
    finally
      FileMasks.Free;
    end;
    Result := Count > 0;
  finally
    EndUpdate;
  end;
end;

procedure TJvImagesViewer.SetDirectory(const Value: string);
begin
  if FDirectory <> Value then
  begin
    FDirectory := Value;
    LoadImages;
  end;
end;

procedure TJvImagesViewer.SetFileMask(const Value: string);
begin
  if FFileMask <> Value then
  begin
    FFileMask := Value;
    LoadImages;
  end;
end;

procedure TJvImagesViewer.ExpandFileMask(const Mask: string;
  Strings: TStrings);
var
  Start, Current: PChar;
  tmpChar: char;
begin
  Current := PChar(Mask);
  Start := Current;
  while (Current <> nil) and (Current^ <> #0) do
  begin
    if Current^ in [',', ';'] then
    begin
      tmpChar := Current^;
      Current^ := #0;
      if Start <> '' then
        Strings.Add(Start);
      Current^ := tmpChar;
      Start := Current + 1;
    end;
    Inc(Current);
  end;
  if Start <> '' then
    Strings.Add(Start);
end;

function TJvImagesViewer.LoadErrorHandled(E: Exception; const Filename: string):
  boolean;
begin
  Result := false;
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, E, Filename, Result);
end;

procedure TJvImagesViewer.DoLoadBegin;
begin
  if Assigned(FOnLoadBegin) then
    FOnLoadBegin(Self);
end;

procedure TJvImagesViewer.DoLoadProgress(Item: TJvImageItem;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string);
begin
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, Item, Stage, PercentDone, ReDrawNow, R, Msg);
end;

procedure TJvImagesViewer.DoLoadEnd;
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
end;

function TJvImagesViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvImageViewerOptions;
end;

function TJvImagesViewer.GetOptions: TJvImageViewerOptions;
begin
  Result := TJvImageViewerOptions(inherited Options);
end;

procedure TJvImagesViewer.SetOptions(const Value: TJvImageViewerOptions);
begin
  inherited Options := Value;
end;

end.
