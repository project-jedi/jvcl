{$I JVCL.INC}
unit JvPrvwDoc;
{ TODO :
    + Adjust zoom when Cols or Rows change
    + Adjust Cols and/or Rows when Zoom changes
    + Center pages in view
    * Only show horizontal scroll when page is too large (1 page), otherwise size Cols to fit
    + Draw to offscreen bitmap
    + User configurable margins (could use DeviceInfo.OffsetLeft etc but needs to be available in inch/mm as well)
    * Handle getting/setting SelectedPage (click on page -> select it)
    * Draw "fake" text when page is small (like Word does)?
    * Handle wheel scroll (scroll: up-down / shift+scroll: left-right)
    * Handle Home, End, PgUp, PgDn (w. Ctrl)

  Scrolling rules:
    * if showing 1 page (page >= clientrect), show horz scrollbar, set scroll size ~ pagesize div 10
    * if showing more than one col/row, hide horz scroll and scale pages to fit
      (i.e if Cols = 3, Rows = 2 -> scale to show 3x2 pages)
      and scroll Rows pages on each click (i.e if Rows = 4 -> scroll 4 pages)
    * if scaling would make pages too small, show as many pages as possible
}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;
const
  WM_PREVIEWADDPAGE = WM_USER + 1001;

type
  TJvPreviewScaleMode = (smFullPage,smPageWidth,smScale,smColsRows);
   
  TJvDrawPreviewEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect) of object;
  TJvDrawPageEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect; var NeedMorePages:boolean) of object;
  TJvCustomPreviewDoc = class;

  IJvPrinter = interface
    ['{FDCCB7CD-8DF7-48B9-9924-CE439AE97999}']
    procedure SetTitle(const Value:string);
    function GetTitle:string;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    function GetAborted: boolean;
    function GetCanvas: TCanvas;
    function GetPageWidth: integer;
    function GetPageHeight: integer;
    function GetPrinting: boolean;
  end;

  TJvDeviceInfo = class(TPersistent)
  private
    FPageHeight: Cardinal;
    FOffsetTop: Cardinal;
    FOffsetLeft: Cardinal;
    FLogPixelsY: Cardinal;
    FPageWidth: Cardinal;
    FLogPixelsX: Cardinal;
    FOnChange: TNotifyEvent;
    FScreenDC, FReferenceHandle: HDC;
    FPhysicalHeight: Cardinal;
    FPhysicalWidth: Cardinal;
    FOffsetBottom: Cardinal;
    FOffsetRight: Cardinal;
    procedure SetLogPixelsY(const Value: Cardinal);
    procedure SetLogPixesX(const Value: Cardinal);
    procedure SetOffsetLeft(const Value: Cardinal);
    procedure SetOffsetTop(const Value: Cardinal);
    procedure SetPageHeight(const Value: Cardinal);
    procedure SetPageWidth(const Value: Cardinal);
    procedure SetupDeviceInfo;
    procedure SetReferenceHandle(const Value: HDC);
    procedure SetPhysicalHeight(const Value: Cardinal);
    procedure SetPhysicalWidth(const Value: Cardinal);
    procedure SetOffsetBottom(const Value: Cardinal);
    procedure SetOffsetRight(const Value: Cardinal);
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    function XPxToInch(Pixels:integer):single;
    function YPxToInch(Pixels:integer):single;
    function XPxToMM(Pixels:integer):single;
    function YPxToMM(Pixels:integer):single;
    function InchToXPx(Inch:single):integer;
    function InchToYPx(Inch:single):integer;
    function MMToXPx(MM:single):integer;
    function MMToYPx(MM:single):integer;

    property ReferenceHandle: HDC read FReferenceHandle write SetReferenceHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LogPixelsX: Cardinal read FLogPixelsX write SetLogPixesX;
    property LogPixelsY: Cardinal read FLogPixelsY write SetLogPixelsY;
    property PhysicalWidth: Cardinal read FPhysicalWidth write SetPhysicalWidth;
    property PhysicalHeight: Cardinal read FPhysicalHeight write SetPhysicalHeight;
    property PageWidth: Cardinal read FPageWidth write SetPageWidth;
    property PageHeight: Cardinal read FPageHeight write SetPageHeight;
    property OffsetLeft: Cardinal read FOffsetLeft write SetOffsetLeft;
    property OffsetTop: Cardinal read FOffsetTop write SetOffsetTop;
    property OffsetRight: Cardinal read FOffsetRight write SetOffsetRight;
    property OffsetBottom: Cardinal read FOffsetBottom write SetOffsetBottom;
  end;

  TJvPageShadow = class(TPersistent)
  private
    FOffset: integer;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: integer);
    procedure Change;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Offset: integer read FOffset write SetOffset default 4;
  end;

  TJvPreviewPageOptions = class(TPersistent)
  private
    FVertSpacing: Cardinal;
    FHorzSpacing: Cardinal;
    FColor: TColor;
    FShadow: TJvPageShadow;
    FOnChange: TNotifyEvent;
    FDrawMargins: boolean;
    FCols: Cardinal;
    FScale: Cardinal;
    FRows: Cardinal;
    FScaleMode: TJvPreviewScaleMode;
    FOnScaleModeChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetHorzSpacing(const Value: Cardinal);
    procedure SetVertSpacing(const Value: Cardinal);
    procedure DoShadowChange(Sender: TObject);
    procedure SetDrawMargins(const Value: boolean);
    procedure SetCols(const Value: Cardinal);
    procedure SetShadow(const Value: TJvPageShadow);
    procedure SetScale(const Value: Cardinal);
    procedure SetRows(const Value: Cardinal);
    procedure Change;
    procedure ScaleModeChange;
    
    function GetCols: Cardinal;
    function GetRows: Cardinal;
    function GetHorzSpacing: Cardinal;
    function GetVertSpacing: Cardinal;
    procedure SetScaleMode(const Value: TJvPreviewScaleMode);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScaleModeChange: TNotifyEvent read FOnScaleModeChange write FOnScaleModeChange;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Cols: Cardinal read GetCols write SetCols default 1;
    property DrawMargins: boolean read FDrawMargins write SetDrawMargins default false;
    property HorzSpacing: Cardinal read GetHorzSpacing write SetHorzSpacing default 8;
    property Rows: Cardinal read GetRows write SetRows;
    property Shadow: TJvPageShadow read FShadow write SetShadow;
    property VertSpacing: Cardinal read GetVertSpacing write SetVertSpacing default 8;
    property Scale: Cardinal read FScale write SetScale default 100;
    property ScaleMode:TJvPreviewScaleMode read FScaleMode write SetScaleMode default smFullPage;
  end;

  TJvCustomPreviewDoc = class(TCustomControl)
  private
    FBuffer:TBitmap;
    FOptions: TJvPreviewPageOptions;
    FPages: TList;
    FScrollPos:TPoint;
    FOnDrawPreviewPage: TJvDrawPreviewEvent;
    FBorderStyle: TBorderStyle;
    FDeviceInfo: TJvDeviceInfo;
    FOnAddPage: TJvDrawPageEvent;
    FSelectedPage: integer;
    FOnChange: TNotifyEvent;
    FUpdateCount:integer;
    procedure DoOptionsChange(Sender: TObject);
    procedure DoScaleModeChange(Sender: TObject);
    function GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
    procedure DrawPreview(PageIndex: integer; APageRect, APrintRect: TRect);

    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure CalcScrollRange;
    function GetPage(Index: integer): TMetaFile;
    function GetPageCount: integer;
    procedure SetDeviceInfo(const Value: TJvDeviceInfo);
    procedure SetOptions(const Value: TJvPreviewPageOptions);
    function GetPreviewPageWidth: Cardinal;
    function GetMaxVirtualWidth:Cardinal;
    function GetPreviewPageHeight: Cardinal;
    function GetMaxVirtualHeight:Cardinal;
    function GetPreviewPageOffsetLeft: Cardinal;
    function GetPreviewPageOffsetTop: Cardinal;
    function GetPreviewPageOffsetRight:Cardinal;
    function GetPreviewPageOffsetBottom:Cardinal;
    procedure SetSelectedPage(const Value: integer);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMPreviewAddPage(var Message: TMessage); message WM_PREVIEWADDPAGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    // returns max no of cols at current scale that will fit in client area
    function GetMaxCols:Cardinal;
    // returns max no of rows at current scale that will fit in client area
    function GetMaxRows:integer;
    // returns the optimal scale value using current cols and rows
    function GetOptimalScale:Cardinal;
    function GetScale(AHeight,AWidth:Cardinal):Cardinal;
    procedure Change;dynamic;
  protected

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DoDrawPreviewPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;
    function DoAddPage(AMetaFile: TMetaFile; PageIndex: integer):boolean; dynamic;

    property SelectedPage: integer read FSelectedPage write SetSelectedPage;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkSpace;
    property DeviceInfo: TJvDeviceInfo read FDeviceInfo write SetDeviceInfo;

    property Options: TJvPreviewPageOptions read FOptions write SetOptions;
    property OnAddPage: TJvDrawPageEvent read FOnAddPage write FOnAddPage;
    property OnDrawPreviewPage: TJvDrawPreviewEvent read FOnDrawPreviewPage write FOnDrawPreviewPage;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Updating:boolean;reintroduce;
    function Add: TMetaFile;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure PrintRange(const APrinter:IJvPrinter; StartPage,EndPage,Copies:integer; Collate:boolean);

    property Pages[Index: integer]: TMetaFile read GetPage;
    property PageCount: integer read GetPageCount;
  end;

  TJvPreviewDoc = class(TJvCustomPreviewDoc)
  published
    property SelectedPage;
    property BorderStyle;
    property Color default clAppWorkSpace;
    property DeviceInfo;

    property Options;
    property OnChange;
    property OnAddPage;
    property OnDrawPreviewPage;

    property Align;
    property Anchors;

    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
uses
  FlatSB;
  
function ceil(Value:double):integer;
begin
  Result := trunc(Value);
  if frac(Value) > 0 then
    Inc(Result);
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

function Min(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 < Val1 then
    Result := Val2;
end;

type
  TCanvasStore = class(TInterfacedObject, IUnknown)
  private
    FCanvas, FStoreCanvas: TCanvas;
    procedure RestoreCanvas;
    procedure StoreCanvas;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;
  end;

  { TCanvasStore }

constructor TCanvasStore.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := TCanvas.Create;
  FStoreCanvas := ACanvas;
  StoreCanvas;
end;

destructor TCanvasStore.Destroy;
begin
  RestoreCanvas;
  FCanvas.Free;
  inherited;
end;

procedure TCanvasStore.RestoreCanvas;
begin
  with FStoreCanvas do
  begin
    Brush.Assign(FCanvas.Brush);
    Pen.Assign(FCanvas.Pen);
    Font.Assign(FCanvas.Font);
  end;
end;

procedure TCanvasStore.StoreCanvas;
begin
  with FCanvas do
  begin
    Brush.Assign(FStoreCanvas.Brush);
    Pen.Assign(FStoreCanvas.Pen);
    Font.Assign(FStoreCanvas.Font);
  end;

end;

function StoreCanvas(ACanvas: TCanvas): IUnknown;
begin
  Result := TCanvasStore.Create(ACanvas);
end;

{ TJvPreviewPageOptions }

procedure TJvPreviewPageOptions.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

constructor TJvPreviewPageOptions.Create;
begin
  inherited Create;
  FShadow := TJvPageShadow.Create;
  FShadow.OnChange := DoShadowChange;
  FCols := 1;
  FRows := 1;
  FScale := 100;
  FScaleMode := smFullPage;
  FColor := clWhite;
  FVertSpacing := 8;
  FHorzSpacing := 8;
end;

destructor TJvPreviewPageOptions.Destroy;
begin
  FShadow.Free;
  inherited;
end;

procedure TJvPreviewPageOptions.DoShadowChange(Sender: TObject);
begin
  Change;
end;

function TJvPreviewPageOptions.GetCols: Cardinal;
begin
  Result := Max(FCols, 1);
end;

function TJvPreviewPageOptions.GetHorzSpacing: Cardinal;
begin
  Result := Max(FHorzSpacing,abs(Shadow.Offset));
end;

function TJvPreviewPageOptions.GetRows: Cardinal;
begin
  Result := Max(FRows, 1);
end;

function TJvPreviewPageOptions.GetVertSpacing: Cardinal;
begin
  Result := Max(FVertSpacing,abs(Shadow.Offset));
end;

procedure TJvPreviewPageOptions.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetCols(const Value: Cardinal);
begin
  if FCols <> Value then
  begin
    FCols := Value;
    if FCols < 1 then
      FCols := 1;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetDrawMargins(const Value: boolean);
begin
  if FDrawMargins <> Value then
  begin
    FDrawMargins := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetHorzSpacing(const Value: Cardinal);
begin
  if FHorzSpacing <> Value then
  begin
    FHorzSpacing := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetRows(const Value: Cardinal);
begin
  if FRows <> Value then
  begin
    FRows := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetShadow(const Value: TJvPageShadow);
begin
  FShadow.Assign(Value);
end;

procedure TJvPreviewPageOptions.SetVertSpacing(const Value: Cardinal);
begin
  if FVertSpacing <> Value then
  begin
    FVertSpacing := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetScale(const Value: Cardinal);
begin
  if FScale <> Value then
  begin
    FScale := Max(Value, 1);
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetScaleMode(
  const Value: TJvPreviewScaleMode);
begin
  if FScaleMode <> Value then
  begin
    FScaleMode := Value;
    ScaleModeChange;
  end;
end;

procedure TJvPreviewPageOptions.ScaleModeChange;
begin
  if Assigned(FOnScaleModeChange) then
    FOnScaleModeChange(self)
  else
    Change;
end;

{ TJvPageShadow }

procedure TJvPageShadow.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

constructor TJvPageShadow.Create;
begin
  inherited Create;
  FColor := clBlack;
  FOffset := 4;
end;

procedure TJvPageShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvPageShadow.SetOffset(const Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Change;
  end;
end;

{ TJvCustomPreviewDoc }

function TJvCustomPreviewDoc.Add: TMetaFile;
begin
  repeat
    Result := TMetafile.Create;
    Result.Width := DeviceInfo.PhysicalWidth;
    Result.Height := DeviceInfo.PhysicalHeight;
  // keep adding pages until user says stop
  until not DoAddPage(Result,FPages.Add(Result));
  Change;
  // post a message to ourself in case the user added the page in the OnAddPage event
//  PostMessage(Handle, WM_PREVIEWADDPAGE, integer(Result), FPages.Add(Result));
end;

procedure TJvCustomPreviewDoc.CalcScrollRange;
var
  si: TScrollInfo;
begin
  FillChar(si, SizeOf(TScrollInfo), 0);
  si.cbSize := SizeOf(TScrollInfo);
  si.fMask := SIF_ALL; //  or SIF_DISABLENOSCROLL;

  si.nMin := 0;
  si.nMax := GetMaxVirtualWidth - Options.HorzSpacing;
  if (GetPreviewPageWidth >= ClientWidth) or (GetPreviewPageHeight >= ClientHeight) then
    si.nPage := ClientWidth div 10
  else
    si.nPage := GetPreviewPageWidth + Options.VertSpacing + 1;

  si.nPos := -FScrollPos.X;
  if abs(si.nPos) > si.nMax then
    si.nPos := -si.nMax;
  FlatSB_SetScrollInfo(Handle, SB_HORZ, si, true);
  FlatSB_ShowScrollbar(Handle, SB_HORZ, true);

  FillChar(si, SizeOf(TScrollInfo), 0);
  si.cbSize := SizeOf(TScrollInfo);
  si.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  if (GetPreviewPageWidth >= ClientWidth) or (GetPreviewPageHeight >= ClientHeight) then
    si.nPage := ClientHeight div 10
  else
    si.nPage := GetPreviewPageHeight + Options.VertSpacing;

  si.nPos := -FScrollPos.Y;
  si.nMax := GetMaxVirtualHeight - Options.VertSpacing + 1;
  if abs(si.nPos) > si.nMax then
    si.nPos := -si.nMax;
  FlatSB_SetScrollInfo(Handle, SB_VERT, si, true);
  FlatSB_ShowScrollbar(Handle, SB_VERT, true);
end;

procedure TJvCustomPreviewDoc.Clear;
var i: integer;
begin
  for i := 0 to FPages.Count - 1 do
    TMetaFile(FPages[i]).Free;
  FPages.Count := 0;
end;

procedure TJvCustomPreviewDoc.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

constructor TJvCustomPreviewDoc.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedPage := -1;
  FPages := TList.Create;
  FPages.Capacity := 64;
  FBuffer := TBitmap.Create;

  FOptions := TJvPreviewPageOptions.Create;
  //  FOptions.Width := 210;
  //  FOptions.Height := 297;
  //  FOptions.PrintMargins := Rect(10,10,10,10);
  FOptions.OnChange := DoOptionsChange;
  FOptions.OnScaleModeChange := DoScaleModeChange;

  FDeviceInfo := TJvDeviceInfo.Create;
  FDeviceInfo.OnChange := DoOptionsChange;

  Color := clAppWorkSpace;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Width := 150;
  Height := 250;
  FBorderStyle := bsSingle;
end;

procedure TJvCustomPreviewDoc.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TJvCustomPreviewDoc.Delete(Index: integer);
begin
  TMetaFile(FPages[Index]).Free;
end;

destructor TJvCustomPreviewDoc.Destroy;
begin
  Clear;
  FDeviceInfo.Free;
  FPages.Free;
  FBuffer.Free;
  inherited;
end;

function TJvCustomPreviewDoc.DoAddPage(AMetaFile: TMetaFile;
  PageIndex: integer):boolean;
var ACanvas: TMetaFileCanvas; APageRect, APrintRect: TRect; i: integer;
begin
  Result := false;
  ACanvas := TMetaFileCanvas.Create(AMetaFile, DeviceInfo.ReferenceHandle);
  SetMapMode(ACanvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, DeviceInfo.PhysicalWidth, DeviceInfo.PhysicalHeight, nil);
  SetViewportExtEx(ACanvas.Handle, DeviceInfo.PhysicalWidth, DeviceInfo.PhysicalHeight, nil);
  i := ACanvas.Font.Size;
  ACanvas.Font.PixelsPerInch := DeviceInfo.LogPixelsY;
  ACanvas.Font.Size := i;
  if Assigned(FOnAddPage) then
    with DeviceInfo do
    begin
      APageRect := Rect(0, 0, PhysicalWidth, PhysicalHeight);
      APrintRect := APageRect;
      Inc(APrintRect.Left, OffsetLeft);
      Inc(APrintRect.Top, OffsetTop);
      Dec(APrintRect.Right, OffsetRight);
      Dec(APrintRect.Bottom, OffsetBottom);
      FOnAddPage(self, PageIndex, ACanvas, APageRect, APrintRect,Result);
    end;
  ACanvas.Free; // spool canvas to metafile
end;

procedure TJvCustomPreviewDoc.DoDrawPreviewPage(PageIndex: integer;
  Canvas: TCanvas; PageRect, PrintRect: TRect);
begin
  if Assigned(FOnDrawPreviewPage) then
    FOnDrawPreviewPage(self, PageIndex, Canvas, PageRect, PrintRect);
end;

procedure TJvCustomPreviewDoc.DoOptionsChange(Sender: TObject);
begin
  Change;
end;

procedure TJvCustomPreviewDoc.DrawPreview(PageIndex: integer; APageRect,
  APrintRect: TRect);
begin
  FBuffer.Canvas.StretchDraw(APrintRect, Pages[PageIndex]);
  DoDrawPreviewPage(PageIndex, FBuffer.Canvas, APageRect, APrintRect);
end;

function TJvCustomPreviewDoc.GetPage(Index: integer): TMetaFile;
begin
  Result := TMetaFile(FPages[Index]);
end;

function TJvCustomPreviewDoc.GetPageCount: integer;
begin
  Result := FPages.Count;
end;

function TJvCustomPreviewDoc.GetPreviewPageHeight: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.PageHeight,GetDeviceCaps(DC,LOGPIXELSY),
      DeviceInfo.LogPixelsY),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetLeft: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.OffsetLeft,GetDeviceCaps(DC,LOGPIXELSX),
      DeviceInfo.LogPixelsX),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetTop: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.OffsetTop,GetDeviceCaps(DC,LOGPIXELSY),
      DeviceInfo.LogPixelsY),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewPageWidth: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.PhysicalWidth,GetDeviceCaps(DC,LOGPIXELSX),
      DeviceInfo.LogPixelsX),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
var ACol, ACols, ARows, ARow, AOffsetX, AOffsetY: integer;
begin
  ACol := (PageIndex mod Options.Cols);
  ARow := (PageIndex div Options.Cols);
  ACols := Max(Min(PageCount,Options.Cols),1);
  ARows := Max(Max(Min(PageCount div ACols,Options.Rows),1),ARow) + Ord(PageCount mod ACols <> 0);
{  if (Options.ScaleMode <> smColsRows) then
  begin
    Options.FCols := ACols;
    Options.FRows := ARows;
  end;
}

  APageRect := Rect(0, 0, GetPreviewPageWidth, GetPreviewPageHeight);
  // find top/left edge when sized
  AOffsetX :=
    Max((ClientWidth - (GetPreviewPageWidth + Options.HorzSpacing) * ACols) div 2,FOptions.HorzSpacing);
  AOffsetY :=
    Max((ClientHeight - (GetPreviewPageHeight + Options.VertSpacing) * ARows) div 2,FOptions.VertSpacing);
  OffsetRect(APageRect, AOffsetX + FScrollPos.X + ACol * (FOptions.HorzSpacing + GetPreviewPageWidth),
    AOffsetY + FScrollPos.Y + ARow * (GetPreviewPageHeight + FOptions.VertSpacing));

  APrintRect := APageRect;
  with FOptions do
  begin
    Inc(APrintRect.Left, GetPreviewPageOffsetLeft);
    Inc(APrintRect.Top, GetPreviewPageOffsetTop);
    Dec(APrintRect.Right, GetPreviewPageOffsetRight);
    Dec(APrintRect.Bottom, GetPreviewPageOffsetBottom);
  end;
  //  if EqualRect(APrintRect,APageRect) then
  //    InflateRect(APrintRect,-1,-1);
  Result := not IsRectEmpty(APrintRect);
end;

procedure TJvCustomPreviewDoc.Paint;
var i: integer;
  APageRect, APrintRect: TRect;
function RectInRect(InnerRect,OuterRect:TRect):boolean;
begin
  Result := (InnerRect.Left >= OuterRect.Left)
    and (InnerRect.Right <= OuterRect.Right)
    and (InnerRect.Top >= OuterRect.Top)
    and (InnerRect.Bottom <= OuterRect.Bottom);
end;

begin
  if Updating then Exit;
  //  StoreCanvas(FBuffer.Canvas);
  with FBuffer.Canvas do
  begin
    Brush.Color := Color;
    FillRect(ClientRect);
    // draw at least one page (even if it's empty)
    for i := 0 to Max(PageCount, 1) - 1 do
    begin
      if GetPreviewRects(i, APageRect, APrintRect) then
      begin
        if (APageRect.Right <= 0) or (APageRect.Left >= ClientWidth) or
          (APageRect.Bottom <= 0) or (APageRect.Top >= ClientHeight) then
          Continue // outside view
        else if (PageCount > 0) and not RectInRect(APageRect,ClientRect)
        and not  (Options.ScaleMode in [smPageWidth]) then
        begin
          if (APageRect.Right > ClientWidth) or (APageRect.Left < 0) or
            (APageRect.Bottom > ClientHeight) or (APageRect.Top < 0) then
          Continue; // some part is outside view
        end;

        // draw shadow
        if (Options.Shadow.Offset <> 0) then
        begin
          OffsetRect(APageRect, Options.Shadow.Offset, Options.Shadow.Offset);
          Brush.Color := Options.Shadow.Color;
          FillRect(APageRect);
          OffsetRect(APageRect, -Options.Shadow.Offset, -Options.Shadow.Offset);
        end;

        // draw background
        Brush.Color := Options.Color;
        FillRect(APageRect);
        // draw preview content
        if i < PageCount then
          DrawPreview(i, APageRect, APrintRect);
        // draw frame
        Brush.Style := bsClear;
        Pen.Color := clWindowText;
        Rectangle(APageRect);
        if i = FSelectedPage then
        begin
          Pen.Color := clNavy;
          Pen.Width := 2;
          Rectangle(APageRect);
          Pen.Color := clWindowText;
          Pen.Width := 1;
        end;
        // draw margins
        if Options.DrawMargins and not EqualRect(APageRect, APrintRect) then
        begin
          Pen.Style := psDot;
          Rectangle(APrintRect);
          Pen.Style := psSolid;
        end;
        Brush.Style := bsSolid;
      end;
    end;
  end;
  BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FBuffer.Canvas.Handle,
    0, 0, SRCCOPY);
end;

procedure TJvCustomPreviewDoc.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomPreviewDoc.SetSelectedPage(const Value: integer);
begin
  if FSelectedPage <> Value then
  begin
    FSelectedPage := Min(Max(Value,-1),PageCount-1);
    Invalidate;
  end;
end;

procedure TJvCustomPreviewDoc.SetDeviceInfo(const Value: TJvDeviceInfo);
begin
  FDeviceInfo.Assign(Value);
end;

procedure TJvCustomPreviewDoc.SetOptions(
  const Value: TJvPreviewPageOptions);
begin
  FOptions.Assign(Value);
end;

procedure TJvCustomPreviewDoc.WMPreviewAddPage(var Message: TMessage);
begin
  with Message do
    DoAddPage(TMetaFile(wParam), lParam);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
//  inherited;
  Message.Result := 1;
end;

procedure TJvCustomPreviewDoc.WMSize(var Message: TWMSize);
begin
  inherited;
  Change;
end;


procedure TJvCustomPreviewDoc.WMHScroll(var Msg: TWMHScroll);
var si:TScrollInfo;tmp:integer;
begin
  si.cbSize := sizeof(si);
  si.fMask := SIF_ALL;
  // TODO: check range!
  FlatSB_GetScrollInfo(Handle,SB_HORZ,si);
  case Msg.ScrollCode of
    SB_TOP:
    begin
      ScrollBy(-si.nPos,0);
      FScrollPos.X := 0;
    end;
    SB_BOTTOM:
    begin
      ScrollBy(-si.nPos + si.nMax,0);
      FScrollPos.X := si.nMax;
    end;
    SB_LINEDOWN,SB_PAGEDOWN:
    begin
      if si.nPos + si.nPage <= si.nMax then
      begin
        ScrollBy(-si.nPage,0);
        FScrollPos.X := FScrollPos.X - si.nPage;
      end;
    end;
    SB_LINEUP,SB_PAGEUP:
    begin
      if si.nPos >= si.nPage then
      begin
        ScrollBy(si.nPage,0);
        FScrollPos.X := FScrollPos.X + si.nPage;
      end;
    end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      tmp := Msg.Pos div si.nPage * si.nPage;
      ScrollBy(-si.nPos-tmp,0);
      FScrollPos.Y := -tmp;
    end;
  end;
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.WMVScroll(var Msg: TWMVScroll);
var si:TSCrollInfo;tmp:integer;
begin
  si.cbSize := sizeof(si);
  si.fMask := SIF_ALL;
  FlatSB_GetScrollInfo(Handle,SB_VERT,si);
  case Msg.ScrollCode of
    SB_TOP:
    begin
      ScrollBy(0,-si.nPos);
      FScrollPos.Y := 0;
    end;
    SB_BOTTOM:
    begin
      ScrollBy(0,-si.nPos + si.nMax);
      FScrollPos.Y := si.nMax;
    end;
    SB_LINEDOWN,SB_PAGEDOWN:
    begin
      if si.nPos + si.nPage <= si.nMax then
      begin
        ScrollBy(0,-si.nPage);
        FScrollPos.Y := FScrollPos.Y - si.nPage;
      end;
    end;
    SB_LINEUP,SB_PAGEUP:
    begin
      if si.nPos >= si.nPage then
      begin
        ScrollBy(0,si.nPage);
        FScrollPos.Y := FScrollPos.Y + si.nPage;
      end;
    end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      tmp := Msg.Pos div si.nPage * si.nPage;
      ScrollBy(0,-si.nPos-tmp);
      FScrollPos.Y := -tmp;
    end;
  end;
  CalcScrollRange;
  Invalidate;
end;

function TJvCustomPreviewDoc.GetMaxVirtualHeight: Cardinal;
begin
  Result := Max(ceil(PageCount / Max(Min(PageCount,Options.Cols),1)),1);
  Result := Result * (GetPreviewPageHeight + Options.VertSpacing) + Options.VertSpacing;
end;

function TJvCustomPreviewDoc.GetMaxVirtualWidth: Cardinal;
begin
  Result := Max(Min(PageCount,Options.Cols),1);
  Result := Result * (GetPreviewPageWidth + Options.HorzSpacing) + Options.HorzSpacing;
end;

procedure TJvCustomPreviewDoc.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

procedure TJvCustomPreviewDoc.PrintRange(const APrinter: IJvPrinter;
  StartPage, EndPage, Copies: integer; Collate: boolean);
var i,j:integer;
begin
  if (APrinter = nil) or APrinter.GetPrinting then Exit;
  if StartPage < 0 then
    StartPage := PageCount - 1;
  if EndPage < 0 then
    EndPage := PageCount - 1;
  if Copies < 1 then
    Copies := 1;
  if Collate then // Range * Copies
  begin
    if StartPage > EndPage then
    begin
      // print backwards
      for i := 0 to Copies - 1 do
        for j := StartPage downto EndPage do
        begin
          if APrinter.GetAborted then
          begin
            if APrinter.GetPrinting then
              APrinter.EndDoc;
            Exit;
          end;
          if (j = StartPage) and (i = 0) then
            APrinter.BeginDoc;
          APrinter.NewPage;
          APrinter.GetCanvas.Draw(0,0,Pages[j]);
        end;
    end
    else
    begin
      for i := 0 to Copies - 1 do
        for j := StartPage to EndPage do
        begin
          if APrinter.GetAborted then
          begin
            if APrinter.GetPrinting then
              APrinter.EndDoc;
            Exit;
          end;
          if (j = StartPage) and (i = 0) then
            APrinter.BeginDoc;
          APrinter.NewPage;
          APrinter.GetCanvas.Draw(0,0,Pages[j]);
        end;
    end;
  end
  else // Page * Copies
  begin
    if StartPage > EndPage then
    begin
      // print backwards
      for j := StartPage downto EndPage do
        for i := 0 to Copies - 1 do
        begin
          if APrinter.GetAborted then
          begin
            if APrinter.GetPrinting then
              APrinter.EndDoc;
            Exit;
          end;
          if (j = StartPage) and (i = 0) then
            APrinter.BeginDoc;
          APrinter.NewPage;
          APrinter.GetCanvas.Draw(0,0,Pages[j]);
        end;
    end
    else
    begin
      for j := StartPage to EndPage do
        for i := 0 to Copies - 1 do
        begin
          if APrinter.GetAborted then
          begin
            if APrinter.GetPrinting then
              APrinter.EndDoc;
            Exit;
          end;
          if (j = StartPage) and (i = 0) then
            APrinter.BeginDoc;
          APrinter.NewPage;
          APrinter.GetCanvas.Draw(0,0,Pages[j]);
        end;
    end;
  end;
  if APrinter.GetPrinting then
    APrinter.EndDoc;
end;

function TJvCustomPreviewDoc.GetMaxCols: Cardinal;
begin
  Result := Max((ClientWidth - Options.HorzSpacing) div (GetPreviewPageWidth + Options.HorzSpacing),1);
end;

function TJvCustomPreviewDoc.GetMaxRows: integer;
begin
  Result := Max((ClientHeight - Options.VertSpacing) div (GetPreviewPageHeight + Options.VertSpacing),1);
end;

function TJvCustomPreviewDoc.GetOptimalScale: Cardinal;
var Val1,Val2:integer;
    ACols,ARows:integer;
begin
  ACols := Max(Min(PageCount,Options.Cols),1);
  ARows := Max(Min(PageCount,Options.Rows),1);
  Val1 := (ClientHeight - Options.VertSpacing) div ARows - Options.VertSpacing * 3 div 2;
  Val2  := (ClientWidth - Options.HorzSpacing) div ACols - Options.HorzSpacing * 3 div 2;
  Result := GetScale(Val1,Val2);
end;

procedure TJvCustomPreviewDoc.Change;
begin
  if Updating then Exit;
  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;
  case Options.ScaleMode of
    smFullPage:
    begin
      Options.FCols := 1;
      Options.FRows := 1;
      Options.FScale := GetOptimalScale;
    end;
    smPageWidth:
    begin
      Options.FCols := 1;
      Options.FRows := 1;
      Options.FScale := GetScale(0,ClientWidth - Options.HorzSpacing * 2);
    end;
    smScale:
    begin
      Options.FCols := GetMaxCols;
      Options.FRows := GetMaxRows;
    end;
    smColsRows:
      Options.FScale := GetOptimalScale;
  end;
  CalcScrollRange;
  if Assigned(FOnChange) then FOnChange(self); 
  Invalidate;
end;

procedure TJvCustomPreviewDoc.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCustomPreviewDoc.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then Change;
  if FUpdateCount < 0 then FUpdateCount := 0;
end;

function TJvCustomPreviewDoc.Updating: boolean;
begin
  Result := FUpdateCount <> 0;
end;

function TJvCustomPreviewDoc.GetScale(AHeight, AWidth: Cardinal): Cardinal;
var DC:HDC;
begin
  // determine scale factor for both sides, choose lesser
  // this is the opposite of GetPreviewPageWidth/Height
  DC := GetDC(0);
  try
    if AWidth > 0 then
      AWidth := MulDiv(AWidth,100,MulDiv(DeviceInfo.PhysicalWidth,
        GetDeviceCaps(DC,LOGPIXELSX),DeviceInfo.LogPixelsX));
    if AHeight > 0 then
      AHeight := MulDiv(AHeight,100,MulDiv(DeviceInfo.PhysicalHeight,
        GetDeviceCaps(DC,LOGPIXELSY),DeviceInfo.LogPixelsY));
    if (AHeight > 0) and (AWidth > 0) then
      Result := Min(AWidth,AHeight)
    else if AHeight > 0 then
      Result := AHeight
    else
      Result := AWidth;
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetBottom: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.OffsetBottom,GetDeviceCaps(DC,LOGPIXELSY),
      DeviceInfo.LogPixelsY),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetRight: Cardinal;
var DC:HDC;
begin
  DC := GetDC(0);
  try
    Result := MulDiv(MulDiv(DeviceInfo.OffsetRight,GetDeviceCaps(DC,LOGPIXELSX),
      DeviceInfo.LogPixelsX),Options.Scale,100);
  finally
    ReleaseDC(0,DC);
  end;
end;

procedure TJvCustomPreviewDoc.DoScaleModeChange(Sender: TObject);
begin
  FScrollPos.X := 0;
  FScrollPos.Y := 0;
  FlatSB_SetScrollPos(Handle,SB_HORZ,0,true);
  FlatSB_SetScrollPos(Handle,SB_VERT,0,true);
  CalcScrollRange;
  Change;
end;

{ TJvDeviceInfo }

procedure TJvDeviceInfo.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

constructor TJvDeviceInfo.Create;
begin
  inherited Create;
  SetupDeviceInfo;
end;

destructor TJvDeviceInfo.Destroy;
begin
  if FScreenDC <> 0 then
    ReleaseDC(0, FScreenDC);
  inherited;
end;

function TJvDeviceInfo.InchToXPx(Inch: single): integer;
begin
  Result := round(Inch * LogPixelsY);
end;

function TJvDeviceInfo.InchToYPx(Inch: single): integer;
begin
  Result := round(Inch * LogPixelsX);
end;

function TJvDeviceInfo.MMToXPx(MM: single): integer;
begin
  Result := InchToXPx(MM * 25.4);
end;

function TJvDeviceInfo.MMToYPx(MM: single): integer;
begin
  Result := InchToYPx(MM * 25.4);
end;

procedure TJvDeviceInfo.SetLogPixelsY(const Value: Cardinal);
begin
  if FLogPixelsY <> Value then
  begin
    FLogPixelsY := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetLogPixesX(const Value: Cardinal);
begin
  if FLogPixelsX <> Value then
  begin
    FLogPixelsX := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetOffsetBottom(const Value: Cardinal);
begin
  if FOffsetBottom <> Value then
  begin
    FOffsetBottom := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetOffsetRight(const Value: Cardinal);
begin
  if FOffsetRight <> Value then
  begin
    FOffsetRight := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetOffsetLeft(const Value: Cardinal);
begin
  if FOffsetLeft <> Value then
  begin
    FOffsetLeft := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetOffsetTop(const Value: Cardinal);
begin
  if FOffsetTop <> Value then
  begin
    FOffsetTop := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetPageHeight(const Value: Cardinal);
begin
  if FPageHeight <> Value then
  begin
    FPageHeight := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetPageWidth(const Value: Cardinal);
begin
  if FPageWidth <> Value then
  begin
    FPageWidth := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetPhysicalHeight(const Value: Cardinal);
begin
  if FPhysicalHeight <> Value then
  begin
    FPhysicalHeight := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetPhysicalWidth(const Value: Cardinal);
begin
  if FPhysicalWidth <> Value then
  begin
    FPhysicalWidth := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetReferenceHandle(const Value: HDC);
begin
  FReferenceHandle := Value;
  if FReferenceHandle = 0 then
    FReferenceHandle := FScreenDC;
  FLogPixelsX := GetDeviceCaps(FReferenceHandle, Windows.LOGPIXELSX);
  FLogPixelsY := GetDeviceCaps(FReferenceHandle, Windows.LOGPIXELSY);
  FPageWidth := GetDeviceCaps(FReferenceHandle, HORZRES);
  FPageHeight := GetDeviceCaps(FReferenceHandle, VERTRES);
  FPhysicalWidth := Max(GetDeviceCaps(FReferenceHandle, Windows.PHYSICALWIDTH), FPageWidth);
  FPhysicalHeight := Max(GetDeviceCaps(FReferenceHandle, Windows.PHYSICALHEIGHT), FPageHeight);
  FOffsetLeft := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETX);
  FOffsetTop := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETY);
  FOffsetRight := Max(FPhysicalWidth - FPageWidth - FOffsetLeft, 0);
  FOffsetBottom := Max(FPhysicalHeight - FPageHeight - FOffsetTop, 0);
  Change;
end;

procedure TJvDeviceInfo.SetupDeviceInfo;
begin
  if FScreenDC = 0 then
    FScreenDC := GetDC(0);
  SetReferenceHandle(FScreenDC);
end;

function TJvDeviceInfo.XPxToInch(Pixels: integer): single;
begin
  Result := Pixels / LogPixelsX;
end;

function TJvDeviceInfo.XPxToMM(Pixels: integer): single;
begin
  Result := XPxToInch(Pixels) / 25.4;
end;

function TJvDeviceInfo.YPxToInch(Pixels: integer): single;
begin
  Result := Pixels / LogPixelsY;
end;

function TJvDeviceInfo.YPxToMM(Pixels: integer): single;
begin
  Result := YPxToInch(Pixels) / 25.4;
end;

end.

