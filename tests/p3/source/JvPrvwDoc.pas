{$I JVCL.INC}
unit JvPrvwDoc;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;
const
  WM_PREVIEWADDPAGE = WM_USER + 1001;

type
  TJvDrawPageEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect) of object;
  TJvCustomPreviewDoc = class;
  IJvPrinter = interface
    ['{FDCCB7CD-8DF7-48B9-9924-CE439AE97999}']
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    function GetAborted:boolean;
    function GetCanvas: TCanvas;
    function GetPageWidth: integer;
    function GetPageHeight: integer;
    function GetPrinting:boolean;
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
    procedure SetOffsetX(const Value: Cardinal);
    procedure SetOffsetY(const Value: Cardinal);
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
    property ReferenceHandle: HDC read FReferenceHandle write SetReferenceHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LogPixelsX: Cardinal read FLogPixelsX write SetLogPixesX;
    property LogPixelsY: Cardinal read FLogPixelsY write SetLogPixelsY;
    property PhysicalWidth:Cardinal read FPhysicalWidth write SetPhysicalWidth;
    property PhysicalHeight:Cardinal read FPhysicalHeight write SetPhysicalHeight;
    property PageWidth: Cardinal read FPageWidth write SetPageWidth;
    property PageHeight: Cardinal read FPageHeight write SetPageHeight;
    property OffsetLeft: Cardinal read FOffsetLeft write SetOffsetX;
    property OffsetTop: Cardinal read FOffsetTop write SetOffsetY;
    property OffsetRight:Cardinal read FOffsetRight write SetOffsetRight;
    property OffsetBottom:Cardinal read FOffsetBottom write SetOffsetBottom;
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
    FCount: Cardinal;
    FHorzSpacing: Cardinal;
    FColor: TColor;
    FShadow: TJvPageShadow;
    FOnChange: TNotifyEvent;
    FDrawMargins: boolean;
    FCols: Cardinal;
    FZoom: Cardinal;
    procedure SetColor(const Value: TColor);
    procedure SetCount(const Value: Cardinal);
    procedure SetHorzSpacing(const Value: Cardinal);
    procedure SetVertSpacing(const Value: Cardinal);
    procedure Change;
    procedure DoShadowChange(Sender: TObject);
    procedure SetDrawMargins(const Value: boolean);
    procedure SetCols(const Value: Cardinal);
    procedure SetShadow(const Value: TJvPageShadow);
    procedure SetZoom(const Value: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Columns: Cardinal read FCols write SetCols default 1;
    property Count: Cardinal read FCount write SetCount default 1;
    property DrawMargins: boolean read FDrawMargins write SetDrawMargins default false;
    property HorzSpacing: Cardinal read FHorzSpacing write SetHorzSpacing default 25;
    property Shadow: TJvPageShadow read FShadow write SetShadow;
    property VertSpacing: Cardinal read FVertSpacing write SetVertSpacing default 25;
    property Zoom: Cardinal read FZoom write SetZoom default 100;
  end;


  TJvCustomPreviewDoc = class(TScrollingWinControl)
  private
    FOptions: TJvPreviewPageOptions;
    FCanvas: TControlCanvas;
    FPages: TList;
    FOnDrawPreviewPage: TJvDrawPageEvent;
    FBorderStyle: TBorderStyle;
    FAutoScroll: boolean;
    FDeviceInfo: TJvDeviceInfo;
    FOnAddPage: TJvDrawPageEvent;
    FCurrentPage: integer;
    procedure DoOptionsChange(Sender: TObject);
    procedure DoDeviceInfoChange(Sender: TObject);
    function GetShadowRect(PageIndex: integer; var AShadowRect: TRect): boolean;
    function GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
    procedure DrawPreviewShadow(ARect: TRect);
    procedure DrawPreviewBack(APageRect, APrintRect: TRect);
    procedure DrawPreview(PageIndex: integer; APageRect, APrintRect: TRect);
    procedure DrawPreviewMargins(APageRect,APrintRect: TRect);

    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetAutoScroll(const Value: boolean);
    procedure CalcScrollRange;
    function GetPage(Index: integer): TMetaFile;
    function GetPageCount: integer;
    procedure SetDeviceInfo(const Value: TJvDeviceInfo);
    procedure SetOptions(const Value: TJvPreviewPageOptions);
    function GetPreviewPageWidth: Cardinal;
    function GetPreviewPageHeight: Cardinal;
    function GetPreviewPageOffsetX: Cardinal;
    function GetPreviewPageOffsetY: Cardinal;
    procedure SetCurrentPage(const Value: integer);
    procedure WmPreviewAddPage(var Message:TMessage);message WM_PREVIEWADDPAGE;
  protected

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure DoDrawPreviewPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;
    procedure DoAddPage(AMetaFile: TMetaFile; PageIndex: integer); dynamic;


    property CurrentPage: integer read FCurrentPage write SetCurrentPage;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkSpace;
    property AutoScroll: boolean read FAutoScroll write SetAutoScroll default true;
    property DeviceInfo: TJvDeviceInfo read FDeviceInfo write SetDeviceInfo;

    property Options: TJvPreviewPageOptions read FOptions write SetOptions;
    property OnAddPage: TJvDrawPageEvent read FOnAddPage write FOnAddPage;
    property OnDrawPreviewPage: TJvDrawPageEvent read FOnDrawPreviewPage write FOnDrawPreviewPage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add: TMetaFile;
    procedure Delete(Index: integer);
    procedure Clear;

    property Pages[Index: integer]: TMetaFile read GetPage;
    property PageCount: integer read GetPageCount;
  end;

  TJvPreviewDoc = class(TJvCustomPreviewDoc)
  published
    property CurrentPage;
    property BorderStyle;
    property Color default clAppWorkSpace;
    property AutoScroll;
    property DeviceInfo;

    property Options;
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
const
  cZoomDivFactor = 300;

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
  FCount := 1;
  FCols := 1;
  FZoom := 100;
  FColor := clWhite;
  FVertSpacing := 25;
  FHorzSpacing := 25;
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

procedure TJvPreviewPageOptions.SetCount(const Value: Cardinal);
begin
  if (FCount <> Value) then
  begin
    FCount := Max(Value, 1);
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

procedure TJvPreviewPageOptions.SetZoom(const Value: Cardinal);
begin
  if FZoom <> Value then
  begin
    FZoom := Max(Value, 1);
    Change;
  end;
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
  Result := TMetafile.Create;
  Result.Width := DeviceInfo.PageWidth;
  Result.Height := DeviceInfo.PageHeight;
  // post a message to ourself in case the user added the page in the OnAddPage event
//  DoAddPage(Result,FPages.Add(Result));
  PostMessage(Handle,WM_PREVIEWADDPAGE,integer(Result),FPages.Add(Result));
end;

procedure TJvCustomPreviewDoc.CalcScrollRange;
var AVisibelCols, AVisibelRows: integer;
begin
  HandleNeeded;
  if HandleAllocated then
  begin
    // at least one col/row but not more than PageCount - 1
    AVisibelCols := Max(Min(Min(Options.Columns, Options.Count), PageCount),1);
    AVisibelRows := Max(Min(Options.Count,PageCount),1) div AVisibelCols + Options.Count mod AVisibelCols;
    HorzScrollBar.Range := (GetPreviewPageWidth + FOptions.HorzSpacing) * AVisibelCols + FOptions.HorzSpacing;
    VertScrollBar.Range := (GetPreviewPageHeight + FOptions.VertSpacing) * AVisibelRows + FOptions.VertSpacing;
{    if Options.Count <= 1 then
    begin
      if not HorzScrollBar.Smooth then
        HorzScrollBar.Increment := 1;
      if not VertScrollBar.Smooth then
        VertScrollBar.Increment := 1;
    end
    else
    begin
      if not HorzScrollBar.Smooth then
        HorzScrollBar.Increment := GetPreviewPageWidth + FOptions.HorzSpacing;
      if not VertScrollBar.Smooth then
        VertScrollBar.Increment := GetPreviewPageHeight + FOptions.VertSpacing;
    end;}
  end;
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
  FPages := TList.Create;
  FPages.Capacity := 64;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := self;

  FOptions := TJvPreviewPageOptions.Create;
//  FOptions.Width := 210;
//  FOptions.Height := 297;
//  FOptions.PrintMargins := Rect(10,10,10,10);
  FOptions.OnChange := DoOptionsChange;

  FDeviceInfo := TJvDeviceInfo.Create;
  FDeviceInfo.OnChange := DoDeviceInfoChange;


  Color := clAppWorkSpace;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Width := 150;
  Height := 250;
  FBorderStyle := bsSingle;
  inherited AutoScroll := false;
  FAutoScroll := true;
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
  FCanvas.Free;
  inherited;
end;

procedure TJvCustomPreviewDoc.DoAddPage(AMetaFile: TMetaFile;
  PageIndex: integer);
var ACanvas: TMetaFileCanvas; APageRect, APrintRect: TRect;i:integer;
begin
  ACanvas := TMetaFileCanvas.Create(AMetaFile, DeviceInfo.ReferenceHandle);
  SetMapMode(ACanvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, DeviceInfo.PhysicalWidth, DeviceInfo.PhysicalHeight, nil);
  SetViewportExtEx(ACanvas.Handle, DeviceInfo.PageWidth, DeviceInfo.PageHeight, nil);
  i := ACanvas.Font.Size;
  ACanvas.Font.PixelsPerInch := DeviceInfo.LogPixelsY;
  ACanvas.Font.Size := i;
  if Assigned(FOnAddPage) then
    with DeviceInfo do
    begin
      APageRect := Rect(0, 0, PhysicalWidth, PhysicalHeight);
      APrintRect := APageRect;
      Inc(APrintRect.Left, OffsetLeft);
      Inc(APrintRect.Top,OffsetTop);
      Dec(APrintRect.Right,OffsetRight);
      Dec(APrintRect.Bottom,OffsetBottom);
      FOnAddPage(self, PageIndex, ACanvas, APageRect, APrintRect);
    end;
  ACanvas.Free; // spool canvas to metafile
end;

procedure TJvCustomPreviewDoc.DoDeviceInfoChange(Sender: TObject);
begin
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.DoDrawPreviewPage(PageIndex: integer;
  Canvas: TCanvas; PageRect, PrintRect: TRect);
begin
  if Assigned(FOnDrawPreviewPage) then
    FOnDrawPreviewPage(self, PageIndex, Canvas, PageRect, PrintRect);
end;

procedure TJvCustomPreviewDoc.DoOptionsChange(Sender: TObject);
begin
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.DrawPreview(PageIndex: integer; APageRect,
  APrintRect: TRect);
begin
  FCanvas.StretchDraw(APrintRect, Pages[PageIndex]);
  DoDrawPreviewPage(PageIndex,FCanvas,APageRect,APrintRect);
end;

procedure TJvCustomPreviewDoc.DrawPreviewBack(APageRect, APrintRect: TRect);
begin
  StoreCanvas(FCanvas);
  FCanvas.Brush.Color := FOptions.Color;
  FCanvas.Pen.Color := clWindowText;
  FCanvas.Rectangle(APageRect);
end;

procedure TJvCustomPreviewDoc.DrawPreviewMargins(APageRect,APrintRect: TRect);
begin
  if FOptions.DrawMargins and not EqualRect(APageRect, APrintRect) then
  begin
    StoreCanvas(FCanvas);
    FCanvas.Brush.Style := bsClear;
    FCanvas.Pen.Style := psDot;
    FCanvas.Rectangle(APrintRect);
  end;
end;

procedure TJvCustomPreviewDoc.DrawPreviewShadow(ARect: TRect);
begin
  StoreCanvas(FCanvas);
  FCanvas.Brush.Color := FOptions.Shadow.Color;
  FCanvas.FillRect(ARect);
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
begin
  Result := round(DeviceInfo.PageHeight * Options.Zoom / cZoomDivFactor);
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetX: Cardinal;
begin
  Result := round(DeviceInfo.OffsetLeft * Options.Zoom / cZoomDivFactor);
end;

function TJvCustomPreviewDoc.GetPreviewPageOffsetY: Cardinal;
begin
  Result := round(DeviceInfo.OffsetTop * Options.Zoom / cZoomDivFactor);
end;

function TJvCustomPreviewDoc.GetPreviewPageWidth: Cardinal;
begin
  Result := round(DeviceInfo.PageWidth * Options.Zoom / cZoomDivFactor);
end;

function TJvCustomPreviewDoc.GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
var ACol, ARow: integer;
begin
  ACol := (PageIndex mod Options.Columns);
  ARow := (PageIndex div Options.Columns);
  APageRect := Rect(0, 0, GetPreviewPageWidth, GetPreviewPageHeight);
  OffsetRect(APageRect, FOptions.HorzSpacing, FOptions.VertSpacing);
  OffsetRect(APageRect, ACol * (FOptions.HorzSpacing + GetPreviewPageWidth),
    ARow * (GetPreviewPageHeight + FOptions.VertSpacing));
  OffsetRect(APageRect, -HorzScrollbar.Position, -VertScrollBar.Position);

  APrintRect := APageRect;
  with FOptions do
  begin
    Inc(APrintRect.Left, GetPreviewPageOffsetX);
    Inc(APrintRect.Top, GetPreviewPageOffsetY);
    Dec(APrintRect.Right, GetPreviewPageOffsetX);
    Dec(APrintRect.Bottom, GetPreviewPageOffsetY);
  end;
//  if EqualRect(APrintRect,APageRect) then
//    InflateRect(APrintRect,-1,-1);
  Result := not IsRectEmpty(APrintRect);
end;

function TJvCustomPreviewDoc.GetShadowRect(PageIndex: integer; var AShadowRect: TRect): boolean;
var ACol, ARow: integer;
begin
  Result := false;
  if (FOptions.Shadow.Offset <> 0) then
  begin
    ACol := (PageIndex mod Options.Columns);
    ARow := (PageIndex div Options.Columns);
    AShadowRect := Rect(0, 0, GetPreviewPageWidth, GetPreviewPageHeight);
    OffsetRect(AShadowRect, FOptions.HorzSpacing, FOptions.VertSpacing);
    OffsetRect(AShadowRect, ACol * (GetPreviewPageWidth + FOptions.HorzSpacing),
      ARow * (GetPreviewPageHeight + FOptions.VertSpacing));
    OffsetRect(AShadowRect, FOptions.Shadow.Offset, FOptions.Shadow.Offset);
    OffsetRect(AShadowRect, -HorzScrollbar.Position, -VertScrollBar.Position);
    Result := true;
  end
  else
    AShadowRect := Rect(0, 0, 0, 0);
end;


procedure TJvCustomPreviewDoc.Paint;
var i: integer;
  APageRect, APrintRect: TRect;
begin
  StoreCanvas(FCanvas);
  FCanvas.Brush.Color := Color;
  FCanvas.FillRect(ClientRect);
  // draw at least one page (even if it's empty)
  for i := FCurrentPage to Max(Min(Options.Count,PageCount) - 1,0) do
  begin
    if GetShadowRect(i, APageRect) then
      DrawPreviewShadow(APageRect);
    if GetPreviewRects(i, APageRect, APrintRect) then
    begin
      DrawPreviewBack(APageRect, APrintRect);
      if i < PageCount then
        DrawPreview(i, APageRect, APrintRect);
      DrawPreviewMargins(APageRect, APrintRect);
    end;
  end;
end;

procedure TJvCustomPreviewDoc.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TJvCustomPreviewDoc.SetAutoScroll(const Value: boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    VertScrollBar.Visible := FAutoScroll;
    HorzScrollBar.Visible := FAutoScroll;
//    Invalidate;
  end;
end;

procedure TJvCustomPreviewDoc.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomPreviewDoc.SetCurrentPage(const Value: integer);
begin
  if FCurrentPage <> Value then
  begin
    FCurrentPage := Value;
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

procedure TJvCustomPreviewDoc.WMPaint(var Message: TWMPaint);
begin
  inherited;
  PaintWindow(Message.DC);
end;

procedure TJvCustomPreviewDoc.WmPreviewAddPage(var Message: TMessage);
begin
  with Message do
    DoAddPage(TMetaFile(wParam),lParam);
  Invalidate;
end;

{ TJvDeviceInfo }

procedure TJvDeviceInfo.Change;
begin
  if Assigned(FOnCHange) then FOnChange(self);
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

procedure TJvDeviceInfo.SetOffsetX(const Value: Cardinal);
begin
  if FOffsetLeft <> Value then
  begin
    FOffsetLeft := Value;
    Change;
  end;
end;

procedure TJvDeviceInfo.SetOffsetY(const Value: Cardinal);
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
  FPhysicalWidth := GetDeviceCaps(FReferenceHandle, Windows.PHYSICALWIDTH);
  FPhysicalHeight := GetDeviceCaps(FReferenceHandle, Windows.PHYSICALHEIGHT);
  FPageWidth := GetDeviceCaps(FReferenceHandle, HORZRES);
  FPageHeight := GetDeviceCaps(FReferenceHandle, VERTRES);
  FOffsetLeft := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETX);
  FOffsetTop := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETY);
  FOffsetRight := Max(FPhysicalWidth - FPageWidth - FOffsetLeft,0);
  FOffsetBottom := Max(FPhysicalHeight - FPageHeight - FOffsetTop,0);
  Change;
end;

procedure TJvDeviceInfo.SetupDeviceInfo;
begin
  if FScreenDC = 0 then
    FScreenDC := GetDC(0);
  SetReferenceHandle(FScreenDC);
end;

end.

