{$I JVCL.INC}
unit JvPrvwDoc;
{ TODO :
    * Adjust zoom when Cols or Rows change - DONE
    * Adjust Cols and/or Rows when Zoom changes - DONE
    * Center pages in view - DONE
    * Only show horizontal scroll when page is too large (1 page), otherwise size Cols to fit - DONE
    + Draw to offscreen bitmap - DONE
    * User configurable margins (could use DeviceInfo.OffsetLeft etc but needs to be available in inch/mm as well)
    * Handle getting/setting SelectedPage (click on page -> select it)
    * Draw "fake" text when page is small (like Word does)?
    * Handle wheel scroll (scroll: up-down / shift+scroll: left-right)
    * Handle Home, End, PgUp, PgDn (w. Ctrl?)
    * Implement TopPage, First, Next, Prior, Last - DONE

  KNOWN ISSUES:
    * smScale doesn't work in all cases
    * centering doesn't always work
    * scrolling down and then changing properties (like Cols or SCale) doesn't always reposition the
      view and the scrollbars correctly
    * sometimes displays more pages (rows) than requested


  Scrolling rules:
    * if showing 1 page (page >= clientrect), show horz scrollbar, set scroll size ~ 1 line
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
  TJvPreviewScaleMode = (
    smFullPage, // always show 1 full page
    smPageWidth, // always show max page width
    smScale,     // always use scale, don't change cols and rows
    smAutoScale, // always use scale, change cols and rows to fit
    smColsRows); // use cols and rows

  TJvDrawPreviewEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect) of object;
  TJvDrawPageEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect; var NeedMorePages: boolean) of object;
  TJvCustomPreviewDoc = class;

  IJvPrinter = interface
    ['{FDCCB7CD-8DF7-48B9-9924-CE439AE97999}']
    procedure SetTitle(const Value: string);
    function GetTitle: string;
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
    FOffsetBottom: Cardinal;
    FOffsetRight: Cardinal;
    FLogPixelsY: Cardinal;
    FPageWidth: Cardinal;
    FLogPixelsX: Cardinal;
    FOnChange: TNotifyEvent;
    FScreenDC, FReferenceHandle: HDC;
    FPhysicalHeight: Cardinal;
    FPhysicalWidth: Cardinal;
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
    function GetScreenDC: HDC;
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    function XPxToInch(Pixels: integer): single;
    function YPxToInch(Pixels: integer): single;
    function XPxToMM(Pixels: integer): single;
    function YPxToMM(Pixels: integer): single;
    function InchToXPx(Inch: single): integer;
    function InchToYPx(Inch: single): integer;
    function MMToXPx(MM: single): integer;
    function MMToYPx(MM: single): integer;

    property ReferenceHandle: HDC read FReferenceHandle write SetReferenceHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LogPixelsX: Cardinal read FLogPixelsX write SetLogPixesX;
    property LogPixelsY: Cardinal read FLogPixelsY write SetLogPixelsY;
    property PhysicalWidth: Cardinal read FPhysicalWidth write SetPhysicalWidth;
    property PhysicalHeight: Cardinal read FPhysicalHeight write SetPhysicalHeight;
    property PageWidth: Cardinal read FPageWidth write SetPageWidth;
    property PageHeight: Cardinal read FPageHeight write SetPageHeight;
    property OffsetLeft: Cardinal read FOffsetLeft write SetOffsetX;
    property OffsetTop: Cardinal read FOffsetTop write SetOffsetY;
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
    procedure SetScaleMode(const Value: TJvPreviewScaleMode);
    procedure Change;
    procedure ScaleModeChange;
    function GetCols: Cardinal;
    function GetRows: Cardinal;
    function GetHorzSpacing: Cardinal;
    function GetVertSpacing: Cardinal;
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
    property ScaleMode: TJvPreviewScaleMode read FScaleMode write SetScaleMode default smFullPage;
  end;

  TJvCustomPreviewDoc = class(TCustomControl)
  private
    FDummy: integer;
    FBuffer: TBitmap;
    FOptions: TJvPreviewPageOptions;
    FPages: TList;
    FScrollPos: TPoint;
    FOnDrawPreviewPage: TJvDrawPreviewEvent;
    FBorderStyle: TBorderStyle;
    FDeviceInfo: TJvDeviceInfo;
    FOnAddPage: TJvDrawPageEvent;
    FSelectedPage: integer;
    FOnChange: TNotifyEvent;
    FUpdateCount: integer;
    FPreviewRect, FPrintRect: TRect;
    FPageWidth, FPageHeight,
      FMaxHeight, FMaxWidth,
      FOffsetLeft, FOffsetTop,
      FOffsetRight, FOffsetBottom,
      FTotalCols, FTotalRows, FVisibleRows: integer;
    FOnHorzScroll: TNotifyEvent;
    FOnVertScroll: TNotifyEvent;
    procedure DoOptionsChange(Sender: TObject);
    procedure DoDeviceInfoChange(Sender: TObject);
    procedure DoScaleModeChange(Sender: TObject);
    function GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
    procedure DrawPreview(PageIndex: integer; APageRect, APrintRect: TRect);

    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetPage(Index: integer): TMetaFile;
    function GetPageCount: integer;
    procedure SetDeviceInfo(const Value: TJvDeviceInfo);
    procedure SetOptions(const Value: TJvPreviewPageOptions);
    procedure SetSelectedPage(const Value: integer);
    procedure SetTopPage(Value: integer);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMPreviewAddPage(var Message: TMessage); message WM_PREVIEWADDPAGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CalcScrollRange;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    // returns the optimal scale value using current cols and rows
    function GetOptimalScale: Cardinal;
    function GetScale(AHeight, AWidth: Cardinal): Cardinal;
    procedure Change; dynamic;
    procedure UpdateSizes;
    function GetTopPage: integer;
  protected

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawPages(ACanvas: TCanvas);
    procedure DrawPages2(ACanvas: TCanvas; Offset: TPoint);
    procedure Paint; override;
    procedure DoDrawPreviewPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;
    function DoAddPage(AMetaFile: TMetaFile; PageIndex: integer): boolean; dynamic;

    property TopPage: integer read GetTopPage write SetTopPage;
    property SelectedPage: integer read FSelectedPage write SetSelectedPage;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkSpace;

    property DeviceInfo: TJvDeviceInfo read FDeviceInfo write SetDeviceInfo;

    property Options: TJvPreviewPageOptions read FOptions write SetOptions;
    property OnAddPage: TJvDrawPageEvent read FOnAddPage write FOnAddPage;
    property OnVertScroll: TNotifyEvent read FOnVertScroll write FOnVertScroll;
    property OnHorzScroll: TNotifyEvent read FOnHorzScroll write FOnHorzScroll;
    property OnDrawPreviewPage: TJvDrawPreviewEvent read FOnDrawPreviewPage write FOnDrawPreviewPage;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    function Add: TMetaFile;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure PrintRange(const APrinter: IJvPrinter; StartPage, EndPage, Copies: integer; Collate: boolean);
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    property TotalCols: integer read FTotalCols;
    property TotalRows: integer read FTotalRows;
    property VisibleRows: integer read FVisibleRows;
    property Pages[Index: integer]: TMetaFile read GetPage;
    property PageCount: integer read GetPageCount;
  end;

  TJvPreviewDoc = class(TJvCustomPreviewDoc)
  published

    property TopPage;
    property SelectedPage;
    property BorderStyle;
    property Color default clAppWorkSpace;
    property DeviceInfo;

    property Options;
    property OnChange;
    property OnVertScroll;
    property OnHorzScroll;
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

uses Math;
//uses
//  FlatSB;

function ceil(Value: double): integer;
begin
  Result := trunc(Value);
  if frac(Value) <> 0.0 then
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
  Result := Max(FHorzSpacing, abs(Shadow.Offset));
end;

function TJvPreviewPageOptions.GetRows: Cardinal;
begin
  Result := Max(FRows, 1);
end;

function TJvPreviewPageOptions.GetVertSpacing: Cardinal;
begin
  Result := Max(FVertSpacing, abs(Shadow.Offset));
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

function TJvDeviceInfo.GetScreenDC: HDC;
begin
  if FScreenDC <> 0 then
    ReleaseDC(0, FScreenDC);
  FScreenDC := GetDC(0);
  Result := FScreenDC;
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
    FReferenceHandle := GetScreenDC;
  FLogPixelsX := GetDeviceCaps(FReferenceHandle, Windows.LOGPIXELSX);
  FLogPixelsY := GetDeviceCaps(FReferenceHandle, Windows.LOGPIXELSY);
  FPageWidth := GetDeviceCaps(FReferenceHandle, HORZRES);
  FPageHeight := GetDeviceCaps(FReferenceHandle, VERTRES);
  FPhysicalWidth := Max(GetDeviceCaps(FReferenceHandle, Windows.PHYSICALWIDTH), FPageWidth);
  FPhysicalHeight := Max(GetDeviceCaps(FReferenceHandle, Windows.PHYSICALHEIGHT), FPageHeight);


  FOffsetLeft := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETX);
  FOffsetTop := GetDeviceCaps(FReferenceHandle, PHYSICALOFFSETY);
  if FPhysicalWidth <> FPageWidth then
    FOffsetRight := Max(FPhysicalWidth - FPageWidth - FOffsetLeft, 0)
  else
    FOffsetRight := FOffsetLeft;
  if FPhysicalHeight <> FPageHeight then
    FOffsetBottom := Max(FPhysicalHeight - FPageHeight - FOffsetTop, 0)
  else
    FOffsetBottom := FOffsetTop;
  Change;
end;

procedure TJvDeviceInfo.SetupDeviceInfo;
begin
  SetReferenceHandle(GetScreenDC);
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

{ TJvCustomPreviewDoc }

function TJvCustomPreviewDoc.Add: TMetaFile;
begin
  repeat
    Result := TMetafile.Create;
    Result.Width := DeviceInfo.PhysicalWidth;
    Result.Height := DeviceInfo.PhysicalHeight;
    // keep adding pages until user says stop
  until not DoAddPage(Result, FPages.Add(Result));
  Change;
  // post a message to ourself in case the user added the page in the OnAddPage event
//  PostMessage(Handle, WM_PREVIEWADDPAGE, integer(Result), FPages.Add(Result));
end;

procedure TJvCustomPreviewDoc.CalcScrollRange;
var
  si: TScrollInfo;
//  tmp: integer;
  factor: double;
begin
//  tmp := TopPage;
  try
    //    TopPage := 0;
    // HORIZONTAL SCROLLBAR
    FillChar(si, sizeof(TScrollInfo), 0);
    si.cbSize := sizeof(TScrollInfo);
    si.fMask := SIF_ALL or SIF_DISABLENOSCROLL;

    si.nMin := 0;
    if FPageWidth >= ClientWidth then
    begin
      si.nMax := FMaxWidth - ClientWidth;
      si.nPage := si.nMax div 10; // TODO!
    end
    else
      si.nMax := 0;
    si.nPos := FScrollPos.X;
    if si.nPos > si.nMax then
      si.nPos := si.nMax;
    ShowScrollbar(Handle, SB_HORZ, true {(FPageWidth > ClientWidth)});
    SetScrollInfo(Handle, SB_HORZ, si, True);

    // VERTICAL SCROLLBAR
    FillChar(si, sizeof(TScrollInfo), 0);
    si.cbSize := sizeof(TScrollInfo);
    si.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    GetScrollInfo(Handle, SB_VERT, si);
    // store current relative position
    if si.nMax <> 0 then
      factor := FScrollPos.Y / si.nMax
    else
      factor := 0.0;

    if (FPageHeight >= ClientHeight) then
      si.nPage := (FPageHeight + Options.VertSpacing) div 10
    else
      si.nPage := FPageHeight + Options.VertSpacing;
    si.nPage := Max(si.nPage, 1);
    if (PageCount = 0) or (FMaxHeight <= ClientHeight) then
      si.nMax := 0
    else
      si.nMax := FMaxHeight - Options.VertSpacing;

    // DONE: if scale or cols changes, ScrollPos should be adjusted to match new settings
    si.nPos := round(factor * si.nMax);
    if si.nPos > si.nMax then
      si.nPos := si.nMax;
    if si.nPos <> FScrollPos.Y then
      ScrollBy(0, -FScrollPos.Y + si.nPos);
    FScrollPos.Y := si.nPos;
    ShowScrollbar(Handle, SB_VERT, true {(FPageHeight > ClientHeight) or (FMaxHeight > ClientHeight)});
    SetScrollInfo(Handle, SB_VERT, si, True);
  finally
//    TopPage := tmp;
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
  FSelectedPage := -1;
  FPages := TList.Create;
  FPages.Capacity := 64;
  FBuffer := TBitmap.Create;

  FOptions := TJvPreviewPageOptions.Create;
  FOptions.OnChange := DoOptionsChange;
  FOptions.OnScaleModeChange := DoScaleModeChange;

  FDeviceInfo := TJvDeviceInfo.Create;
  FDeviceInfo.OnChange := DoDeviceInfoChange;

  Color := clAppWorkSpace;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
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
  PageIndex: integer): boolean;
var ACanvas: TMetaFileCanvas;
  APageRect, APrintRect: TRect;
  i: integer;
begin
  Result := false;
  ACanvas := TMetaFileCanvas.Create(AMetaFile, DeviceInfo.ReferenceHandle);
  SetMapMode(ACanvas.Handle, MM_ISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, DeviceInfo.PhysicalWidth, DeviceInfo.PhysicalHeight, nil);
  SetViewportExtEx(ACanvas.Handle, DeviceInfo.PhysicalWidth, DeviceInfo.PhysicalHeight, nil);
  // NB! Font.Size is changed when PPI is changed, so store and reset
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
      FOnAddPage(self, PageIndex, ACanvas, APageRect, APrintRect, Result);
    end;
  // spool canvas to metafile
  ACanvas.Free;
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

procedure TJvCustomPreviewDoc.DoScaleModeChange(Sender: TObject);
begin
  Change;
end;

procedure TJvCustomPreviewDoc.DoDeviceInfoChange(Sender: TObject);
begin
  Change;
end;

procedure TJvCustomPreviewDoc.DrawPages(ACanvas: TCanvas);
var i: integer;
  APageRect, APrintRect: TRect;
begin
  with ACanvas do
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
        begin
          FDummy := i;
          Continue // outside view
        end
        else if (PageCount > 0) and (FPageWidth < ClientWidth)
          and (FPageHeight < ClientHeight) and not (Options.ScaleMode in [smFullPage, smPageWidth]) then
        begin
          if (APageRect.Right >= ClientWidth) or (APageRect.Left <= 0) or
            (APageRect.Bottom >= ClientHeight) or (APageRect.Top <= 0) then
          begin
            FDummy := i;
            Continue; // some part is outside view
          end;
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
end;

procedure TJvCustomPreviewDoc.DrawPages2(ACanvas: TCanvas; Offset: TPoint);
var i, j, k, AOffsetX, AOffsetY, APageIndex: integer;
  APageRect, APrintRect: TRect;
  si: TScrollInfo;
  // use our own InRange since D5 doesn't have it
  function InRange(const AValue, AMin, AMax: Integer): Boolean;
  begin
    Result := (AValue >= AMin) and (AValue <= AMax);
  end;

  function RectInRect(Inner, Outer: TRect): boolean;
  begin
    Result :=
      InRange(Inner.Left, Outer.Left, Outer.Right)
      and InRange(Inner.Top, Outer.Top, Outer.Bottom)
      and InRange(Inner.Right, Outer.Left, Outer.Right)
      and InRange(Inner.Bottom, Outer.Top, Outer.Bottom);
  end;

begin
  APageRect := FPreviewRect;
  APrintRect := FPrintRect;
  // initial top/left offset
  AOffsetX := -Offset.X + Max((ClientWidth - ((FPageWidth + Options.HorzSpacing) * TotalCols)) div 2, FOptions.HorzSpacing);
  AOffsetY := -Offset.Y + Max((ClientHeight - ((FPageHeight + Options.VertSpacing) * VisibleRows)) div 2, FOptions.VertSpacing);
  with ACanvas do
  begin
    Brush.Color := Color;
    FillRect(ClipRect);
//    i := TopPage;
//    k := Min(i + VisibleRows,TotalRows-1);
//    APageIndex := i;
//    while i <= k do
    APageIndex := 0;
    for i := 0 to TotalRows - 1 do
    begin
      APrintRect := FPrintRect;
      APageRect := FPreviewRect;
      OffsetRect(APrintRect, AOffsetX, AOffsetY + (FPageHeight + Options.VertSpacing) * i);
      OffsetRect(APageRect, AOffsetX, AOffsetY + (FPageHeight + Options.VertSpacing) * i);
      for j := 0 to TotalCols - 1 do
      begin
        // avoid drawing partial pages when previewrect < clientrect
        if ((FPageHeight >= ClientHeight - Options.VertSpacing) or (FPageWidth >= ClientWidth - Options.HorzSpacing) or
          RectInRect(APageRect, ClientRect) or (Options.ScaleMode = smScale)) and ((APageIndex < PageCount) or (PageCount = 0)) then
        begin
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
          if APageIndex < PageCount then
            DrawPreview(APageIndex, APageRect, APrintRect);
          // draw frame
          Brush.Style := bsClear;
          Pen.Color := clWindowText;
          Rectangle(APageRect);
          if APageIndex = FSelectedPage then
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
          if PageCount = 0 then
            Exit; // we've drawn one empty page, so let's skip the rest
        end;
        OffsetRect(APrintRect, FPageWidth + Options.HorzSpacing, 0);
        OffsetRect(APageRect, FPageWidth + Options.HorzSpacing, 0);
        Inc(APageIndex);
      end;
    end;
  end;
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

function TJvCustomPreviewDoc.GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect): boolean;
var ACol, ACols, ARows, ARow, AOffsetX, AOffsetY: integer;
begin
  ACol := (PageIndex mod Options.Cols);
  ARow := (PageIndex div Options.Cols);
  ACols := TotalCols;
  ARows := TotalRows;
  APageRect := Rect(0, 0, FPageWidth, FPageHeight);
  // find top/left edge when sized
  AOffsetX :=
    Max((ClientWidth - (FPageWidth + Options.HorzSpacing) * ACols) div 2, FOptions.HorzSpacing);
  AOffsetY :=
    Max((ClientHeight - (FPageHeight + Options.VertSpacing) * ARows) div 2, FOptions.VertSpacing);
  OffsetRect(APageRect, AOffsetX - FScrollPos.X + ACol * (FOptions.HorzSpacing + FPageWidth),
    AOffsetY - FScrollPos.Y + ARow * (FPageHeight + FOptions.VertSpacing));

  APrintRect := APageRect;
  with FOptions do
  begin
    Inc(APrintRect.Left, FOffsetLeft);
    Inc(APrintRect.Top, FOffsetTop);
    Dec(APrintRect.Right, FOffsetRight);
    Dec(APrintRect.Bottom, FOffsetBottom);
  end;
  //  if EqualRect(APrintRect,APageRect) then
  //    InflateRect(APrintRect,-1,-1);
  Result := not IsRectEmpty(APrintRect);
end;

procedure TJvCustomPreviewDoc.Paint;
var sih, siv: TScrollInfo;
begin
  if IsUpdating then
    Exit;
  FillChar(sih, sizeof(sih), 0);
  sih.cbSize := sizeof(sih);
  sih.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_HORZ, sih);

  FillChar(siv, sizeof(siv), 0);
  siv.cbSize := sizeof(siv);
  siv.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, siv);

  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;
//  Canvas.Brush.Color := Color;
//  Canvas.FillRect(ClientRect);
  DrawPages2(FBuffer.Canvas, Point(sih.nPos, siv.nPos));
  BitBlt(Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, FBuffer.Canvas.Handle,
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
    FSelectedPage := Value;
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
var si: TScrollInfo;
begin
  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_HORZ, si);
  // TODO: check range!
  case Msg.ScrollCode of
    SB_TOP:
      begin
        ScrollBy(-si.nPos, 0);
        FScrollPos.X := 0;
      end;
    SB_BOTTOM:
      begin
        ScrollBy(-si.nPos + si.nMax, 0);
        FScrollPos.X := si.nMax;
      end;
    SB_LINEDOWN, SB_PAGEDOWN:
      begin
        if si.nPos + si.nPage < si.nMax - GetSystemMetrics(SM_CXHSCROLL) then
        begin
          ScrollBy(si.nPage, 0);
          FScrollPos.X := FScrollPos.X + si.nPage;
        end;
      end;
    SB_LINEUP, SB_PAGEUP:
      begin
        if si.nPos - si.nPage >= 0 then
        begin
          ScrollBy(-si.nPage, 0);
          FScrollPos.X := FScrollPos.X - si.nPage;
        end;
      end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        // use cbSize as temp variable
        si.cbSize := si.nTrackPos - si.nTrackPos mod si.nPage;
        ScrollBy(-si.nPos - si.cbSize, 0);
        SetScrollPos(Handle, SB_HORZ, si.cbSize, true);
        FScrollPos.X := si.cbSize;
      end;
  end;
  if Assigned(FOnHorzScroll) then
    FOnHorzScroll(self);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.WMVScroll(var Msg: TWMVScroll);
var si: TScrollInfo;
begin
  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, si);
  case Msg.ScrollCode of
    SB_TOP:
      begin
        TopPage := 0;
        // ScrollBy(0, -si.nPos);
        // FScrollPos.Y := 0;
      end;
    SB_BOTTOM:
      begin
        //        TopPage := PageCount - 1;
        ScrollBy(0, si.nMax - si.nPos);
        FScrollPos.Y := si.nMax;
      end;
    SB_LINEDOWN, SB_PAGEDOWN:
      begin
        if si.nPos + si.nPage < si.nMax - GetSystemMetrics(SM_CYHSCROLL) then
        begin
          ScrollBy(0, si.nPage);
          FScrollPos.Y := FScrollPos.Y + si.nPage;
        end
        else
        begin
          ScrollBy(0, -si.nPos + si.nMax + GetSystemMetrics(SM_CYHSCROLL));
          FScrollPos.Y := si.nMax;
        end;
      end;
    SB_LINEUP, SB_PAGEUP:
      begin
        if si.nPos - si.nPage >= 0 then
        begin
          ScrollBy(0, -si.nPage);
          FScrollPos.Y := FScrollPos.Y - si.nPage;
        end
        else
        begin
          ScrollBy(0, -si.nPos);
          FScrollPos.Y := 0;
        end;
      end;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        // use cbSize as temp variable
        si.cbSize := si.nTrackPos - si.nTrackPos mod si.nPage;
        ScrollBy(0, si.nPos - si.cbSize);
        SetScrollPos(Handle, SB_VERT, si.cbSize, true);
        FScrollPos.Y := si.cbSize;
        // TODO: FTopPage
      end;
  end;
  if Assigned(FOnVertScroll) then
    FOnVertScroll(self);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

procedure TJvCustomPreviewDoc.PrintRange(const APrinter: IJvPrinter;
  StartPage, EndPage, Copies: integer; Collate: boolean);
var i, j: integer;
begin
  if (APrinter = nil) or APrinter.GetPrinting then
    Exit;
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
          APrinter.GetCanvas.Draw(0, 0, Pages[j]);
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
          APrinter.GetCanvas.Draw(0, 0, Pages[j]);
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
          APrinter.GetCanvas.Draw(0, 0, Pages[j]);
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
          APrinter.GetCanvas.Draw(0, 0, Pages[j]);
        end;
    end;
  end;
  if APrinter.GetPrinting then
    APrinter.EndDoc;
end;

function TJvCustomPreviewDoc.GetOptimalScale: Cardinal;
var Val1, Val2: integer;
begin
  Val1 := (ClientHeight - Options.VertSpacing * 2) div VisibleRows - Options.VertSpacing;
  Val2 := (ClientWidth - Options.HorzSpacing * 2) div TotalCols - Options.HorzSpacing;
  Result := GetScale(Val1, Val2);
end;

procedure TJvCustomPreviewDoc.Change;
begin
  UpdateSizes;
  case Options.ScaleMode of
    smFullPage:
      begin
        Options.FCols := 1;
        Options.FRows := 1;
        FTotalRows := PageCount-1;
        Options.FScale := GetOptimalScale;
      end;
    smPageWidth:
      begin
        Options.FCols := 1;
        Options.FRows := 1;
        FTotalRows := PageCount-1;
        Options.FScale := GetScale(0, ClientWidth - Options.HorzSpacing * 2 - GetSystemMetrics(SM_CYHSCROLL));
      end;
    smScale:
    begin
      FTotalCols := Options.Cols;
      FVisibleRows := Options.Rows;
      Options.FScale := GetOptimalScale;
    end;
    smAutoScale:
      begin
        Options.FCols := TotalCols;
        Options.FRows := VisibleRows;
        FTotalRows := Max((PageCount div TotalCols) + Ord(PageCount mod TotalCols <> 0), 1);
      end;
    smColsRows:
      Options.FScale := GetOptimalScale;
  end;
  // call again since some values might have changed (like scale):
  UpdateSizes;
  CalcScrollRange;
  if Assigned(FOnChange) then
    FOnChange(self);
  Invalidate;
end;

procedure TJvCustomPreviewDoc.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCustomPreviewDoc.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Change;
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

function TJvCustomPreviewDoc.GetScale(AHeight, AWidth: Cardinal): Cardinal;
var DC: HDC;
begin
  // determine scale factor for both sides, choose lesser
  // this is the opposite of FPageWidth/Height
  DC := GetDC(0);
  try
    if AWidth > 0 then
      AWidth := MulDiv(AWidth, 100, MulDiv(DeviceInfo.PhysicalWidth,
        GetDeviceCaps(DC, LOGPIXELSX), DeviceInfo.LogPixelsX));
    if AHeight > 0 then
      AHeight := MulDiv(AHeight, 100, MulDiv(DeviceInfo.PhysicalHeight,
        GetDeviceCaps(DC, LOGPIXELSY), DeviceInfo.LogPixelsY));
    if (AHeight > 0) and (AWidth > 0) then
      Result := Min(AWidth, AHeight)
    else if AHeight > 0 then
      Result := AHeight
    else
      Result := AWidth;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TJvCustomPreviewDoc.IsUpdating: boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TJvCustomPreviewDoc.SetTopPage(Value: integer);
var ARow, tmp: integer;
  si: TScrollInfo;
begin
  ARow := Min(Value div TotalCols, TotalRows);
  FillChar(si, sizeof(si), 0);
  si.cbSize := sizeof(si);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, si);
  ScrollBy(0, -FScrollPos.Y);
  tmp := (FPageHeight + Options.VertSpacing) * ARow;
  ScrollBy(0, -FScrollPos.Y + tmp);
  FScrollPos.Y := tmp;
  si.nPos := tmp;
  SetScrollInfo(Handle, SB_VERT, si, true);
  Invalidate;
end;

procedure TJvCustomPreviewDoc.UpdateSizes;
var DC: HDC;
begin
  // precalc as much as possible to speed up rendering
  DC := GetDC(0);
  try
    FPageWidth := MulDiv(MulDiv(DeviceInfo.PhysicalWidth, GetDeviceCaps(DC, LOGPIXELSX),
      DeviceInfo.LogPixelsX), Options.Scale, 100);
    FPageHeight := MulDiv(MulDiv(DeviceInfo.PhysicalHeight, GetDeviceCaps(DC, LOGPIXELSY),
      DeviceInfo.LogPixelsY), Options.Scale, 100);

    FOffsetLeft := MulDiv(MulDiv(DeviceInfo.OffsetLeft, GetDeviceCaps(DC, LOGPIXELSX),
      DeviceInfo.LogPixelsX), Options.Scale, 100);
    FOffsetTop := MulDiv(MulDiv(DeviceInfo.OffsetTop, GetDeviceCaps(DC, LOGPIXELSY),
      DeviceInfo.LogPixelsY), Options.Scale, 100);
    FOffsetRight := MulDiv(MulDiv(DeviceInfo.OffsetRight, GetDeviceCaps(DC, LOGPIXELSX),
      DeviceInfo.LogPixelsX), Options.Scale, 100);
    FOffsetBottom := MulDiv(MulDiv(DeviceInfo.OffsetBottom, GetDeviceCaps(DC, LOGPIXELSY),
      DeviceInfo.LogPixelsY), Options.Scale, 100);
    ;
    FPreviewRect := Rect(0, 0, FPageWidth, FPageHeight);
    FPrintRect := FPreviewRect;
    with FPrintRect do
    begin
      Inc(Left, FOffsetLeft);
      Inc(Top, FOffsetTop);
      Dec(Right, FOffsetRight);
      Dec(Bottom, FOffsetBottom);
    end;

    if (Options.ScaleMode in [smFullPage, smPageWidth]) or (FPageWidth >= ClientWidth) or (FPageHeight >= ClientHeight) then
    begin
      FTotalCols := 1;
      FVisibleRows := 1;
    end
    else
      case Options.ScaleMode of
        smAutoScale:
          begin
            FTotalCols := Min(PageCount + 1, Max((ClientWidth - Options.HorzSpacing) div (FPageWidth + Options.HorzSpacing), 1));
            FVisibleRows := Max((ClientHeight - Options.VertSpacing) div (FPageHeight + Options.VertSpacing), 1);
            if (VisibleRows > 1) and (VisibleRows * TotalCols > PageCount) then
              FVisibleRows := (PageCount div TotalCols) + Ord(PageCount mod TotalCols <> 0);
          end
      else
        begin
          FTotalCols := Min(PageCount + 1, Options.Cols);
          FVisibleRows := Min(PageCount + 1, Options.Rows);
        end;
      end;

    FTotalRows := Max((PageCount div TotalCols) + Ord(PageCount mod TotalCols <> 0), 1);

    FMaxHeight := TotalRows * (FPageHeight + Options.VertSpacing) + Options.VertSpacing;
      // - (VisibleRows - 1) * (FPageHeight + Options.VertSpacing);
    FMaxWidth := TotalCols * (FPageWidth + Options.HorzSpacing) + Options.HorzSpacing;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TJvCustomPreviewDoc.GetTopPage: integer;
var si: TScrollInfo;
begin
  FillChar(si, sizeof(si), 0);
  si.cbSize := sizeof(si);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, si);
  Result := (si.nPos div (FPageHeight + Options.VertSpacing) + Ord(si.nPos mod (FPageHeight + Options.VertSpacing) <> 0)) * TotalCols;
end;

procedure TJvCustomPreviewDoc.First;
begin
  TopPage := 0;
end;

procedure TJvCustomPreviewDoc.Last;
begin
  TopPage := PageCount;
end;

procedure TJvCustomPreviewDoc.Next;
var tmp: integer;
begin
  tmp := TopPage;
  tmp := tmp + TotalCols;
  TopPage := tmp;
end;

procedure TJvCustomPreviewDoc.Prior;
var tmp: integer;
begin
  tmp := TopPage;
  tmp := tmp - TotalCols;
  TopPage := tmp;
end;


end.

