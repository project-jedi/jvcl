{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPrvwDoc.pas, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (c) 2003 by Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-04-08

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}

{$IFOPT D+}
{.$DEFINE DEBUG}
{$ENDIF}
unit JvPrvwDoc;
{ TODO :
    * Adjust zoom when Cols or Rows change - DONE
    * Adjust Cols and/or Rows when Zoom changes - DONE
    * Center pages in view - DONE
    * Only show horizontal scroll when page is too large (1 page), otherwise size Cols to fit - DONE
    + Draw to offscreen bitmap - DONE
    * Handle wheel scroll (scroll: up-down / shift+scroll: left-right) - DONE
    * Implement TopRow, First, Next, Prior, Last - DONE
    * Page Number Hints when thumb scrolling - DONE
    * User configurable margins (could use DeviceInfo.OffsetLeft etc but needs to
      be available in inch/mm as well) - DONE

    * Handle getting/setting SelectedPage (click on page -> select it)
    * Draw "fake" text when page is small (like Word does)?
    * Handle Home, End, PgUp, PgDn (w. Ctrl?)

  KNOWN ISSUES:
    * smScale doesn't work in all cases
    * centering doesn't always work
    * scrolling down and then changing properties (like Cols or Scale) doesn't always reposition the
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Forms, Dialogs, JvComponent;


type
  TJvPreviewScaleMode = (
    smFullPage, // always show 1 full page
    smPageWidth, // always show max page width
    smScale, // always use scale, don't change cols and rows
    smAutoScale, // always use scale, change cols and rows to fit
    smColsRows); // use cols and rows

  TJvDrawPreviewEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect) of object;
  TJvDrawPageEvent = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect; var NeedMorePages: boolean) of object;
  TJvScrollHintEvent = procedure(Sender: TObject; AScrollPos: integer; var AHint: string) of object;
  TJvCustomPreviewControl = class;

  IJvPrinter = interface
    ['{FDCCB7CD-8DF7-48B9-9924-CE439AE97999}']
    procedure SetTitle(const Value: string);
    function GetTitle: string;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Abort;
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
    procedure DefaultDeviceInfo;
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

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ReferenceHandle: HDC read FReferenceHandle write SetReferenceHandle;
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
    property DrawMargins: boolean read FDrawMargins write SetDrawMargins default true;
    property HorzSpacing: Cardinal read GetHorzSpacing write SetHorzSpacing default 8;
    property Rows: Cardinal read GetRows write SetRows;
    property Shadow: TJvPageShadow read FShadow write SetShadow;
    property VertSpacing: Cardinal read GetVertSpacing write SetVertSpacing default 8;
    property Scale: Cardinal read FScale write SetScale default 100;
    property ScaleMode: TJvPreviewScaleMode read FScaleMode write SetScaleMode default smFullPage;
  end;

  TJvCustomPreviewControl = class(TJvCustomControl)
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
    FOnHorzScroll: TScrollEvent;
    FOnVertScroll: TScrollEvent;
    FOnAfterScroll: TNotifyEvent;
    FScrollBars: TScrollStyle;
    FHideScrollBars: boolean;
    FOnDeviceInfoChange: TNotifyEvent;
    FOnScaleModeChange: TNotifyEvent;
    FOnOptionsChange: TNotifyEvent;
    FOnScrollHint: TJvScrollHintEvent;
    procedure DoOptionsChange(Sender: TObject);
    procedure DoDeviceInfoChange(Sender: TObject);
    procedure DoScaleModeChange(Sender: TObject);
    procedure DrawPreview(PageIndex: integer; APageRect, APrintRect: TRect);

    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetPage(Index: integer): TMetaFile;
    function GetPageCount: integer;
    procedure SetDeviceInfo(const Value: TJvDeviceInfo);
    procedure SetOptions(const Value: TJvPreviewPageOptions);
    procedure SetSelectedPage(const Value: integer);
    procedure SetTopRow(Value: integer);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CalcScrollRange;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    // returns the optimal scale value using current cols and rows
    function GetOptimalScale: Cardinal;
    function GetLesserScale(AHeight, AWidth: Cardinal): Cardinal;
    procedure Change; dynamic;
    procedure UpdateSizes;
    procedure UpdateScale;
    function GetTopRow: integer;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetHideScrollBars(const Value: boolean);
    function IsPageMode: boolean;
  protected

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DoScrollHint(NewPos: integer);

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawPages(ACanvas: TCanvas; Offset: TPoint);
    procedure DrawShadow(ACanvas: TCanvas; APageRect: TRect);
    procedure Paint; override;
    procedure DoDrawPreviewPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;
    function DoAddPage(AMetaFile: TMetaFile; PageIndex: integer): boolean; dynamic;

    property TopRow: integer read GetTopRow write SetTopRow;
    property SelectedPage: integer read FSelectedPage write SetSelectedPage;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkSpace;

    property DeviceInfo: TJvDeviceInfo read FDeviceInfo write SetDeviceInfo;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property HideScrollBars: boolean read FHideScrollBars write SetHideScrollBars default false;

    property Options: TJvPreviewPageOptions read FOptions write SetOptions;
    property OnAddPage: TJvDrawPageEvent read FOnAddPage write FOnAddPage;
    property OnVertScroll: TScrollEvent read FOnVertScroll write FOnVertScroll;
    property OnHorzScroll: TScrollEvent read FOnHorzScroll write FOnHorzScroll;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnScrollHint: TJvScrollHintEvent read FOnScrollHint write FOnScrollHint;
    property OnDrawPreviewPage: TJvDrawPreviewEvent read FOnDrawPreviewPage write FOnDrawPreviewPage;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeviceInfoChange: TNotifyEvent read FOnDeviceInfoChange write FOnDeviceInfoChange;
    property OnOptionsChange: TNotifyEvent read FOnOptionsChange write FOnOptionsChange;
    property OnScaleModeChange: TNotifyEvent read FOnScaleModeChange write FOnScaleModeChange;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
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

  TJvPreviewControl = class(TJvCustomPreviewControl)
  published

    property TopRow;
    property ScrollBars;
    property HideScrollBars;
    property SelectedPage;
    property BorderStyle;
    property Color default clAppWorkSpace;
    property DeviceInfo;

    property Options;
    property OnChange;
    property OnDeviceInfoChange;
    property OnOptionsChange;
    property OnScaleModeChange;
    property OnVertScroll;
    property OnHorzScroll;
    property OnAfterScroll;
    property OnScrollHint;

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
    property TabStop default true;
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

var
  HintWindow: THintWindow = nil;

function GetHintWindow: THintWindow;
begin
  if HintWindow = nil then
  begin
    HintWindow := HintWindowClass.Create(Application);
    HintWindow.Visible := false;
  end;
  Result := HintWindow;
end;

type
  TDeactiveHintThread = class(TThread)
  private
    FHintWindow:ThintWindow;
    FDelay:integer;
  protected
    procedure Execute; override;
  public
    constructor Create(Delay:integer; HintWindow:THintWindow);
  end;
// returns Value rounded up to nearest integer

function ceil(Value: double): integer;
begin
  Result := trunc(Value);
  if frac(Value) <> 0.0 then
    Inc(Result);
end;

// returns greater of Val1 and Val2

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

// returns lesser of Val1 and Val2

function Min(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 < Val1 then
    Result := Val2;
end;

// use our own InRange since D5 doesn't have it

function InRange(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

// returns true if Inner is completely within Outer

function RectInRect(Inner, Outer: TRect): boolean;
begin
  Result :=
    InRange(Inner.Left, Outer.Left, Outer.Right)
    and InRange(Inner.Top, Outer.Top, Outer.Bottom)
    and InRange(Inner.Right, Outer.Left, Outer.Right)
    and InRange(Inner.Bottom, Outer.Top, Outer.Bottom);
end;

// returns true if any part of Inner is "visible" inside Outer
// (any edge of Inner within Outer or Outer within Inner)

function PartialInRect(Inner, Outer: TRect): boolean;
begin
  Result :=
    (Inner.Left < Outer.Right)
    and (Inner.Top < Outer.Bottom)
    and (Inner.Right > Outer.Left)
    and (Inner.Bottom > Outer.Top);
end;

// use our own EnsureRange since D5 doesn't have it

function EnsureRange(const AValue, AMin, AMax: Integer): integer;
begin
  Result := AValue;
  Assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
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
  FDrawMargins := true;
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
  DefaultDeviceInfo;
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
  begin
    DefaultDeviceInfo;
    Exit;
  end;
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

procedure TJvDeviceInfo.DefaultDeviceInfo;
begin
  // default sizes using my current printer (HP DeskJet 690C)
  FReferenceHandle := 0;
  FLogPixelsX := 300;
  FLogPixelsY := 300;
  FPhysicalWidth := 2480;
  FPhysicalHeight := 3507;
  FPageWidth      := 2400;
  FPageHeight     := 3281;

  FOffsetLeft   := 40;
  FOffsetTop    := 40;
  FOffsetRight  := 40;
  FOffsetBottom := 40;
  Change;
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

{ TJvCustomPreviewControl }

function TJvCustomPreviewControl.Add: TMetaFile;
begin
  repeat
    Result := TMetafile.Create;
    Result.Width := DeviceInfo.PhysicalWidth;
    Result.Height := DeviceInfo.PhysicalHeight;
    // keep adding pages until user says stop
  until not DoAddPage(Result, FPages.Add(Result));
  Change;
end;

procedure TJvCustomPreviewControl.CalcScrollRange;
var
  si: TScrollInfo;
begin
  // HORIZONTAL SCROLLBAR
  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  if not HideScrollBars then
    Inc(si.fMask, SIF_DISABLENOSCROLL);
  GetScrollInfo(Handle, SB_HORZ, si);

  si.nMax := FMaxWidth - ClientWidth;
  si.nPage := 0;
  ShowScrollbar(Handle, SB_HORZ, not HideScrollBars and (ScrollBars in [ssHorizontal, ssBoth]) {(FPageWidth > ClientWidth)});
  SetScrollInfo(Handle, SB_HORZ, si, true);
  // update scroll pos if it has changed
  GetScrollInfo(Handle, SB_HORZ, si);
  if si.nPos <> FScrollPos.X then
  begin
    ScrollBy(-FScrollPos.X + si.nPos, 0);
    FScrollPos.X := si.nPos;
  end;

  // VERTICAL SCROLLBAR
  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  if not HideScrollBars then
    Inc(si.fMask, SIF_DISABLENOSCROLL);
  GetScrollInfo(Handle, SB_VERT, si);
  if PageCount = 0 then
  begin
    si.nMax := 0;
    si.nPage := 0;
  end
  else
  begin
    si.nMax := FMaxHeight - ClientHeight;
    si.nPage := 0; // FMaxHeight div TotalRows;
  end;
  ShowScrollbar(Handle, SB_VERT, not HideScrollBars and (ScrollBars in [ssVertical, ssBoth]));
  SetScrollInfo(Handle, SB_VERT, si, true);
  // update scroll pos if it has changed
  GetScrollInfo(Handle, SB_VERT, si);
  if si.nPos <> FScrollPos.Y then
  begin
    ScrollBy(0, -FScrollPos.Y + si.nPos);
    FScrollPos.Y := si.nPos;
  end;
end;

procedure TJvCustomPreviewControl.Clear;
var i: integer;
begin
  for i := 0 to FPages.Count - 1 do
    TMetaFile(FPages[i]).Free;
  FPages.Count := 0;
  if not (csDestroying in ComponentState) then
    Change;
end;

procedure TJvCustomPreviewControl.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

constructor TJvCustomPreviewControl.Create(AOwner: TComponent);
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
  ControlStyle := [csOpaque, csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks];
  Width := 150;
  Height := 250;
  FBorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FHideScrollBars := false;
  TabStop := true;
end;

procedure TJvCustomPreviewControl.CreateParams(var Params: TCreateParams);
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

procedure TJvCustomPreviewControl.Delete(Index: integer);
begin
  TMetaFile(FPages[Index]).Free;
end;

destructor TJvCustomPreviewControl.Destroy;
begin
  Clear;
  FDeviceInfo.Free;
  FPages.Free;
  FBuffer.Free;
  inherited;
end;

function TJvCustomPreviewControl.DoAddPage(AMetaFile: TMetaFile;
  PageIndex: integer): boolean;
var ACanvas: TMetaFileCanvas;
  APageRect, APrintRect: TRect;
  i: integer;
begin
  Result := false;
  ACanvas := TMetaFileCanvas.Create(AMetaFile, DeviceInfo.ReferenceHandle);
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  with DeviceInfo do
  begin
    SetWindowOrgEx(ACanvas.Handle, 0, 0, nil);
    SetWindowExtEx(ACanvas.Handle, PhysicalWidth, PhysicalHeight, nil);
    SetViewportExtEx(ACanvas.Handle, PhysicalWidth, PhysicalHeight, nil);
  end;
  // NB! Font.Size is changed when PPI is changed, so store and reset
  i := ACanvas.Font.Size;
  ACanvas.Font.PixelsPerInch := DeviceInfo.LogPixelsY;
  ACanvas.Font.Size := i;

  if Assigned(FOnAddPage) then
    with DeviceInfo do
    begin
      APageRect := Rect(0, 0, PhysicalWidth, PhysicalHeight);
      APrintRect := APageRect;
      Dec(APageRect.Left, OffsetLeft);
      Dec(APageRect.Top, OffsetTop);
      Inc(APageRect.Right, OffsetRight);
      Inc(APageRect.Bottom, OffsetBottom);
      FOnAddPage(self, PageIndex, ACanvas, APageRect, APrintRect, Result);
    end;
  // spool canvas to metafile
  ACanvas.Free;
end;

procedure TJvCustomPreviewControl.DoDrawPreviewPage(PageIndex: integer;
  Canvas: TCanvas; PageRect, PrintRect: TRect);
begin
  if Assigned(FOnDrawPreviewPage) then
    FOnDrawPreviewPage(self, PageIndex, Canvas, PageRect, PrintRect);
end;

procedure TJvCustomPreviewControl.DoOptionsChange(Sender: TObject);
begin
  Change;
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange(self);
end;

procedure TJvCustomPreviewControl.DoScaleModeChange(Sender: TObject);
begin
  Change;
  if Assigned(FOnScaleModeChange) then
    FOnScaleModeChange(self);
end;

procedure TJvCustomPreviewControl.DoDeviceInfoChange(Sender: TObject);
begin
  Change;
  if Assigned(FOnDeviceInfoChange) then
    FOnDeviceInfoChange(self);
end;

procedure TJvCustomPreviewControl.DrawPages(ACanvas: TCanvas; Offset: TPoint);
var i, j, k, m, AOffsetX, AOffsetY, APageIndex: integer;
  APageRect, APrintRect: TRect;
  si: TScrollInfo;
  tmp: boolean;
  function CanDrawPage(APageIndex: integer; APageRect: TRect): boolean;
  begin
    Result := (APageIndex < PageCount) or (PageCount = 0);
    if not Result then
      Exit;
    Result := not IsPageMode;
    if not Result then
      Result := RectInRect(APageRect, ClientRect)
    else
      Result := PartialInRect(APageRect, ClientRect);
  end;
begin
  APageRect := FPreviewRect;
  APrintRect := FPrintRect;

  // initial top/left offset
  AOffsetX := -Offset.X + Max((ClientWidth - ((FPageWidth + Options.HorzSpacing) * TotalCols)) div 2, FOptions.HorzSpacing);
  if IsPageMode then
    AOffsetY := -Offset.Y + Max((ClientHeight - ((FPageHeight + Options.VertSpacing) * VisibleRows)) div 2, FOptions.VertSpacing)
  else
    AOffsetY := -Offset.Y + Options.VertSpacing;
  k := 0;
  with ACanvas do
  begin
    Brush.Color := Color;
    FillRect(ClipRect);
    Pen.Color := clBlack;
    Pen.Style := psDot;
{$IFDEF DEBUG}
    Polyline([
      Point(AOffsetX, AOffsetY),
        Point(AOffsetX, AOffsetY + FMaxHeight),
        Point(AOffsetX + FMaxWidth, AOffsetY + FMaxHeight),
        Point(AOffsetX + FMaxWidth, AOffsetY),
        Point(AOffsetX, AOffsetY)
        ]);
{$ENDIF}
    Pen.Style := psSolid;
    APageIndex := k * TotalCols;
    m := Max(0, PageCount - 1);
//    if not IsPageMode and (k > 0) then
//      Dec(k);
    for i := k to m do
    begin
      APrintRect := FPrintRect;
      APageRect := FPreviewRect;
      OffsetRect(APrintRect, AOffsetX, AOffsetY + (FPageHeight + Options.VertSpacing) * i);
      OffsetRect(APageRect, AOffsetX, AOffsetY + (FPageHeight + Options.VertSpacing) * i);
      for j := 0 to TotalCols - 1 do
      begin
        // avoid drawing partial pages when previewrect < clientrect
        tmp := CanDrawPage(APageIndex, APageRect);
        if tmp then
        begin
          DrawShadow(ACanvas, APageRect);
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

procedure TJvCustomPreviewControl.DrawPreview(PageIndex: integer; APageRect,
  APrintRect: TRect);
begin
  FBuffer.Canvas.StretchDraw(APrintRect, Pages[PageIndex]);
  DoDrawPreviewPage(PageIndex, FBuffer.Canvas, APageRect, APrintRect);
end;

function TJvCustomPreviewControl.GetPage(Index: integer): TMetaFile;
begin
  Result := TMetaFile(FPages[Index]);
end;

function TJvCustomPreviewControl.GetPageCount: integer;
begin
  Result := FPages.Count;
end;

procedure TJvCustomPreviewControl.Paint;
begin
  if IsUpdating then
    Exit;
  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;
  //  Canvas.Brush.Color := Color;
  //  Canvas.FillRect(ClientRect);
  DrawPages(FBuffer.Canvas, Point(FScrollPos.X, FScrollPos.Y));
  BitBlt(Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, FBuffer.Canvas.Handle,
    0, 0, SRCCOPY);
end;

procedure TJvCustomPreviewControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomPreviewControl.SetSelectedPage(const Value: integer);
begin
  if FSelectedPage <> Value then
  begin
    FSelectedPage := Value;
    Invalidate;
  end;
end;

procedure TJvCustomPreviewControl.SetDeviceInfo(const Value: TJvDeviceInfo);
begin
  FDeviceInfo.Assign(Value);
end;

procedure TJvCustomPreviewControl.SetOptions(
  const Value: TJvPreviewPageOptions);
begin
  FOptions.Assign(Value);
end;

procedure TJvCustomPreviewControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  //  inherited;
  Message.Result := 1;
end;

procedure TJvCustomPreviewControl.WMSize(var Message: TWMSize);
var tmpRow: integer;
begin
  inherited;
  if IsPageMode then
    tmpRow := TopRow; // workaround...
  Change;
  if IsPageMode then
    TopRow := tmpRow; // workaround...
end;

procedure TJvCustomPreviewControl.WMHScroll(var Msg: TWMHScroll);
var si: TScrollInfo;
  NewPos, AIncrement: integer;
begin
  if IsPageMode then
    Exit;
  AIncrement := FPageWidth div 3;
  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_HORZ, si);
  case Msg.ScrollCode of
    SB_TOP:
      NewPos := 0;
    SB_BOTTOM:
      NewPos := FMaxWidth;
    SB_LINEDOWN, SB_PAGEDOWN:
      NewPos := FScrollPos.X + AIncrement;
    SB_LINEUP, SB_PAGEUP:
      NewPos := FScrollPos.X - AIncrement;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        NewPos := si.nTrackPos;
        if NewPos = FScrollPos.X then
          Exit;
      end;
    SB_ENDSCROLL:
      Exit;
  end;
  NewPos := EnsureRange(NewPos, si.nMin, si.nMax);
  if Assigned(FOnHorzScroll) then
    FOnHorzScroll(self, TScrollCode(Msg.ScrollCode), NewPos);
  NewPos := EnsureRange(NewPos, si.nMin, si.nMax);
  ScrollBy(-FScrollPos.X + NewPos, 0);
  FScrollPos.X := NewPos;
  si.nPos := NewPos;
  SetScrollInfo(Handle, SB_HORZ, si, true);
  if Assigned(FOnAfterScroll) then
    FOnAfterScroll(self);
  Refresh;
end;

procedure TJvCustomPreviewControl.WMVScroll(var Msg: TWMVScroll);
var si: TScrollInfo;
  NewPos, AIncrement: integer;
begin
  AIncrement := FPageHeight + Options.VertSpacing;
  if not IsPageMode then
    AIncrement := AIncrement div 3;
  if AIncrement < 1 then
    AIncrement := 1;

  FillChar(si, sizeof(TScrollInfo), 0);
  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, si);
  case Msg.ScrollCode of
    SB_TOP:
      NewPos := 0;
    SB_BOTTOM:
      NewPos := FMaxHeight;
    SB_LINEDOWN, SB_PAGEDOWN:
      NewPos := FScrollPos.Y + AIncrement;
    SB_LINEUP, SB_PAGEUP:
      NewPos := FScrollPos.Y - AIncrement;
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        NewPos := si.nTrackPos;
        if IsPageMode then
          NewPos := NewPos - si.nTrackPos mod AIncrement;
        if NewPos = FScrollPos.Y then
          Exit;
      end;
    SB_ENDSCROLL:
    begin
      TDeactiveHintThread.Create(500,HintWindow);
      HintWindow := nil;
      Exit;
    end;
  end;
  NewPos := EnsureRange(NewPos, si.nMin, si.nMax);
  if Assigned(FOnVertScroll) then
    FOnVertScroll(self, TScrollCode(Msg.ScrollCode), NewPos);
  NewPos := EnsureRange(NewPos, si.nMin, si.nMax);
  ScrollBy(0, -FScrollPos.Y + NewPos);
  FScrollPos.Y := NewPos;
  si.nPos := NewPos;
  SetScrollInfo(Handle, SB_VERT, si, true);
  DoScrollHint(NewPos);
  if Assigned(FOnAfterScroll) then
    FOnAfterScroll(self);
  Refresh;
end;

procedure TJvCustomPreviewControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

procedure TJvCustomPreviewControl.PrintRange(const APrinter: IJvPrinter;
  StartPage, EndPage, Copies: integer; Collate: boolean);
var i, j: integer;
begin
  if (APrinter = nil) or APrinter.GetPrinting then
    Exit;
  if StartPage < 0 then
    StartPage := PageCount - 1;
  if StartPage >= PageCount then
    StartPage := PageCount - 1;
  if EndPage < 0 then
    EndPage := PageCount - 1;
  if EndPage >= PageCount then
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

function TJvCustomPreviewControl.GetOptimalScale: Cardinal;
var Val1, Val2: integer;
begin
  Val1 := (ClientHeight - Options.VertSpacing) div VisibleRows - Options.VertSpacing * 2;
  Val2 := (ClientWidth - Options.HorzSpacing) div TotalCols - Options.HorzSpacing * 2;
  Result := GetLesserScale(Val1, Val2);
end;

procedure TJvCustomPreviewControl.Change;
begin
  //  TopRow := 0; // DONE: make this unnecessary...
  UpdateSizes;
  UpdateScale;
  // call again since some values might have changed (like scale):
  UpdateSizes;
  CalcScrollRange;
  if Assigned(FOnChange) then
    FOnChange(self);
  Refresh;
end;

procedure TJvCustomPreviewControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCustomPreviewControl.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Change;
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

function TJvCustomPreviewControl.GetLesserScale(AHeight, AWidth: Cardinal): Cardinal;
var DC: HDC;
begin
  // determine scale factor for both sides, choose lesser
  // this is the opposite of setting FPageWidth/FPageHeight
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

function TJvCustomPreviewControl.IsUpdating: boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TJvCustomPreviewControl.SetTopRow(Value: integer);
var ARow, tmp: integer;
  si: TScrollInfo;
begin
  ARow := Max(Min(Value, TotalRows - 1), 0);
  tmp := (FPageHeight + Options.VertSpacing) * ARow;
  ScrollBy(0, -FScrollPos.Y + tmp);
  FScrollPos.Y := tmp;
  SetScrollPos(Handle, SB_VERT, FScrollPos.Y, true);
  Refresh;
end;

procedure TJvCustomPreviewControl.UpdateSizes;
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

    FPreviewRect := Rect(0, 0, FPageWidth, FPageHeight);
    FPrintRect := FPreviewRect;
    with FPrintRect do
    begin
      Inc(Left, FOffsetLeft);
      Inc(Top, FOffsetTop);
      Dec(Right, FOffsetRight);
      Dec(Bottom, FOffsetBottom);
    end;

    if (Options.ScaleMode in [smFullPage, smPageWidth]) or (FPageWidth >= ClientWidth) or (FPageHeight >= ClientHeight) and not
      (Options.ScaleMode in [smScale, smAutoScale]) then
    begin
      FTotalCols := 1;
      FVisibleRows := 1;
    end
    else
      case Options.ScaleMode of
        smAutoScale:
          begin
            FTotalCols := Max(Min(PageCount, Max((ClientWidth - Options.HorzSpacing) div (FPageWidth + Options.HorzSpacing), 1)), 1);
            FVisibleRows := Min(Max((ClientHeight - Options.VertSpacing) div (FPageHeight + Options.VertSpacing), 1), TotalRows);
            if (VisibleRows > 1) and (VisibleRows * TotalCols > PageCount) then
              FVisibleRows := Min((PageCount div TotalCols) + Ord(PageCount mod TotalCols <> 0), TotalRows);
            if (FPageWidth + Options.HorzSpacing * 2 >= ClientWidth) or
              (FPageHeight + Options.VertSpacing * 2 >= ClientHeight) then
            begin
              FTotalCols := 1;
              FVisibleRows := 1;
              Options.FScale := GetOptimalScale;
            end;
          end
      else
        begin
          FTotalCols := Max(Min(PageCount - 1, Options.Cols), 1);
          FVisibleRows := Max(Min(PageCount div Options.Cols + Ord(PageCount mod Options.Cols <> 0), Options.Rows), 1);
        end;
      end;

    FTotalRows := Max((PageCount div TotalCols) + Ord(PageCount mod TotalCols <> 0), 1);

    // TODO: this just isn't right...
    FMaxHeight := TotalRows * (FPageHeight + Options.VertSpacing) + Options.VertSpacing;
//    if (FMaxHeight > ClientHeight) and (TotalRows > 1) then
//      Dec(FMaxHeight,FPageHeight - Options.VertSpacing);
    FMaxWidth := TotalCols * (FPageWidth + Options.HorzSpacing) + Options.HorzSpacing;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TJvCustomPreviewControl.GetTopRow: integer;
begin
  Result := FScrollPos.Y div (FPageHeight + Options.VertSpacing);
  Inc(Result, Ord(FScrollPos.Y mod (FPageHeight + Options.VertSpacing) <> 0));
  Result := Min(Result,TotalRows-1);
end;

procedure TJvCustomPreviewControl.First;
begin
  TopRow := 0;
end;

procedure TJvCustomPreviewControl.Last;
begin
  TopRow := TotalRows;
end;

procedure TJvCustomPreviewControl.Next;
begin
  TopRow := TopRow + 1;
end;

procedure TJvCustomPreviewControl.Prior;
begin
  TopRow := TopRow - 1;
end;

function TJvCustomPreviewControl.ItemAtPos(Pos: TPoint;
  Existing: Boolean): Integer;
begin
  // TODO: return the page at Pos or -1 if no page or outside range
  // if Existing is false, returns the page that should have been at Pos
end;

procedure TJvCustomPreviewControl.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    Change;
  end;
end;

procedure TJvCustomPreviewControl.SetHideScrollBars(const Value: boolean);
begin
  if FHideScrollBars <> Value then
  begin
    FHideScrollBars := Value;
    Change;
  end;
end;

function TJvCustomPreviewControl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var Msg: TWMScroll;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Msg.Msg := WM_VSCROLL;
    if WheelDelta > 0 then
      Msg.ScrollCode := SB_PAGEUP
    else
      Msg.ScrollCode := SB_PAGEDOWN;
    Msg.Pos := FScrollPos.Y;
    Msg.Result := 0;
    WMVScroll(Msg);
    Refresh;
    Result := true;
  end;
end;

procedure TJvCustomPreviewControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if CanFocus then
    SetFocus;
end;

function TJvCustomPreviewControl.IsPageMode: boolean;
begin
  Result := (Options.ScaleMode in [smFullPage, smAutoScale, smColsRows])
    or ((Options.ScaleMode = smScale) and (FPageHeight + Options.VertSpacing * 2 <= ClientHeight));
end;

procedure TJvCustomPreviewControl.UpdateScale;
begin
  case Options.ScaleMode of
    smFullPage:
      begin
        Options.FCols := 1;
        Options.FRows := 1;
        FTotalRows := PageCount - 1;
        Options.FScale := GetOptimalScale;
      end;
    smPageWidth:
      begin
        Options.FCols := 1;
        Options.FRows := 1;
        FTotalRows := PageCount - 1;
        Options.FScale := GetLesserScale(0, ClientWidth - Options.HorzSpacing * 2 - GetSystemMetrics(SM_CYHSCROLL));
      end;
    smScale:
      begin
        FTotalCols := Min(Options.Cols, TotalCols);
        FVisibleRows := Min(Options.Rows, VisibleRows);
        //      Options.FScale := GetOptimalScale;
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
end;

procedure TJvCustomPreviewControl.DoScrollHint(NewPos: integer);
var S: string;
  HW: THintWindow;
  pt: TPoint;
  rc: TRect;
begin
  // stolen from SynEdit, thanks guys!
  if Assigned(FOnScrollHint) then
  begin
    S := '';
    FOnScrollHint(self, NewPos, S);
    if S <> '' then
    begin
      HW := GetHintWindow;
      if not HW.Visible then
      begin
        HW.Color := Application.HintColor;
        HW.Visible := true;
      end;
      rc := Rect(0, 0, HW.Canvas.TextWidth(S) + 6,
        HW.Canvas.TextHeight(S) + 4);
      GetCursorPos(pt);
      pt := ScreenToClient(pt);
      pt.X := ClientWidth - HW.Canvas.TextWidth(S) - 12;
      pt := ClientToScreen(pt);
      OffsetRect(rc,pt.x,pt.y - 4);
      HW.ActivateHint(rc, s);
      HW.Invalidate;
      HW.Update;
    end;
  end;
end;

procedure TJvCustomPreviewControl.DrawShadow(ACanvas: TCanvas;
  APageRect: TRect);
var tmpRect: TRect; tmpColor: TColor;
begin
  tmpColor := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Color := Options.Shadow.Color;
    if (Options.Shadow.Offset <> 0) then
    begin
      // draw full background shadow if necessary
      if (Abs(Options.Shadow.Offset) >= (APageRect.Left - ApAgeRect.Right))
        or (Abs(Options.Shadow.Offset) >= (APageRect.Bottom - ApAgeRect.Top)) then
      begin
        tmpRect := APageRect;
        OffsetRect(tmpRect, Options.Shadow.Offset, Options.Shadow.Offset);
        ACanvas.FillRect(tmpRect);
      end
      // draw two smaller rects (does this *really* reduce flicker?)
      else if Options.Shadow.Offset < 0 then
      begin
        // left side
        tmpRect := APageRect;
        tmpRect.Right := tmpRect.Left - Options.Shadow.Offset;
        OffsetRect(tmpRect, Options.Shadow.Offset, Options.Shadow.Offset);
        ACanvas.FillRect(tmpRect);
        // top side
        tmpRect := APageRect;
        tmpRect.Bottom := tmpRect.Top - Options.Shadow.Offset;
        OffsetRect(tmpRect, Options.Shadow.Offset, Options.Shadow.Offset);
        ACanvas.FillRect(tmpRect);
      end
      else
      begin
        // right side
        tmpRect := APageRect;
        tmpRect.Left := tmpRect.Right - Options.Shadow.Offset;
        OffsetRect(tmpRect, Options.Shadow.Offset, Options.Shadow.Offset);
        ACanvas.FillRect(tmpRect);
        // bottom side
        tmpRect := APageRect;
        tmpRect.Top := tmpRect.Bottom - Options.Shadow.Offset;
        OffsetRect(tmpRect, Options.Shadow.Offset, Options.Shadow.Offset);
        ACanvas.FillRect(tmpRect);
      end;
    end;
  finally
    ACanvas.Brush.Color := tmpColor;
  end;

end;

{ TDeactiveHintThread }

constructor TDeactiveHintThread.Create(Delay: integer; HintWindow: THintWindow);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FHintWindow := HintWindow;
  FDelay := Delay;
  if FDelay = 0 then
    FDelay := Application.HintHidePause; 
  Resume;
end;

procedure TDeactiveHintThread.Execute;
begin
  sleep(FDelay);
  if FHintWindow <> nil then
  begin
    FHintWindow.Visible := false;
    FHintWindow.ActivateHint(Rect(0,0,0,0),'');
    FHintWindow := nil;
  end;
  Terminate;
end;

end.

