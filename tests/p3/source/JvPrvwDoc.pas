unit JvPrvwDoc;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TJvCustomPreviewDoc = class;
  IJvPrinter = interface
  ['{FDCCB7CD-8DF7-48B9-9924-CE439AE97999}']
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Abort;
    function GetPageWidth:integer;
    function GetPageHeight:integer;
    function GetHandle:HDC;
    function GetCanvas:TCanvas;
  end;

  IJvPageRenderer = interface
    ['{B767A584-DF2E-4B7B-80BD-56BD6254FC7A}']
    procedure SetPreviewDoc(PreviewDoc: TJvCustomPreviewDoc);
    function GetHandle:HDC;
    procedure DrawPage(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
      PageRect, PrintRect: TRect);
  end;

  TJvPageShadow = class(TPersistent)
  private
    FOffset: byte;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: byte);
    procedure Change;
  public
    constructor Create;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Offset: byte read FOffset write SetOffset default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvPreviewPageOptions = class(TPersistent)
  private
    FVertSpacing: Cardinal;
    FHeight: Cardinal;
    FCount: Cardinal;
    FWidth: Cardinal;
    FHorzSpacing: Cardinal;
    FColor: TColor;
    FShadow: TJvPageShadow;
    FPrintMargins: TRect;
    FOnChange: TNotifyEvent;
    FDrawMargins: boolean;
    procedure SetColor(const Value: TColor);
    procedure SetCount(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetHorzSpacing(const Value: Cardinal);
    procedure SetPrintMargins(const Value: TRect);
    procedure SetVertSpacing(const Value: Cardinal);
    procedure SetWidth(const Value: Cardinal);
    procedure Change;
    procedure DoShadowChange(Sender: TObject);
    procedure SetDrawMargins(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property PrintMargins: TRect read FPrintMargins;
  published
    property Count: Cardinal read FCount write SetCount default 1;
    property Width: Cardinal read FWidth write SetWidth default 210;
    property Height: Cardinal read FHeight write SetHeight default 297;
    property Color: TColor read FColor write SetColor default clWhite;
    property DrawMargins: boolean read FDrawMargins write SetDrawMargins default false;
    property VertSpacing: Cardinal read FVertSpacing write SetVertSpacing default 25;
    property HorzSpacing: Cardinal read FHorzSpacing write SetHorzSpacing default 25;
    property Shadow: TJvPageShadow read FShadow;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvDrawPreviewPage = procedure(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect) of object;

  TJvCustomPreviewDoc = class(TScrollingWinControl)
  private
    FOptions: TJvPreviewPageOptions;
    FCanvas: TControlCanvas;
    FRows: Cardinal;
    FCols: Cardinal;
    FPages: TList;
    FRenderer: IJvPageRenderer;
    FOnDrawPreviewPage: TJvDrawPreviewPage;
    FBorderStyle: TBorderStyle;
    FAutoScroll: boolean;
    FOnDrawPage: TJvDrawPreviewPage;
    procedure SetCols(const Value: Cardinal);
    procedure SetRenderer(const Value: IJvPageRenderer);
    procedure SetRows(const Value: Cardinal);
    procedure DoOptionsChange(Sender: TObject);
    function GetShadowRect(PageIndex: integer; var AShadowRect: TRect):boolean;
    function GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect):boolean;
    procedure DrawPreviewShadow(ARect:TRect);
    procedure DrawPage(PageIndex: integer);
    procedure DrawPreview(PageIndex: integer;APageRect,APrintRect:TRect);
    procedure DrawPreviewBack(APageRect,APrintRect:TRect);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetAutoScroll(const Value: boolean);
    procedure CalcScrollRange;
    function GetPage(Index: integer): TMetaFile;
    function GetPageCount: integer;
  protected
    
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure DoDrawPreviewPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;
    procedure DoDrawPage(PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect); dynamic;

    procedure AddPage;
    procedure DeletePage;
    procedure Clear;
    property Pages[Index:integer]:TMetaFile read GetPage;
    property PageCount:integer read GetPageCount;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clAppWorkSpace;
    property AutoScroll: boolean read FAutoScroll write SetAutoScroll default true;

    property Cols: Cardinal read FCols write SetCols default 1;
    property Options: TJvPreviewPageOptions read FOptions;
    property Renderer: IJvPageRenderer read FRenderer write SetRenderer;
    property Rows: Cardinal read FRows write SetRows default 1;
    property OnDrawPreviewPage: TJvDrawPreviewPage read FOnDrawPreviewPage write FOnDrawPreviewPage;
    property OnDrawPage: TJvDrawPreviewPage read FOnDrawPage write FOnDrawPage;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvPreviewDoc = class(TJvCustomPreviewDoc)
  published
    property Options;
    property Cols;
    property Rows;
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color;
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

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

type
  TCanvasStore = class(TInterfacedObject, IUnknown)
  private
    FCanvas,FStoreCanvas:TCanvas;
    procedure RestoreCanvas;
    procedure StoreCanvas;
  public
    constructor Create(ACanvas:TCanvas);
    destructor Destroy;override;
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

function StoreCanvas(ACanvas:TCanvas):IUnknown;
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
  FWidth := 210;
  FHeight := 297;
  FColor := clWhite;
  FVertSpacing := 25;
  FHorzSpacing := 25;
  FPrintMargins := Rect(15, 15, 15, 15);
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

procedure TJvPreviewPageOptions.SetHeight(const Value: Cardinal);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
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

procedure TJvPreviewPageOptions.SetPrintMargins(const Value: TRect);
begin
  FPrintMargins := Value;
  Change;
end;

procedure TJvPreviewPageOptions.SetVertSpacing(const Value: Cardinal);
begin
  if FVertSpacing <> Value then
  begin
    FVertSpacing := Value;
    Change;
  end;
end;

procedure TJvPreviewPageOptions.SetWidth(const Value: Cardinal);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
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
  FOffset := 2;
end;

procedure TJvPageShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvPageShadow.SetOffset(const Value: byte);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Change;
  end;
end;

{ TJvCustomPreviewDoc }

procedure TJvCustomPreviewDoc.AddPage;
var M: TMetaFile;
begin
  M := TMetafile.Create;
  FPages.Add(M);
end;

procedure TJvCustomPreviewDoc.CalcScrollRange;
begin
  if AutoScroll and HandleAllocated then
  begin
    HorzScrollBar.Range := (FOptions.Width + FOptions.HorzSpacing) * (Options.Count div Rows) + FOptions.HorzSpacing;
    VertScrollBar.Range := (FOptions.Height + FOptions.VertSpacing) * (Options.Count div Cols) + FOptions.VertSpacing;
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
  FOptions.Width := 210;
  FOptions.Height := 297;
//  FOptions.PrintMargins := Rect(10,10,10,10);

  FOptions.OnChange := DoOptionsChange;
  FRows := 1;
  FCols := 1;
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

procedure TJvCustomPreviewDoc.DeletePage;
var M: TMetaFile;
begin
  M := FPages.Last;
  M.Free;
  FPages.Count := FPages.Count - 1;
end;

destructor TJvCustomPreviewDoc.Destroy;
begin
  Clear;
  FPages.Free;
  FCanvas.Free;
  inherited;
end;

procedure TJvCustomPreviewDoc.DoDrawPage(PageIndex: integer;
  Canvas: TCanvas; PageRect, PrintRect: TRect);
begin
  if Assigned(FRenderer) then
    FRenderer.DrawPage(self, PageIndex, Canvas, PageRect, PrintRect);
  if Assigned(FOnDrawPage) then
    FOnDrawPage(self, PageIndex, Canvas, PageRect, PrintRect);
end;

procedure TJvCustomPreviewDoc.DoDrawPreviewPage(PageIndex: integer;
  Canvas: TCanvas; PageRect, PrintRect: TRect);
begin
  if Assigned(FOnDrawPreviewPage) then
    FOnDrawPreviewPage(self, PageIndex, Canvas, PageRect, PrintRect);
end;

procedure TJvCustomPreviewDoc.DoOptionsChange(Sender: TObject);
begin
  while Options.Count < PageCount do
    DeletePage;
  while Options.Count > PageCount do
    AddPage;
  Rows := Max(Options.Count div Cols,1);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.DrawPage(PageIndex: integer);
begin
//  if Assigned(FOnDrawPage) then
//    FOnDrawPage(self,PageIndex,ACanvas,APageRect,APrintRect);
end;

procedure TJvCustomPreviewDoc.DrawPreview(PageIndex: integer; APageRect,
  APrintRect: TRect);
begin
  // call SetWindowExtEx / SetViewportExtEx for the preview canvas
  // copy data of ref canvas to temp canvas
  // draw temp canvas on preview
end;

procedure TJvCustomPreviewDoc.DrawPreviewBack(APageRect,APrintRect:TRect);
begin
  StoreCanvas(FCanvas);
  FCanvas.Brush.Color := FOptions.Color;
  FCanvas.Pen.Color := clWindowText;
  FCanvas.Rectangle(APageRect);
  if FOptions.DrawMargins then
  begin
    FCanvas.Brush.Style := bsClear;
    FCanvas.Pen.Style := psDot;
    FCanvas.Rectangle(APrintRect);
  end;
end;

procedure TJvCustomPreviewDoc.DrawPreviewShadow(ARect:TRect);
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

function TJvCustomPreviewDoc.GetPreviewRects(PageIndex: integer; var APageRect, APrintRect: TRect):boolean;
var ACol, ARow: integer;
begin
  ACol := (PageIndex mod Cols);
  ARow := (PageIndex div Cols);
  APageRect := Rect(0, 0, FOptions.Width, FOptions.Height);
  OffsetRect(APageRect, FOptions.HorzSpacing, FOptions.VertSpacing);
  OffsetRect(APageRect, ACol * (FOptions.HorzSpacing + FOptions.Width),
    ARow * (FOptions.Height + FOptions.VertSpacing));
  OffsetRect(APageRect, -HorzScrollbar.Position, -VertScrollBar.Position);

  APrintRect := APageRect;
  with FOptions do
  begin
    Inc(APrintRect.Left, PrintMargins.Left);
    Inc(APrintRect.Top, PrintMargins.Top);
    Dec(APrintRect.Right, PrintMargins.Right);
    Dec(APrintRect.Bottom, PrintMargins.Bottom);
  end;
  Result := not IsRectEmpty(APrintRect);
end;

function TJvCustomPreviewDoc.GetShadowRect(PageIndex: integer; var AShadowRect: TRect):boolean;
var ACol, ARow: integer;
begin
  Result := false;
  if (FOptions.Shadow.Offset > 0) then
  begin
    ACol := (PageIndex mod Cols);
    ARow := (PageIndex div Cols);
    AShadowRect := Rect(0, 0, FOptions.Width, FOptions.Height);
    OffsetRect(AShadowRect, FOptions.HorzSpacing, FOptions.VertSpacing);
    OffsetRect(AShadowRect, ACol * (FOptions.Width + FOptions.HorzSpacing),
      ARow * (FOptions.Height + FOptions.VertSpacing));
    OffsetRect(AShadowRect, FOptions.Shadow.Offset, FOptions.Shadow.Offset);
    OffsetRect(AShadowRect, -HorzScrollbar.Position, -VertScrollBar.Position);
    Result := true;
  end
  else
    AShadowRect := Rect(0, 0, 0, 0);
end;

procedure TJvCustomPreviewDoc.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(Renderer) and AComponent.IsImplementorOf(Renderer) then
    Renderer := nil;
end;

procedure TJvCustomPreviewDoc.Paint;
var i: integer;
  APageRect, APrintRect: TRect;
begin
  StoreCanvas(FCanvas);
  FCanvas.Brush.Color := Color;
  FCanvas.FillRect(ClientRect);
  for i := 0 to FOptions.Count - 1 do
  begin
    DrawPage(i);
    if GetShadowRect(i,APageRect) then
      DrawPreviewShadow(APageRect);
    if GetPreviewRects(i, APageRect,APrintRect) then
    begin
      DrawPreviewBack(APageRect,APrintRect);
      DrawPreview(i,APageRect,APrintRect);
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
  if FAutoSCroll <> Value then
  begin
    FAutoScroll := Value;
    if not FAutoScroll then
    begin
      VertScrollBar.Range := 0;
      HorzScrollBar.Range := 0;
    end
    else
      CalcScrollRange;
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

procedure TJvCustomPreviewDoc.SetCols(const Value: Cardinal);
begin
  FCols := Value;
  if FCols < 1 then
    FCols := 1;
//  FRows := Max(FOptions.Count div FCols, 1);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.SetRenderer(const Value: IJvPageRenderer);
begin
  if FRenderer <> Value then
  begin
    ReferenceInterface(Renderer,opRemove);
    FRenderer := Value;
    ReferenceInterface(Renderer,opInsert);
  end;
end;

procedure TJvCustomPreviewDoc.SetRows(const Value: Cardinal);
begin
  FRows := Value;
  if FRows < 1 then
    FRows := 1;
//  FCols := Max(FOptions.Count div FRows, 1);
  CalcScrollRange;
  Invalidate;
end;

procedure TJvCustomPreviewDoc.WMPaint(var Message: TWMPaint);
begin
  inherited;
  PaintWindow(Message.DC);
end;



end.

