unit JvPrvwRender;

interface
uses
  Windows, SysUtils, Messages, Classes, Controls, Graphics, Dialogs, ComCtrls, JvPrvwDoc, RichEdit, Printers;

type
  EPrintPreviewError = Exception;
  TJvCustomPreviewRender = class(TComponent)
  private
    FPrintPreview: TJvCustomPreviewDoc;
    FOldAddPage: TJvDrawPageEvent;
    procedure SetPrintPreview(const Value: TJvCustomPreviewDoc);
    procedure InternalDoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean); virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property PrintPreview: TJvCustomPreviewDoc read FPrintPreview write SetPrintPreview;
  public
    function CreatePreview(Append: boolean): boolean; virtual;
  end;

  TJvPreviewRichEditRender = class(TJvCustomPreviewRender)
  private
    FFinished: boolean;
    FLastChar: integer;
    FRichEdit: TCustomRichEdit;
    procedure SetRichEdit(const Value: TCustomRichEdit);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    function CreatePreview(Append: boolean): boolean; override;
  published
    property PrintPreview;
    property RichEdit: TCustomRichEdit read FRichEdit write SetRichEdit;
  end;

  TJvPreviewStringsRender = class(TJvCustomPreviewRender)
  private
    FFinished: boolean;
    FCurrentRow: integer;
    FStrings: TStrings;
    FFont: TFont;
    procedure SetStrings(const Value: TStrings);
    procedure SetFont(const Value: TFont);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePreview(Append: boolean): boolean; override;
  published
    property PrintPreview;
    property Strings: TStrings read FStrings write SetStrings;
    property Font:TFont read FFont write SetFont;
  end;

  TJvPreviewGraphicItem = class(TCollectionItem)
  private
    FPicture: TPicture;
    FTransparent: boolean;
    FCenter: boolean;
    FStretch: boolean;
    FProportional: boolean;
    procedure SetPicture(const Value: TPicture);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function DestRect(RefRect: TRect): TRect;
    procedure UpdateGraphic;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Center: boolean read FCenter write FCenter default true;
    property Proportional: boolean read FProportional write FProportional default true;
    property Stretch: boolean read FStretch write FStretch default true;
    property Transparent: boolean read FTransparent write FTransparent default false;
  end;

  TJvPreviewGraphicList = class(TOwnedCollection)
  private
    function GetItems(Index: integer): TJvPreviewGraphicItem;
    procedure SetItems(Index: integer; const Value: TJvPreviewGraphicItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvPreviewGraphicItem;
    property Items[Index: integer]: TJvPreviewGraphicItem read GetItems write SetItems; default;
  end;

  TJvPreviewGraphicRender = class(TJvCustomPreviewRender)
  private
    FImages: TJvPreviewGraphicList;
    procedure SetImages(const Value: TJvPreviewGraphicList);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean); override;
  public
    function CreatePreview(Append: boolean): boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PrintPreview;
    property Images: TJvPreviewGraphicList read FImages write SetImages;
  end;

  TJvPreviewControlRender = class(TJvCustomPreviewRender)
  private
    FControl: TControl;
    procedure SetControl(const Value: TControl);
    procedure DrawSubControl(ADC:HDC;AWidth,AHeight:integer; AControl:TControl);
    procedure DrawWinControl(ADC: HDC; AWidth,AHeight:integer; AWinControl: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect: TRect; PrintRect: TRect;
      var NeedMorePages: Boolean); override;
  published
    property PrintPreview;
    property Control: TControl read FControl write SetControl;
    function CreatePreview(Append: boolean): boolean; override;
  end;

    // a class that implements the IJvPrinter interface
  TJvPreviewPrinter = class(TComponent, IUnknown, IJvPrinter)
  private
    FPrinter: TPrinter;
    FPrintPreview: TJvCustomPreviewDoc;
    FCollate: Boolean;
    FToPage: Integer;
    FFromPage: Integer;
    FCopies: Integer;
    FOptions: TPrintDialogOptions;
    FPrintRange: TPrintRange;
    procedure SetPrinter(const Value: TPrinter);
    procedure CheckPrinter;
    procedure CheckActive;
    procedure SetPrintPreview(const Value: TJvCustomPreviewDoc);
    procedure SetNumCopies(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    { IJvPrinter }
    procedure BeginDoc;
    procedure EndDoc;
    function GetAborted: Boolean;
    function GetCanvas: TCanvas;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetPrinting: Boolean;
    procedure NewPage;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    procedure Print;
    procedure Assign(Source: TPersistent);override;

    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write SetNumCopies default 0;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;

    property Printer: TPrinter read FPrinter write SetPrinter;
    property PrintPreview: TJvCustomPreviewDoc read FPrintPreview write SetPrintPreview;
    property Title:string read GetTitle write SetTitle;
  end;

implementation
uses
  Forms;
  
type
  TAccessPrvwDoc = class(TJvCustomPreviewDoc);
  TAccessGraphicControl = class(TGraphicControl);

function CalcDestRect(AWidth, AHeight: integer; DstRect: TRect; Stretch, Proportional, Center: boolean): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := AWidth;
  h := AHeight;
  cw := DstRect.Right - DstRect.Left;
  ch := DstRect.Bottom - DstRect.Top;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := trunc(cw / xyaspect);
        if h > ch then // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := trunc(ch * xyaspect);
        if w > cw then // woops, too big
        begin
          w := cw;
          h := trunc(cw / xyaspect);
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

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
  OffsetRect(Result, DstRect.Left, DstRect.Top);
end;

{ TJvCustomPreviewRender }

function TJvCustomPreviewRender.CreatePreview(Append: boolean): boolean;
begin
  Result := false;
  if PrintPreview = nil then
    raise EPrintPreviewError.Create('A PrintPreview component must be assigned in CreatePreview!');
  if not Append then
    PrintPreview.Clear;
  FOldAddPage := TAccessPrvwDoc(PrintPreview).OnAddPage;
  TAccessPrvwDoc(PrintPreview).OnAddPage := InternalDoAddPage;
  PrintPreview.Add;
  TAccessPrvwDoc(PrintPreview).OnAddPage := FOldAddPage;
end;

procedure TJvCustomPreviewRender.InternalDoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
begin
  DoAddPage(Sender, PageIndex, Canvas, PageRect, PrintRect, NeedMOrePages);
  if Assigned(FOldAddPage) then
    FOldAddPage(Sender, PageIndex, Canvas, PageRect, PrintRect, NeedMOrePages);
end;

procedure TJvCustomPreviewRender.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = PrintPreview) then
    PrintPreview := nil;
end;

procedure TJvCustomPreviewRender.SetPrintPreview(
  const Value: TJvCustomPreviewDoc);
begin
  if FPrintPreview <> value then
  begin
    if FPrintPreview <> nil then
      FPrintPreview.RemoveFreeNotification(self);
    FPrintPreview := Value;
    if FPrintPreview <> nil then
      FPrintPreview.FreeNotification(self);
  end;
end;

{ TJvPreviewRichEditRender }

function TJvPreviewRichEditRender.CreatePreview(Append: boolean): boolean;
begin
  if RichEdit = nil then
    raise
      EPrintPreviewError.Create('A RichEdit component must be assigned in CreatePreview!');
  Result := RichEdit.Lines.Count > 0;
  FFinished := not Result;
  FLastChar := 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

procedure TJvPreviewRichEditRender.DoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
var
  Range: TFormatRange;
  OutDC: HDC;
  MaxLen, LogX, LogY, OldMap: Integer;
begin
  FFinished := (RichEdit = nil) or (PrintPreview = nil);
  if not FFinished then
  begin
    FillChar(Range, SizeOf(TFormatRange), 0);
    OutDC := Canvas.Handle;
    Range.hdc := OutDC;
    Range.hdcTarget := OutDC;
    LogX := GetDeviceCaps(OutDC, LOGPIXELSX);
    LogY := GetDeviceCaps(OutDC, LOGPIXELSY);
    if IsRectEmpty(RichEdit.PageRect) then
    begin
      Range.rc.right := (PrintRect.Right - PrintRect.Left) * 1440 div LogX;
      Range.rc.bottom := (PrintRect.Bottom - PrintRect.Top) * 1440 div LogY;
    end
    else
    begin
      Range.rc.left := RichEdit.PageRect.Left * 1440 div LogX;
      Range.rc.top := RichEdit.PageRect.Top * 1440 div LogY;
      Range.rc.right := RichEdit.PageRect.Right * 1440 div LogX;
      Range.rc.bottom := RichEdit.PageRect.Bottom * 1440 div LogY;
    end;
    Range.rcPage := Range.rc;
    MaxLen := RichEdit.GetTextLen;
    Range.chrg.cpMax := -1;

    // ensure the output DC is in text map mode
    OldMap := SetMapMode(Range.hdc, MM_TEXT);
    try
      SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0); // flush buffer

      Range.chrg.cpMin := FLastChar;
      FLastChar := SendMessage(RichEdit.Handle, EM_FORMATRANGE, 1, Longint(@Range));
      FFinished := (FLastChar >= MaxLen) or (FLastChar = -1);
      NeedMorePages := not FFinished;
      SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0); // flush buffer
    finally
      SetMapMode(OutDC, OldMap);
    end;
    Exit;
  end;
end;

procedure TJvPreviewRichEditRender.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = RichEdit) then
    RichEdit := nil;
end;

procedure TJvPreviewRichEditRender.SetRichEdit(
  const Value: TCustomRichEdit);
begin
  if FRichEdit <> Value then
  begin
    if FRichEdit <> nil then
      FRichEdit.RemoveFreeNotification(self);
    FRichEdit := Value;
    if FRichEdit <> nil then
      FRichEdit.FreeNotification(self);
  end;
end;

{ TJvPreviewStringsRender }

constructor TJvPreviewStringsRender.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringlist.Create;
  FFont := TFont.Create;

end;

function TJvPreviewStringsRender.CreatePreview(Append: boolean): boolean;
begin
  Result := Strings.Count > 0;
  FFinished := not Result;
  FCurrentRow := 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

destructor TJvPreviewStringsRender.Destroy;
begin
  FStrings.Free;
  FFont.Free;
  inherited;
end;

procedure TJvPreviewStringsRender.DoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
var i, IncValue: integer;
  ARect: TRect;
  tm: TTextMetric;
  S: string;
begin
  if not FFinished then
  begin
    Canvas.Font  := Font;
    ARect := PrintRect;

    GetTextMetrics(Canvas.Handle, tm);
    IncValue := Canvas.TextHeight('Wq') + tm.tmInternalLeading + tm.tmExternalLeading;
    ARect.Bottom := ARect.Top + IncValue;
    for i := FCurrentRow to FStrings.Count - 1 do
    begin
      ARect.Right := PrintRect.Right;
      S := FStrings[i];
      IncValue := DrawText(Canvas.Handle, PChar(S), Length(S), ARect, DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      if ARect.Right > PrintRect.Right then
      begin
        ARect.Right := PrintRect.Right; // reset and jsut force a line break in the middle (not fail proof!)
        S := Copy(S, 1, Length(S) div 2) + #13#10 +
          Copy(S, Length(S) div 2 + 1, Length(S));
        IncValue := DrawText(Canvas.Handle, PChar(S), Length(S), ARect, DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      end;
      if ARect.Bottom > PrintRect.Bottom then
      begin
        FCurrentRow := i;
        NeedMorePages := true;
        Exit;
      end;
      DrawText(Canvas.Handle, PChar(S), Length(S), ARect, DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      OffsetRect(ARect, 0, IncValue);
    end;
  end;
  FFinished := true;
end;

procedure TJvPreviewStringsRender.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvPreviewStringsRender.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

{ TJvPreviewControlRender }

function TJvPreviewControlRender.CreatePreview(Append: boolean): boolean;
begin
  Result := Control <> nil;
  if Result then
    Result := inherited CreatePreview(Append);
end;


procedure TJvPreviewControlRender.DrawWinControl(ADC:HDC;AWidth,AHeight:integer; AWinControl:TWinControl);
var i:integer;
begin
  AWinControl.PaintTo(ADC, 0, 0);
  for i := 0 to AWinControl.ControlCount - 1 do
    if AWinControl.Controls[i] is TWinControl then
      DrawWinControl(ADC, AWidth,AHeight, TWinControl(AWinControl.Controls[i]))
    else
      DrawSubControl(ADC,AWidth,AHeight, AWinControl.Controls[i]);
end;

procedure TJvPreviewControlRender.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
var Bitmap: TBitmap;ARect:TRect;
begin
  NeedMorePages := false;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Control.Width;
    Bitmap.Height := Control.Height;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    if Control is TWinControl then
      DrawWinControl(Bitmap.Canvas.Handle,Bitmap.Width,Bitmap.Height,TWinControl(Control))
    else
      DrawSubControl(Bitmap.Canvas.Handle,Bitmap.Width,Bitmap.Height,Control);
    if (Bitmap.Width > 0) and (Bitmap.Height > 0) then
    begin
      ARect := CalcDestRect(Bitmap.Width, Bitmap.Height, PrintRect, true, true, true);
      Canvas.StretchDraw(ARect, Bitmap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TJvPreviewControlRender.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TJvPreviewControlRender.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotification(self);
    FControl := Value;
    if FControl <> nil then
      FControl.FreeNotification(self);
  end;
end;


procedure TJvPreviewControlRender.DrawSubControl(ADC: HDC; AWidth,AHeight:integer; AControl: TControl);
var SaveIndex:integer;
begin
  SaveIndex := SaveDC(ADC);
  try
    MoveWindowOrg(ADC,0,0);
    IntersectClipRect(ADC,0,0,AWidth,AHeight);
    AControl.Perform(WM_ERASEBKGND, ADC, 0);
    AControl.Perform(WM_PAINT,ADC,0);
  finally
    RestoreDC(ADC,SaveIndex);
  end;
end;

{ TJvPreviewGraphicList }

function TJvPreviewGraphicList.Add: TJvPreviewGraphicItem;
begin
  Result := TJvPreviewGraphicItem(inherited Add);
end;

constructor TJvPreviewGraphicList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvPreviewGraphicItem);
end;

function TJvPreviewGraphicList.GetItems(
  Index: integer): TJvPreviewGraphicItem;
begin
  Result := TJvPreviewGraphicItem(inherited Items[Index]);
end;

procedure TJvPreviewGraphicList.SetItems(Index: integer;
  const Value: TJvPreviewGraphicItem);
begin
  inherited Items[Index] := Value;
end;

{ TJvPreviewGraphicItem }

constructor TJvPreviewGraphicItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TPicture.Create;
  FCenter := true;
  FProportional := true;
  FStretch := true;
end;

function TJvPreviewGraphicItem.DestRect(RefRect: TRect): TRect;
begin
  UpdateGraphic;
  Result := CalcDestRect(Picture.Width, Picture.Height, RefRect, Stretch, Proportional, Center);
end;

destructor TJvPreviewGraphicItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TJvPreviewGraphicItem.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvPreviewGraphicItem.UpdateGraphic;
var G: TGraphic;
begin
  if (Picture.Width > 0) and (Picture.Height > 0) then
  begin
    G := Picture.Graphic;
    if (G <> nil) and not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := Transparent;
  end;
end;

{ TJvPreviewGraphicRender }

constructor TJvPreviewGraphicRender.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TJvPreviewGraphicList.Create(self);
end;

function TJvPreviewGraphicRender.CreatePreview(Append: boolean): boolean;
begin
  Result := FImages.Count > 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

destructor TJvPreviewGraphicRender.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TJvPreviewGraphicRender.DoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
var img:TImageList;
begin
  with Images[PageIndex] do
    if (PageIndex < Images.Count) and (Picture.Height > 0) and (Picture.Width > 0) and (Picture.Graphic <> nil)
      and not Picture.Graphic.Empty then
      begin
        if (Picture.Graphic is TIcon) then
        begin
          img := TImageList.CreateSize(Picture.Width,Picture.Height);
          try
            img.AddIcon(Picture.Icon);
            img.getBitmap(0,Picture.Bitmap);
          finally
            img.Free;
          end;
        end;
      Canvas.StretchDraw(DestRect(PrintRect), Picture.Graphic);
    end;
  NeedMorePages := PageIndex < Images.Count - 1;
end;

procedure TJvPreviewGraphicRender.SetImages(const Value: TJvPreviewGraphicList);
begin
  FImages.Assign(Value);
end;

{ TJvPreviewPrinter }

procedure TJvPreviewPrinter.Assign(Source: TPersistent);
begin
  CheckActive;
  if Source is TJvPreviewPrinter then
  begin
    Collate     := TJvPreviewPrinter(Source).Collate;
    Copies      := TJvPreviewPrinter(Source).Copies;
    FromPage    := TJvPreviewPrinter(Source).FromPage;
    Options     := TJvPreviewPrinter(Source).Options;
    PrintRange  := TJvPreviewPrinter(Source).PrintRange;
    ToPage      := TJvPreviewPrinter(Source).ToPage;
    Title       := TJvPreviewPrinter(Source).Title;
  end
  else if Source is TPrintDialog then
  begin
    Collate     := TPrintDialog(Source).Collate;
    Copies      := TPrintDialog(Source).Copies;
    FromPage    := TPrintDialog(Source).FromPage;
    Options     := TPrintDialog(Source).Options;
    PrintRange  := TPrintDialog(Source).PrintRange;
    ToPage      := TPrintDialog(Source).ToPage;
  end
  else
    inherited;
end;

procedure TJvPreviewPrinter.BeginDoc;
begin
  CheckPrinter;
  FPrinter.BeginDoc;
end;

procedure TJvPreviewPrinter.CheckActive;
begin
  if (Printer <> nil) and GetPrinting then
    raise EPrintPreviewError.Create('Cannot perfrom this operation while printing!');
end;

procedure TJvPreviewPrinter.CheckPrinter;
begin
  if Printer = nil then
    raise EPrintPreviewError.Create('Printer not assigned!');
end;

procedure TJvPreviewPrinter.EndDoc;
begin
  CheckPrinter;
  FPrinter.EndDoc;
end;

function TJvPreviewPrinter.GetAborted: Boolean;
begin
  CheckPrinter;
  Result := FPrinter.Aborted;
end;

function TJvPreviewPrinter.GetCanvas: TCanvas;
begin
  CheckPrinter;
  Result := FPrinter.Canvas;
end;

function TJvPreviewPrinter.GetPageHeight: Integer;
begin
  CheckPrinter;
  Result := FPrinter.PageHeight;
end;

function TJvPreviewPrinter.GetPageWidth: Integer;
begin
  CheckPrinter;
  Result := FPrinter.PageWidth;
end;

function TJvPreviewPrinter.GetPrinting: Boolean;
begin
  CheckPrinter;
  Result := FPrinter.Printing;
end;

function TJvPreviewPrinter.GetTitle: string;
begin
  CheckPrinter;
  Result := FPrinter.Title;
end;

procedure TJvPreviewPrinter.NewPage;
begin
  CheckPrinter;
  FPrinter.NewPage;
end;

procedure TJvPreviewPrinter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = PrintPreview) then
    PrintPreview := nil;
end;

procedure TJvPreviewPrinter.Print;
var AMin,AMax:integer;
begin
  if PrintPreview = nil then
    raise EPrintPreviewError.Create('No PrintPreview assigned!');
  if PrintRange = prAllPages then
  begin
    AMin := 0;
    AMax := PrintPreview.PageCount - 1;
  end
  else
  begin
    AMin := FromPage-1;
    AMax := ToPage-1;
  end;
  PrintPreview.PrintRange(self,AMin,AMax,Copies,Collate);
end;

procedure TJvPreviewPrinter.SetNumCopies(const Value: Integer);
begin
  FCopies := Value;
  if FCopies < 1 then
    FCopies := 1;
end;

procedure TJvPreviewPrinter.SetPrinter(const Value: TPrinter);
begin
  CheckActive;
  FPrinter := Value;
end;

procedure TJvPreviewPrinter.SetPrintPreview(const Value: TJvCustomPreviewDoc);
begin
  CheckActive;
  if FPrintPreview <> Value then
  begin
    if FPrintPreview <> nil then
      FPrintPreview.RemoveFreeNotification(self);
    FPrintPreview := Value;
    if FPrintPreview <> nil then
      FPrintPreview.FreeNotification(self);
  end;
end;

procedure TJvPreviewPrinter.SetTitle(const Value: string);
begin
  CheckPrinter;
  FPrinter.Title := Value;
end;


end.

