unit JvPrvwRender;

interface
uses
  Windows, SysUtils, Classes, Graphics, ComCtrls, JvPrvwDoc, RichEdit, Printers;

type
  EPrintPreviewError = Exception;
  TJvCustomPreviewRender = class(TComponent)
  private
    FPrintPreview: TJvCustomPreviewDoc;
    FOldAddPage:TJvDrawPageEvent;
    procedure SetPrintPreview(const Value: TJvCustomPreviewDoc);
    procedure InternalDoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);virtual;abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property PrintPreview:TJvCustomPreviewDoc read FPrintPreview write SetPrintPreview;
  public
    function CreatePreview(Append:boolean):boolean;virtual;
  end;

  TJvPreviewRichEditRender = class(TJvCustomPreviewRender)
  private
    FFinished: boolean;
    FLastChar: integer;
    FRichEdit: TCustomRichEdit;
    procedure SetRichEdit(const Value: TCustomRichEdit);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    function CreatePreview(Append:boolean):boolean;override;
  published
    property PrintPreview;
    property RichEdit:TCustomRichEdit read FRichEdit write SetRichEdit;
  end;

  TJvPreviewStringsRender = class(TJvCustomPreviewRender)
  private
    FFinished: boolean;
    FCurrentRow: integer;
    FStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function CreatePreview(Append:boolean):boolean;override;
  published
    property Strings:TStrings read FStrings write SetStrings;
  end;

  TJvPreviewGraphicRender = class(TJvCustomPreviewRender)
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: boolean);override;
  public
    function CreatePreview(Append:boolean):boolean;override;
  end;

    // a class that implements the IJvPrinter interface
  TJvPrinter = class(TComponent, IUnknown, IJvPrinter)
  private
    FPrinter: TPrinter;
    procedure SetPrinter(const Value: TPrinter);
    procedure CheckPrinter;
  public
    property Printer:TPrinter read FPrinter write SetPrinter;
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
  end;

implementation
type
  TAccessPrvw = class(TJvCustomPreviewDoc);

{ TJvCustomPreviewRender }

function TJvCustomPreviewRender.CreatePreview(Append: boolean): boolean;
begin
  Result := false;
  if PrintPreview = nil then raise EPrintPreviewError.Create('Preview must be assigned in CreatePreview!');
  if not Append then PrintPreview.Clear;
  FOldAddPage := TAccessPrvw(PrintPreview).OnAddPage;
  TAccessPrvw(PrintPreview).OnAddPage := InternalDoAddPage;
  PrintPreview.Add;
  TAccessPrvw(PrintPreview).OnAddPage := FOldAddPage;
end;

procedure TJvCustomPreviewRender.InternalDoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
begin
  DoAddPage(Sender,PageIndex,Canvas,PageRect,PrintRect,NeedMOrePages);
  if Assigned(FOldAddPage) then
    FOldAddPage(Sender,PageIndex,Canvas,PageRect,PrintRect,NeedMOrePages);
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
  if RichEdit = nil then raise
    EPrintPreviewError.Create('RichEdit must be assigned in CreatePreview!');
  Result := RichEdit.Lines.Count > 0;
  FFinished := not Result;
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

end;

function TJvPreviewStringsRender.CreatePreview(Append: boolean): boolean;
begin
  Result := Strings.Count > 0;
  FFinished := not Result;
  if Result then
    Result := inherited CreatePreview(Append);
end;

destructor TJvPreviewStringsRender.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TJvPreviewStringsRender.DoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
var i, IncValue: integer; ARect: TRect; tm: TTextMetric; S: string;
begin
  if not FFinished then
  begin
    Canvas.Font.Name := 'Verdana';
    Canvas.Font.Size := 10;
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

procedure TJvPreviewStringsRender.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

{ TJvPreviewGraphicRender }

function TJvPreviewGraphicRender.CreatePreview(Append: boolean): boolean;
begin

end;

procedure TJvPreviewGraphicRender.DoAddPage(Sender: TObject;
  PageIndex: integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: boolean);
begin

end;

{ TJvPrinter }

procedure TJvPrinter.BeginDoc;
begin
  CheckPrinter;
  FPrinter.BeginDoc;
end;

procedure TJvPrinter.CheckPrinter;
begin
  if Printer = nil then
    raise EPrintPreviewError.Create('Printer must be assigned in TJvPrinter!');
end;

procedure TJvPrinter.EndDoc;
begin
  CheckPrinter;
  FPrinter.EndDoc;
end;

function TJvPrinter.GetAborted: Boolean;
begin
  CheckPrinter;
  Result := FPrinter.Aborted;
end;

function TJvPrinter.GetCanvas: TCanvas;
begin
  CheckPrinter;
  Result := FPrinter.Canvas;
end;

function TJvPrinter.GetPageHeight: Integer;
begin
  CheckPrinter;
  Result := FPrinter.PageHeight;
end;

function TJvPrinter.GetPageWidth: Integer;
begin
  CheckPrinter;
  Result := FPrinter.PageWidth;
end;

function TJvPrinter.GetPrinting: Boolean;
begin
  CheckPrinter;
  Result := FPrinter.Printing;
end;

function TJvPrinter.GetTitle: string;
begin
  CheckPrinter;
  Result := FPrinter.Title;
end;

procedure TJvPrinter.NewPage;
begin
  CheckPrinter;
  FPrinter.NewPage;
end;

procedure TJvPrinter.SetPrinter(const Value: TPrinter);
begin
  FPrinter := Value;
end;

procedure TJvPrinter.SetTitle(const Value: string);
begin
  CheckPrinter;
  FPrinter.Title := Value;
end;

end.
