{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPrvwRender.pas, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (c) 2003 by Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* setting Stretch to false for graphic items, renders them at the wrong scale
* the TStrings previewer has a *very* simple word-wrap feature - use the RTF variant if possible

-----------------------------------------------------------------------------}
// $Id$

unit JvPrvwRender;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Graphics,
  Dialogs, ComCtrls, RichEdit, Printers,
  JvComponent, JvPrvwDoc, JvRichEdit;

type
  EPrintPreviewError = Exception;
  TJvCustomPreviewRenderer = class(TJvComponent)
  private
    FPrintPreview: TJvCustomPreviewControl;
    FOldAddPage: TJvDrawPageEvent;
    procedure SetPrintPreview(const Value: TJvCustomPreviewControl);
    procedure InternalDoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean); virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property PrintPreview: TJvCustomPreviewControl read FPrintPreview write SetPrintPreview;
  public
    function CreatePreview(Append: Boolean): Boolean; virtual;
  end;

  TJvPreviewRenderRichEdit = class(TJvCustomPreviewRenderer)
  private
    FFinished: Boolean;
    FLastChar: Integer;
    FRichEdit: TCustomRichEdit;
    procedure SetRichEdit(const Value: TCustomRichEdit);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    function CreatePreview(Append: Boolean): Boolean; override;
  published
    property PrintPreview;
    property RichEdit: TCustomRichEdit read FRichEdit write SetRichEdit;
  end;

  TJvPreviewRenderJvRichEdit = class(TJvCustomPreviewRenderer)
  private
    FFinished: Boolean;
    FLastChar: Integer;
    FRichEdit: TJvCustomRichEdit;
    procedure SetRichEdit(const Value: TJvCustomRichEdit);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    function CreatePreview(Append: Boolean): Boolean; override;
  published
    property PrintPreview;
    property RichEdit: TJvCustomRichEdit read FRichEdit write SetRichEdit;
  end;

  TJvPreviewRenderStrings = class(TJvCustomPreviewRenderer)
  private
    FFinished: Boolean;
    FCurrentRow: Integer;
    FStrings: TStringList;
    FFont: TFont;
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure SetFont(const Value: TFont);
  protected
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePreview(Append: Boolean): Boolean; override;
  published
    property PrintPreview;
    property Strings: TStrings read GetStrings write SetStrings;
    property Font: TFont read FFont write SetFont;
  end;

  TJvPreviewGraphicItem = class(TCollectionItem)
  private
    FPicture: TPicture;
    FTransparent: Boolean;
    FCenter: Boolean;
    FStretch: Boolean;
    FProportional: Boolean;
    procedure SetPicture(const Value: TPicture);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function DestRect(RefRect: TRect; DestDC: HDC): TRect;
    procedure UpdateGraphic;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Center: Boolean read FCenter write FCenter default True;
    property Proportional: Boolean read FProportional write FProportional default True;
    property Stretch: Boolean read FStretch write FStretch default True;
    property Transparent: Boolean read FTransparent write FTransparent default False;
  end;

  TJvPreviewGraphicItems = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TJvPreviewGraphicItem;
    procedure SetItems(Index: Integer; const Value: TJvPreviewGraphicItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvPreviewGraphicItem;
    property Items[Index: Integer]: TJvPreviewGraphicItem read GetItems write SetItems; default;
  end;

  TJvPreviewRenderGraphics = class(TJvCustomPreviewRenderer)
  private
    FImages: TJvPreviewGraphicItems;
    procedure SetImages(const Value: TJvPreviewGraphicItems);
  protected
    function GetPPX(ADC: HDC): Integer;
    function GetPPY(ADC: HDC): Integer;
    procedure DoAddPage(Sender: TObject; PageIndex: Integer; Canvas: TCanvas;
      PageRect, PrintRect: TRect; var NeedMorePages: Boolean); override;
  public
    function CreatePreview(Append: Boolean): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PrintPreview;
    property Images: TJvPreviewGraphicItems read FImages write SetImages;
  end;

  // preview a TControl descendant
  TJvPreviewRenderControl = class(TJvCustomPreviewRenderer)
  private
    FControl: TControl;
    FProportional: Boolean;
    FCenter: Boolean;
    FStretch: Boolean;
    procedure SetControl(const Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect: TRect; PrintRect: TRect;
      var NeedMorePages: Boolean); override;
    procedure DrawControl(ACanvas: TCanvas; AWidth, AHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PrintPreview;
    property Control: TControl read FControl write SetControl;
    function CreatePreview(Append: Boolean): Boolean; override;
    property Center: Boolean read FCenter write FCenter default True;
    property Proportional: Boolean read FProportional write FProportional default True;
    property Stretch: Boolean read FStretch write FStretch default True;
  end;

  TJvNewPageEvent = procedure(Sender: TObject; PageIndex: Integer) of object;

  // a class that implements the IJvPrinter interface
  TJvPreviewPrinter = class(TJvComponent, IUnknown, IJvPrinter)
  private
    FPrinter: TPrinter;
    FPrintPreview: TJvCustomPreviewControl;
    FCollate: Boolean;
    FToPage: Integer;
    FFromPage: Integer;
    FCopies: Integer;
    FPageIndex: Integer;
    FOptions: TPrintDialogOptions;
    FPrintRange: TPrintRange;
    FOnEndDoc: TNotifyEvent;
    FOnNewPage: TJvNewPageEvent;
    FOnBeginDoc: TNotifyEvent;
    FOnAbort: TNotifyEvent;
    procedure SetPrinter(const Value: TPrinter);
    procedure CheckPrinter;
    procedure CheckActive;
    procedure SetPrintPreview(const Value: TJvCustomPreviewControl);
    procedure SetNumCopies(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { IJvPrinter }
    procedure BeginDoc;
    procedure EndDoc;
    function GetAborted: Boolean;
    function GetCanvas: TCanvas;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    function GetPrinting: Boolean;
    procedure NewPage;
    procedure Abort;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    procedure Print;
    procedure Assign(Source: TPersistent);override;
    property Title: string read GetTitle write SetTitle;
    property Printer: TPrinter read FPrinter write SetPrinter;
  published
    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write SetNumCopies default 0;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;

    property PrintPreview: TJvCustomPreviewControl read FPrintPreview write SetPrintPreview;
    property OnBeginDoc: TNotifyEvent read FOnBeginDoc write FOnBeginDoc;
    property OnNewPage: TJvNewPageEvent read FOnNewPage write FOnNewPage;
    property OnEndDoc: TNotifyEvent read FOnEndDoc write FOnEndDoc;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
  end;

implementation

uses
  Forms,
  JvJVCLUtils, JvJCLUtils, JvConsts, JvResources;

const
  cTwipsPerInch = 1440;

procedure StretchDrawBitmap(Canvas: TCanvas; const ARect: TRect; Bitmap: TBitmap);
begin
  {$IFDEF VCL}
  if (Canvas = Printer.Canvas) or
     (Printer.Printing and (Canvas.Handle = Printer.Canvas.Handle)) then
    CopyRectDIBits(Canvas, ARect,
      Bitmap, Rect(0, 0, Bitmap.Width, Bitmap.Height))
  else
  {$ENDIF VCL}
    Canvas.StretchDraw(ARect, Bitmap);
end;

type
  TJvCustomPreviewAccessProtected = class(TJvCustomPreviewControl);

function CalcDestRect(AWidth, AHeight: Integer; DstRect: TRect; Stretch, Proportional, Center: Boolean): TRect;
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
        h := Trunc(cw / xyaspect);
        if h > ch then // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then // woops, too big
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

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
  OffsetRect(Result, DstRect.Left, DstRect.Top);
end;

//=== { TJvCustomPreviewRenderer } ===========================================

function TJvCustomPreviewRenderer.CreatePreview(Append: Boolean): Boolean;
begin
  Result := False;
  if PrintPreview = nil then
    raise EPrintPreviewError.CreateRes(@RsEAPrintPreviewComponentMustBeAssigne);
  if not Append then
    PrintPreview.Clear;
  FOldAddPage := TJvCustomPreviewAccessProtected(PrintPreview).OnAddPage;
  try
    TJvCustomPreviewAccessProtected(PrintPreview).OnAddPage := InternalDoAddPage;
    PrintPreview.Add;
  finally
    TJvCustomPreviewAccessProtected(PrintPreview).OnAddPage := FOldAddPage;
  end;
end;

procedure TJvCustomPreviewRenderer.InternalDoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
begin
  DoAddPage(Sender, PageIndex, Canvas, PageRect, PrintRect, NeedMorePages);
  if Assigned(FOldAddPage) then
    FOldAddPage(Sender, PageIndex, Canvas, PageRect, PrintRect, NeedMorePages);
end;

procedure TJvCustomPreviewRenderer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PrintPreview) then
    PrintPreview := nil;
end;

procedure TJvCustomPreviewRenderer.SetPrintPreview(
  const Value: TJvCustomPreviewControl);
begin
  if FPrintPreview <> Value then
  begin
    if FPrintPreview <> nil then
      FPrintPreview.RemoveFreeNotification(Self);
    FPrintPreview := Value;
    if FPrintPreview <> nil then
      FPrintPreview.FreeNotification(Self);
  end;
end;

//=== { TJvPreviewRenderRichEdit } ===========================================

function TJvPreviewRenderRichEdit.CreatePreview(Append: Boolean): Boolean;
begin
  if RichEdit = nil then
    raise EPrintPreviewError.CreateRes(@RsEARichEditComponentMustBeAssignedInC);
  Result := RichEdit.Lines.Count > 0;
  FFinished := not Result;
  FLastChar := 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

procedure TJvPreviewRenderRichEdit.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
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
      Range.rc.Right := (PrintRect.Right - PrintRect.Left) * cTwipsPerInch div LogX;
      Range.rc.Bottom := (PrintRect.Bottom - PrintRect.Top) * cTwipsPerInch div LogY;
    end
    else
    begin
      Range.rc.Left := RichEdit.PageRect.Left * cTwipsPerInch div LogX;
      Range.rc.Top := RichEdit.PageRect.Top * cTwipsPerInch div LogY;
      Range.rc.Right := RichEdit.PageRect.Right * cTwipsPerInch div LogX;
      Range.rc.Bottom := RichEdit.PageRect.Bottom * cTwipsPerInch div LogY;
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
  end;
end;

procedure TJvPreviewRenderRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RichEdit) then
    RichEdit := nil;
end;

procedure TJvPreviewRenderRichEdit.SetRichEdit(
  const Value: TCustomRichEdit);
begin
  if FRichEdit <> Value then
  begin
    if FRichEdit <> nil then
      FRichEdit.RemoveFreeNotification(Self);
    FRichEdit := Value;
    if FRichEdit <> nil then
      FRichEdit.FreeNotification(Self);
  end;
end;

//=== { TJvPreviewRenderJvRichEdit } =========================================

function TJvPreviewRenderJvRichEdit.CreatePreview(Append: Boolean): Boolean;
begin
  if RichEdit = nil then
    raise EPrintPreviewError.CreateRes(@RsEARichEditComponentMustBeAssignedInC);
  Result := RichEdit.Lines.Count > 0;
  FFinished := not Result;
  FLastChar := 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

procedure TJvPreviewRenderJvRichEdit.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
var
  Range: TFormatRange;
  OutDC: HDC;
  ALastChar, MaxLen, LogX, LogY, OldMap: Integer;
  TextLenEx: TGetTextLengthEx;
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
      Range.rc.Right := (PrintRect.Right - PrintRect.Left) * cTwipsPerInch div LogX;
      Range.rc.Bottom := (PrintRect.Bottom - PrintRect.Top) * cTwipsPerInch div LogY;
    end
    else
    begin
      Range.rc.Left := RichEdit.PageRect.Left * cTwipsPerInch div LogX;
      Range.rc.Top := RichEdit.PageRect.Top * cTwipsPerInch div LogY;
      Range.rc.Right := RichEdit.PageRect.Right * cTwipsPerInch div LogX;
      Range.rc.Bottom := RichEdit.PageRect.Bottom * cTwipsPerInch div LogY;
    end;
    Range.rcPage := Range.rc;
    if RichEditVersion >= 2 then
    begin
      with TextLenEx do
      begin
        Flags := GTL_DEFAULT;
        codepage := CP_ACP;
      end;
      MaxLen := RichEdit.Perform(EM_GETTEXTLENGTHEX, WParam(@TextLenEx), 0);
    end
    else
      MaxLen := RichEdit.GetTextLen;

    Range.chrg.cpMax := -1;

    // ensure the output DC is in text map mode
    OldMap := SetMapMode(Range.hdc, MM_TEXT);
    try
      SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0); // flush buffer
      Range.chrg.cpMin := FLastChar;
      ALastChar := SendMessage(RichEdit.Handle, EM_FORMATRANGE, 1, Longint(@Range));
      FFinished := (ALastChar >= MaxLen) or (ALastChar = -1) or (ALastChar <= FLastChar);
      FLastChar := ALastChar;
      NeedMorePages := not FFinished;
      if FFinished then
        SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0); // flush buffer
    finally
      SetMapMode(OutDC, OldMap);
    end;
  end;
end;
  
procedure TJvPreviewRenderJvRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RichEdit) then
    RichEdit := nil;
end;

procedure TJvPreviewRenderJvRichEdit.SetRichEdit(const Value: TJvCustomRichEdit);
begin
  if FRichEdit <> Value then
  begin
    if FRichEdit <> nil then
      FRichEdit.RemoveFreeNotification(Self);
    FRichEdit := Value;
    if FRichEdit <> nil then
      FRichEdit.FreeNotification(Self);
  end;
end;

//=== { TJvPreviewRenderStrings } ============================================

constructor TJvPreviewRenderStrings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FFont := TFont.Create;
end;

function TJvPreviewRenderStrings.CreatePreview(Append: Boolean): Boolean;
begin
  Result := Strings.Count > 0;
  FFinished := not Result;
  FCurrentRow := 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

destructor TJvPreviewRenderStrings.Destroy;
begin
  FStrings.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvPreviewRenderStrings.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
var
  i, IncValue: Integer;
  ARect: TRect;
  tm: TTextMetric;
  S: string;
begin
  if not FFinished then
  begin
    Canvas.Font := Font;
    ARect := PrintRect;

    GetTextMetrics(Canvas.Handle, tm);
    IncValue := CanvasMaxTextHeight(Canvas) + tm.tmInternalLeading + tm.tmExternalLeading;
    ARect.Bottom := ARect.Top + IncValue;
    for i := FCurrentRow to Strings.Count - 1 do
    begin
      ARect.Right := PrintRect.Right;
      S := Strings[i];
      IncValue := DrawText(Canvas, PChar(S), Length(S), ARect,
        DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      if ARect.Right > PrintRect.Right then
      begin
        ARect.Right := PrintRect.Right; // reset and just force a line break in the middle (not fail proof!)
        S := Copy(S, 1, Length(S) div 2) + CrLf +
          Copy(S, Length(S) div 2 + 1, Length(S));
        IncValue := DrawText(Canvas, PChar(S), Length(S), ARect,
          DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      end;
      if ARect.Bottom > PrintRect.Bottom then
      begin
        FCurrentRow := i;
        NeedMorePages := True;
        Exit;
      end;
      DrawText(Canvas, PChar(S), Length(S), ARect,
        DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);
      OffsetRect(ARect, 0, IncValue);
    end;
  end;
  FFinished := True;
end;

procedure TJvPreviewRenderStrings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

function TJvPreviewRenderStrings.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TJvPreviewRenderStrings.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

//=== { TJvPreviewRenderControl } ============================================

function TJvPreviewRenderControl.CreatePreview(Append: Boolean): Boolean;
begin
  Result := Control <> nil;
  if Result then
    Result := inherited CreatePreview(Append);
end;

procedure TJvPreviewRenderControl.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
var
  Bitmap: TBitmap;
  ARect: TRect;
begin
  NeedMorePages := False;
  Bitmap := TBitmap.Create;
  try
    if Control is TCustomForm then
    begin
      Bitmap.Width := Control.ClientWidth;
      Bitmap.Height := Control.ClientHeight;
    end
    else
    begin
      Bitmap.Width := Control.Width;
      Bitmap.Height := Control.Height;
    end;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.HandleType := bmDIB;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    DrawControl(Bitmap.Canvas, Bitmap.Width, Bitmap.Height);
    if (Bitmap.Width > 0) and (Bitmap.Height > 0) then
    begin
      ARect := CalcDestRect(Bitmap.Width, Bitmap.Height, PrintRect, Stretch,
        Proportional, Center);
      StretchDrawBitmap(Canvas, ARect, Bitmap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TJvPreviewRenderControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TJvPreviewRenderControl.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if FControl <> nil then
      FControl.FreeNotification(Self);
  end;
end;

procedure TJvPreviewRenderControl.DrawControl(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  SaveIndex: Integer;
  ADC: HDC;
begin
  ACanvas.Lock;
  try
    ADC := ACanvas.Handle;
    if Control is TWinControl then
      TWinControl(Control).PaintTo(ADC, 0, 0)
    else
    if Control <> nil then
    begin
      SaveIndex := SaveDC(ADC);
      try
        Control.ControlState := Control.ControlState + [csPaintCopy];
        MoveWindowOrg(ADC, 0, 0);
        IntersectClipRect(ADC, 0, 0, Control.Width, Control.Height);
        Control.Perform(WM_ERASEBKGND, ADC, 0);
        Control.Perform(WM_PAINT, ADC, 0);
      finally
        RestoreDC(ADC, SaveIndex);
        Control.ControlState := Control.ControlState - [csPaintCopy];
      end;
    end
  finally
    ACanvas.Unlock;
  end;
end;

constructor TJvPreviewRenderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStretch := True;
  FProportional := True;
  FCenter := True;
end;

//=== { TJvPreviewGraphicItems } =============================================

function TJvPreviewGraphicItems.Add: TJvPreviewGraphicItem;
begin
  Result := TJvPreviewGraphicItem(inherited Add);
end;

constructor TJvPreviewGraphicItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvPreviewGraphicItem);
end;

function TJvPreviewGraphicItems.GetItems(
  Index: Integer): TJvPreviewGraphicItem;
begin
  Result := TJvPreviewGraphicItem(inherited Items[Index]);
end;

procedure TJvPreviewGraphicItems.SetItems(Index: Integer;
  const Value: TJvPreviewGraphicItem);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvPreviewGraphicItem } ==============================================

constructor TJvPreviewGraphicItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPicture := TPicture.Create;
  FCenter := True;
  FProportional := True;
  FStretch := True;
end;

function TJvPreviewGraphicItem.DestRect(RefRect: TRect; DestDC: HDC): TRect;
// var Points: TPoint;
begin
  UpdateGraphic;
  Result := CalcDestRect(Picture.Width,Picture.Height, RefRect, Stretch, Proportional, Center);
end;

destructor TJvPreviewGraphicItem.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvPreviewGraphicItem.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvPreviewGraphicItem.UpdateGraphic;
var
  G: TGraphic;
begin
  if (Picture.Width > 0) and (Picture.Height > 0) then
  begin
    G := Picture.Graphic;
    if (G <> nil) and not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := Transparent;
  end;
end;

//=== { TJvPreviewRenderGraphics } ===========================================

constructor TJvPreviewRenderGraphics.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := TJvPreviewGraphicItems.Create(Self);
end;

function TJvPreviewRenderGraphics.CreatePreview(Append: Boolean): Boolean;
begin
  Result := FImages.Count > 0;
  if Result then
    Result := inherited CreatePreview(Append);
end;

destructor TJvPreviewRenderGraphics.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TJvPreviewRenderGraphics.DoAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
var
  Img: TImageList;
begin
  with Images[PageIndex] do
    if (PageIndex < Images.Count) and (Picture.Height > 0) and (Picture.Width > 0)
      and (Picture.Graphic <> nil)
      and not Picture.Graphic.Empty then
    begin
      if (Picture.Graphic is TIcon) then
      begin
        Img := TImageList.CreateSize(Picture.Width, Picture.Height);
        try
          Img.AddIcon(Picture.Icon);
          Img.GetBitmap(0, Picture.Bitmap);
        finally
          Img.Free;
        end;
      end;
      if Picture.Graphic is TBitmap then
        StretchDrawBitmap(Canvas, DestRect(PrintRect,Canvas.Handle), Picture.Bitmap)
      else
        Canvas.StretchDraw(DestRect(PrintRect,Canvas.Handle), Picture.Graphic);
    end;
  NeedMorePages := PageIndex < Images.Count - 1;
end;

function TJvPreviewRenderGraphics.GetPPX(ADC: HDC): Integer;
begin
  Result := GetDeviceCaps(ADC, LOGPIXELSX);
end;

function TJvPreviewRenderGraphics.GetPPY(ADC: HDC): Integer;
begin
  Result := GetDeviceCaps(ADC, LOGPIXELSY);
end;

procedure TJvPreviewRenderGraphics.SetImages(const Value: TJvPreviewGraphicItems);
begin
  FImages.Assign(Value);
end;

//=== { TJvPreviewPrinter } ==================================================

procedure TJvPreviewPrinter.Abort;
begin
  CheckPrinter;
  if GetPrinting then
    FPrinter.Abort;
  if Assigned(FOnAbort) then
    FOnAbort(Self);
end;

procedure TJvPreviewPrinter.Assign(Source: TPersistent);
begin
  CheckActive;
  if Source is TJvPreviewPrinter then
  begin
    Collate := TJvPreviewPrinter(Source).Collate;
    Copies := TJvPreviewPrinter(Source).Copies;
    FromPage := TJvPreviewPrinter(Source).FromPage;
    Options := TJvPreviewPrinter(Source).Options;
    PrintRange := TJvPreviewPrinter(Source).PrintRange;
    ToPage := TJvPreviewPrinter(Source).ToPage;
    Title := TJvPreviewPrinter(Source).Title;
  end
  else
  if Source is TPrintDialog then
  begin
    Collate := TPrintDialog(Source).Collate;
    Copies := TPrintDialog(Source).Copies;
    FromPage := TPrintDialog(Source).FromPage;
    Options := TPrintDialog(Source).Options;
    PrintRange := TPrintDialog(Source).PrintRange;
    ToPage := TPrintDialog(Source).ToPage;
  end
  else
    inherited Assign(Source);
end;

procedure TJvPreviewPrinter.BeginDoc;
begin
  CheckPrinter;
  FPrinter.BeginDoc;
  if Assigned(FOnBeginDoc) then
    FOnBeginDoc(Self);
  FPageIndex := 0;
end;

procedure TJvPreviewPrinter.CheckActive;
begin
  if (Printer <> nil) and GetPrinting then
    raise EPrintPreviewError.CreateRes(@RsECannotPerfromThisOperationWhilePrin);
end;

procedure TJvPreviewPrinter.CheckPrinter;
begin
  if Printer = nil then
    raise EPrintPreviewError.CreateRes(@RsEPrinterNotAssigned);
end;

procedure TJvPreviewPrinter.EndDoc;
begin
  CheckPrinter;
  FPrinter.EndDoc;
  if Assigned(FOnEndDoc) then
    FOnEndDoc(Self);
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
  if Assigned(FOnNewPage) then
    FOnNewPage(Self, FPageIndex);
  Inc(FPageIndex);
end;

procedure TJvPreviewPrinter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PrintPreview) then
    PrintPreview := nil;
end;

procedure TJvPreviewPrinter.Print;
var
  AMin, AMax: Integer;
begin
  if PrintPreview = nil then
    raise EPrintPreviewError.CreateRes(@RsENoPrintPreviewAssigned);
  if PrintRange = prAllPages then
  begin
    AMin := 0;
    AMax := PrintPreview.PageCount - 1;
  end
  else
  begin
    AMin := FromPage - 1;
    AMax := ToPage - 1;
  end;
  PrintPreview.PrintRange(Self, AMin, AMax, Copies, Collate);
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

procedure TJvPreviewPrinter.SetPrintPreview(const Value: TJvCustomPreviewControl);
begin
  CheckActive;
  if FPrintPreview <> Value then
  begin
    if FPrintPreview <> nil then
      FPrintPreview.RemoveFreeNotification(Self);
    FPrintPreview := Value;
    if FPrintPreview <> nil then
      FPrintPreview.FreeNotification(Self);
  end;
end;

procedure TJvPreviewPrinter.SetTitle(const Value: string);
begin
  CheckPrinter;
  FPrinter.Title := Value;
end;

end.

