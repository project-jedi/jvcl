{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridPreview.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGridPreviewForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, StdCtrls, ComCtrls, Buttons, Printers, ExtDlgs,
  {$IFDEF VisualCLX}
  QComCtrlsEx,
  {$ENDIF VisualCLX}
  JvGridPrinter, JvComponent, QExtDlgs;

type
  TJvGridPreviewForm = class(TJvForm)
    ScrollBox1: TScrollBox;
    PreviewImage: TImage;
    Panel1: TPanel;
    Header: TEdit;
    Headers: TListBox;
    Margin: TUpDown;
    ckborders: TCheckBox;
    Margins: TListBox;
    btnprint: TSpeedButton;
    PreviewPage: TUpDown;
    btnshow: TSpeedButton;
    lblpages: TLabel;
    cklive: TCheckBox;
    btnsetup: TSpeedButton;
    btnfull: TSpeedButton;
    btnpic: TSpeedButton;
    OpenPictureDialog1: TOpenPictureDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure btnshowClick(Sender: TObject);
    procedure MarginsClick(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure HeaderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckbordersClick(Sender: TObject);
    procedure HeadersClick(Sender: TObject);
    procedure ckliveClick(Sender: TObject);
    procedure btnsetupClick(Sender: TObject);
    procedure btnfullClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewImageClick(Sender: TObject);
    procedure btnpicClick(Sender: TObject);
    procedure MarginClick(Sender: TObject; Button: TUDBtnType);
    procedure PreviewPageClick(Sender: TObject; Button: TUDBtnType);
  private
    FGridPrinter: TJvGridPrinter;
    FPrintImage: TBitmap;
    FGrid: TStringGrid;
    FPageCount: Cardinal;
    procedure SetGridPrinter(const Value: TJvGridPrinter);
    procedure FullSize;
    procedure Zoom(Factor: Extended);
    procedure SetGrid(const Value: TStringGrid);
    procedure UpdateRowHeights;
    function PageCount: Integer;
    procedure UpdatePreview(ACanvas: TCanvas);
  public
    procedure DrawToCanvas(ACanvas: TCanvas; Mode: TJvPrintMode;
      FromRow, ToRow: Integer);
    procedure Print;
  published
    property GridPrinter: TJvGridPrinter read FGridPrinter write SetGridPrinter;
    property Grid: TStringGrid read FGrid write SetGrid;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvPaintFX, JvConsts, JvResources;

{$R *.dfm}

var
  RowHeights: array of Integer;

procedure TJvGridPreviewForm.SetGridPrinter(const Value: TJvGridPrinter);
begin
  FGridPrinter := Value;
end;

procedure TJvGridPreviewForm.btnshowClick(Sender: TObject);
begin
  if Assigned(FGridPrinter) then
  begin
    UpdatePreview(FPrintImage.Canvas);
    PreviewImage.Picture.Bitmap.Assign(FPrintImage);
  end;
end;

procedure TJvGridPreviewForm.MarginsClick(Sender: TObject);
begin
  case Margins.ItemIndex of
    -1:
      Exit;
    0:
      Margin.Position := GridPrinter.PrintOptions.MarginTop;
    1:
      Margin.Position := GridPrinter.PrintOptions.PageTitleMargin;
    2:
      Margin.Position := GridPrinter.PrintOptions.MarginLeft;
    3:
      Margin.Position := GridPrinter.PrintOptions.MarginRight;
    4:
      Margin.Position := GridPrinter.PrintOptions.MarginBottom;
    5:
      Margin.Position := GridPrinter.PrintOptions.LeftPadding;
    6:
      Margin.Position := GridPrinter.PrintOptions.HeaderSize;
    7:
      Margin.Position := GridPrinter.PrintOptions.FooterSize;
  else
    Exit;
  end;
  if Margins.ItemIndex > 5 then
  begin
    {$IFDEF VCL}
    Margin.Min := 6;
    Margin.Max := 72;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Margin.Min := 6;
    Margin.Max := 72;
    {$ENDIF VisualCLX}
  end
  else
  begin
    {$IFDEF VCL}
    Margin.Min := 0;
    Margin.Max := 400;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Margin.Min := 0;
    Margin.Max := 400;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvGridPreviewForm.btnprintClick(Sender: TObject);
begin
  Print;
end;

procedure TJvGridPreviewForm.HeaderChange(Sender: TObject);
begin
  case Headers.ItemIndex of
    -1:
      Exit;
    0:
      GridPrinter.PrintOptions.PageTitle := Header.Text;
    1:
      GridPrinter.PrintOptions.PageFooter := Header.Text;
    2:
      GridPrinter.PrintOptions.DateFormat := Header.Text;
    3:
      GridPrinter.PrintOptions.TimeFormat := Header.Text;
    4:
      GridPrinter.PrintOptions.Logo := Header.Text;
  end;
  if cklive.Checked then
    btnshow.Click;
end;

procedure TJvGridPreviewForm.FormShow(Sender: TObject);
begin
  UpdateRowHeights;
  FPageCount := 0;
  DrawToCanvas(FPrintImage.Canvas, pmPreview, 1, Grid.RowCount - 1);
  PreviewImage.Picture.Bitmap.Assign(FPrintImage);
  Header.Text := GridPrinter.PrintOptions.PageTitle;
  Margin.Position := GridPrinter.PrintOptions.MarginTop;
  Margins.ItemIndex := 0;
  {$IFDEF VCL}
  PreviewPage.Max := PageCount;
  lblpages.Caption := Format(RsOfd, [PreviewPage.Max]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  PreviewPage.Max := PageCount;
  lblpages.Caption := Format(RsOfd, [PreviewPage.Max]);
  {$ENDIF VisualCLX}
  GridPrinter.PrintOptions.PreviewPage := 1;
  PreviewPage.Position := 1;
  ckborders.Checked := (GridPrinter.PrintOptions.BorderStyle = bsSingle);
  Header.Text := GridPrinter.PrintOptions.PageTitle;
  Headers.ItemIndex := 0;
  btnshow.Click;
end;

procedure TJvGridPreviewForm.ckbordersClick(Sender: TObject);
begin
  if ckborders.Checked then
    GridPrinter.PrintOptions.BorderStyle := bsSingle
  else
    GridPrinter.PrintOptions.BorderStyle := bsNone;
  if cklive.Checked then
    btnshow.Click;
end;

procedure TJvGridPreviewForm.HeadersClick(Sender: TObject);
begin
  case Headers.ItemIndex of
    0:
      Header.Text := GridPrinter.PrintOptions.PageTitle;
    1:
      Header.Text := GridPrinter.PrintOptions.PageFooter;
    2:
      Header.Text := GridPrinter.PrintOptions.DateFormat;
    3:
      Header.Text := GridPrinter.PrintOptions.TimeFormat;
    4:
      Header.Text := GridPrinter.PrintOptions.Logo;
  end;
end;

procedure TJvGridPreviewForm.ckliveClick(Sender: TObject);
begin
  if cklive.Checked then
    btnshow.Click;
end;

procedure TJvGridPreviewForm.btnsetupClick(Sender: TObject);
begin
  if PrinterSetupDialog1.Execute then
  begin
    GridPrinter.PrintOptions.Orientation := Printer.Orientation;
    if cklive.Checked then
      btnshow.Click;
  end;
end;

procedure TJvGridPreviewForm.FullSize;
var
  Bmp: TBitmap;
  W, H: Integer;
begin
  W := FPrintImage.Width;
  H := FPrintImage.Height;
  Bmp := TBitmap.Create;
  Bmp.Width := ScrollBox1.ClientWidth;
  Bmp.Height := Round(H / W * Bmp.Width);
  FPrintImage.PixelFormat := pf24bit;
  Bmp.PixelFormat := pf24bit;
  TJvPaintFX.SmoothResize(FPrintImage, Bmp);
  PreviewImage.Picture.Bitmap.Assign(Bmp);
  Bmp.Free;
end;

procedure TJvGridPreviewForm.btnfullClick(Sender: TObject);
begin
  FullSize;
end;

procedure TJvGridPreviewForm.FormCreate(Sender: TObject);
begin
  FPrintImage := TBitmap.Create;
end;

procedure TJvGridPreviewForm.FormDestroy(Sender: TObject);
begin
  FPrintImage.Free;
end;

procedure TJvGridPreviewForm.PreviewImageClick(Sender: TObject);
var
  W, W1: Integer;
begin
  W1 := PreviewImage.Picture.Bitmap.Width;
  W := FPrintImage.Width;
  if Round(W * 0.8) < W1 then
    PreviewImage.Picture.Bitmap.Assign(FPrintImage)
  else
    Zoom(W1 / W / 0.8);
end;

procedure TJvGridPreviewForm.Zoom(Factor: Extended);
var
  Bmp: TBitmap;
  W, H: Integer;
begin
  W := FPrintImage.Width;
  H := FPrintImage.Height;
  Bmp := TBitmap.Create;
  Bmp.Width := Round(Factor * W);
  Bmp.Height := Round(H / W * Bmp.Width);
  FPrintImage.PixelFormat := pf24bit;
  Bmp.PixelFormat := pf24bit;
  TJvPaintFX.SmoothResize(FPrintImage, Bmp);
  PreviewImage.Picture.Bitmap.Assign(Bmp);
  Bmp.Free;
end;

procedure TJvGridPreviewForm.btnpicClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    if Headers.ItemIndex = 4 then
    begin
      Header.Text := OpenPictureDialog1.FileName;
      GridPrinter.PrintOptions.Logo := OpenPictureDialog1.FileName;
    end;
end;

procedure TJvGridPreviewForm.DrawToCanvas(ACanvas: TCanvas; Mode: TJvPrintMode; FromRow, ToRow: Integer);
var
  PageWidth, PageHeight, PageRow, PageCol, I, IRow, FromCol, ToCol, X, Y: Integer;
  DoPaint, HasLogo: Boolean;
  HHeader, HFooter: Integer;
  LogoPic, LogoPics: TBitmap;

  function ScaleX(I: Integer): Integer;
  begin
    if Mode = pmPreview then
      Result := I
    else
      Result := Round(I * (GetDeviceCaps(Printer.Handle, LOGPIXELSX) / Screen.PixelsPerInch));
  end;

  function ScaleY(I: Integer): Integer;
  begin
    if Mode = pmPreview then
      Result := I
    else
      Result := Round(I * (GetDeviceCaps(Printer.Handle, LOGPIXELSY) / Screen.PixelsPerInch));
  end;

  procedure DrawCells(IRow: Integer);
  var
    ICol, I: Integer;
    R: TRect;
    drs: string;
    nr: Boolean;
    v: Extended;
  begin
    //Alignment must be done another day
    for ICol := FromCol to ToCol do
    begin
      //X Offset
      X := ScaleX(GridPrinter.PrintOptions.MarginLeft);
      for I := FromCol to ICol - 1 do
        Inc(X, ScaleX(Grid.ColWidths[I] + 1));
      //Text Rect
      R := Rect(X, Y, X + ScaleX(Grid.ColWidths[ICol]), Y + ScaleY(RowHeights[IRow]));
      //Draw on the Canvas
      if DoPaint then
      begin
        if GridPrinter.PrintOptions.BorderStyle = bsSingle then
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.Rectangle(R.Left, R.Top, R.Right + ScaleX(2), R.Bottom + ScaleY(1));
        end;
        drs := Grid.Cells[ICol, IRow];
        nr := False;
        if (IRow = 0) and (ICol > 0) then
          ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
        else
          ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
        R.Left := R.Left + ScaleX(GridPrinter.PrintOptions.LeftPadding);
        if GridPrinter.WordWrap and (ICol <> 0) and (IRow <> 0) then
        begin
          if GridPrinter.NumbersalRight and (not nr) then
          try
            v := StrToFloat(drs);
            nr := True;
            drs := Format(GridPrinter.NumberFormat, [v]);
          except
            // do nothing
          end;
          if nr then
            DrawText(ACanvas.Handle, PChar(drs), -1, R, DT_WORDBREAK or DT_RIGHT)
          else
            DrawText(ACanvas.Handle, PChar(drs), -1, R, DT_WORDBREAK or DT_LEFT);
        end
        else
        begin
          if GridPrinter.NumbersalRight and (not nr) then
          try
            v := StrToFloat(drs);
            nr := True;
            drs := Format(GridPrinter.NumberFormat, [v]);
          except
            // do nothing
          end;
          if nr then
            DrawText(ACanvas.Handle, PChar(drs), -1, R, DT_SINGLELINE or DT_RIGHT)
          else
            DrawText(ACanvas.Handle, PChar(drs), -1, R, DT_SINGLELINE or DT_LEFT);
        end;
      end;
    end;
    Inc(Y, ScaleY(RowHeights[IRow]));
  end;

  procedure DrawTitle; //draw Header and Footer
  var
    S, fstr: string;
    List: TStringList;
    I: Integer;
    TmpFont: TFont; //I have no idea why you can't use gettextwidth when ACanvas = printer.Canvas, it returns wrong Value
  begin
    TmpFont := nil;
    if DoPaint then
    begin
      ACanvas.Font.Size := GridPrinter.PrintOptions.HeaderSize;
      TmpFont := Grid.Font;
      Grid.Canvas.Font := ACanvas.Font;
    end;
    //Title
    Y := ScaleY(GridPrinter.PrintOptions.MarginTop);
    S := GridPrinter.PrintOptions.PageTitle;
    HHeader := Grid.Canvas.TextHeight(S);
    if HasLogo then
      if LogoPic.Height > HHeader then
        HHeader := LogoPic.Height;
    if DoPaint then
    begin
      if HasLogo then
      begin
        ACanvas.Draw(ScaleX(GridPrinter.PrintOptions.MarginLeft), Y, LogoPics);
      end;
      ACanvas.TextOut((PageWidth div 2) - (ScaleX(Grid.Canvas.TextWidth(S) div 2)), Y, S);
    end;
    Y := Y + ScaleY(HHeader);
    //Page nr
    S := Format(RsPaged, [PageRow]);
    if (ToCol < Grid.ColCount - 1) or (PageCol > 1) then
      S := S + '-' + IntToStr(PageCol);
    fstr := GridPrinter.PrintOptions.PageFooter;
    HFooter := Grid.Canvas.TextHeight(fstr);
    if fstr <> '' then
      if DoPaint then
      begin
        ACanvas.Font.Size := GridPrinter.PrintOptions.FooterSize;
        Grid.Canvas.Font := ACanvas.Font;
        HFooter := Grid.Canvas.TextHeight(fstr);
        List := TStringList.Create;
        List.Text := StringReplace(fstr, '|', cr, [rfreplaceall]);
        while List.Count < 3 do
          List.Append('');
        for I := 0 to 2 do
        begin
          List[I] := StringReplace(List[I], 'date', FormatDateTime(GridPrinter.PrintOptions.DateFormat, Now), []);
          List[I] := StringReplace(List[I], 'time', FormatDateTime(GridPrinter.PrintOptions.TimeFormat, Now), []);
          List[I] := StringReplace(List[I], 'page', S, []);
        end;
        //paint Left footer
        if List[0] <> '' then
          ACanvas.TextOut(ScaleX(Integer(GridPrinter.PrintOptions.MarginLeft) + Grid.Canvas.TextWidth(List[0])),
            PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.MarginBottom) + Grid.Canvas.TextHeight(List[0])),
            List[0]);
        //paint center footer
        if List[1] <> '' then
          ACanvas.TextOut((PageWidth div 2) - (ScaleX(Grid.Canvas.TextWidth(List[1])) div 2), PageHeight -
            ScaleY(Integer(GridPrinter.PrintOptions.MarginBottom) + Grid.Canvas.TextHeight(List[1])), List[1]);
        //paint Right footer
        if List[2] <> '' then
          ACanvas.TextOut(PageWidth - ScaleX(Integer(GridPrinter.PrintOptions.MarginRight) +
            Grid.Canvas.TextWidth(List[2]) + 10), PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.MarginBottom) +
            Grid.Canvas.TextHeight(List[2])), List[2]);
        List.Free;
      end;

    if DoPaint then
    begin
      ACanvas.Font.Size := Grid.Font.Size;
      Grid.Canvas.Font := TmpFont;
    end;
    Y := Y + ScaleY(GridPrinter.PrintOptions.PageTitleMargin);
    DrawCells(0);
  end;

begin
  // Do not set the Printer's orientation after BeginDoc because this might lead
  // to a blank page.
  if Mode = pmPreview then
    Printer.Orientation := GridPrinter.PrintOptions.Orientation;

  //page size
  PageWidth := Printer.PageWidth;
  PageHeight := Printer.PageHeight;
  if Mode = pmPreview then
  begin
    PageWidth := PageWidth div ((GetDeviceCaps(Printer.Handle, LOGPIXELSX) div Screen.PixelsPerInch));
    PageHeight := PageHeight div ((GetDeviceCaps(Printer.Handle, LOGPIXELSY) div Screen.PixelsPerInch));
    FPrintImage.Width := PageWidth;
    FPrintImage.Height := PageHeight;
    ACanvas.Brush.Color := clWhite;
    ACanvas.FillRect(Rect(0, 0, PageWidth, PageHeight));
  end;
  HasLogo := False;
  if GridPrinter.PrintOptions.Logo <> '' then
    if FileExists(GridPrinter.PrintOptions.Logo) then
    begin
      LogoPic := TBitmap.Create;
      LogoPic.LoadFromFile(GridPrinter.PrintOptions.Logo);
      HasLogo := True;
      LogoPics := TBitmap.Create;
      LogoPics.Width := ScaleX(LogoPic.Width);
      LogoPics.Height := ScaleY(LogoPic.Height);
      LogoPic.PixelFormat := pf24bit;
      LogoPics.PixelFormat := pf24bit;
      TJvPaintFX.SmoothResize(LogoPic, LogoPics);
    end;

  if Mode <> pmPageCount then
  begin
    ACanvas.Font := Grid.Font;
    ACanvas.Font.Color := clBlack;
  end;
  PageCol := 0;
  FromCol := -1;
  ToCol := -0;
  //scan cols
  repeat
    //Scan missing cols
    if FromCol = ToCol then
      Inc(FromCol)
    else
      FromCol := ToCol + 1;
    Inc(ToCol);
    //Get Cols with Width that fits page
    X := GridPrinter.PrintOptions.MarginLeft;
    for I := FromCol to Grid.ColCount - 1 do
    begin
      Inc(X, ScaleX(Grid.ColWidths[I] + 1));
      if X <= (PageWidth - Integer(GridPrinter.PrintOptions.MarginRight)) then
        ToCol := I;
    end;
    PageRow := 1;
    Inc(PageCol);
    //Mode = PageCount
    Inc(FPageCount);
    //preview mode
    DoPaint := (((Mode = pmPreview) and (FPageCount = GridPrinter.PrintOptions.PreviewPage)) or (Mode = pmPrint));
    //Header & Footer
    DrawTitle;
    //Contents
    IRow := FromRow;
    repeat
      //      Inc(Y, ScaleY(RowHeights[IRow]));
      if (Y + ScaleY(RowHeights[IRow])) <= (PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.MarginBottom) + 20 +
        HFooter)) then
      begin //draw contents to Canvas
        DrawCells(IRow);
        Inc(IRow);
      end
      else //New page
      begin
        if DoPaint and (Mode = pmPreview) then
          Exit;
        if Mode = pmPrint then
          Printer.NewPage;
        Inc(FPageCount); //pagecount
        DoPaint := (((Mode = pmPreview) and (FPageCount = GridPrinter.PrintOptions.PreviewPage)) or (Mode = pmPrint));
        Inc(PageRow);
        DrawTitle;
      end;
      if (IRow = ToRow + 1) and (ToCol < Grid.ColCount - 1) and (Y <= PageHeight - ScaleY(20)) then
      begin
        if DoPaint and (Mode = pmPreview) then
          Exit;
        if Mode = pmPrint then
          Printer.NewPage;
        DrawTitle;
      end;
    until
      IRow = ToRow + 1;
  until
    ToCol = Grid.ColCount - 1;
  if HasLogo then
  begin
    LogoPic.Free;
    LogoPics.Free;
  end;
end;

procedure TJvGridPreviewForm.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridPreviewForm.UpdateRowHeights;
var
  C, MaxH, H, ARow: Integer;
  R: TRect;
  S: string;
begin
  SetLength(RowHeights, Grid.RowCount);
  RowHeights[0] := Grid.RowHeights[0];
  MaxH := Grid.DefaultRowHeight;
  for ARow := 1 to Grid.RowCount - 1 do
  begin
    for C := 0 to Grid.ColCount - 1 do
    begin
      S := Grid.Cells[C, ARow];
      R := Grid.CellRect(C, ARow);
      DrawText(Grid.Canvas.Handle, PChar(S), -1, R, DT_CALCRECT or DT_WORDBREAK);
      H := R.Bottom - R.Top + 1;
      if H > MaxH then
        MaxH := H;
    end;
    if GridPrinter.WordWrap then
      RowHeights[ARow] := MaxH
    else
      RowHeights[ARow] := Grid.RowHeights[ARow];
  end;
end;

function TJvGridPreviewForm.PageCount: Integer;
begin
  Result := 0;
  if not Assigned(FGrid) then
    Exit;
  UpdateRowHeights;
  FPageCount := 0;
  DrawToCanvas(nil, pmPageCount, 1, Grid.RowCount - 1);
  Result := FPageCount;
end;

procedure TJvGridPreviewForm.UpdatePreview(ACanvas: TCanvas);
begin
  FPageCount := 0;
  DrawToCanvas(ACanvas, pmPreview, 1, Grid.RowCount - 1);
end;

procedure TJvGridPreviewForm.Print;
begin
  if not Assigned(FGrid) then
    Exit;
  UpdateRowHeights;
  if Printer.Printers.Count = 0 then
  begin
    MessageDlg(RsNoPrinterIsInstalled, mtError, [mbOK], 0);
    Exit;
  end;
  Printer.Title := GridPrinter.PrintOptions.JobTitle;
  Printer.Copies := GridPrinter.PrintOptions.Copies;
  Printer.Orientation := GridPrinter.PrintOptions.Orientation;
  Printer.BeginDoc;
  DrawToCanvas(Printer.Canvas, pmPrint, 1, Grid.ColCount - 1);
  Printer.EndDoc;
end;

procedure TJvGridPreviewForm.MarginClick(Sender: TObject;
  Button: TUDBtnType);
begin
  case Margins.ItemIndex of
    -1:
      Exit;
    0:
      GridPrinter.PrintOptions.MarginTop := Margin.Position;
    1:
      GridPrinter.PrintOptions.PageTitleMargin := Margin.Position;
    2:
      GridPrinter.PrintOptions.MarginLeft := Margin.Position;
    3:
      GridPrinter.PrintOptions.MarginRight := Margin.Position;
    4:
      GridPrinter.PrintOptions.MarginBottom := Margin.Position;
    5:
      GridPrinter.PrintOptions.LeftPadding := Margin.Position;
    6:
      GridPrinter.PrintOptions.HeaderSize := Margin.Position;
    7:
      GridPrinter.PrintOptions.FooterSize := Margin.Position;
  end;
  if cklive.Checked then
    btnshow.Click;
end;

procedure TJvGridPreviewForm.PreviewPageClick(Sender: TObject;
  Button: TUDBtnType);
begin
  {$IFDEF VCL}
  if PreviewPage.Position < PreviewPage.Min then
    PreviewPage.Position := PreviewPage.Min;
  if PreviewPage.Position > PreviewPage.Max then
    PreviewPage.Position := PreviewPage.Max;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if PreviewPage.Position < PreviewPage.Min then
    PreviewPage.Position := PreviewPage.Min;
  if PreviewPage.Position > PreviewPage.Max then
    PreviewPage.Position := PreviewPage.Max;
  {$ENDIF VisualCLX}
  GridPrinter.PrintOptions.PreviewPage := PreviewPage.Position;
  if cklive.Checked then
    btnshow.Click;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

