{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridPreview.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvGridPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, grids, JvGridPrinter, StdCtrls, Spin, ComCtrls, Buttons, printers,
  ExtDlgs;

type

  TJvGridPreviewF = class(TForm)
    ScrollBox1: TScrollBox;
    PreviewImage: TImage;
    Panel1: TPanel;
    Header: TEdit;
    Headers: TListBox;
    Margin: TSpinEdit;
    ckborders: TCheckBox;
    Margins: TListBox;
    btnprint: TSpeedButton;
    previewpage: TSpinEdit;
    btnshow: TSpeedButton;
    lblpages: TLabel;
    cklive: TCheckBox;
    btnsetup: TSpeedButton;
    PrinterSetupDialog1: TPrinterSetupDialog;
    btnfull: TSpeedButton;
    OpenPictureDialog1: TOpenPictureDialog;
    btnpic: TSpeedButton;
    procedure btnshowClick(Sender: TObject);
    procedure MarginsClick(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure MarginChange(Sender: TObject);
    procedure HeaderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckbordersClick(Sender: TObject);
    procedure previewpageChange(Sender: TObject);
    procedure HeadersClick(Sender: TObject);
    procedure ckliveClick(Sender: TObject);
    procedure btnsetupClick(Sender: TObject);
    procedure btnfullClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewImageClick(Sender: TObject);
    procedure btnpicClick(Sender: TObject);
  private
    FGridPrinter: TJvGridPrinter;
    FPrintImage: TBitmap;
    FGrid: TStringGrid;
    procedure SetGridPrinter(const Value: TJvGridPrinter);
    procedure FullSize;
    procedure Zoom(factor: extended);
    procedure SetGrid(const Value: TStringGrid);
    procedure UpdaterowHeights;
    function PageCount: integer;
    procedure UpdatePreview(Acanvas: TCanvas);
    { Private declarations }
  public
    { Public declarations }
    procedure DrawToCanvas(ACanvas: TCanvas; Mode: TPrintMode; FromRow,
      ToRow: Integer);
    procedure Print;
  published
    property GridPrinter: TJvGridPrinter read FGridPrinter write SetGridPrinter;
    property Grid: TStringGrid read FGrid write SetGrid;
  end;

var
  JvGridPreviewF: TJvGridPreviewF;
  FPageCount: cardinal;

procedure SmoothResize(var Src, Dst: TBitmap);

implementation

{$R *.DFM}

{ TJvGridPreviewF }

const
  cr = chr(13) + chr(10);
  tab = chr(9);

var
  RowHeights: array of integer;

procedure SmoothResize(var Src, Dst: TBitmap);
var
  x, y, xP, yP,
    yP2, xP2: Integer;
  Read, Read2: PByteArray;
  t, z, z2, iz2: Integer;
  pc: PBytearray;
  w1, w2, w3, w4: Integer;
  Col1r, col1g, col1b, Col2r, col2g, col2b: byte;
begin
  xP2 := ((src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    Read := src.ScanLine[yP shr 15];
    if yP shr 16 < src.Height - 1 then
      Read2 := src.ScanLine[yP shr 15 + 1]
    else
      Read2 := src.ScanLine[yP shr 15];
    pc := Dst.scanline[y];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      Col1r := Read[t * 3];
      Col1g := Read[t * 3 + 1];
      Col1b := Read[t * 3 + 2];
      Col2r := Read2[t * 3];
      Col2g := Read2[t * 3 + 1];
      Col2b := Read2[t * 3 + 2];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[x * 3 + 2] :=
        (Col1b * w1 + Read[(t + 1) * 3 + 2] * w2 +
        Col2b * w3 + Read2[(t + 1) * 3 + 2] * w4) shr 15;
      pc[x * 3 + 1] :=
        (Col1g * w1 + Read[(t + 1) * 3 + 1] * w2 +
        Col2g * w3 + Read2[(t + 1) * 3 + 1] * w4) shr 15;
      pc[x * 3] :=
        (Col1r * w1 + Read2[(t + 1) * 3] * w2 +
        Col2r * w3 + Read2[(t + 1) * 3] * w4) shr 15;
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

procedure TJvGridPreviewF.SetGridPrinter(const Value: TJvGridPrinter);
begin
  FGridPrinter := Value;
end;

procedure TJvGridPreviewF.btnshowClick(Sender: TObject);
begin
  if assigned(FGridPrinter) then
  begin
    UpdatePreview(FprintImage.canvas);
    PreviewImage.picture.bitmap.Assign(FprintImage);
  end;
end;

procedure TJvGridPreviewF.MarginsClick(Sender: TObject);
var
  index: integer;
begin
  index := margins.itemindex;
  if index = -1 then exit;
  if index = 0 then
    margin.Value := GridPrinter.PrintOptions.MarginTop
  else if index = 1 then
    margin.value := GridPrinter.PrintOptions.PageTitleMargin
  else if index = 2 then
    margin.value := GridPrinter.PrintOptions.MarginLeft
  else if index = 3 then
    margin.value := GridPrinter.PrintOptions.MarginRight
  else if index = 4 then
    margin.value := GridPrinter.PrintOptions.MarginBottom
  else if index = 5 then
    margin.value := GridPrinter.PrintOptions.Leftpadding
  else if index = 6 then
    margin.value := GridPrinter.PrintOptions.HeaderSize
  else if index = 7 then
    margin.value := GridPrinter.PrintOptions.FooterSize;
  if index > 5 then
  begin
    margin.MinValue := 6;
    margin.MaxValue := 72;
  end
  else
  begin
    margin.MinValue := 0;
    margin.MaxValue := 400;
  end;
end;

procedure TJvGridPreviewF.btnprintClick(Sender: TObject);
begin
  Print;
end;

procedure TJvGridPreviewF.MarginChange(Sender: TObject);
var
  index: integer;
begin
  index := margins.ItemIndex;
  if index = -1 then exit;
  if index = 0 then
    GridPrinter.PrintOptions.MarginTop := margin.Value
  else if index = 1 then
    GridPrinter.PrintOptions.PageTitleMargin := margin.value
  else if index = 2 then
    GridPrinter.PrintOptions.MarginLeft := margin.value
  else if index = 3 then
    GridPrinter.PrintOptions.MarginRight := margin.value
  else if index = 4 then
    Gridprinter.PrintOptions.MarginBottom := margin.value
  else if index = 5 then
    GridPrinter.PrintOptions.Leftpadding := margin.value
  else if index = 6 then
    GridPrinter.PrintOptions.HeaderSize := margin.value
  else if index = 7 then
    GridPrinter.PrintOptions.FooterSize := margin.value;
  if cklive.checked then btnshow.Click;
end;

procedure TJvGridPreviewF.HeaderChange(Sender: TObject);
var
  index: integer;
begin
  index := headers.ItemIndex;
  if index = -1 then exit;
  if index = 0 then
    GridPrinter.PrintOptions.PageTitle := header.text
  else if index = 1 then
    GridPrinter.PrintOptions.PageFooter := header.text
  else if index = 2 then
    GridPrinter.PrintOptions.DateFormat := header.text
  else if index = 3 then
    GridPrinter.PrintOptions.TimeFormat := header.text
  else if index = 4 then
    GridPrinter.PrintOptions.Logo := header.text;
  if cklive.checked then btnshow.Click;
end;

procedure TJvGridPreviewF.FormShow(Sender: TObject);
begin
  UpdateRowHeights;
  fPageCount := 0;
  DrawToCanvas(FPrintImage.Canvas, pmPreview, 1, Grid.RowCount - 1);
  PreviewImage.picture.bitmap.Assign(FprintImage);
  Header.text := GridPrinter.PrintOptions.PageTitle;
  Margin.Value := GridPrinter.Printoptions.margintop;
  Margins.itemindex := 0;
  previewpage.MaxValue := PageCount;
  lblpages.caption := 'of ' + inttostr(previewpage.maxvalue);
  GridPrinter.PrintOptions.PreviewPage := 1;
  previewpage.value := 1;
  ckBorders.Checked := (GridPrinter.printoptions.borderstyle = bssingle);
  header.text := GridPrinter.PrintOptions.PageTitle;
  headers.itemindex := 0;
  btnshow.click;
end;

procedure TJvGridPreviewF.ckbordersClick(Sender: TObject);
begin
  if ckborders.checked then
    GridPrinter.PrintOptions.BorderStyle := bssingle
  else
    GridPrinter.PrintOptions.BorderStyle := bsnone;
  if cklive.checked then btnshow.Click;
end;

procedure TJvGridPreviewF.previewpageChange(Sender: TObject);
begin
  if previewpage.value < previewpage.MinValue then
    previewpage.value := previewpage.MinValue;
  if previewpage.value > previewpage.MaxValue then
    previewpage.value := previewpage.MaxValue;
  GridPrinter.PrintOptions.PreviewPage := previewpage.Value;
  if cklive.checked then btnshow.Click;
end;

procedure TJvGridPreviewF.HeadersClick(Sender: TObject);
var
  index: integer;
begin
  index := headers.itemindex;
  if index = -1 then exit;
  if index = 0 then
    header.text := GridPrinter.PrintOptions.PageTitle
  else if index = 1 then
    header.text := GridPrinter.PrintOptions.PageFooter
  else if index = 2 then
    header.text := GridPrinter.PrintOptions.DateFormat
  else if index = 3 then
    header.text := GridPrinter.PrintOptions.TimeFormat
  else if index = 4 then
    header.text := GridPrinter.printoptions.Logo;

end;

procedure TJvGridPreviewF.ckliveClick(Sender: TObject);
begin
  if cklive.checked then btnshow.Click;
end;

procedure TJvGridPreviewF.btnsetupClick(Sender: TObject);
begin
  if printersetupdialog1.Execute then
  begin
    GridPrinter.PrintOptions.Orientation := printer.Orientation;
    if cklive.checked then btnshow.Click;
  end;
end;

procedure TJvGridPreviewF.FullSize;
var
  bm: tbitmap;
  w, h: integer;
begin
  w := Fprintimage.width;
  h := FprintImage.Height;
  bm := tbitmap.create;
  bm.Width := scrollbox1.ClientWidth;
  bm.height := round(h / w * bm.width);
  FPrintImage.PixelFormat := pf24bit;
  bm.PixelFormat := pf24bit;
  smoothresize(FprintImage, bm);
  previewimage.picture.bitmap.assign(bm);
  bm.free;
end;

procedure TJvGridPreviewF.btnfullClick(Sender: TObject);
begin
  FullSize;
end;

procedure TJvGridPreviewF.FormCreate(Sender: TObject);
begin
  FprintImage := TBitmap.Create;
end;

procedure TJvGridPreviewF.FormDestroy(Sender: TObject);
begin
  FPrintImage.free;
end;

procedure TJvGridPreviewF.PreviewImageClick(Sender: TObject);
var
  w, w1: integer;
begin
  w1 := PreviewImage.picture.bitmap.width;
  w := FPrintImage.width;
  if (round(w * 0.8) < w1) then
  begin
    PreviewImage.Picture.Bitmap.Assign(FPrintImage);
  end
  else
  begin
    Zoom(w1 / w / 0.8);
  end;
end;

procedure TJvGridPreviewF.Zoom(factor: extended);
var
  bm: tbitmap;
  w, h: integer;
begin
  w := Fprintimage.width;
  h := FprintImage.Height;
  bm := tbitmap.create;
  bm.Width := round(factor * w);
  bm.height := round(h / w * bm.width);
  FPrintImage.PixelFormat := pf24bit;
  bm.PixelFormat := pf24bit;
  smoothresize(FprintImage, bm);
  previewimage.picture.bitmap.assign(bm);
  bm.free;
end;

procedure TJvGridPreviewF.btnpicClick(Sender: TObject);
begin
  if openpicturedialog1.Execute then
    if headers.itemindex = 4 then
    begin
      header.text := openpicturedialog1.filename;
      GridPrinter.PrintOptions.Logo := openpicturedialog1.filename;
      ;
    end;
end;

procedure TJvGridPreviewF.DrawToCanvas(ACanvas: TCanvas; Mode: TPrintMode; FromRow, ToRow: Integer);
var
  PageWidth, PageHeight, PageRow, PageCol, I, iRow, FromCol, ToCol, X, Y: Integer;
  DoPaint, haslogo: Boolean;
  Hheader, Hfooter: integer;
  logopic, logopics: TBitmap;

  function ScaleX(I: Integer): Integer;
  begin
    if Mode = pmPreview then
      Result := I
    else
      Result := round(I * (GetDeviceCaps(Printer.Handle, LOGPIXELSX) / Screen.PixelsPerInch));
  end;

  function ScaleY(I: Integer): Integer;
  begin
    if Mode = pmPreview then
      Result := I
    else
      Result := round(I * (GetDeviceCaps(Printer.Handle, LOGPIXELSY) / Screen.PixelsPerInch));
  end;

  procedure DrawCells(iRow: Integer);
  var
    iCol, I: Integer;
    R: TRect;
    drs: string;
    nr: boolean;
    v: extended;
  begin
    //Alignment must be done another day
    for iCol := FromCol to ToCol do
    begin
      //X Offset
      X := scaleX(GridPrinter.printoptions.marginleft);
      for I := FromCol to iCol - 1 do
        Inc(X, ScaleX(Grid.ColWidths[I] + 1));
      //Text Rect
      R := Rect(X, Y, X + ScaleX(Grid.ColWidths[iCol]), Y + ScaleY(RowHeights[iRow]));
      //Draw on the Canvas
      if DoPaint then
      begin
        if GridPrinter.PrintOptions.BorderStyle = bssingle then
        begin
          Acanvas.brush.Style := bsclear;
          Acanvas.Rectangle(r.left, r.top, r.right + ScaleX(2), r.bottom + scaleY(1));
        end;
        drs := Grid.Cells[iCol, iRow];
        nr := false;
        if ((irow = 0) and (icol > 0)) then
          Acanvas.font.style := Acanvas.Font.style + [fsbold]
        else
          Acanvas.font.style := Acanvas.Font.style - [fsbold];
        R.left := R.left + scaleX(GridPrinter.PrintOptions.Leftpadding);
        if (GridPrinter.WordWrap and (iCol <> 0) and (iRow <> 0)) then
        begin
          if (GridPrinter.NumbersalRight and (not nr)) then
          try
            v := strtofloat(drs);
            nr := true;
            drs := format(GridPrinter.NumberFormat, [v]);
          except
            // do nothing
          end;
          if nr then
            DrawText(Acanvas.handle, pchar(drs), -1, R, DT_WORDBREAK or DT_RIGHT)
          else
            DrawText(Acanvas.handle, pchar(drs), -1, R, DT_WORDBREAK or DT_LEFT)
        end
        else
        begin
          if (GridPrinter.NumbersalRight and (not nr)) then
          try
            v := strtofloat(drs);
            nr := true;
            drs := format(GridPrinter.NumberFormat, [v]);
          except
            // do nothing
          end;
          if nr then
            DrawText(Acanvas.handle, pchar(drs), -1, R, DT_SINGLELINE or DT_RIGHT)
          else
            DrawText(Acanvas.handle, pchar(drs), -1, R, DT_SINGLELINE or DT_LEFT);
        end;
      end;
    end;
    Inc(Y, ScaleY(RowHeights[iRow]));
  end;

  procedure DrawTitle; //draw Header and Footer
  var
    S, fstr: string;
    flist: tstringlist;
    i: integer;
    tmpfont: tfont; //I have no idea why you can't use gettextwidth when acanvas = printer.canvas, it returns wrong value
  begin
    tmpfont := nil; // removes bad warning.
    if DoPaint then
    begin
      ACanvas.Font.Size := GridPrinter.printOptions.HeaderSize;
      tmpfont := Grid.font;
      Grid.canvas.font := acanvas.font;
    end;
    //Title
    Y := ScaleY(GridPrinter.PrintOptions.MarginTop);
    S := GridPrinter.PrintOptions.PageTitle;
    HHeader := Grid.canvas.textheight(s);
    if haslogo then
      if logopic.Height > HHeader then HHeader := logopic.height;
    if DoPaint then
    begin
      if haslogo then
      begin
        Acanvas.Draw(scaleX(GridPrinter.printoptions.marginleft), Y, logopics);
      end;
      ACanvas.TextOut((PageWidth div 2) - (ScaleX(Grid.Canvas.TextWidth(S) div 2)), Y, S);
    end;
    Y := Y + ScaleY(HHeader);
    //Page nr
    S := 'Page ' + IntToStr(PageRow);
    if (ToCol < Grid.ColCount - 1) or (PageCol > 1) then
      S := S + '-' + IntToStr(PageCol);
    fstr := GridPrinter.Printoptions.PageFooter;
    HFooter := Grid.canvas.textheight(fstr);
    if fstr <> '' then
      if DoPaint then
      begin
        ACanvas.Font.Size := GridPrinter.printOptions.FooterSize;
        Grid.canvas.font := acanvas.font;
        HFooter := Grid.canvas.textheight(fstr);
        flist := tstringlist.create;
        flist.text := stringreplace(fstr, '|', cr, [rfreplaceall]);
        while flist.count < 3 do
          flist.Append('');
        for i := 0 to 2 do
        begin
          flist[i] := stringreplace(flist[i], 'date', formatdatetime(GridPrinter.PrintOptions.Dateformat, now), []);
          flist[i] := stringreplace(flist[i], 'time', formatdatetime(GridPrinter.PrintOptions.Timeformat, now), []);
          flist[i] := stringreplace(flist[i], 'page', s, []);
        end;
        //paint left footer
        if flist[0] <> '' then
          ACanvas.TextOut(scaleX(Integer(GridPrinter.Printoptions.marginleft) + Grid.Canvas.TextWidth(flist[0])), PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.marginbottom) + Grid.canvas.TextHeight(flist[0])), flist[0]);
        //paint center footer
        if flist[1] <> '' then
          ACanvas.TextOut((PageWidth div 2) - (scaleX(Grid.Canvas.TextWidth(flist[1])) div 2), PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.marginbottom) + Grid.canvas.TextHeight(flist[1])), flist[1]);
        //paint right footer
        if flist[2] <> '' then
          ACanvas.TextOut(PageWidth - scaleX(Integer(GridPrinter.Printoptions.marginright) + Grid.Canvas.TextWidth(flist[2]) + 10), PageHeight - ScaleY(Integer(GridPrinter.PrintOptions.marginbottom) + Grid.canvas.TextHeight(flist[2])), flist[2]);
        flist.free;
      end;

    if DoPaint then
    begin
      ACanvas.Font.Size := Grid.Font.Size;
      Grid.canvas.font := tmpfont; //Delphi warning is wrong
    end;
    Y := Y + ScaleY(GridPrinter.PrintOptions.PageTitleMargin);
    DrawCells(0);
  end;

begin
  //page size
  Printer.Orientation := GridPrinter.PrintOptions.Orientation;
  PageWidth := Printer.PageWidth;
  PageHeight := Printer.PageHeight;
  if Mode = pmPreview then
  begin
    PageWidth := PageWidth div ((GetDeviceCaps(Printer.Handle, LOGPIXELSX) div Screen.PixelsPerInch));
    PageHeight := PageHeight div ((GetDeviceCaps(Printer.Handle, LOGPIXELSY) div Screen.PixelsPerInch));
    FPrintImage.width := pagewidth;
    FPrintImage.height := pageheight;
    ACanvas.Brush.Color := ClWhite;
    ACanvas.FillRect(Rect(0, 0, PageWidth, PageHeight));
  end;
  haslogo := false;
  if GridPrinter.printoptions.Logo <> '' then
    if fileexists(GridPrinter.printoptions.logo) then
    begin
      logopic := tbitmap.create;
      logopic.LoadFromFile(GridPrinter.printoptions.logo);
      haslogo := true;
      logopics := tbitmap.create;
      logopics.width := scaleX(logopic.width);
      logopics.height := scaleY(logopic.height);
      logopic.PixelFormat := pf24bit;
      logopics.pixelformat := pf24bit;
      smoothresize(logopic, logopics);
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
    //Get Cols with width that fits page
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
    Inc(fPageCount);
    //preview mode
    DoPaint := (((Mode = pmPreview) and (fPageCount = GridPrinter.PrintOptions.PreviewPage)) or (Mode = pmPrint));
    //Header & Footer
    DrawTitle;
    //Contents
    iRow := FromRow;
    repeat
      //      Inc(Y, ScaleY(RowHeights[iRow]));
      if (Y + ScaleY(RowHeights[iRow])) <= (PageHeight - ScaleY(Integer(GridPrinter.Printoptions.marginbottom) + 20 + HFooter)) then
      begin //draw contents to canvas
        DrawCells(iRow);
        Inc(iRow);
      end
      else //New page
      begin
        if (DoPaint = True) and (Mode = pmPreview) then
          Exit;
        if Mode = pmPrint then
          Printer.NewPage;
        Inc(fPageCount); //pagecount
        DoPaint := (((Mode = pmPreview) and (fPageCount = GridPrinter.PrintOptions.PreviewPage)) or (Mode = pmPrint));
        Inc(PageRow);
        DrawTitle;
      end;
      if (iRow = ToRow + 1) and (ToCol < Grid.ColCount - 1) and (Y <= PageHeight - ScaleY(20)) then
      begin
        if (DoPaint = True) and (Mode = pmPreview) then
          Exit;
        if Mode = pmPrint then
          Printer.NewPage;
        DrawTitle;
      end;
    until
      iRow = ToRow + 1;
  until
    ToCol = Grid.ColCount - 1;
  if haslogo then
  begin
    logopic.free;
    logopics.free;
  end;
end;

procedure TJvGridPreviewF.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridPreviewF.UpdaterowHeights;
var
  c, maxh, h, arow: integer;
  R: trect;
  s: string;
begin
  setlength(RowHeights, Grid.rowcount);
  RowHeights[0] := Grid.rowheights[0];
  maxh := Grid.defaultrowheight;
  for arow := 1 to Grid.rowcount - 1 do
  begin
    for c := 0 to Grid.colcount - 1 do
    begin
      s := Grid.cells[c, Arow];
      R := Grid.CellRect(c, Arow);
      drawtext(Grid.canvas.handle, pchar(s), -1, R, DT_CALCRECT or DT_WORDBREAK);
      h := R.bottom - R.top + 1;
      if h > maxh then maxh := h;
    end;
    if GridPrinter.Wordwrap then
      RowHeights[Arow] := maxh
    else
      RowHeights[Arow] := Grid.rowheights[arow];
  end;
end;

function TJvGridPreviewF.PageCount: integer;
begin
  Result := 0;
  if not assigned(FGrid) then exit;
  UpdateRowHeights;
  fPageCount := 0;
  DrawToCanvas(nil, pmPageCount, 1, Grid.RowCount - 1);
  result := fPageCount;
end;

procedure TJvGridpreviewF.UpdatePreview(Acanvas: TCanvas);
begin
  fPageCount := 0;
  DrawToCanvas(ACanvas, pmPreview, 1, Grid.RowCount - 1);
end;

procedure TJvGridPreviewF.Print;
begin
  if not assigned(FGrid) then exit;
  UpdateRowHeights;
  if Printer.Printers.Count = 0 then
  begin
    MessageDlg('No Printer is installed', mtError, [mbOK], 0);
    Exit;
  end;
  Printer.Title := GridPrinter.PrintOptions.JobTitle;
  Printer.Copies := GridPrinter.PrintOptions.Copies;
  Printer.BeginDoc;
  DrawToCanvas(Printer.Canvas, pmPrint, 1, Grid.colcount - 1);
  Printer.EndDoc;
end;

end.
