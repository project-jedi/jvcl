unit JvFormAsBitmapFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, Mask, ComCtrls, Buttons, ImgList;

type
  TForm1 = class(TForm)
    imgRegion: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PrintDialog1: TPrintDialog;
    procedure imgRegionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);

  private
    { Private declarations }
    FRGN: HRGN;
    procedure MakeTransparent(AColor: TColor);
  protected
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  JclGraphics, JvImage, Printers;

{$R *.dfm}

procedure TForm1.MakeTransparent(AColor: TColor);
begin
  Screen.Cursor := crHourGlass;
  try
    FRGN := CreateRegionFromBitmap(imgRegion.Picture.Bitmap, AColor, rmExclude);
    if FRGN <> 0 then
      SetWindowRgn(Handle, FRGN, true);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.imgRegionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  MakeTransparent(clFuchsia);
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  Close;
end;

procedure PrintDC(AHDC: HDC; APrinter: TPrinter; ALeft, ATop, AWidth, AHeight: integer);
var
  MemDC: HDC;
  isDcPalDevice: BOOL;
  MemBitmap, OldMemBitmap: hBitmap;
  pal, Oldpal: hPalette;
  hDibHeader, hBits: THandle;
  pDibHeader, pBits: Pointer;
  ScaleX, ScaleY: Double;
  ppal: PLOGPALETTE;
  i: integer;
begin
  {Create a compatible dc}
  MemDc := CreateCompatibleDc(AHDC);
  {create a bitmap}
  MemBitmap := CreateCompatibleBitmap(AHDC, AWidth, AHeight);
  {select the bitmap into the dc}
  OldMemBitmap := SelectObject(MemDc, MemBitmap);

  {Lets prepare to try a fixup for broken video drivers}
  isDcPalDevice := false;
  pPal := nil;
  pal := 0;
  OldPal := 0;
  if GetDeviceCaps(AHDC, RASTERCAPS) and RC_PALETTE = RC_PALETTE then
  begin
    GetMem(pPal, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)));
    FillChar(pPal^, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)), #0);
    pPal^.palVersion := $300;
    pPal^.palNumEntries :=
      GetSystemPaletteEntries(AHDC,
      0,
      256,
      pPal^.palPalEntry);
    if pPal^.PalNumEntries <> 0 then
    begin
      pal := CreatePalette(pPal^);
      oldPal := SelectPalette(MemDc, Pal, false);
      isDcPalDevice := true
    end
    else
      FreeMem(pPal, sizeof(TLOGPALETTE) +
        (255 * sizeof(TPALETTEENTRY)));
  end;

  {copy from the screen to the memdc/bitmap}
  BitBlt(MemDc, 0, 0, AWidth, AHeight, AHDC, ALeft, ATop, SRCCOPY);

  if isDcPalDevice = true then
  begin
    SelectPalette(MemDc, OldPal, false);
    DeleteObject(Pal);
  end;

  {unselect the bitmap}
  SelectObject(MemDc, OldMemBitmap);
  {delete the memory AHDC}
  DeleteDc(MemDc);
  {Allocate memory for a DIB structure}
  hDibHeader := GlobalAlloc(GHND,
    sizeof(TBITMAPINFO) +
    (sizeof(TRGBQUAD) * 256));
  {get a pointer to the alloced memory}
  pDibHeader := GlobalLock(hDibHeader);

  {fill in the dib structure with info on the way we want the DIB}
  FillChar(pDibHeader^,
    sizeof(TBITMAPINFO) + (sizeof(TRGBQUAD) * 256),
    #0);
  PBITMAPINFOHEADER(pDibHeader)^.biSize :=
    sizeof(TBITMAPINFOHEADER);
  PBITMAPINFOHEADER(pDibHeader)^.biPlanes := 1;
  PBITMAPINFOHEADER(pDibHeader)^.biBitCount := 8;
  PBITMAPINFOHEADER(pDibHeader)^.biWidth := AWidth;
  PBITMAPINFOHEADER(pDibHeader)^.biHeight := AHeight;
  PBITMAPINFOHEADER(pDibHeader)^.biCompression := BI_RGB;

  {find out how much memory for the bits}
  GetDIBits(AHDC, MemBitmap, 0, AHeight, nil, TBitmapInfo(pDibHeader^), DIB_RGB_COLORS);

  {Alloc memory for the bits}
  hBits := GlobalAlloc(GHND,
    PBitmapInfoHeader(pDibHeader)^.BiSizeImage);
  {Get a pointer to the bits}
  pBits := GlobalLock(hBits);

  {Call fn again, but this time give us the bits!}
  GetDIBits(AHDC, MemBitmap, 0, AHeight, pBits, PBitmapInfo(pDibHeader)^, DIB_RGB_COLORS);

  {Lets try a fixup for broken video drivers}
  if isDcPalDevice = true then
  begin
    for i := 0 to (pPal^.PalNumEntries - 1) do
    begin
      PBitmapInfo(pDibHeader)^.bmiColors[i].rgbRed :=
        pPal^.palPalEntry[i].peRed;
      PBitmapInfo(pDibHeader)^.bmiColors[i].rgbGreen :=
        pPal^.palPalEntry[i].peGreen;
      PBitmapInfo(pDibHeader)^.bmiColors[i].rgbBlue :=
        pPal^.palPalEntry[i].peBlue;
    end;
    FreeMem(pPal, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)));
  end;

  {Delete the bitmap}
  DeleteObject(MemBitmap);

  {Start print job}
  APrinter.BeginDoc;

  {Scale print size}
  if APrinter.PageWidth < APrinter.PageHeight then
  begin
    ScaleX := APrinter.PageWidth;
    ScaleY := AHeight * (APrinter.PageWidth / AWidth);
  end
  else
  begin
    ScaleX := AWidth * (APrinter.PageHeight / AHeight);
    ScaleY := APrinter.PageHeight;
  end;

  {Just in case the printer driver is a palette device}
  isDcPalDevice := false;
  if GetDeviceCaps(APrinter.CAnvas.Handle, RASTERCAPS) and
    RC_PALETTE = RC_PALETTE then
  begin
    {Create palette from dib}
    GetMem(pPal, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)));
    FillChar(pPal^, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)), #0);
    pPal^.palVersion := $300;
    pPal^.palNumEntries := 256;
    for i := 0 to (pPal^.PalNumEntries - 1) do
    begin
      pPal^.palPalEntry[i].peRed :=
        PBitmapInfo(pDibHeader)^.bmiColors[i].rgbRed;
      pPal^.palPalEntry[i].peGreen :=
        PBitmapInfo(pDibHeader)^.bmiColors[i].rgbGreen;
      pPal^.palPalEntry[i].peBlue :=
        PBitmapInfo(pDibHeader)^.bmiColors[i].rgbBlue;
    end;
    pal := CreatePalette(pPal^);
    FreeMem(pPal, sizeof(TLOGPALETTE) +
      (255 * sizeof(TPALETTEENTRY)));
    oldPal := SelectPalette(APrinter.Canvas.Handle, Pal, false);
    isDcPalDevice := true
  end;

  {send the bits to the printer}
  StretchDiBits(APrinter.Canvas.Handle,
    0, 0,
    Round(scaleX), Round(scaleY),
    0, 0,
    AWidth, AHeight,
    pBits,
    PBitmapInfo(pDibHeader)^,
    DIB_RGB_COLORS,
    SRCCOPY);

  {Just incase you printer driver is a palette device}
  if isDcPalDevice = true then
  begin
    SelectPalette(APrinter.Canvas.Handle, oldPal, false);
    DeleteObject(Pal);
  end;

  {Clean up allocated memory}
  GlobalUnlock(hBits);
  GlobalFree(hBits);
  GlobalUnlock(hDibHeader);
  GlobalFree(hDibHeader);

  {End the print job}
  APrinter.EndDoc;
end;

procedure TForm1.Label4Click(Sender: TObject);
var
  ADC: HDC;
begin
  if PrintDialog1.Execute then
  begin
    ADC := GetDC(0);
    try
      PrintDC(Canvas.Handle, Printer, 0, 0, Width, Height);
    finally
      ReleaseDC(0, ADC);
    end;
  end;
end;

end.

