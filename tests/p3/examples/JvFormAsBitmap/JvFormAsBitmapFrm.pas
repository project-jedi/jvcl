unit JvFormAsBitmapFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs;

type
  TForm1 = class(TForm)
    imgRegion: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlSpacer: TPanel;
    pnlControls: TPanel;
    Label1: TLabel;
    lblColorName: TLabel;
    lblHoverColorName: TLabel;
    btnActivate: TButton;
    btnImage: TButton;
    pnlColor: TPanel;
    pnlHoverColor: TPanel;
    btnClose: TButton;
    procedure btnActivateClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure imgRegionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlControlsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure imgRegionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnCloseClick(Sender: TObject);

  private
    { Private declarations }
    FActive: boolean;
    FRGN: HRGN;
    procedure SetTransparentColor(AColor: TColor);
  protected
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  JclGraphics;

{$R *.dfm}

procedure TForm1.btnActivateClick(Sender: TObject);
var
  i:integer;
  FControlRGN:HRGN;
begin
  Screen.Cursor := crHourGlass;
  try
    if FActive then
    begin
      SetWindowRgn(Handle, 0, true);
      FActive := false;
      btnActivate.Caption := '&Activate';
      DisableAlign;
      try
        imgRegion.Align := alLeft;
        pnlControls.Align := alLeft;
        pnlSpacer.Align := alLeft;
      finally
        EnableAlign;
      end;
    end
    else if (imgRegion.Picture.Bitmap <> nil) then
    begin
      FRGN := CreateRegionFromBitmap(imgRegion.Picture.Bitmap, pnlColor.Color, rmExclude);
      FActive := FRGN <> 0;
      if FActive then
      begin
        btnActivate.Caption := '&Deactivate';
        DisableAlign;
        try
          imgRegion.Align := alNone;
          pnlControls.Align := alNone;
          pnlSpacer.Align := alNone;
        finally
          EnableAlign;
        end;
        with pnlControls do
           FControlRGN := CreateRectRgn(Left, Top,
              Left + Width, Top + Height);
            CombineRgn(FRGN, FRGN, FControlRGN, RGN_OR);
            DeleteObject(FControlRGN);
{          for i := 0 to ControlCount - 1 do
          begin
            FControlRGN := CreateRectRgn(Left + 2 + Controls[i].Left, Top + 2 + Controls[i].Top,
              Left + 2 + Controls[i].Left + Controls[i].Width, Top + 2 + Controls[i].Top + Controls[i].Height);
            CombineRgn(FRGN, FRGN, FControlRGN, RGN_OR);
            DeleteObject(FControlRGN);
          end; }
        SetWindowRgn(Handle, FRGN, true);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnImageClick(Sender: TObject);
var
  tmp: boolean;
begin
  tmp := FActive;
  if tmp then
    btnActivate.Click;
  if OpenPictureDialog1.Execute then
  begin
    imgRegion.Picture.LoadFromFile(OpenPictureDialog1.Filename);
    imgRegion.Picture.Bitmap.Transparent := true;
    SetTransparentColor(imgRegion.Picture.Bitmap.TransparentColor);
  end;
  if tmp then
    btnActivate.Click;
end;

procedure TForm1.imgRegionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tmp: boolean;
begin
  tmp := FActive;
  if tmp then
    btnActivate.Click;
  SetTransparentColor(imgRegion.Canvas.Pixels[X, Y]);
  if tmp then
    btnActivate.Click;
end;

procedure TForm1.pnlControlsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if imgRegion.Picture.Bitmap <> nil then
    SetTransparentColor(imgRegion.Picture.Bitmap.TransparentColor);
end;

procedure TForm1.SetTransparentColor(AColor: TColor);
begin
  // strip off high bits
  pnlColor.Color := ColorToRGB(AColor) and $00FFFFFF;
  lblColorName.Caption := '$' + IntToHex(pnlColor.Color, 8);
end;

procedure TForm1.imgRegionMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  pnlHoverColor.Color := imgRegion.Canvas.Pixels[X, Y];
  lblHoverColorName.Caption := '$' + IntToHex(pnlHoverColor.Color, 8);
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

