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
    procedure imgRegionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Label3Click(Sender: TObject);

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
  JclGraphics;

{$R *.dfm}

procedure TForm1.MakeTransparent(AColor:TColor);
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

end.

