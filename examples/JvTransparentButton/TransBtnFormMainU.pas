unit TransBtnFormMainU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, Buttons, JvTransBtn, JvComponent;

type
  TTransBtnFormMain = class(TForm)
    Image1: TImage;
    TransparentButton1: TJvTransparentButton;
    TransparentButton2: TJvTransparentButton;
    TransparentButton3: TJvTransparentButton;
    TransparentButton7: TJvTransparentButton;
    TransparentButton8: TJvTransparentButton;
    TransparentButton9: TJvTransparentButton;
    TransparentButton6: TJvTransparentButton;
    TransparentButton10: TJvTransparentButton;
    TransparentButton11: TJvTransparentButton;
    Label1: TLabel;
    Label2: TLabel;
    PopupMenu1: TPopupMenu;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Allgroups1: TMenuItem;
    Selectedgroups1: TMenuItem;
    Delete1: TMenuItem;
    Previous1: TMenuItem;
    N2: TMenuItem;
    Exit2: TMenuItem;
    TransparentButton5: TJvTransparentButton;
    Label3: TLabel;
    TransparentButton15: TJvTransparentButton;
    TransparentButton12: TJvTransparentButton;
    TransparentButton13: TJvTransparentButton;
    TransparentButton14: TJvTransparentButton;
    procedure FormPaint(Sender: TObject);
    procedure TransparentButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1MouseEnter(Sender: TObject);
    procedure TransparentButton1MouseExit(Sender: TObject);
    procedure TB1Click(Sender: TObject);
    procedure TransparentButton4MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1Click(Sender: TObject);
    procedure TransparentButton3Click(Sender: TObject);
    procedure TransparentButton10Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TransparentButton6Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
  end;

var
  TransBtnFormMain: TTransBtnFormMain;

implementation

{$R *.DFM}

function GetOS:string;
begin
  case Win32Platform of
  VER_PLATFORM_WIN32_NT: Result := 'Windows NT 4.0';
  VER_PLATFORM_WIN32_WINDOWS: Result := 'Windows 95';
  VER_PLATFORM_WIN32S: Result := 'Windows 3.1 with Win32s';
  end;
end;

{ tile the background }
procedure TTransBtnFormMain.FormPaint(Sender: TObject);
var
  X, Y, W, H: LongInt;
begin
  with Image1.Picture.Bitmap do begin
    W := Width;
    H := Height;
  end;
  Y := 0;
  while Y < Height do begin
    X := 0;
    while X < Width do begin
      Canvas.Draw(X, Y, Image1.Picture.Bitmap);
      Inc(X, W);
    end;
    Inc(Y, H);
  end;
end;

procedure TTransBtnFormMain.TransparentButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     self.Caption := 'Down';
end;

procedure TTransBtnFormMain.TransparentButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     self.Caption := 'Up';
end;

procedure TTransBtnFormMain.TransparentButton1MouseEnter(Sender: TObject);
begin
     self.Caption := 'MouseEnter';
end;

procedure TTransBtnFormMain.TransparentButton1MouseExit(Sender: TObject);
begin
  self.Caption := 'MouseExit';
end;

procedure TTransBtnFormMain.TB1Click(Sender: TObject);
begin
    self.Caption := 'Clicked';
end;

procedure TTransBtnFormMain.TransparentButton4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    self.Caption := 'MouseMove';
end;

procedure TTransBtnFormMain.TransparentButton1Click(Sender: TObject);
begin
  TransparentButton2.Enabled := not TransparentButton2.Enabled;
  TransparentButton3.Enabled := not TransparentButton2.Enabled;
end;


procedure TTransBtnFormMain.TransparentButton3Click(Sender: TObject);
begin
     ShowMessage('Clicked button. (try the shortkey Alt+W too)');
end;

procedure TTransBtnFormMain.TransparentButton10Click(Sender: TObject);
begin
  TransparentButton6.Down := not TransparentButton6.Down;
end;

procedure TTransBtnFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then
     begin
       Key := #0;
       TransparentButton1.OnClick(nil);
     end;
end;

procedure TTransBtnFormMain.TransparentButton6Click(Sender: TObject);
begin
  TransparentButton6.Down := not TransparentButton6.Down;
  ShowMessage('OS is: ' + GetOS + #13'(Note that an OnClick event is generated when going down and when going up)');
end;

procedure TTransBtnFormMain.Exit2Click(Sender: TObject);
begin
  Close;
end;

end.

