unit ViewerFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, ActnList, Menus;

type
  TfrmImageViewer = class(TForm)
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ActionList1: TActionList;
    acFullScreen: TAction;
    acClose: TAction;
    PopupMenu1: TPopupMenu;
    FullScreen1: TMenuItem;
    Close1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure acFullScreenExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure AdjustFormSize;
  public
    { Public declarations }
    class function View(const Filename: string; Transparent:boolean; BackColor:TColor): boolean; overload;
    class function View(Picture: TPicture; BackColor:TColor): boolean; overload;
  end;

implementation
uses
  Math;

{$R *.dfm}

{ TfrmImageViewer }

class function TfrmImageViewer.View(const Filename: string; Transparent:boolean; BackColor:TColor): boolean;
var
  frmImageViewer: TfrmImageViewer;
begin
  frmImageViewer := self.Create(Application);
  try
    frmImageViewer.Image1.Transparent := Transparent;
    frmImageViewer.Image1.Picture.LoadFromFile(Filename);
    frmImageViewer.Caption := Filename;
    frmImageViewer.ScrollBox1.Color := BackColor;
    with frmImageViewer.Image1 do
      frmImageViewer.StatusBar1.Panels[0].Text := Format('(%s) - %d x %d',
        [Picture.Graphic.ClassName, Picture.Width, Picture.Height]);
    frmImageViewer.Left := (Screen.Width - frmImageViewer.Width) div 2;
    frmImageViewer.Top := (Screen.Height - frmImageViewer.Height) div 2;
    frmImageViewer.ShowModal;
    Result := true;
  finally
    frmImageViewer.Free;
  end;
end;

class function TfrmImageViewer.View(Picture: TPicture; BackColor:TColor): boolean;
var
  frmImageViewer: TfrmImageViewer;
begin
  frmImageViewer := self.Create(Application);
  try
    frmImageViewer.Image1.Picture.Assign(Picture);
    frmImageViewer.Caption := Picture.Graphic.ClassName;
    frmImageViewer.ScrollBox1.Color := BackColor;
    frmImageViewer.StatusBar1.Panels[0].Text := Format(' (%s) - %d x %d',
      [Picture.Graphic.ClassName, Picture.Width, Picture.Height]);
    frmImageViewer.Left := (Screen.Width - frmImageViewer.Width) div 2;
    frmImageViewer.Top := (Screen.Height - frmImageViewer.Height) div 2;

    frmImageViewer.ShowModal;
    Result := true;

  finally
    frmImageViewer.Free;
  end;
end;

procedure TfrmImageViewer.FormResize(Sender: TObject);
begin
  // make sure these are set correctly
//  Image1.AutoSize := true;
//  Image1.Center := true;
  if (ScrollBox1.ClientWidth < Image1.Width) then
    Image1.Left := -ScrollBox1.HorzScrollBar.Position
  else
    Image1.Left := (ScrollBox1.ClientWidth - Image1.Width) div 2;
  if (ScrollBox1.ClientHeight < Image1.Height) then
    Image1.Top := -ScrollBox1.VertScrollBar.Position
  else
    Image1.Top := (ScrollBox1.ClientHeight - Image1.Height) div 2;
end;

procedure TfrmImageViewer.FormCreate(Sender: TObject);
begin
  // minimize flicker
  ScrollBox1.DoubleBuffered := true;
end;

procedure TfrmImageViewer.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  // handle wheel event in form so scrolbox doesn't have to be focused to scroll
  Handled := true;
  if ScrollBox1.VertScrollBar.IsScrollBarVisible and not (ssShift in Shift) then
    ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta
  else
    ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position - WheelDelta;
end;

procedure TfrmImageViewer.acFullScreenExecute(Sender: TObject);
var
  P: TWindowPlacement;
begin
  acFullScreen.Checked := not acFullScreen.Checked;
  FillChar(P, sizeof(P), 0);
  P.length := sizeof(P);
  // get default and current values
  GetWindowPlacement(Handle, @P);
  // adjust UI
  if acFullScreen.Checked then
  begin
    BorderStyle := bsNone;
    StatusBar1.Visible := false;
    P.showCmd := SW_SHOWMAXIMIZED;
  end
  else
  begin
    BorderStyle := bsSizeable;
    StatusBar1.Visible := true;
    P.showCmd := SW_RESTORE;
  end;
  // set new size/position
  SetWindowPlacement(Handle, @P);
end;

procedure TfrmImageViewer.AdjustFormSize;
var R:TRect;W,H:integer;
begin
  SystemParametersInfo(SPI_GETWORKAREA,0,@R,0);
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  with Image1 do
  begin
    if Picture.Width > self.Width then
    begin
      if Picture.Width + 32 < W then
        self.Width := Picture.Width + 32
      else
        self.Width := W;
      self.Left := R.Left + (W - self.Width) div 2;
    end;
    if Picture.Height + StatusBar1.Height > self.Height then
    begin
      if Picture.Height + 32 < H then
        self.Height := Picture.Height + StatusBar1.Height + 32
      else
        self.Height := H;
      self.Top := R.Top + (H - self.Height) div 2;
    end;
  end;
end;

procedure TfrmImageViewer.FormShow(Sender: TObject);
begin
  AdjustFormSize;
end;

procedure TfrmImageViewer.acCloseExecute(Sender: TObject);
begin
  if acFullScreen.Checked then
    acFullScreen.Execute
  else
    Close;
end;

procedure TfrmImageViewer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Value: integer;
begin
  if ssCtrl in Shift then
    Value := 10
  else
    Value := 1;
  case Key of
    VK_LEFT:
      with ScrollBox1.HorzScrollBar do
        Position := Position - Increment * Value;
    VK_RIGHT:
      with ScrollBox1.HorzScrollBar do
        Position := Position + Increment * Value;
    VK_UP:
      with ScrollBox1.VertScrollBar do
        Position := Position - Increment * Value;
    VK_DOWN:
      with ScrollBox1.VertScrollBar do
        Position := Position + Increment * Value;
    VK_PRIOR:
      ScrollBox1.VertScrollBar.Position := 0;
    VK_NEXT:
      with ScrollBox1.VertScrollBar do
        Position := Range;
    VK_HOME:
      ScrollBox1.HorzScrollBar.Position := 0;
    VK_END:
      with ScrollBox1.HorzScrollBar do
        Position := Range;
  end;
end;

end.

