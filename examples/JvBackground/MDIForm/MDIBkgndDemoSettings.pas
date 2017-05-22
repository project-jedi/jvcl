unit MDIBkgndDemoSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, JvBackgrounds;

type
  TBkgndImageSettings = class(TForm)
    ScrollBar1: TScrollBar;
    ShiftMode: TRadioGroup;
    Label1: TLabel;
    Mode: TComboBox;
    Label2: TLabel;
    ColorDialog: TColorDialog;
    TranspColor: TButton;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    TileWidth: TSpinEdit;
    TileHeight: TSpinEdit;
    GroupBox2: TGroupBox;
    ZigZag: TCheckBox;
    Transparent: TCheckBox;
    Enabled: TCheckBox;
    GrayMapped: TCheckBox;
    AutoSizeTile: TCheckBox;
    FitPictureSize: TCheckBox;
    procedure ScrollBar1Change(Sender: TObject);
    procedure ShiftModeClick(Sender: TObject);
    procedure ZigZagClick(Sender: TObject);
    procedure ModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TransparentClick(Sender: TObject);
    procedure EnabledClick(Sender: TObject);
    procedure GrayMappedClick(Sender: TObject);
    procedure TileWidthChange(Sender: TObject);
    procedure TileHeightChange(Sender: TObject);
    procedure AutoSizeTileClick(Sender: TObject);
    procedure TranspColorClick(Sender: TObject);
    procedure FitPictureSizeClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  BkgndImageSettings: TBkgndImageSettings;

implementation

uses
  Graphics,
  MDIBkgndDemoMain;

{$R *.DFM}

procedure TBkgndImageSettings.ScrollBar1Change(Sender: TObject);
begin
  MDIMainForm.Background.Image.Shift := ScrollBar1.Position;
  ZigZag.Checked := MDIMainForm.Background.Image.ZigZag;
end;

procedure TBkgndImageSettings.ShiftModeClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.ShiftMode := TJvBackgroundShiftMode(ShiftMode.ItemIndex);
end;

procedure TBkgndImageSettings.ZigZagClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.ZigZag := ZigZag.Checked;
end;

procedure TBkgndImageSettings.ModeChange(Sender: TObject);
begin
  MDIMainForm.Background.Image.Mode := TJvBackgroundMode(Mode.ItemIndex);
end;

procedure TBkgndImageSettings.FormCreate(Sender: TObject);
var
  Wp: TJvBackgroundImage;
begin
  Mode.ItemIndex := 0;
  GrayMapped.Enabled := True;
  Wp := MDIMainForm.Background.Image;
  with TileWidth do
  begin
    MinValue := Wp.Picture.Width;
    Value := Wp.TileWidth;
  end;
  with TileHeight do
  begin
    MinValue := Wp.Picture.Height;
    Value := Wp.TileHeight;
  end;
  AutoSizeTile.Checked := Wp.AutoSizeTile;
end;

procedure TBkgndImageSettings.TransparentClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.Transparent := Transparent.Checked;
end;

procedure TBkgndImageSettings.EnabledClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.Enabled := Enabled.Checked;
end;

procedure TBkgndImageSettings.GrayMappedClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.GrayMapped := GrayMapped.Checked;
end;

procedure TBkgndImageSettings.TileWidthChange(Sender: TObject);
begin
  MDIMainForm.Background.Image.TileWidth := TileWidth.Value;
end;

procedure TBkgndImageSettings.TileHeightChange(Sender: TObject);
begin
  MDIMainForm.Background.Image.TileHeight := TileHeight.Value;
end;

procedure TBkgndImageSettings.AutoSizeTileClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.AutoSizeTile := AutoSizeTile.Checked;
  MDIMainForm.Background.Image.TileWidth := TileWidth.Value;
  MDIMainForm.Background.Image.TileHeight := TileHeight.Value;
end;

procedure TBkgndImageSettings.TranspColorClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := MDIMainForm.Background.Image.TransparentColor;
    if Execute then
      MDIMainForm.Background.Image.TransparentColor := Color;
  end;
end;

procedure TBkgndImageSettings.FitPictureSizeClick(Sender: TObject);
begin
  MDIMainForm.Background.Image.FitPictureSize := FitPictureSize.Checked;
end;

end.
