unit MDIBkgndDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, Graphics, JvBackgrounds, MDIBkgndDemoFrame;

type
  TMDIMainForm = class(TForm)
    MainMenu1: TMainMenu;
    Image1: TMenuItem;
    ImageLoad: TMenuItem;
    OpenDialog: TOpenDialog;
    Background: TJvBackground;
    DemoFrame1: TDemoFrame;
    Panel1: TPanel;
    procedure ImageLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MDIMainForm: TMDIMainForm;

implementation

uses MDIBkgndDemoSettings;

{$R *.DFM}

procedure TMDIMainForm.ImageLoadClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    if Background.Image.Picture.Graphic <> nil
      then DefaultExt := GraphicExtension(TGraphicClass(Background.Image.Picture.Graphic.ClassType))
      else DefaultExt := GraphicExtension(TBitmap);
    if Execute then
    with Background.Image.Picture do
    begin
      LoadFromFile(FileName);
      BkgndImageSettings.TileWidth.MinValue := Width;
      BkgndImageSettings.TileHeight.MinValue := Height;
      BkgndImageSettings.TileWidth.Value := Width;
      BkgndImageSettings.TileHeight.Value := Height;
    end;
  end;
end;

procedure TMDIMainForm.FormCreate(Sender: TObject);
begin
  OpenDialog.Filter := GraphicFilter(TGraphic);
  Panel1.Brush.Style := bsClear;
end;

end.
