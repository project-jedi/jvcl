unit ImageWindowMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ImgList, JvImageWindow, JvComponent;

type
  TImageWindowMainForm = class(TForm)
    ImageList1: TImageList;
    ImageWindow1: TJvImageWindow;
    MainMenu1: TMainMenu;
    Open1: TMenuItem;
    N2ndform1: TMenuItem;
    N2ndform2: TMenuItem;
    Columns1: TMenuItem;
    Change1: TMenuItem;
    Pics1: TMenuItem;
    procedure ImageWindow1Click(Sender: TObject);
    procedure N2ndform1Click(Sender: TObject);
    procedure N2ndform2Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure Pics1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  ImageWindowMainForm: TImageWindowMainForm;

implementation

uses ImageWindowChild1U, ImageWindowChild2U;

{$R *.DFM}

procedure TImageWindowMainForm.ImageWindow1Click(Sender: TObject);
begin
  Caption := Format('You clicked image %d',[ImageWindow1.ImageIndex]);
end;

procedure TImageWindowMainForm.N2ndform1Click(Sender: TObject);
begin
  ImageWindowChild1.Show;
end;

procedure TImageWindowMainForm.N2ndform2Click(Sender: TObject);
begin
  ImageWindowChild2.ShowModal;
end;

procedure TImageWindowMainForm.Change1Click(Sender: TObject);
var S:string;C:integer;
begin
  S := IntToStr(ImageWindow1.ColCount);
  if InputQuery('Change columns','Columns:',S) then
  begin
    C := StrToInt(S);
    with ImageWindow1 do
    begin
      if ImageCount < C then
        ImageCount := C;
      ColCount := C;
    end;
  end;
end;

procedure TImageWindowMainForm.Pics1Click(Sender: TObject);
var S:string;C:integer;
begin
  S := IntToStr(ImageWindow1.ImageCount);
  if InputQuery('Change pics display',
  Format('Show number of pics: ( max %d )',[ImageList1.Count]),S) then
  begin
    C := StrToInt(S);
    ImageWindow1.ImageCount := C;
  end;
end;

procedure TImageWindowMainForm.FormCreate(Sender: TObject);
begin
 ImageWindowChild1 := TImageWindowChild1.Create(nil);
 ImageWindowChild2 := TImageWindowChild2.Create(nil);
end;

procedure TImageWindowMainForm.FormDestroy(Sender: TObject);
begin
 ImageWindowChild1.free;
 ImageWindowChild2.free;
end;

end.


