unit JvZoomMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvZoom, ExtCtrls, JvComponent, JvCaptionPanel;
  
type
  TJvZoomMainForm = class(TForm)
    Label1: TLabel;
    JvZoom1: TJvZoom;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  JvZoomMainForm: TJvZoomMainForm;

implementation

{$R *.DFM}

procedure TJvZoomMainForm.CheckBox1Click(Sender: TObject);
begin
  JvZoom1.active := checkbox1.checked;
end;

procedure TJvZoomMainForm.Button1Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel div 2;
end;

procedure TJvZoomMainForm.Button2Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel*2;
end;

procedure TJvZoomMainForm.FormCreate(Sender: TObject);
begin
  JvZoom1.Active := true;
end;

end.
