unit JvZoomU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvZoom, ExtCtrls, JvComponent, JvCaptionPanel;
  
type
  TJvZoomForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    JvZoom1: TJvZoom;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  JvZoomForm: TJvZoomForm;

implementation

{$R *.DFM}

procedure TJvZoomForm.CheckBox1Click(Sender: TObject);
begin
  JvZoom1.active := checkbox1.checked;
end;

procedure TJvZoomForm.Button1Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel div 2;
end;

procedure TJvZoomForm.Button2Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel*2;
end;

procedure TJvZoomForm.FormCreate(Sender: TObject);
begin
 JvZoom1.Active := true;
end;

end.
