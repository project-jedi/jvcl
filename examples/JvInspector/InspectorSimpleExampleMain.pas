unit InspectorSimpleExampleMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JvComponent, JvInspector;

type
  TForm1 = class(TForm)
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspector1: TJvInspector;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  JvInspector1.Clear;
  JvInspector1.AddComponent(Self,'A Form Inspecting Itself', true {expanded?});
end;

end.
