unit ToolboxUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm;

type
  TToolboxForm = class(TForm)
    lbDockClient1: TJvDockClient;
    HTML_Panel: TPanel;
    Server_Objects_Panel: TPanel;
    Design_Time_Controls_Panel: TPanel;
    ActiveX_Controls_Panel: TPanel;
    General_Panel1: TPanel;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ToolboxForm: TToolboxForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TToolboxForm.FormResize(Sender: TObject);
begin
  HTML_Panel.Width := ClientWidth;
  Server_Objects_Panel.Width := ClientWidth;
  Design_Time_Controls_Panel.Width := ClientWidth;
  ActiveX_Controls_Panel.Width := ClientWidth;
  General_Panel1.Width := ClientWidth;
end;

end.
