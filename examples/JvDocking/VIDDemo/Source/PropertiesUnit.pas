unit PropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, Grids;

type
  TPropertiesForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ComboBox: TComboBox;
    Panel1: TPanel;
    StringGrid: TStringGrid;
    procedure PanelResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TPropertiesForm.PanelResize(Sender: TObject);
begin
  ComboBox.Left := 0;
  ComboBox.Width := Panel.Width;
end;

procedure TPropertiesForm.FormResize(Sender: TObject);
begin
  StringGrid.ColWidths[0] := (StringGrid.Width - 23) div 2;
  StringGrid.ColWidths[1] := (StringGrid.Width - 23) div 2;
end;

end.
