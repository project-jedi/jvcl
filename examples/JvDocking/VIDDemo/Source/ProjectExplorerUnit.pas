unit ProjectExplorerUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm;

type
  TProjectExplorerForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    TreeView1: TTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProjectExplorerForm: TProjectExplorerForm;

implementation

uses MainFormUnit;

{$R *.DFM}

end.
