unit glHelpPanel_demo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvgHelpPanel, JvComponent;

type
  TfTglHelpPanel = class(TForm)
    glHelpPanel1: TJvgHelpPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fTglHelpPanel: TfTglHelpPanel;

implementation

{$R *.DFM}

end.
