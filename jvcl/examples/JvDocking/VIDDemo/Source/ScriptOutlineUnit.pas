unit ScriptOutlineUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TScriptOutlineForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptOutlineForm: TScriptOutlineForm;

implementation

uses MainFormUnit;

{$R *.DFM}

end.
