unit JvDebugForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvEditor, JvHLEditor;

type
  TJvDebugLog  = class(TForm)
    memDebug: TJvHLEditor;
    Panel1: TPanel;
    cbDebug: TCheckBox;
  public
  end;

var
  DebugLog: TJvDebugLog;

implementation

{$R *.dfm}

end.
