unit JvDotNetCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, JvDotNetControls, JvDBDotNetControls;
{$R ..\Resources\JvDotNetCtrlsReg.dcr}

{-----------------------------------------------------------------------------
  Procedure: Register
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Jv DotNet', [TJvDotNetCheckListBox,
    TJvDotNetEdit, TJvDotNetHotKey, TJvDotNetListBox,
    TJvDotNetListView, TJvDotNetMaskEdit, TJvDotNetMemo,
    TJvDotNetRichEdit, TJvDotNetScrollBox, TJvDotNetTreeView]);
  RegisterComponents('Jv DotNet DB', [TJvDotNetDBEdit, TJvDotNetDBListBox,
    TJvDotNetDBLookupListBox, TJvDotNetDBMemo, TJvDotNetDBRichEdit]);
end;

end.
