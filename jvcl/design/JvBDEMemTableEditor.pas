{$I JVCL.INC}
unit JvBDEMemTableEditor;

interface
uses
  Classes, DB,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  SysUtils, JvDBMemDatasetEditor, JvBDEMemTable;

type
  TJvBDEMemoryTableEditor = class(TJvAbstractMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

implementation
uses
  DsnDBCst, DSDesign, DBReg, JvJVCLUtils, JvSelectDataSetForm, JvConsts;

//=== TJvBDEMemoryTableEditor ===================================================

function TJvBDEMemoryTableEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvBDEMemoryTable;
  if Result then
    TJvBDEMemoryTable(Dest).CopyStructure(Source);
end;

end.
