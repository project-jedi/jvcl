{$I JVCL.INC}
unit JvMemTableEditor;

interface
uses
  Classes, DB,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  SysUtils, JvMemTable;

type
  TJvMemDataSetEditor = class(TComponentEditor)
  private
    function UniqueName(Field: TField): string;
    procedure BorrowStructure;
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvMemoryTableEditor = class(TJvMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

implementation
uses
  DsnDBCst, DSDesign, DBReg, JvVCLUtils, JvSelDSFrm, JvxDConst;

//=== TJvMemDataSetEditor ====================================================

procedure TJvMemDataSetEditor.BorrowStructure;
var
  DataSet: TDataSet;
  I: Integer;
  Caption: string;
begin
  Caption := Component.Name;
  if (Component.Owner <> nil) and (Component.Owner.Name <> '') then
    Caption := Format({$IFDEF CBUILDER} '%s->%s' {$ELSE} '%s.%s' {$ENDIF},
      [Component.Owner.Name, Caption]);
  DataSet := SelectDataSet(Designer, Caption, TDataSet(Component));
  if DataSet <> nil then
  begin
    StartWait;
    try
      if not CopyStructure(DataSet, Component as TDataSet) then
        Exit;
      with TDataSet(Component) do
      begin
        for I := 0 to FieldCount - 1 do
          if Fields[I].Name = '' then
            Fields[I].Name := UniqueName(Fields[I]);
      end;
    finally
      StopWait;
    end;
    Designer.Modified;
  end;
end;

function TJvMemDataSetEditor.UniqueName(Field: TField): string;
const
  AlphaNumeric = ['A'..'Z', 'a'..'z', '_'] + ['0'..'9'];
var
  Temp: string;
  Comp: TComponent;
  I: Integer;
begin
  Result := '';
  if (Field <> nil) then
  begin
    Temp := Field.FieldName;
    for I := Length(Temp) downto 1 do
      if not (Temp[I] in AlphaNumeric) then
        System.Delete(Temp, I, 1);
    if (Temp = '') or not IsValidIdent(Temp) then
    begin
      Temp := Field.ClassName;
      if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
        System.Delete(Temp, 1, 1);
    end;
  end
  else
    Exit;
  Temp := Component.Name + Temp;
  {$IFDEF COMPILER6_UP}
  Comp := Designer.GetComponent(Temp);
  if (Comp = nil) or (Comp = Field) then
    Result := Temp
  else
    Result := Designer.UniqueName(Temp);
  {$ELSE}
  I := 0;
  repeat
    Result := Temp;
    if I > 0 then
      Result := Result + IntToStr(I);
    Comp := Designer.Form.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Field);
  {$ENDIF}
end;

procedure TJvMemDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    {$IFDEF COMPILER5_UP}
    0:
      ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
    {$ELSE}
    0:
      ShowDatasetDesigner(Designer, TDataSet(Component));
    {$ENDIF}
    1:
      BorrowStructure;
  end;
end;

function TJvMemDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := ResStr(SDatasetDesigner);
    1:
      Result := srBorrowStructure;
  end;
end;

function TJvMemDataSetEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//=== TJvMemoryTableEditor ===================================================

function TJvMemoryTableEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvMemoryTable;
  if Result then
    TJvMemoryTable(Dest).CopyStructure(Source);
end;

end.
