{$I JVCL.INC}
unit JvScrollMaxEditor;

interface
uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF COMPILER6_UP};

type
  TJvScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;


implementation
uses
  JvScrollMax;


//=== TJvScrollMaxEditor =====================================================

function TJvScrollMaxEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvScrollMaxEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add Band'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvScrollMaxEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    Designer.CreateComponent(TJvScrollMaxBand, Component, 0, 0, 0, 50)
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvScrollMaxEditor.Edit;
begin
  // We don't need to add band on double click
end;


end.
