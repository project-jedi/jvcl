{$I JVCL.INC}
unit JVHLEditEditor;

interface
uses
  Windows,
  SysUtils, Controls,
  JvEditor, JvHLEditor, JvHLEditorPropertyForm,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$IFDEF COMPLIB_VCL}
  VCLEditors,
  {$ENDIF COMPLIB_VCL}
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvFormPlacement, JvConsts, Classes;

resourcestring
  RS_JvHLEditorMsg = 'Please select "JvHLEditor" first';
  RS_JvHLEditorMsgTitle = 'Cannot edit';

type
  TJvHLEdPropDlgEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TJvHLEditorColorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  JvAppINIStore;

//=== TJvHLEdPropDlgEditor ===================================================

function TJvHLEdPropDlgEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvHLEdPropDlgEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := _('Execute')
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.Edit;
var
  PakName: string;
  NewRegAuto: TJvFormStorage;
  NewStore: TJvAppINIFileStore;
  OldRegAuto: TJvFormStorage;
begin
  if (Component as TJvHLEdPropDlg).JvHLEditor <> nil then
    begin
      NewRegAuto := nil;
      NewStore := nil;
      try
        NewRegAuto := TJvFormStorage.Create(nil);
        NewStore := TJvAppINIFileStore.Create(nil);
        NewRegAuto.AppStorage := NewStore;
        SetLength(PakName, MAX_PATH);
        SetLength(PakName, GetModuleFileName(hInstance, PChar(PakName), MAX_PATH));
        NewStore.FileName := ExtractFilePath(PakName) + srJvHLEdPropDlgIni;
        with Component as TJvHLEdPropDlg do
        begin
          OldRegAuto := RegAuto;
          try
            RegAuto := NewRegAuto;
            if Execute then
              Designer.Modified;
          finally
            RegAuto := OldRegAuto;
          end;
        end;
      finally
        NewRegAuto.Free;
        NewStore.Free;
      end;
    end
  else
    MessageBox(0, PChar(RS_JvHLEditorMsg), PChar(RS_JvHLEditorMsgTitle), MB_OK + MB_ICONERROR);
end;

procedure TJvHLEditorColorProperty.Edit;
begin
  with TJvHLEdPropDlg.Create(nil) do
    try
      JvHLEditor := GetComponent(0) as TJvHLEditor;
      HighlighterCombo := False;
      ReadFrom := rfHLEditor;
      Pages := [epColors];
      if Execute then
        Designer.Modified;
    finally
      Free;
    end;
end;

function TJvHLEditorColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

end.
