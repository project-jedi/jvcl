unit JvAppletProperty;
{$I JVCL.INC}

interface
uses
  Windows,
  Classes,
  Controls,
  Forms,
  Dialogs,
  {$IFNDEF COMPILER6_UP}
  DsgnIntf
  {$ELSE}
  DesignIntf,
  DesignEditors
  {$ENDIF}
  ;


type
  TAppletFileProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

procedure TAppletFileProperty.Edit;
var
  APFileOpen: TOpenDialog;
begin
  APFileOpen := TOpenDialog.Create(Application);
  APFileOpen.Filename := GetValue;
  APFileOpen.Filter := 'Applet File (*.cpl)|*.cpl';
  APFileOpen.Options := APFileOpen.Options + [ofPathMustExist,
    ofFileMustExist];
  try
    if APFileOpen.Execute then SetValue(APFileOpen.Filename);
  finally
    APFileOpen.Free;
  end;
end;

function TAppletFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
