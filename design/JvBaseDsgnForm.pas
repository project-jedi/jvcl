unit JvBaseDsgnForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TJvBaseDesign = class(TForm)
  private
    { Private declarations }
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHowingChanged;
  protected
    { Determines the key to write the settings to or read from. Generally you don't need to override
      this method.
      Default will return (DELPHIRootKey)\Property Editors\(DesignerFormName)\(ClassName), where
        (DELPHIRootKey) is the root registry key for this Delphi version,
        (DesignerFormName) is the return value of said class function,
        (ClassName) is the return value of ClassName. }
    function GetRegKey: string; dynamic;
    { Editor name. Defaults to 'JEDI-VCL Editor' but should be renamed to an appropiate editor type
      name (e.g. 'Provider Editor' or 'Form Storage Editor'). }
    class function DesignerFormName: string; dynamic;
    { Determines if the settings for this class should be automatically stored/restored upon class
      destruction/streaming back in. Defaults to False. }
    class function AutoStoreSettings: Boolean; dynamic;
    { Store the settings for this form. Descendants that want to store additional settings should
      override this method. You should always call the inherited method (which stores the position
      and size information). }
    procedure StoreSettings; dynamic;
    { Restore the settings for this form. Descendants that want to restore additional settings
      should override this method. You should always call the inherited method (which restores the
      position and size information). }
    procedure RestoreSettings; dynamic;
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Loaded; override;
  end;

  TCompareDsgFunc = function(DsgnForm: TJvBaseDesign; const Args: array of const): Boolean;

function GetDesignerForm(CompareFunc: TCompareDsgFunc; const Args: array of const): TJvBaseDesign;

implementation

uses
  Registry,
  JvConsts;

{$R *.DFM}

var
  DsgnFrmList: TList;

function GetDesignerForm(CompareFunc: TCompareDsgFunc; const Args: array of const): TJvBaseDesign;
var
  I: Integer;
begin
  Result := nil;
  if (DsgnFrmList <> nil) and (@CompareFunc = nil) then
  begin
    I := DsgnFrmList.Count - 1;
    while (I >= 0) and not CompareFunc(TJvBaseDesign(DsgnFrmList[I]), Args) do
      Dec(I);
    if I >= 0 then
      Result := TJvBaseDesign(DsgnFrmList[I]);
  end
end;

procedure TJvBaseDesign.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and AutoStoreSettings then
  try
    if Showing then
      RestoreSettings
    else
      StoreSettings;
  except
    Application.HandleException(Self);
  end;
end;

function TJvBaseDesign.GetRegKey: string;
begin
  Result := SDelphiKey + '\Property Editors\' + Trim(DesignerFormName) + '\' + ClassName
end;

class function TJvBaseDesign.DesignerFormName: string;
begin
  Result := 'JEDI-VCL Editor';
end;

class function TJvBaseDesign.AutoStoreSettings: Boolean;
begin
  Result := False;
end;

procedure TJvBaseDesign.StoreSettings;
begin
  with TRegistry.Create do
  try
    LazyWrite := False;
    if OpenKey(GetRegKey, True) then
    try
      WriteInteger('Left', Left);
      WriteInteger('Top', Top);
      WriteInteger('Width', Width);
      WriteInteger('Height', Height);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TJvBaseDesign.RestoreSettings;
begin
  with TRegistry.Create do
  try
    if OpenKey(GetRegKey, False) then
    try
      if ValueExists('Width') then
        Width := ReadInteger('Width');
      if ValueExists('Height') then
        Height := ReadInteger('Height');
      if ValueExists('Left') then
        Left := ReadInteger('Left');
      if ValueExists('Top') then
        Top := ReadInteger('Top');
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TJvBaseDesign.AfterConstruction;
begin
  if DsgnFrmList = nil then
    DsgnFrmList := TList.Create;
  if DsgnFrmList.IndexOf(Self) < 0 then
    DsgnFrmList.Add(Self);
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TJvBaseDesign.BeforeDestruction;
begin
  inherited BeforeDestruction;
//  if AutoStoreSettings then
//    StoreSettings;
  DsgnFrmList.Remove(Self);
end;

procedure TJvBaseDesign.Loaded;
begin
  inherited Loaded;
//  if not (csDesigning in ComponentState) and AutoStoreSettings then
//    RestoreSettings;
end;

end.
 