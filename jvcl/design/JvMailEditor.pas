{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvMailEditor;

interface
uses
  Windows, SysUtils, Classes, Dlgs, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  JvMail;
  
type
  TJvMailEditor = class(TComponentEditor)
  private
    procedure Address;
    procedure SendMail;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


resourcestring
  sSend = 'Send';
  sAddress = 'Address';

implementation

//=== TJvMailEditor ==========================================================

procedure TJvMailEditor.Address;
begin
  with Component as TJvMail do
  try
    Address(Owner.Name + '.' + Name);
  finally
    FreeSimpleMapi;
  end;
end;

procedure TJvMailEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      SendMail;
    1:
      Address;
  end;
end;

function TJvMailEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := sSend;
    1:
      Result := sAddress;
  end;
end;

function TJvMailEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TJvMailEditor.SendMail;
begin
  with Component as TJvMail do
  try
    SendMail;
  finally
    FreeSimpleMapi;
  end;
end;

end.
