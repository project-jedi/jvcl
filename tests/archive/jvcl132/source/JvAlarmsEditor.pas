
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAlarmsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF DELPHI5}DsgnIntf, {$ENDIF}{$IFDEF DELPHI6_UP}DesignEditors, DesignIntf, {$ENDIF}
  JvFormAlarms;

type
  TJvAlarmsEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

{*************************************************}

function TJvAlarmsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

{*************************************************}

function TJvAlarmsEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{*************************************************}

procedure TJvAlarmsEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{*************************************************}

procedure TJvAlarmsEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue('Click to edit...');
end;

{*************************************************}

procedure TJvAlarmsEditor.Edit;
var
  Dlg: TFormAlarm;
  Res: TStringList;
begin
  Res := TStringList(GetOrdValue);
  Dlg := TFormAlarm.Create(Application);
  Dlg.LoadFromStr(Res);
  try
    Dlg.Tag := 1;
    Dlg.ShowModal;
    if Dlg.Tag = 0 then
    begin
      Res.Assign(Dlg.SetFromStr);
      SetOrdValue(Integer(Res));
    end;
  finally
    Dlg.Free;
  end;
end;

end.
