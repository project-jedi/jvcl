unit JvFillPropEdits;

{$I JVCL.INC}

interface

procedure RegFillerPropEdits;

implementation

{$IFNDEF COMPILER6_UP}

{ Since D5 doesn't support published properties of type interface (or rather, the OI/streaming
  system doesn't), D5 will use a simple TComponent property. A property editor is created that
  will only list components that support both the IFiller as well as the IInterfaceComponentRef
  interfaces. }

uses
  Classes, Consts, DsgnIntf, SysUtils, TypInfo,
  JvFillBasicImpl, JvFillIntf;
//  Dialogs; // for testing only!! remove later!!

type
  TInterfaceProperty = class(TComponentProperty)
  private
    FOrgStrProc: TGetStrProc;
    function IntfSupported(Component: TComponent): Boolean;
    procedure CheckAndAddComp(const S: string);
  protected
    function GetInterfaceGUID: TGUID; virtual; abstract;
    property OrgStrProc: TGetStrProc read FOrgStrProc write FOrgStrProc;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TFillerProperty = class(TInterfaceProperty)
  protected
    function GetInterfaceGUID: TGUID; override;
  end;

{ TInterfaceProperty }

function TInterfaceProperty.IntfSupported(Component: TComponent): Boolean;
var
  Ref: IUnknown;
begin
  with Component do
    Result := GetInterface(IInterfaceComponentReference, Ref) and GetInterface(GetInterfaceGUID, Ref);
end;

procedure TInterfaceProperty.CheckAndAddComp(const S: string);
var
  Comp: TComponent;
begin
  Comp := Designer.GetComponent(S);
  if (Comp <> nil) and IntfSupported(Comp) then
    OrgStrProc(S);
end;

procedure TInterfaceProperty.GetValues(Proc: TGetStrProc);
begin
  OrgStrProc := Proc;
  inherited GetValues(CheckAndAddComp);
end;

procedure TInterfaceProperty.SetValue(const Value: string);
var
  Comp: TComponent;
begin
  if Value = '' then
    Comp := nil
  else
  begin
    Comp := Designer.GetComponent(Value);
    if not (Comp is GetTypeData(GetPropType)^.ClassType) and not IntfSupported(Comp) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(Longint(Comp));
end;

{ TFillerProperty }

function TFillerProperty.GetInterfaceGUID: TGUID;
begin
  Result := IFiller;
end;
{$ENDIF COMPILER6_UP}

procedure RegFillerPropEdits;
begin
{$IFNDEF COMPILER6_UP}
  RegisterPropertyEditor(TypeInfo(TComponent), TComponent, 'Filler', TFillerProperty);
{$ENDIF COMPILER6_UP}
end;

end.
