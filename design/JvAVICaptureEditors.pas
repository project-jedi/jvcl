{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvAVICaptureEditors;

interface

uses Windows, VFW, JvAVICapture,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes;

type
  TJvDriverIndexEditor = class(TIntegerProperty)
  protected
    FDrivers : TStringList;

    procedure EnumDrivers;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;

  end;

  TJvVirtualKeyEditor = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

implementation

uses JvVirtualKeyEditorForm, Controls;

{ TJvDriverIndexEditor }

constructor TJvDriverIndexEditor.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FDrivers := TStringList.Create;
  EnumDrivers;
end;

destructor TJvDriverIndexEditor.Destroy;
begin
  FDrivers.Free;
  inherited;
end;

procedure TJvDriverIndexEditor.EnumDrivers;
var i : Integer;
  deviceName : array [0..MAX_PATH] of char;
  deviceVersion : array [0..MAX_PATH] of char;
begin
  // no more than 10 drivers in the system (cf Win32 API)
  for i := 0 to 9 do
  begin
    if capGetDriverDescription(i, deviceName, sizeof(deviceName), deviceVersion, sizeof(deviceVersion)) then
    begin
      FDrivers.Add(deviceName);
    end;
  end;
end;

function TJvDriverIndexEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

function TJvDriverIndexEditor.GetValue: string;
var Index : TJvDriverIndex;
begin
  Index := GetOrdValue;
  if Index = -1 then
    Result := IntToStr(GetOrdValue) + ' - Disconnected'
  else
    Result := IntToStr(GetOrdValue) + ' - ' + FDrivers[GetOrdValue];
end;

procedure TJvDriverIndexEditor.GetValues(Proc: TGetStrProc);
var i : Integer;
begin
  Proc('-1 - Disconnected');
  for i := 0 to FDrivers.Count-1 do
  begin
    Proc(IntToStr(i)+' - '+FDrivers[i]);
  end;
end;

procedure TJvDriverIndexEditor.SetValue(const Value: string);
var NewIndex : Integer;
begin
  // only consider string until the first space
  // then convert it into a integer
  NewIndex := StrToInt(Copy(Value, 1, Pos(' ', Value)-1));

  // check its validity
  if (NewIndex >= -1) and
    (NewIndex < FDrivers.Count) then
  begin
    SetOrdValue(NewIndex);
  end
  else
  begin
    raise ERangeError.CreateFmt(
      '%d is not within the valid range of %d..%d',
      [NewIndex, -1, FDrivers.Count-1]);
  end;
end;

{ TJvVirtualKeyEditor }

procedure TJvVirtualKeyEditor.Edit;
begin
  with TfrmJvVirtualKeyEditor.Create(nil) do
  begin
    EditingFrame.CombinedKeyCode := GetOrdValue;
    if ShowModal = mrOk then
    begin
      SetOrdValue(EditingFrame.CombinedKeyCode);
    end;
    Free;
  end;
end;

function TJvVirtualKeyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TJvVirtualKeyEditor.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TJvVirtualKeyEditor.SetValue(const Value: string);
begin
  SetOrdValue(StrToInt(value));
end;

end.
