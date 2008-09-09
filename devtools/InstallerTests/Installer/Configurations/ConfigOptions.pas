unit ConfigOptions;

interface

uses
  SysUtils, Classes, Contnrs, TypInfo, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, Buttons;

type
  TOptionControl = class;

  TOptionList = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TOptionControl;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AOption: TOptionControl): TOptionControl;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TOptionControl read GetItem; default;
  end;

  TOptionControl = class(TObject)
  public
    function CreateControl: TControl; virtual; abstract;
  end;

  TOptionSpacer = class(TOptionControl)
  public
    function CreateControl: TControl; override;
  end;

  TOptionFrame = class(TOptionControl)
  private
    FFrame: TFrame;
  public
    constructor Create(AFrame: TFrame);
    function CreateControl: TControl; override;
  end;

  TOption = class(TOptionControl)
  private
    FPropName: string;
    FInstance: TObject;
    FInfo: PPropInfo;
    FCaption: string;
    FHint: string;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: string;
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
  public
    constructor Create(AInstance: TObject; const APropName, ACaption, AHint: string);

    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property Instance: TObject read FInstance;
    property PropName: string read FPropName;
    property Caption: string read FCaption;
    property Hint: string read FHint;
  end;

  TOptionCheckBox = class(TOption)
  private
    procedure EvClick(Sender: TObject);
  public
    function CreateControl: TControl; override;
  end;

{  TOptionGroup = class(TOptionControl)
  end;

  TOptionRadioButton = class(TOption)
  end;

  TOptionEdit = class(TOption)
  end;}

  TOptionDirectoryEdit = class(TOption)
  public
    function CreateControl: TControl; override;
  end;

{  TOptionDropDown = class(TOption)
  end;

  TOptionDropDownList = class(TOptionDropDown)
  end;

  TOptionButton = class(TOption)
  end;

  TOptionCheckListBox = class(TOption)
  end;}

  EOptionError = class(Exception);

implementation

uses
  InstallerConsts, FrmeDirectoryEdit;

procedure InvalidOption(const PropName: string);
begin
  raise EOptionError.CreateFmt(RsInvalidOptionProperty, [PropName]);
end;

{ TOptionList }

constructor TOptionList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TOptionList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TOptionList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TOptionList.GetItem(Index: Integer): TOptionControl;
begin
  Result := TOptionControl(FItems[Index]);
end;

function TOptionList.Add(AOption: TOptionControl): TOptionControl;
begin
  FItems.Add(AOption);
  Result := AOption;
end;

{ TOption }

constructor TOption.Create(AInstance: TObject; const APropName, ACaption, AHint: string);
begin
  inherited Create;
  FInstance := AInstance;
  FPropName := APropName;
  FInfo := GetPropInfo(Instance, PropName);
  if not Assigned(FInfo) then
    InvalidOption(PropName);
  FCaption := ACaption;
  FHint := AHint;
end;

function TOption.GetAsInteger: Integer;
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      Result := GetOrdProp(Instance, FInfo);
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      Result := StrToInt(GetStrProp(Instance, FInfo));
    tkFloat:
      Result := Trunc(GetFloatProp(Instance, FInfo));
    tkWString:
      Result := StrToInt(GetWideStrProp(Instance, FInfo));
  else
    InvalidOption(PropName);
    Result := 0;
  end;
end;

procedure TOption.SetAsInteger(const Value: Integer);
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      SetOrdProp(Instance, FInfo, Value);
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      SetStrProp(Instance, FInfo, IntToStr(Value));
    tkFloat:
      SetFloatProp(Instance, FInfo, Value);
    tkWString:
      SetWideStrProp(Instance, FInfo, IntToStr(Value));
  else
    InvalidOption(PropName);
  end;
end;

function TOption.GetAsFloat: Double;
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      Result := GetOrdProp(Instance, FInfo);
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      Result := StrToFloat(GetStrProp(Instance, FInfo));
    tkFloat:
      Result := GetFloatProp(Instance, FInfo);
    tkWString:
      Result := StrToFloat(GetWideStrProp(Instance, FInfo));
  else
    InvalidOption(PropName);
    Result := 0;
  end;
end;

procedure TOption.SetAsFloat(const Value: Double);
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      SetOrdProp(Instance, FInfo, Trunc(Value));
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      SetStrProp(Instance, FInfo, FloatToStr(Value));
    tkFloat:
      SetFloatProp(Instance, FInfo, Value);
    tkWString:
      SetWideStrProp(Instance, FInfo, FloatToStr(Value));
  else
    InvalidOption(PropName);
  end;
end;

function TOption.GetAsString: string;
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      Result := IntToStr(GetOrdProp(Instance, FInfo));
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      Result := GetStrProp(Instance, FInfo);
    tkFloat:
      Result := FloatToStr(GetFloatProp(Instance, FInfo));
    tkWString:
      Result := GetWideStrProp(Instance, FInfo);
  else
    InvalidOption(PropName);
    Result := '';
  end;
end;

procedure TOption.SetAsString(const Value: string);
begin
  case FInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
      SetOrdProp(Instance, FInfo, StrToInt(Value));
    {$IFDEF UNICODE} tkUString, {$ENDIF UNICODE}
    tkString, tkLString:
      SetStrProp(Instance, FInfo, Value);
    tkFloat:
      SetFloatProp(Instance, FInfo, StrToFloat(Value));
    tkWString:
      SetWideStrProp(Instance, FInfo, Value);
  else
    InvalidOption(PropName);
  end;
end;

function TOption.GetAsBoolean: Boolean;
begin
  Result := Boolean(AsInteger);
end;

procedure TOption.SetAsBoolean(const Value: Boolean);
begin
  AsInteger := Integer(Value);
end;

{ TOptionSpacer }

function TOptionSpacer.CreateControl: TControl;
begin
  Result := TPanel.Create(nil);
  with TPanel(Result) do
  begin
    Height := 20;
    Caption := '';
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;
end;

{ TOptionFrame }

constructor TOptionFrame.Create(AFrame: TFrame);
begin
  inherited Create;
  FFrame := AFrame;
end;

function TOptionFrame.CreateControl: TControl;
begin
  Result := FFrame;
end;

{ TOptionDirectoryEdit }

function TOptionDirectoryEdit.CreateControl: TControl;
var
  Frame: TFrameDirectoryEdit;
begin
  Frame := TFrameDirectoryEdit.Create(nil);
  Frame.Option := Self;
  Result := Frame;
end;

{ TOptionCheckBox }

function TOptionCheckBox.CreateControl: TControl;
var
  cbx: TCheckBox;
begin
  cbx := TCheckBox.Create(nil);
  cbx.Caption := Caption;
  cbx.Hint := Hint;
  cbx.ShowHint := Hint <> '';
  cbx.OnClick := EvClick;
  cbx.Checked := AsBoolean;
  Result := cbx;
end;

procedure TOptionCheckBox.EvClick(Sender: TObject);
begin
  AsBoolean := (Sender as TCheckBox).Checked;
end;

end.
