unit JvSegmentedLEDDisplayMappingForm;

{$I JVCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, 
  {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JVBaseDsgnForm, JvSegmentedLEDDisplayMapperFrame, JvSegmentedLEDDisplay,
  JvBaseDsgnFrame;

type
  {$IFDEF COMPILER6_UP}
  IFormDesigner = IDesigner;
  {$ENDIF}
  TfrmJvSLDMappingEditor = class(TJvBaseDesign)
    fmeMapper: TfmeJvSegmentedLEDDisplayMapper;
    lblDigitClassCaption: TLabel;
    lblSegmentCountCaption: TLabel;
    lblCharCaption: TLabel;
    lblMapperValueCaption: TLabel;
    lblSegmentsCaption: TLabel;
    lblDigitClass: TLabel;
    lblSegmentCount: TLabel;
    lblChar: TLabel;
    lblMapperValue: TLabel;
    lblSegments: TLabel;
    btnOK: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FDesigner: IFormDesigner;
    function GetDisplay: TJvCustomSegmentedLEDDisplay;
    procedure SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
    procedure SetDesigner(Value: IFormDesigner);
  protected
    function DesignerFormName: string; override;
    function AutoStoreSettings: Boolean; override;
    procedure StoreSettings; override;
    procedure RestoreSettings; override;
    procedure UpdateDigitClass(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
    procedure MappingChanged(Sender: TObject);
  public
    { Public declarations }
    procedure Loaded; override;
    property Designer: IFormDesigner read FDesigner write SetDesigner;
    property Display: TJvCustomSegmentedLEDDisplay read GetDisplay write SetDisplay;
  end;

procedure EditSLDMapping(ADisplay: TJvCustomSegmentedLEDDisplay; ADesigner: IFormDesigner);

implementation

{$R *.DFM}

uses
  Registry;

function IsSLDMappingEditForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvSLDMappingEditor;
  if Result then
  begin
    with (Form as TfrmJvSLDMappingEditor) do
      Result := (Pointer(Display) = Args[0].VObject) and
        (Pointer(Designer) = Args[1].VInterface);
  end;
end;

procedure EditSLDMapping(ADisplay: TJvCustomSegmentedLEDDisplay; ADesigner: IFormDesigner);
var
  Form: TfrmJvSLDMappingEditor;
begin
  Form := TfrmJvSLDMappingEditor(GetDesignerForm(IsSLDMappingEditForm, [ADisplay, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvSLDMappingEditor.Create(nil);
    try
      Form.Display := ADisplay;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//===TfrmJvSLDMappingEditor=========================================================================

function TfrmJvSLDMappingEditor.GetDisplay: TJvCustomSegmentedLEDDisplay;
begin
  Result := fmeMapper.Display;
end;

procedure TfrmJvSLDMappingEditor.SetDisplay(Value: TJvCustomSegmentedLEDDisplay);
begin
  if Value <> Display then
    fmeMapper.Display := Value;
end;

procedure TfrmJvSLDMappingEditor.SetDesigner(Value: IFormDesigner);
begin
  if Value <> FDesigner then
    FDesigner := Value;
end;

function TfrmJvSLDMappingEditor.DesignerFormName: string;
begin
  Result := _('Segmented LED Display Mapping Editor');
end;

function TfrmJvSLDMappingEditor.AutoStoreSettings: Boolean;
begin
  Result := True;
end;

procedure TfrmJvSLDMappingEditor.StoreSettings;
begin
  inherited StoreSettings;
  with TRegistry.Create do
  try
    LazyWrite := False;
    if OpenKey(GetRegKey, True) then
    try
      WriteString('LastOpenFolder', fmeMapper.LastOpenFolder);
      WriteString('LastSaveFolder', fmeMapper.LastSaveFolder);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TfrmJvSLDMappingEditor.RestoreSettings;
begin
  inherited RestoreSettings;
  with TRegistry.Create do
  try
    if OpenKey(GetRegKey, False) then
    try
      if ValueExists('LastOpenFolder') then
        fmeMapper.LastOpenFolder := ReadString('LastOpenFolder');
      if ValueExists('LastSaveFolder') then
        fmeMapper.LastSaveFolder := ReadString('LastSaveFolder');
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TfrmJvSLDMappingEditor.UpdateDigitClass(Sender: TObject);
begin
  if fmeMapper.Display <> nil then
  begin
    lblDigitClass.Caption := fmeMapper.DigitClass.ClassName;
    lblSegmentCount.Caption := IntToStr(fmeMapper.DigitClass.SegmentCount);
  end
  else
  begin
    lblDigitClass.Caption := '';
    lblSegmentCount.Caption := '';
  end;
end;

procedure TfrmJvSLDMappingEditor.UpdateInfo(Sender: TObject);
begin
  with fmeMapper do
  begin
    if CharSelected then
    begin
      if CurChar in ['!' .. 'z'] then
        lblChar.Caption := CurChar + ' (#' + IntToStr(Ord(CurChar)) + ')'
      else
        lblChar.Caption := '#' + IntToStr(Ord(CurChar));
    end
    else
      lblChar.Caption := '';
    if Display <> nil then
    begin
      lblMapperValue.Caption := IntToStr(sldEdit.Digits[0].GetSegmentStates);
      lblSegments.Caption := sldEdit.Digits[0].GetSegmentString;
    end
    else
    begin
      lblMapperValue.Caption := '';
      lblSegments.Caption := '';
    end;
  end;
end;

procedure TfrmJvSLDMappingEditor.MappingChanged(Sender: TObject);
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure TfrmJvSLDMappingEditor.Loaded;
begin
  inherited Loaded;
  if fmeMapper <> nil then
  begin
    fmeMapper.OnMappingChanged := MappingChanged;
    fmeMapper.OnDisplayChanged := UpdateDigitClass;
    fmeMapper.OnInfoUpdate := UpdateInfo;
  end;
end;

procedure TfrmJvSLDMappingEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := fmeMapper.CanClose;
  if CanClose then
    inherited;
end;

end.
