{$I JVCL.INC}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ComCtrls,
  JvComponent, JvImageWindow, JvErrProvider, JvOLBar;

type
  // Example of a control that implements the IJvErrorProviderClient interface
  TJvErrorClientEdit = class(TEdit, IUnknown, IJvErrorProviderClient)
  private
    FErrorMessage: WideString;
    FErrorProvider: IJvErrorProvider;
    {$IFNDEF COMPILER6_UP}
    // D5 and below doesn't support interface properties, so we fake out with a TComponent property
    // and instead check the supported interfaces in the setErrorProviderComp
    FErrorProviderComp: TComponent;
    procedure setErrorProviderComp(const Value: TComponent);
    {$ENDIF}
    { IJvErrorProviderClient}
    procedure UpdateProvider;
    procedure ClearProvider;
  protected
    procedure setErrorProvider(const Value: IJvErrorProvider); virtual;
    function getErrorProvider: IJvErrorProvider; virtual;
    function getControl: TControl; virtual;
    procedure setErrorMessage(const Value: WideString); virtual;
    function getErrorMessage: WideString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    {$IFDEF COMPILER6_UP}
    property ErrorProvider: IJvErrorProvider read getErrorProvider write setErrorProvider;
    {$ELSE}
    property ErrorProvider: TComponent read FErrorProviderComp write setErrorProviderComp;
    {$ENDIF}
    property ErrorMessage: WideString read getErrorMessage write setErrorMessage;
  end;

  TfrmErrProviderDemo = class(TForm)
    btnClearErrors: TButton;
    memDescription: TMemo;
    il16: TImageList;
    btnShowErrors: TButton;
    gbOptions: TGroupBox;
    Label1: TLabel;
    cbImageAlignment: TComboBox;
    Label2: TLabel;
    edImagePadding: TEdit;
    udImagePadding: TUpDown;
    Label3: TLabel;
    cbBlinkStyle: TComboBox;
    Label4: TLabel;
    edBlinkRate: TEdit;
    udBlinkRate: TUpDown;
    btnUpdate: TButton;
    Label5: TLabel;
    edImageIndex: TEdit;
    udImageIndex: TUpDown;
    isPreview: TJvImageSquare;
    chkAutoScroll: TCheckBox;
    il32: TImageList;
    chkLarge: TCheckBox;
    lblClient: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnClearErrorsClick(Sender: TObject);
    procedure btnShowErrorsClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure udImageIndexClick(Sender: TObject; Button: TUDBtnType);
    procedure chkAutoScrollClick(Sender: TObject);
    procedure chkLargeClick(Sender: TObject);
  private
    { Private declarations }
    jep: TJvErrorProvider;
    edClient: TJvErrorClientEdit;
    procedure DoClientKey(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    { Public declarations }
  end;

var
  frmErrProviderDemo: TfrmErrProviderDemo;

implementation
uses
  JvEdit;

{$R *.dfm}

procedure TfrmErrProviderDemo.FormCreate(Sender: TObject);
begin
  cbImageAlignment.ItemIndex := 3;
  cbBlinkStyle.ItemIndex := 1;

  jep := TJvErrorProvider.Create(Self);
  jep.Imagelist := il16;
  isPreview.ImageList := TImageList(jep.Imagelist);
  jep.ImageIndex := udImageIndex.Position;
  isPreview.ImageIndex := udImageIndex.Position;
  udImageIndex.Max := jep.Imagelist.Count - 1;

  // Create an edit dynamically that implements the IJvErrorProviderClient interface
  // For this demo, hitting RETURN will display it's Text as an error message
  edClient := TJvErrorClientEdit.Create(Self);
  edClient.Parent := Self;
  edClient.SetBounds(lblClient.Left, lblClient.Top + lblClient.Height + 4,
    edClient.Width, edClient.Height);
  edClient.Name := 'JvErrorClientEdit1';
  edClient.ErrorProvider := jep;
  edClient.TabOrder := 2;
  edClient.Text := 'Type and hit RETURN to show this text as an error message';
  edClient.OnKeyUp := DoClientKey;
end;

procedure TfrmErrProviderDemo.DoClientKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    // you must set both ErrorProvider <> nil and ErrorMessage <> '' to trigger the ErrorProvider:
    if edClient.Text = '' then
      edClient.ErrorMessage := 'Please supply a value!'
    else
      edClient.ErrorMessage := 'You typed: "' + edClient.Text + '"';
  end;
end;

procedure TfrmErrProviderDemo.btnClearErrorsClick(Sender: TObject);
begin
  jep.ClearErrors;
end;

procedure TfrmErrProviderDemo.btnShowErrorsClick(Sender: TObject);
var
  i: integer;
begin
  jep.BeginUpdate; // suspend blinking until all controls have been updated
  // update any previous controls
  btnUpdate.Click;
  // only show error icons for wincontrols since the form gets crowded anyway...
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TWinControl)
      // avoid duplicate icons for the edit/updown combos:
    and (not (Components[i] is TEdit) or (Components[i] is TJvErrorClientEdit)) then
      jep.Error[TWinControl(Components[i])] := Format('Example error message for %s',
        [TWinControl(Components[i]).Name]);
  // update any option changes
  btnUpdate.Click;
  jep.EndUpdate; // restart blinking
end;

procedure TfrmErrProviderDemo.btnUpdateClick(Sender: TObject);
begin
  jep.ImageAlignment[nil] := TJvErrorImageAlignment(cbImageAlignment.ItemIndex);
  jep.ImagePadding[nil] := udImagePadding.Position;
  jep.BlinkStyle := TJvErrorBlinkStyle(cbBlinkStyle.ItemIndex);
  jep.BlinkRate := udBlinkRate.Position;
  jep.ImageIndex := udImageIndex.Position;
end;

procedure TfrmErrProviderDemo.udImageIndexClick(Sender: TObject; Button: TUDBtnType);
begin
  isPreview.ImageIndex := udImageIndex.Position;
end;

procedure TfrmErrProviderDemo.chkAutoScrollClick(Sender: TObject);
begin
  AutoScroll := chkAutoScroll.Checked;
end;

procedure TfrmErrProviderDemo.chkLargeClick(Sender: TObject);
begin
  if chkLarge.Checked then
    jep.Imagelist := il32
  else
    jep.Imagelist := il16;
  isPreview.ImageList := TImageList(jep.Imagelist);
  udImageIndex.Max := jep.Imagelist.Count - 1;
end;

{ TJvErrorClientEdit }

procedure TJvErrorClientEdit.ClearProvider;
var
  tmp: string;
begin
  if (FErrorProvider <> nil) and not (csFreeNotification in ComponentState) then
  begin
    tmp := FErrorMessage;
    FErrorMessage := '';
    FErrorProvider.SetClientError(Self);
    FErrorMessage := tmp;
  end;
end;

function TJvErrorClientEdit.getControl: TControl;
begin
  Result := Self;
end;

function TJvErrorClientEdit.getErrorMessage: WideString;
begin
  Result := FErrorMessage;
end;

function TJvErrorClientEdit.getErrorProvider: IJvErrorProvider;
begin
  Result := FErrorProvider;
end;

procedure TJvErrorClientEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$IFDEF COMPILER6_UP}
    if (Assigned(ErrorProvider)) and (AComponent.IsImplementorOf(ErrorProvider)) then
      ErrorProvider := nil;
    {$ELSE}
    if AComponent = ErrorProvider then
      ErrorProvider := nil;
    {$ENDIF}
  end;
end;

procedure TJvErrorClientEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  ClearProvider;
  UpdateProvider;
end;

procedure TJvErrorClientEdit.setErrorMessage(const Value: WideString);
begin
  FErrorMessage := Value;
  UpdateProvider;
end;

procedure TJvErrorClientEdit.setErrorProvider(const Value: IJvErrorProvider);
begin
  ClearProvider;
  {$IFDEF COMPILER6_UP}
  ReferenceInterface(FErrorProvider, opRemove);
  FErrorProvider := Value;
  ReferenceInterface(FErrorProvider, opInsert);
  {$ELSE}
  FErrorProvider := Value;
  {$ENDIF}
  UpdateProvider;
end;

{$IFNDEF COMPILER6_UP}

procedure TJvErrorClientEdit.setErrorProviderComp(const Value: TComponent);
var
  obj: IJvErrorProvider;
begin
  if FErrorProviderComp <> Value then
  begin
    if FErrorProviderComp <> nil then
      FErrorProviderComp.RemoveFreeNotification(Self);
    if Value = nil then
    begin
      FErrorProviderComp := nil;
      setErrorProvider(nil);
      Exit;
    end;
    if not Supports(Value, IJvErrorProvider, obj) then
      Exception.CreateFmt('%s does not support the IJvErrorProvider interface', [Value.Name]);
    FErrorProviderComp := Value;
    setErrorProvider(obj);
    if FErrorProviderComp <> nil then
      FErrorProviderComp.FreeNotification(Self);
  end;
end;
{$ENDIF}

procedure TJvErrorClientEdit.UpdateProvider;
begin
  if (FErrorProvider <> nil) then
    FErrorProvider.SetClientError(Self);
end;

end.

