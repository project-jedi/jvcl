{$I JVCL.INC}
unit MainFrm;

interface
                  
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ComCtrls,
  JvComponent, JvImageWindow, JvErrProvider, JvOLBar;

type
  // Example of a control that implements the IErrorProviderClient interface
  TJvErrorClientEdit = class(TEdit, IErrorProviderClient)
  private
    FErrorMessage:WideString;
    FErrorProvider:IErrorProvider;
    procedure setErrorProvider(const Value:IErrorProvider);
    function getErrorProvider:IErrorProvider;
    function getControl:TControl;
    procedure setErrorMessage(const Value:WideString);
    function getErrorMessage:WideString;
    procedure UpdateProvider;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;

  published
    property ErrorProvider:IErrorProvider read getErrorProvider write setErrorProvider;
    property ErrorMessage:WideString read getErrorMessage write setErrorMessage;
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
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    jep:TJvErrorProvider;
    edClient:TJvErrorClientEdit;
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

  jep := TJvErrorProvider.Create(self);
  jep.Imagelist := il16;
  isPreview.ImageList := TImageList(jep.Imagelist);
  jep.ImageIndex := udImageIndex.Position;
  isPreview.ImageIndex := udImageIndex.Position;
  udImageIndex.Max := jep.Imagelist.Count-1;

  // Create an edit dynamically that implements the IErrorProviderClient
  // For this demo, hitting RETURN will display it's Text as an error message
  edClient := TJvErrorClientEdit.Create(self);
  edClient.Parent := self;
  edClient.SetBounds(lblClient.Left,lblClient.Top + lblClient.Height + 4,
    edClient.Width,edClient.Height);
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
var i:integer;
begin
  jep.BeginUpdate; // suspend blinking until all controls have been updated
  // update any previous controls
  btnUpdate.Click;
  // only show error icons for wincontrols since the form gets crowded anyway...
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TWinControl)
      // avoid duplicate icons for the edit/updown combos:
      and (not (Components[i] is TEdit) or (Components[i] is TJvErrorClientEdit)) then
        jep.Error[TWinControl(Components[i])] := Format('Example error message for %s',[TWinControl(Components[i]).Name]);
  // update any option changes
  btnUpdate.Click;
  jep.EndUpdate; // restart blinking
end;

procedure TfrmErrProviderDemo.btnUpdateClick(Sender: TObject);
begin
  jep.ImageAlignment[nil] := TJvErrorImageAlignment(cbImageAlignment.ItemIndex);
  jep.ImagePadding[nil]   := udImagePadding.Position;
  jep.BlinkStyle         := TJvErrorBlinkStyle(cbBlinkStyle.ItemIndex);
  jep.BlinkRate          := udBlinkRate.Position;
  jep.ImageIndex         := udImageIndex.Position;
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

function TJvErrorClientEdit.getControl: TControl;
begin
  Result := self;
end;

function TJvErrorClientEdit.getErrorMessage: WideString;
begin
  Result := FErrorMessage;
end;

function TJvErrorClientEdit.getErrorProvider: IErrorProvider;
begin
  Result := FErrorProvider;
end;

procedure TJvErrorClientEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  {$IFDEF COMPILER6_UP}
  if (Assigned(ErrorProvider)) and (AComponent.IsImplementorOf(ErrorProvider)) then
    ErrorProvider := nil;
  {$ENDIF}
end;

procedure TJvErrorClientEdit.setErrorMessage(const Value: WideString);
begin
  FErrorMessage := Value;
  UpdateProvider;
end;

procedure TJvErrorClientEdit.setErrorProvider(const Value: IErrorProvider);
begin
  {$IFDEF COMPILER6_UP}
  ReferenceInterface(FErrorProvider, opRemove);
  FErrorProvider := Value;
  ReferenceInterface(FErrorProvider, opInsert);
  {$ELSE}
  FErrorProvider := Value; // is this how it's handled in D5, i.e not at all?
  {$ENDIF}
  UpdateProvider;
end;

procedure TJvErrorClientEdit.UpdateProvider;
begin
  if ErrorProvider <> nil then
    ErrorProvider.SetClientError(self);
end;

procedure TfrmErrProviderDemo.FormDestroy(Sender: TObject);
begin
  {$IFNDEF COMPILER6_UP}
  edClient.ErrorProvider := nil;
  {$ENDIF}
end;

end.
