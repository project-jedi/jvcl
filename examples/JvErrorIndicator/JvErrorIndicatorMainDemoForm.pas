{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$I jvcl.inc}

unit JvErrorIndicatorMainDemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ComCtrls,
  JvComponent, JvErrorIndicator, JvExControls, JvImageSquare;

type
  // Example of a control that implements the IJvErrorIndicatorClient interface
  TJvErrorClientEdit = class(TEdit, IUnknown, IJvErrorIndicatorClient)
  private
    FErrorMessage: WideString;
    FErrorIndicator: IJvErrorIndicator;
    {$IFNDEF COMPILER6_UP}
    // D5 and below doesn't support interface properties, so we fake out with a TComponent property
    // and instead check the supported interfaces in the setErrorIndicatorComp
    FErrorIndicatorComp: TComponent;
    procedure setErrorIndicatorComp(const Value: TComponent);
    {$ENDIF}
    { IJvErrorIndicatorClient}
    procedure UpdateProvider;
    procedure ClearProvider;
  protected
    procedure setErrorIndicator(const Value: IJvErrorIndicator); virtual;
    function getErrorIndicator: IJvErrorIndicator; virtual;
    function getControl: TControl; virtual;
    procedure setErrorMessage(const Value: WideString); virtual;
    function getErrorMessage: WideString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    {$IFDEF COMPILER6_UP}
    property ErrorIndicator: IJvErrorIndicator read getErrorIndicator write setErrorIndicator;
    {$ELSE}
    property ErrorIndicator: TComponent read FErrorIndicatorComp write setErrorIndicatorComp;
    {$ENDIF}
    property ErrorMessage: WideString read getErrorMessage write setErrorMessage;
  end;

  TJvErrorIndicatorMainDemoFrm  = class(TForm)
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
    jep: TJvErrorIndicator;
    edClient: TJvErrorClientEdit;
    procedure DoClientKey(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
  end;

var
  JvErrorIndicatorMainDemoFrm: TJvErrorIndicatorMainDemoFrm;

implementation

uses
  JvEdit;

{$R *.dfm}

procedure TJvErrorIndicatorMainDemoFrm .FormCreate(Sender: TObject);
begin
  cbImageAlignment.ItemIndex := 3;
  cbBlinkStyle.ItemIndex := 1;

  jep := TJvErrorIndicator.Create(Self);
  jep.Images := il16;
  isPreview.Images := TImageList(jep.Images);
  jep.ImageIndex := udImageIndex.Position;
  isPreview.ImageIndex := udImageIndex.Position;
  udImageIndex.Max := jep.Images.Count - 1;

  // Create an edit dynamically that implements the IJvErrorIndicatorClient interface
  // For this demo, hitting RETURN will display it's Text as an error message
  edClient := TJvErrorClientEdit.Create(Self);
  edClient.Parent := Self;
  edClient.SetBounds(lblClient.Left, lblClient.Top + lblClient.Height + 4,
    edClient.Width, edClient.Height);
  edClient.Name := 'JvErrorClientEdit1';
  edClient.ErrorIndicator := jep;
  edClient.TabOrder := 2;
  edClient.Text := 'Type and hit RETURN to show this text as an error message';
  edClient.OnKeyUp := DoClientKey;
  memDescription.WordWrap := true;
end;

procedure TJvErrorIndicatorMainDemoFrm .DoClientKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    // you must set both ErrorIndicator <> nil and ErrorMessage <> '' to trigger the ErrorIndicator:
    if edClient.Text = '' then
      edClient.ErrorMessage := 'Please supply a value!'
    else
      edClient.ErrorMessage := 'You typed: "' + edClient.Text + '"';
  end;
end;

procedure TJvErrorIndicatorMainDemoFrm .btnClearErrorsClick(Sender: TObject);
begin
  jep.ClearErrors;
end;

procedure TJvErrorIndicatorMainDemoFrm .btnShowErrorsClick(Sender: TObject);
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

procedure TJvErrorIndicatorMainDemoFrm .btnUpdateClick(Sender: TObject);
begin
  jep.ImageAlignment[nil] := TJvErrorImageAlignment(cbImageAlignment.ItemIndex);
  jep.ImagePadding[nil] := udImagePadding.Position;
  jep.BlinkStyle := TJvErrorBlinkStyle(cbBlinkStyle.ItemIndex);
  jep.BlinkRate := udBlinkRate.Position;
  jep.ImageIndex := udImageIndex.Position;
end;

procedure TJvErrorIndicatorMainDemoFrm .udImageIndexClick(Sender: TObject; Button: TUDBtnType);
begin
  isPreview.ImageIndex := udImageIndex.Position;
end;

procedure TJvErrorIndicatorMainDemoFrm .chkAutoScrollClick(Sender: TObject);
begin
  AutoScroll := chkAutoScroll.Checked;
end;

procedure TJvErrorIndicatorMainDemoFrm .chkLargeClick(Sender: TObject);
begin
  if chkLarge.Checked then
    jep.Images := il32
  else
    jep.Images := il16;
  isPreview.Images := TImageList(jep.Images);
  udImageIndex.Max := jep.Images.Count - 1;
end;

{ TJvErrorClientEdit }

procedure TJvErrorClientEdit.ClearProvider;
var
  tmp: string;
begin
  if (FErrorIndicator <> nil) and not (csFreeNotification in ComponentState) then
  begin
    tmp := FErrorMessage;
    FErrorMessage := '';
    FErrorIndicator.SetClientError(Self);
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

function TJvErrorClientEdit.getErrorIndicator: IJvErrorIndicator;
begin
  Result := FErrorIndicator;
end;

procedure TJvErrorClientEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$IFDEF COMPILER6_UP}
    if (Assigned(ErrorIndicator)) and (AComponent.IsImplementorOf(ErrorIndicator)) then
      ErrorIndicator := nil;
    {$ELSE}
    if AComponent = ErrorIndicator then
      ErrorIndicator := nil;
    {$ENDIF}
  end;
end;

procedure TJvErrorClientEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

procedure TJvErrorClientEdit.setErrorIndicator(const Value: IJvErrorIndicator);
begin
  ClearProvider;
  {$IFDEF COMPILER6_UP}
  ReferenceInterface(FErrorIndicator, opRemove);
  FErrorIndicator := Value;
  ReferenceInterface(FErrorIndicator, opInsert);
  {$ELSE}
  FErrorIndicator := Value;
  {$ENDIF}
  UpdateProvider;
end;

{$IFNDEF COMPILER6_UP}

procedure TJvErrorClientEdit.setErrorIndicatorComp(const Value: TComponent);
var
  obj: IJvErrorIndicator;
begin
  if FErrorIndicatorComp <> Value then
  begin
    if FErrorIndicatorComp <> nil then
      FErrorIndicatorComp.RemoveFreeNotification(Self);
    if Value = nil then
    begin
      FErrorIndicatorComp := nil;
      setErrorIndicator(nil);
      Exit;
    end;
    if not Supports(Value, IJvErrorIndicator, obj) then
      Exception.CreateFmt('%s does not support the IJvErrorIndicator interface', [Value.Name]);
    FErrorIndicatorComp := Value;
    setErrorIndicator(obj);
    if FErrorIndicatorComp <> nil then
      FErrorIndicatorComp.FreeNotification(Self);
  end;
end;
{$ENDIF}

procedure TJvErrorClientEdit.UpdateProvider;
begin
  if (FErrorIndicator <> nil) then
    FErrorIndicator.SetClientError(Self);
end;

end.

